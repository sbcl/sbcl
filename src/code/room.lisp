;;;; heap-grovelling memory usage stuff

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'sb-sys::get-page-size "SB-SYS"))

;;;; type format database

(defstruct (room-info (:constructor make-room-info (mask name kind))
                      (:copier nil))
    ;; the mask applied to HeaderValue to compute object size
    (mask 0 :type (and fixnum unsigned-byte) :read-only t)
    ;; the name of this type
    (name nil :type symbol :read-only t)
    ;; kind of type (how to reconstitute an object)
    (kind nil
          :type (member :other :closure :instance :list :code :fdefn)
          :read-only t))

(defun room-info-type-name (info)
    (if (specialized-array-element-type-properties-p info)
        (saetp-primitive-type-name info)
        (room-info-name info)))

(defconstant tiny-boxed-size-mask #xFF)
(defun !compute-room-infos ()
  (let ((infos (make-array 256 :initial-element nil))
        (default-size-mask (mask-field (byte 23 0) -1)))
    (dolist (obj *primitive-objects*)
      (let ((widetag (primitive-object-widetag obj))
            (lowtag (primitive-object-lowtag obj))
            (name (primitive-object-name obj)))
        (when (and (eq lowtag 'other-pointer-lowtag)
                   (not (member widetag '(t nil))))
          (setf (svref infos (symbol-value widetag))
                (case name
                 (fdefn  (make-room-info 0 name :fdefn))
                 (symbol (make-room-info tiny-boxed-size-mask name :other))
                 (t      (make-room-info default-size-mask name :other)))))))

    (let ((info (make-room-info default-size-mask 'array-header :other)))
      (dolist (code (list #+sb-unicode complex-character-string-widetag
                          complex-base-string-widetag simple-array-widetag
                          complex-bit-vector-widetag complex-vector-widetag
                          complex-array-widetag complex-vector-nil-widetag))
        (setf (svref infos code) info)))

    (setf (svref infos bignum-widetag)
          ;; Lose 1 more bit than n-widetag-bits because fullcgc robs 1 bit,
          ;; not that this is expected to work concurrently with gc.
          (make-room-info (ash most-positive-word (- (1+ n-widetag-bits)))
                          'bignum :other))

    (setf (svref infos closure-widetag)
          (make-room-info short-header-max-words 'closure :closure))

    (dotimes (i (length *specialized-array-element-type-properties*))
      (let ((saetp (aref *specialized-array-element-type-properties* i)))
        (when (saetp-specifier saetp) ;; SIMPLE-ARRAY-NIL is a special case.
          (setf (svref infos (saetp-typecode saetp)) saetp))))

    (setf (svref infos simple-array-nil-widetag)
          (make-room-info 0 'simple-array-nil :other))

    (setf (svref infos code-header-widetag)
          (make-room-info 0 'code :code))

    (setf (svref infos instance-widetag)
          (make-room-info 0 'instance :instance))

    (setf (svref infos funcallable-instance-widetag)
          (make-room-info short-header-max-words 'funcallable-instance :closure))

    (let ((cons-info (make-room-info 0 'cons :list)))
      ;; A cons consists of two words, both of which may be either a
      ;; pointer or immediate data.  According to the runtime this means
      ;; either a fixnum, a character, an unbound-marker, a single-float
      ;; on a 64-bit system, or a pointer.
      (dotimes (i (ash 1 (- n-widetag-bits n-fixnum-tag-bits)))
        (setf (svref infos (ash i n-fixnum-tag-bits)) cons-info))

      (dotimes (i (ash 1 (- n-widetag-bits n-lowtag-bits)))
        (setf (svref infos (logior (ash i n-lowtag-bits) instance-pointer-lowtag))
              cons-info)
        (setf (svref infos (logior (ash i n-lowtag-bits) list-pointer-lowtag))
              cons-info)
        (setf (svref infos (logior (ash i n-lowtag-bits) fun-pointer-lowtag))
              cons-info)
        (setf (svref infos (logior (ash i n-lowtag-bits) other-pointer-lowtag))
              cons-info))

      (setf (svref infos character-widetag) cons-info)

      (setf (svref infos unbound-marker-widetag) cons-info)

      ;; Single-floats are immediate data on 64-bit systems.
      #+64-bit (setf (svref infos single-float-widetag) cons-info))

    infos))

(define-load-time-global *room-info* (!compute-room-infos))

(defconstant-eqx +heap-spaces+
  '((:dynamic   "Dynamic space"   sb-kernel:dynamic-usage)
    #+immobile-space
    (:immobile  "Immobile space"  sb-kernel::immobile-space-usage)
    (:read-only "Read-only space" sb-kernel::read-only-space-usage)
    (:static    "Static space"    sb-kernel::static-space-usage))
  #'equal)

(defconstant-eqx +stack-spaces+
  '((:control-stack "Control stack" sb-kernel::control-stack-usage)
    (:binding-stack "Binding stack" sb-kernel::binding-stack-usage))
  #'equal)

(defconstant-eqx +all-spaces+ (append +heap-spaces+ +stack-spaces+) #'equal)

(defconstant-eqx +heap-space-keywords+ (mapcar #'first +heap-spaces+) #'equal)
(deftype spaces () `(member . ,+heap-space-keywords+))


;;;; MAP-ALLOCATED-OBJECTS

;;; Return the lower limit and current free-pointer of SPACE as fixnums
;;; whose raw bits (at the register level) represent a pointer.
;;; This makes it "off" by a factor of (EXPT 2 N-FIXNUM-TAG-BITS) - and/or
;;; possibly negative - if you look at the value in Lisp,
;;; but avoids potentially needing a bignum on 32-bit machines.
;;; 64-bit machines have no problem since most current generation CPUs
;;; use an address width that is narrower than 64 bits.
;;; This function is private because of the wacky representation.
(defun %space-bounds (space)
  (macrolet ((bounds (a b) `(values (%make-lisp-obj ,a) (%make-lisp-obj ,b))))
    (ecase space
      (:static
       (bounds static-space-start
               (sap-int *static-space-free-pointer*)))
      (:read-only
       (bounds read-only-space-start
               (sap-int *read-only-space-free-pointer*)))
      #+immobile-space
      (:fixed
       (bounds fixedobj-space-start
               (sap-int *fixedobj-space-free-pointer*)))
      #+immobile-space
      (:variable
       (bounds varyobj-space-start
               (sap-int *varyobj-space-free-pointer*)))
      (:dynamic
       (bounds (current-dynamic-space-start)
               (sap-int (dynamic-space-free-pointer)))))))

;;; Return the total number of bytes used in SPACE.
(defun space-bytes (space)
  (if (eq space :immobile)
      (+ (space-bytes :immobile-fixed)
         (space-bytes :immobile-variable))
      (multiple-value-bind (start end) (%space-bounds space)
        (ash (- end start) n-fixnum-tag-bits))))

;;; Round SIZE (in bytes) up to the next dualword boundary. A dualword
;;; is eight bytes on platforms with 32-bit word size and 16 bytes on
;;; platforms with 64-bit word size.
#-sb-fluid (declaim (inline round-to-dualword))
(defun round-to-dualword (size)
  (logand (the word (+ size lowtag-mask)) (lognot lowtag-mask)))

;;; Return the vector OBJ, its WIDETAG, and the number of octets
;;; required for its storage (including padding and alignment).
(defun reconstitute-vector (obj saetp)
  (declare (type (simple-array * (*)) obj)
           (type specialized-array-element-type-properties saetp))
  (let* ((length (+ (length obj)
                    (saetp-n-pad-elements saetp)))
         (n-bits (saetp-n-bits saetp))
         (alignment-pad (floor 7 n-bits))
         (n-data-octets (if (>= n-bits 8)
                            (* length (ash n-bits -3))
                            (ash (* (+ length alignment-pad)
                                    n-bits)
                                 -3))))
    (values obj
            (saetp-typecode saetp)
            (round-to-dualword (+ (* vector-data-offset n-word-bytes)
                                  n-data-octets)))))

(defun primitive-object-size (object)
  "Return number of bytes of heap or stack directly consumed by OBJECT"
  (if (is-lisp-pointer (get-lisp-obj-address object))
      (let ((words
              (typecase object
                (cons 2)
                (instance (1+ (%instance-length object)))
                (function
                 (when (= (fun-subtype object) simple-fun-widetag)
                   (return-from primitive-object-size
                     (primitive-object-size (fun-code-header object))))
                 (1+ (get-closure-length object)))
                ;; NIL is larger than a symbol. I don't care to think about
                ;; why these fudge factors are right, but they make the result
                ;; equal to what MAP-ALLOCATED-OBJECTS reports.
                (null (+ symbol-size 1 #+64-bit 1))
                (code-component
                 (return-from primitive-object-size (code-object-size object)))
                (fdefn 4) ; no length stored in the header
                ;; Anything else is an OTHER pointer.
                (t
                 (let ((room-info
                         (aref *room-info* (%other-pointer-widetag object))))
                   (if (arrayp object)
                       (cond ((array-header-p object)
                              (+ array-dimensions-offset (array-rank object)))
                             ((simple-array-nil-p object) 2)
                             (t
                              (return-from primitive-object-size
                                (nth-value 2 (reconstitute-vector object room-info)))))
                       ;; Other things (symbol, value-cell, etc)
                       ;; don't have a sizer, so use GET-HEADER-DATA
                       (1+ (logand (get-header-data object)
                                   (logand (get-header-data object)
                                           (room-info-mask room-info))))))))))
        (* (align-up words 2) n-word-bytes))
      0))

;;; Given the address (untagged, aligned, and interpreted as a FIXNUM)
;;; of a lisp object, return the object, its "type code" (either
;;; LIST-POINTER-LOWTAG or a header widetag), and the number of octets
;;; required for its storage (including padding and alignment).  Note
;;; that this function is designed to NOT CONS, even if called
;;; out-of-line.
;;; FIXME: size calculation should be via PRIMITIVE-OBJECT-SIZE, not reinvented
(defun reconstitute-object (address)
  (let* ((object-sap (int-sap (get-lisp-obj-address address)))
         (header (sap-ref-word object-sap 0))
         (widetag (logand header widetag-mask))
         (header-value (ash header (- n-widetag-bits)))
         (info (svref *room-info* widetag)))
    (macrolet
        ((boxed-size (header-value)
           `(round-to-dualword (ash (1+ ,header-value) word-shift)))
         (tagged-object (tag)
           `(%make-lisp-obj (logior ,tag (get-lisp-obj-address address)))))
      (cond
          ;; Pick off arrays, as they're the only plausible cause for
          ;; a non-nil, non-ROOM-INFO object as INFO.
        ((specialized-array-element-type-properties-p info)
         (reconstitute-vector (tagged-object other-pointer-lowtag) info))
        ((= widetag filler-widetag)
         (values nil filler-widetag (boxed-size header-value)))
        ((null info)
         (error "Unrecognized widetag #x~2,'0X in reconstitute-object"
                widetag))

        (t
         (case (room-info-kind info)
          (:list
           (values (tagged-object list-pointer-lowtag)
                   list-pointer-lowtag
                   (* 2 n-word-bytes)))

          (:instance
           (values (tagged-object instance-pointer-lowtag)
                   widetag
                   (boxed-size (logand header-value short-header-max-words))))

          (:closure ; also funcallable-instance
           (values (tagged-object fun-pointer-lowtag)
                   widetag
                   (boxed-size (logand header-value short-header-max-words))))

          (:code
           (let ((c (tagged-object other-pointer-lowtag)))
             (values c
                     code-header-widetag
                     (code-object-size c))))

          (:fdefn
           (values (tagged-object other-pointer-lowtag) widetag
                   (* fdefn-size n-word-bytes)))

          (:other
           (values (tagged-object other-pointer-lowtag)
                   widetag
                   (boxed-size (logand header-value (room-info-mask info)))))))))))

;;; Iterate over all the objects in the contiguous block of memory
;;; with the low address at START and the high address just before
;;; END, calling FUN with the object, the object's type code, and the
;;; object's total size in bytes, including any header and padding.
;;; START and END are untagged, aligned memory addresses interpreted
;;; as FIXNUMs (unlike SAPs or tagged addresses, these will not cons).
(defun map-objects-in-range (fun start end &optional (strict-bound t))
  (declare (type function fun))
  (declare (dynamic-extent fun))
  (named-let iter ((start start))
  (cond
    ((< (get-lisp-obj-address start) (get-lisp-obj-address end))
     (multiple-value-bind (obj typecode size) (reconstitute-object start)
      ;; SIZE is almost surely a fixnum. Non-fixnum would mean at least
      ;; a 512MB object if 32-bit words, and is inconceivable if 64-bit.
       (aver (not (logtest (the word size) lowtag-mask)))
       (unless (= typecode filler-widetag)
         (funcall fun obj typecode size))
             ;; This special little dance is to add a number of octets
             ;; (and it had best be a number evenly divisible by our
             ;; allocation granularity) to an unboxed, aligned address
             ;; masquerading as a fixnum.  Without consing.
      (iter (%make-lisp-obj
              (mask-field (byte #.n-word-bits 0)
                          (+ (get-lisp-obj-address start)
                             size))))))
    (strict-bound
     ;; If START is not eq to END, then we have blown past our endpoint.
     (aver (eq start end))))))

;;; Access to the GENCGC page table for better precision in
;;; MAP-ALLOCATED-OBJECTS
#+gencgc
(progn
  (define-alien-type (struct page)
      (struct page
              ;; To cut down the size of the page table, the scan_start_offset
              ;; - a/k/a "start" - is measured in 4-byte integers regardless
              ;; of word size. This is fine for 32-bit address space,
              ;; but if 64-bit then we have to scale the value. Additionally
              ;; there is a fallback for when even the scaled value is too big.
              ;; (None of this matters to Lisp code for the most part)
              (start #+64-bit (unsigned 32) #-64-bit signed)
              ;; On platforms with small enough GC pages, this field
              ;; will be a short. On platforms with larger ones, it'll
              ;; be an int.
              ;; Measured in bytes; the low bit has to be masked off.
              (bytes-used (unsigned
                           #.(if (typep gencgc-card-bytes '(unsigned-byte 16))
                                 16
                                 32)))
              (flags (unsigned 8))
              (gen (signed 8))))
  #+immobile-space
  (progn
    (define-alien-type (struct immobile-page)
        ;; ... and yet another place for Lisp to become out-of-sync with C.
        (struct immobile-page
                (flags (unsigned 8))
                (obj-spacing (unsigned 8))
                (obj-size (unsigned 8))
                (generations (unsigned 8))
                (free-index (unsigned 32))
                (page-link (unsigned 16))
                (prior-free-index (unsigned 16))))
    (define-alien-variable "fixedobj_pages" (* (struct immobile-page))))
  (declaim (inline find-page-index))
  (define-alien-routine ("ext_find_page_index" find-page-index)
    long (index unsigned))
  (define-alien-variable "next_free_page" sb-kernel::page-index-t)
  (define-alien-variable "page_table" (* (struct page))))

#+immobile-space
(progn
(deftype immobile-subspaces () '(member :fixed :variable))
(declaim (ftype (sfunction (function &rest immobile-subspaces) null)
                map-immobile-objects))
(defun map-immobile-objects (function &rest subspaces) ; Perform no filtering
  (declare (dynamic-extent function))
  (do-rest-arg ((subspace) subspaces)
    (multiple-value-bind (start end) (%space-bounds subspace)
      (map-objects-in-range function start end)))))

#|
MAP-ALLOCATED-OBJECTS is fundamentally unsafe to use if the user-supplied
function allocates anything. Consider what can happens when NEXT-FREE-PAGE
points to a partially filled page, and one more object is created extending
an allocation region that began on the formerly "last" page:

   0x10027cfff0: 0x00000000000000d9     <-- this was Lisp's view of
   0x10027cfff8: 0x0000000000000006         the last page (page 1273)
   ---- page boundary ----
   0x10027d0000: 0x0000001000005ecf     <-- next_free_page moves here (page 1274)
   0x10027d0008: 0x00000000000000ba
   0x10027d0010: 0x0000000000000040
   0x10027d0018: 0x0000000000000000

Lisp did not think that the page starting at 0x10027d0000 was allocated,
so it believes the stopping point is page 1273.  When we read the bytes-used
on that page, we see a totally full page, but do not consider adjoining any
additional pages into the contiguous block.
However the object, a vector, that started on page 1273 ends on page 1274,
causing MAP-OBJECTS-IN-RANGE to assert that it overran 0x10027d0000.

We could try a few things to mitigate this:
* Try to "chase" the value of next-free-page.  This is literally impossible -
  it's a moving target, and it's extremely likely to exhaust memory doing so,
  especially if the supplied lambda is an interpreted function.
  (Each object scanned causes consing of more bytes, and we never
  "catch up" to the moving next-free-page)

* If the page that we're looking at is full but the FINALLY clause is hit,
  don't stop looking for more pages in that one case. Instead keep looking
  for the end of the contiguous block, but stop as soon as any potential
  stopping point is found; don't chase next-free-page.  This is tricky
  as well and just about as infeasible.

* Pass a flag to MAP-OBJECTS-IN-RANGE specifying that it's OK to
  surpass the expected bound - silently accept our fate.
  This is what we do since it's simple, and seems to work.
|#

;;; Iterate over all the objects allocated in each of the SPACES, calling FUN
;;; with the object, the object's type code, and the object's total size in
;;; bytes, including any header and padding. As a special case, if exactly one
;;; space named :ALL is requested, then map over the known spaces.
(defun map-allocated-objects (fun &rest spaces)
  (declare (type function fun)
           ;; KLUDGE: rest-arg and self calls do not play nice and it'll get consed
           (optimize (sb-c::recognize-self-calls 0)))
  (declare (dynamic-extent fun))
  (when (and (= (length spaces) 1) (eq (first spaces) :all))
    (return-from map-allocated-objects
     (map-allocated-objects fun
                            :read-only :static
                            #+immobile-space :immobile
                            :dynamic)))
  ;; You can't specify :ALL and also a list of spaces. Check that up front.
  (do-rest-arg ((space) spaces) (the spaces space))
  (flet ((do-1-space (space)
    (ecase space
      (:static
       ;; Static space starts with NIL, which requires special
       ;; handling, as the header and alignment are slightly off.
       (multiple-value-bind (start end) (%space-bounds space)
         ;; This "8" is very magical. It happens to work for both
         ;; word sizes, even though symbols differ in length
         ;; (they can be either 6 or 7 words).
         (funcall fun nil symbol-widetag (* 8 n-word-bytes))
         (map-objects-in-range fun
                               (+ (ash (* 8 n-word-bytes) (- n-fixnum-tag-bits))
                                  start)
                               end)))

      ((:read-only #-gencgc :dynamic)
       ;; Read-only space (and dynamic space on cheneygc) is a block
       ;; of contiguous allocations.
       (multiple-value-bind (start end) (%space-bounds space)
         (map-objects-in-range fun start end)))
      #+immobile-space
      (:immobile
       ;; Filter out filler objects. These either look like cons cells
       ;; in fixedobj subspace, or code without enough header words
       ;; in varyobj subspace. (cf 'filler_obj_p' in gc-internal.h)
       (dx-flet ((filter (obj type size)
                   (unless (= type list-pointer-lowtag)
                     (funcall fun obj type size))))
         (map-immobile-objects #'filter :fixed))
       (dx-flet ((filter (obj type size)
                   (unless (and (code-component-p obj)
                                (code-obj-is-filler-p obj))
                     (funcall fun obj type size))))
         (map-immobile-objects #'filter :variable)))

      #+gencgc
      (:dynamic
       ;; Dynamic space on gencgc requires walking the GC page tables
       ;; in order to determine what regions contain objects.

       ;; We explicitly presume that any pages in an allocation region
       ;; that are in-use have a BYTES-USED of GENCGC-CARD-BYTES
       ;; (indicating a full page) or an otherwise-valid BYTES-USED.
       ;; We also presume that the pages of an open allocation region
       ;; after the first page, and any pages that are unallocated,
       ;; have a BYTES-USED of zero.  GENCGC seems to guarantee this.

       ;; Our procedure is to scan forward through the page table,
       ;; maintaining an "end pointer" until we reach a page where
       ;; BYTES-USED is not GENCGC-CARD-BYTES or we reach
       ;; NEXT-FREE-PAGE.  We then MAP-OBJECTS-IN-RANGE if the range
       ;; is not empty, and proceed to the next page (unless we've hit
       ;; NEXT-FREE-PAGE).  We happily take advantage of the fact that
       ;; MAP-OBJECTS-IN-RANGE will simply return if passed two
       ;; coincident pointers for the range.

       ;; FIXME: WITHOUT-GCING prevents a GC flip, but doesn't prevent
       ;; closing allocation regions and opening new ones.  This may
       ;; prove to be an issue with concurrent systems, or with
       ;; spectacularly poor timing for closing an allocation region
       ;; in a single-threaded system.
       #+sb-thread
       (close-current-gc-region)
       (do ((initial-next-free-page next-free-page)
            ;; This is a "funny" fixnum - essentially the bit cast of a pointer
            (start (the fixnum (%make-lisp-obj (current-dynamic-space-start))))
            (page-index 0 (1+ page-index))
            ;; SPAN is a page count, for which an unsigned fixnum is adequate.
            (span 0))
           ;; We can safely iterate up to and including next_free_page
           ;; even if next_free_page is the total number of pages in the space,
           ;; because there is one extra page table entry as a sentinel.
           ;; The extra page always has 0 bytes used, so we'll observe the end
           ;; of a contiguous block without needing a termination clause to
           ;; handle a final sequence of totally full pages that exactly abut
           ;; the heap end. [Also note that for our purposes, contiguous blocks
           ;; can span different GC generations and page types, whereas within
           ;; GC, a page ends a block if the next differs in those aspects]
           ((> page-index initial-next-free-page))
         ;; The type constraint on PAGE-INDEX is probably too generous,
         ;; but it does its job of producing efficient code.
         (declare (type (integer 0 (#.(/ (ash 1 n-machine-word-bits) gencgc-card-bytes)))
                        page-index)
                  (type (and fixnum unsigned-byte) span))
         (let ((page-bytes-used ; The low bit of bytes-used is the need-to-zero flag.
                (logandc1 1 (slot (deref page-table page-index) 'bytes-used))))
           (if (= page-bytes-used gencgc-card-bytes)
               (incf span)
               ;; RANGE-END forces funny increment of START by the extent in bytes,
               ;; returning a negative fixum when the word's high bit flips.
               ;; The proper way to add "funny" fixnums is via +-MODFX which for
               ;; reasons unknown isn't defined on all backends.
               (macrolet
                   ((range-end (extra)
                      `(truly-the fixnum
                        (%make-lisp-obj
                         (logand most-positive-word
                                 ;; The first and third addends are known good
                                 ;; The second needs help to avoid an expensive ASH
                                 (+ (get-lisp-obj-address start)
                                    (logand (* span gencgc-card-bytes) most-positive-word)
                                    ,extra))))))
                 (map-objects-in-range fun start (range-end page-bytes-used)
                                       (< page-index initial-next-free-page))
                 ;; In a certain rare situation, we must restart the next loop iteration
                 ;; at exactly PAGE-INDEX instead of 1+ PAGE-INDEX.
                 ;; Normally the contents of PAGE-INDEX are always included in the
                 ;; current range. But what if it contributed 0 bytes to the range?
                 ;;
                 ;;     N : #x8000 bytes used
                 ;;   N+1 : #x8000 bytes used
                 ;;   N+2 : 0 bytes used        <- PAGE-INDEX now points here
                 ;;   N+3 : initially 0 bytes, then gets some bytes
                 ;;
                 ;; We invoked the map function with page_address(N) for #x10000 bytes.
                 ;; Suppose that function consed a partly unboxed object starting on
                 ;; page N+2 extending to page N+3, and that NEXT-FREE-PAGE was greater
                 ;; than both to begin with, so the termination condition isn't in play.
                 ;; In the best case scenario, resuming the scan at page N+3 (mid-object)
                 ;; would read valid widetags, but in the worst case scenario, we get
                 ;; random bits. Page N+2 needs a fighting chance to be the start of
                 ;; a range, so go back 1 page if the current page had zero bytes used
                 ;; and the span exceeded 1. That is, always make forward progress.
                 (setq start (range-end (cond ((and (zerop page-bytes-used) (plusp span))
                                               (decf page-index)
                                               0)
                                              (t
                                               gencgc-card-bytes)))
                       span 0)))))))))

  (do-rest-arg ((space) spaces)
    (if (eq space :dynamic)
        (without-gcing (do-1-space space))
        (do-1-space space)))))

#+(and sb-thread gencgc)
;; Start with a Lisp rendition of ensure_region_closed() on the active
;; thread's region, since users are often surprised to learn that a
;; just-consed object can't necessarily be seen by MAP-ALLOCATED-OBJECTS.
;; Since we're in WITHOUT-GCING, there can be no interrupts.
;; This is probably better than calling MAP-OBJECTS-IN-RANGE on the
;; thread's region, because then we might visit some page twice
;; by doing that.
;; (And seeing small consing by other threads is hopeless either way)
(defun close-current-gc-region ()
  (unless (eql (sap-int (current-thread-offset-sap
                         (+ thread-alloc-region-slot 3)))
               0)                       ; start_addr
    (alien-funcall
     (extern-alien "gc_close_region" (function void unsigned int))
     (truly-the word
                (+ (sb-thread::thread-primitive-thread sb-thread:*current-thread*)
                   (ash thread-alloc-region-slot word-shift)))
     1)))

;;;; MEMORY-USAGE

#+immobile-space
(progn
(declaim (ftype (function (immobile-subspaces) (values t t t &optional))
                immobile-fragmentation-information))
(defun immobile-fragmentation-information (subspace)
  (binding* (((start free-pointer) (%space-bounds subspace))
             (used-bytes (ash (- free-pointer start) n-fixnum-tag-bits))
             (holes '())
             (hole-bytes 0))
    (map-immobile-objects
     (lambda (obj type size)
       (let ((address (logandc2 (get-lisp-obj-address obj) lowtag-mask)))
         (when (case subspace
                 (:fixed (= type list-pointer-lowtag))
                 (:variable (hole-p address)))
           (push (cons address size) holes)
           (incf hole-bytes size))))
     subspace)
    (values holes hole-bytes used-bytes)))

(defun show-fragmentation (&key (subspaces '(:fixed :variable))
                                (stream *standard-output*))
  (dolist (subspace subspaces)
    (format stream "~(~A~) subspace fragmentation:~%" subspace)
    (multiple-value-bind (holes hole-bytes total-space-used)
        (immobile-fragmentation-information subspace)
      (loop for (start . size) in holes
            do (format stream "~2@T~X..~X ~8:D~%" start (+ start size) size))
      (format stream "~2@T~18@<~:D hole~:P~> ~8:D (~,2,2F% of ~:D ~
                      bytes used)~%"
              (length holes) hole-bytes
              (/ hole-bytes total-space-used) total-space-used))))

(defun sb-kernel::immobile-space-usage ()
  (binding* (((nil fixed-hole-bytes fixed-used-bytes)
              (immobile-fragmentation-information :fixed))
             ((nil variable-hole-bytes variable-used-bytes)
              (immobile-fragmentation-information :variable))
             (total-used-bytes (+ fixed-used-bytes variable-used-bytes))
             (total-hole-bytes (+ fixed-hole-bytes variable-hole-bytes)))
    (values total-used-bytes total-hole-bytes)))
) ; end PROGN

;;; Return a list of 3-lists (bytes object type-name) for the objects
;;; allocated in Space.
(defun type-breakdown (space)
  (declare (muffle-conditions compiler-note))
  (let ((sizes (make-array 256 :initial-element 0 :element-type 'word))
        (counts (make-array 256 :initial-element 0 :element-type 'word)))
    (map-allocated-objects
     (lambda (obj type size)
       (declare (word size) (optimize (speed 3)) (ignore obj))
       (incf (aref sizes type) size)
       (incf (aref counts type)))
     space)

    (let ((totals (make-hash-table :test 'eq)))
      (dotimes (i 256)
        (let ((total-count (aref counts i)))
          (unless (zerop total-count)
            (let* ((total-size (aref sizes i))
                   (name (room-info-type-name (aref *room-info* i)))
                   (found (ensure-gethash name totals (list 0 0 name))))
              (incf (first found) total-size)
              (incf (second found) total-count)))))

      (collect ((totals-list))
        (maphash (lambda (k v)
                   (declare (ignore k))
                   (totals-list v))
                 totals)
        (sort (totals-list) #'> :key #'first)))))

;;; Handle the summary printing for MEMORY-USAGE. Totals is a list of lists
;;; (space-name . totals-for-space), where totals-for-space is the list
;;; returned by TYPE-BREAKDOWN.
(defun print-summary (spaces totals)
  (let ((summary (make-hash-table :test 'eq))
        (space-count (length spaces)))
    (dolist (space-total totals)
      (dolist (total (cdr space-total))
        (push (cons (car space-total) total)
              (gethash (third total) summary))))

    (collect ((summary-totals))
      (maphash (lambda (k v)
                 (declare (ignore k))
                 (let ((sum 0))
                   (declare (unsigned-byte sum))
                   (dolist (space-total v)
                     (incf sum (first (cdr space-total))))
                   (summary-totals (cons sum v))))
               summary)

      (format t "~2&Summary of space~P: ~(~{~A ~}~)~%" space-count spaces)
      (let ((summary-total-bytes 0)
            (summary-total-objects 0))
        (declare (unsigned-byte summary-total-bytes summary-total-objects))
        (dolist (space-totals
                 (mapcar #'cdr (sort (summary-totals) #'> :key #'car)))
          (let ((total-objects 0)
                (total-bytes 0)
                name)
            (declare (unsigned-byte total-objects total-bytes))
            (collect ((spaces))
              (dolist (space-total space-totals)
                (let ((total (cdr space-total)))
                  (setq name (third total))
                  (incf total-bytes (first total))
                  (incf total-objects (second total))
                  (spaces (cons (car space-total) (first total)))))
              (format t "~%~A:~%    ~:D bytes, ~:D object~:P"
                      name total-bytes total-objects)
              (unless (= 1 space-count)
                (dolist (space (spaces))
                  (format t ", ~D% ~(~A~)"
                          (round (* (cdr space) 100) total-bytes) (car space))))
              (format t ".~%")
              (incf summary-total-bytes total-bytes)
              (incf summary-total-objects total-objects))))
        (format t "~%Summary total:~%    ~:D bytes, ~:D objects.~%"
                summary-total-bytes summary-total-objects)))))

(declaim (ftype (sfunction (index &key (:comma-interval (and (integer 1) index))) index)
                decimal-with-grouped-digits-width))
(defun decimal-with-grouped-digits-width (value &key (comma-interval 3))
  (let ((digits (length (write-to-string value :base 10))))
    (+ digits (floor (1- digits) comma-interval))))

;;; Report object usage for a single space.
(defun report-space-total (space-info cutoff)
  (declare (list space-info) (type (or single-float null) cutoff))
  (destructuring-bind (space . types) space-info
    (format t "~2&Breakdown for ~(~A~) space:~%" space)
    (let* ((total-bytes (reduce #'+ (mapcar #'first types)))
           (bytes-width (decimal-with-grouped-digits-width total-bytes))
           (total-objects (reduce #'+ (mapcar #'second types)))
           (objects-width (decimal-with-grouped-digits-width total-objects))
           (cutoff-point (if cutoff
                             (truncate (* (float total-bytes) cutoff))
                             0))
           (reported-bytes 0)
           (reported-objects 0))
      (declare (unsigned-byte total-objects total-bytes cutoff-point
                              reported-objects reported-bytes))
      (flet ((type-usage (bytes objects name &optional note)
               (format t "  ~V:D bytes for ~V:D ~(~A~) object~2:*~P~*~
                          ~:[~; ~:*(~A)~]~%"
                       bytes-width bytes objects-width objects name note)))
        (loop for (bytes objects name) in types do
             (when (<= bytes cutoff-point)
               (type-usage (- total-bytes reported-bytes)
                           (- total-objects reported-objects)
                           "other")
               (return))
             (incf reported-bytes bytes)
             (incf reported-objects objects)
             (type-usage bytes objects name))
        (terpri)
        (type-usage total-bytes total-objects space "space total")))))

;;; Print information about the heap memory in use. PRINT-SPACES is a
;;; list of the spaces to print detailed information for.
;;; COUNT-SPACES is a list of the spaces to scan. For either one, T
;;; means all spaces (i.e. :STATIC, :DYNAMIC and :READ-ONLY.) If
;;; PRINT-SUMMARY is true, then summary information will be printed.
;;; The defaults print only summary information for dynamic space. If
;;; true, CUTOFF is a fraction of the usage in a report below which
;;; types will be combined as OTHER.
(defun memory-usage (&key print-spaces (count-spaces '(:dynamic #+immobile-space :immobile))
                          (print-summary t) cutoff)
  (declare (type (or single-float null) cutoff))
  (let* ((spaces (if (eq count-spaces t) +heap-space-keywords+ count-spaces))
         (totals (mapcar (lambda (space)
                           (cons space (type-breakdown space)))
                         spaces)))

    (dolist (space-total totals)
      (when (or (eq print-spaces t)
                (member (car space-total) print-spaces))
        (report-space-total space-total cutoff)))

    (when print-summary (print-summary spaces totals)))

  (values))

;;; Print a breakdown by instance type of all the instances allocated
;;; in SPACE. If TOP-N is true, print only information for the
;;; TOP-N types with largest usage.
(defun instance-usage (space &key (top-n 15))
  (declare (type spaces space) (type (or fixnum null) top-n))
  (format t "~2&~@[Top ~W ~]~(~A~) instance types:~%" top-n space)
  (let ((totals (make-hash-table :test 'eq))
        (total-objects 0)
        (total-bytes 0))
    (declare (unsigned-byte total-objects total-bytes))
    (map-allocated-objects
     (lambda (obj type size)
       (declare (optimize (speed 3)))
       (when (or (eql type instance-widetag)
                 (eql type funcallable-instance-widetag))
         (incf total-objects)
         (let* ((layout (if (eql type funcallable-instance-widetag)
                            (%funcallable-instance-layout obj)
                            (%instance-layout obj)))
                (classoid (layout-classoid layout))
                (found (ensure-gethash classoid totals (cons 0 0)))
                (size size))
           (declare (fixnum size))
           (incf total-bytes size)
           (incf (the fixnum (car found)))
           (incf (the fixnum (cdr found)) size))))
     space)
    (let* ((sorted (sort (%hash-table-alist totals) #'> :key #'cddr))
           (interesting (if top-n
                            (subseq sorted 0 (min (length sorted) top-n))
                            sorted))
           (bytes-width (decimal-with-grouped-digits-width total-bytes))
           (objects-width (decimal-with-grouped-digits-width total-objects))
           (totals-label (format nil "~:(~A~) instance total" space))
           (types-width (reduce #'max interesting
                                :key (lambda (info)
                                       (let ((type (first info)))
                                         (length
                                          (typecase type
                                            (string
                                             type)
                                            (classoid
                                             (with-output-to-string (stream)
                                               (sb-ext:print-symbol-with-prefix
                                                stream (classoid-name type))))))))
                                :initial-value (length totals-label)))
           (printed-bytes 0)
           (printed-objects 0))
      (declare (unsigned-byte printed-bytes printed-objects))
      (flet ((type-usage (type objects bytes)
               (etypecase type
                 (string
                  (format t "  ~V@<~A~> ~V:D bytes, ~V:D object~:P.~%"
                          (1+ types-width) type bytes-width bytes
                          objects-width objects))
                 (classoid
                  (format t "  ~V@<~/sb-ext:print-symbol-with-prefix/~> ~
                             ~V:D bytes, ~V:D object~:P.~%"
                          (1+ types-width) (classoid-name type) bytes-width bytes
                          objects-width objects)))))
        (loop for (type . (objects . bytes)) in interesting
              do (incf printed-bytes bytes)
                 (incf printed-objects objects)
                 (type-usage type objects bytes))
        (terpri)
        (let ((residual-objects (- total-objects printed-objects))
              (residual-bytes (- total-bytes printed-bytes)))
          (unless (zerop residual-objects)
            (type-usage "Other types" residual-objects residual-bytes)))
        (type-usage totals-label total-objects total-bytes))))
  (values))

;;;; PRINT-ALLOCATED-OBJECTS

;;; This notion of page-size is completely arbitrary - it affects 2 things:
;;; (1) how much output to print "per page" in print-allocated-objects
;;; (2) sb-sprof deciding how many regions [sic] were made if #+cheneygc
(defun get-page-size () sb-c:+backend-page-bytes+)

;;; This function is sheer madness.  You're better off using
;;; LIST-ALLOCATED-OBJECTS and then iterating over that, to avoid
;;; seeing all the junk created while doing this thing.
(defun print-allocated-objects (space &key (percent 0) (pages 5)
                                      type larger smaller count
                                      (stream *standard-output*))
  (declare (type (integer 0 99) percent) (type index pages)
           (type stream stream) (type spaces space)
           (type (or index null) type larger smaller count))
  (multiple-value-bind (start end) (%space-bounds space)
    (let* ((space-start (ash start n-fixnum-tag-bits))
           (space-end (ash end n-fixnum-tag-bits))
           (space-size (- space-end space-start))
           (pagesize (get-page-size))
           (start (+ space-start (round (* space-size percent) 100)))
           (printed-conses (make-hash-table :test 'eq))
           (pages-so-far 0)
           (count-so-far 0)
           (last-page 0))
      (declare (type word last-page start)
               (fixnum pages-so-far count-so-far pagesize))
      (labels ((note-conses (x)
                 (unless (or (atom x) (gethash x printed-conses))
                   (setf (gethash x printed-conses) t)
                   (note-conses (car x))
                   (note-conses (cdr x)))))
        (map-allocated-objects
         (lambda (obj obj-type size)
           (let ((addr (get-lisp-obj-address obj)))
             (when (>= addr start)
               (when (if count
                         (> count-so-far count)
                         (> pages-so-far pages))
                 (return-from print-allocated-objects (values)))

               (unless count
                 (let ((this-page (* (the (values word t)
                                       (truncate addr pagesize))
                                     pagesize)))
                   (declare (type word this-page))
                   (when (/= this-page last-page)
                     (when (< pages-so-far pages)
                       ;; FIXME: What is this? (ERROR "Argh..")? or
                       ;; a warning? or code that can be removed
                       ;; once the system is stable? or what?
                       (format stream "~2&**** Page ~W, address ~X:~%"
                               pages-so-far addr))
                     (setq last-page this-page)
                     (incf pages-so-far))))

               (when (and (or (not type) (eql obj-type type))
                          (or (not smaller) (<= size smaller))
                          (or (not larger) (>= size larger)))
                 (incf count-so-far)
                 (case type
                   (#.code-header-widetag
                    (let ((dinfo (%code-debug-info obj)))
                      (format stream "~&Code object: ~S~%"
                              (if dinfo
                                  (sb-c::compiled-debug-info-name dinfo)
                                  "No debug info."))))
                   (#.symbol-widetag
                    (format stream "~&~S~%" obj))
                   (#.list-pointer-lowtag
                    (unless (gethash obj printed-conses)
                      (note-conses obj)
                      (let ((*print-circle* t)
                            (*print-level* 5)
                            (*print-length* 10))
                        (format stream "~&~S~%" obj))))
                   (t
                    (fresh-line stream)
                    (let ((str (write-to-string obj :level 5 :length 10
                                                :pretty nil)))
                      (unless (eql type instance-widetag)
                        (format stream "~S: " (type-of obj)))
                      (format stream "~A~%"
                              (subseq str 0 (min (length str) 60))))))))))
         space))))
  (values))

;;;; LIST-ALLOCATED-OBJECTS, LIST-REFERENCING-OBJECTS

(defvar *ignore-after* nil)

(defun valid-obj (space x)
  (or (not (eq space :dynamic))
      ;; this test looks bogus if the allocator doesn't work linearly,
      ;; which I suspect is the case for GENCGC.  -- CSR, 2004-06-29
      (< (get-lisp-obj-address x) (get-lisp-obj-address *ignore-after*))))

(defun maybe-cons (space x stuff)
  (if (valid-obj space x)
      (cons x stuff)
      stuff))

(defun list-allocated-objects (space &key type larger smaller count
                                     test)
  (declare (type (or (eql :all) spaces) space)
           (type (or (unsigned-byte 8) null) type)
           (type (or index null) larger smaller count)
           (type (or function null) test))
  (declare (dynamic-extent test))
  (when (eql count 0)
    (return-from list-allocated-objects nil))
  ;; This function was pretty much random as to what subset of the heap it
  ;; visited- it might see half the heap, 1/10th of the heap, who knows, because
  ;; it stopped based on hitting a sentinel cons cell made just prior to the loop.
  ;; That stopping condition was totally wrong because allocation does not occur
  ;; linearly.  Taking 2 passes (first count, then store) stands a chance of
  ;; getting a reasonable point-in-time view as long as other threads are not consing
  ;; like crazy. If the user-supplied TEST function conses at all, then the result is
  ;; still very arbitrary - including possible duplication of objects if we visit
  ;; something and then see it again after GC transports it higher. The only way to
  ;; allow consing in the predicate would be to use dedicated "arenas" for new
  ;; allocations, that being a concept which we do not now - and may never - support.
  (sb-int:dx-flet ((wantp (obj widetag size)
                     (and (or (not type) (eql widetag type))
                          (or (not smaller) (<= size smaller))
                          (or (not larger) (>= size larger))
                          (or (not test) (funcall test obj)))))
    ;; Unless COUNT is smallish, always start by counting. Don't just trust the user
    ;; because s/he might specify :COUNT huge-num which is acceptable provided that
    ;; huge-num is an INDEX which could either exhaust the heap, or at least be
    ;; wasteful if but a tiny handful of objects would actually satisfy WANTP.
    (let* ((output (make-array
                    (if (typep count '(integer 0 100000))
                        count
                        (let ((n 0))
                          (map-allocated-objects
                           (lambda (obj widetag size)
                             (when (wantp obj widetag size) (incf n)))
                           space)
                          n))))
           (index 0))
      (block done
       (map-allocated-objects
        (lambda (obj widetag size)
          (when (wantp obj widetag size)
            (setf (aref output index) obj)
            (when (= (incf index) (length output))
              (return-from done))))
        space))
      (let ((list
             (cond ((= index (length output)) ; easy case
                    (coerce output 'list))
                   (t ; didn't fill the array
                    (collect ((res))
                      (dotimes (i index (res))
                        (res (svref output i))))))))
        (fill output 0) ; assist GC a bit
        list))))

;;; Calls FUNCTION with all objects that have (possibly conservative)
;;; references to them on current stack.
;;; This is for use by SB-INTROSPECT. (Other consumers, at your own risk)
;;; Note that we do not call MAKE-LISP-OBJ in the errorp=nil mode, as it
;;; potentially uses FORMAT and MAKE-UNPRINTABLE-OBJECT with each invocation.
;;; And see the cautionary remarks above that function regarding its dangerous
;;; nature (more so on precise GC).  On conservative GC we should be OK here
;;; because we know that there's a stack reference.
(defun map-stack-references (function)
  (declare (type function function))
  (declare (dynamic-extent function))
  (macrolet ((iter (step limit test)
               `(do ((sp (current-sp) (sap+ sp (,step n-word-bytes)))
                     (limit (sb-di::descriptor-sap ,limit))
                     (seen nil))
                    ((,test sp limit))
                  (let ((word (sap-ref-word sp 0)))
                    ;; Explicitly skip non-pointer words. The callable that
                    ;; SB-INTROSPECT provides ignores immediate values anyway.
                    (when (and (is-lisp-pointer word)
                               (not (zerop (sb-di::valid-lisp-pointer-p (int-sap word)))))
                      (let ((obj (%make-lisp-obj word)))
                        (unless (memq obj seen)
                          (push obj seen)
                          (funcall function obj))))))))
    #+stack-grows-downward-not-upward (iter + *control-stack-end* sap>)
    #-stack-grows-downward-not-upward (iter - *control-stack-start* sap<)))

(declaim (inline symbol-extra-slot-p))
(defun symbol-extra-slot-p (x)
  (> (logand (get-header-data x) tiny-boxed-size-mask)
     (1- symbol-size)))

;;; Invoke FUNCTOID (a macro or function) on OBJ and any values in MORE.
;;; Note that neither OBJ nor items in MORE undergo ONCE-ONLY treatment.
;;; The fact that FUNCTOID can be a macro allows treatment of its first argument
;;; as a generalized place in the manner of SETF, allowing read/write access.
;;; CLAUSES are used to modify the output of this macro. See example uses
;;; for more detail.
;;; HIGH EXPERIMENTAL: PROCEED AT YOUR OWN RISK.
(defmacro do-referenced-object ((obj functoid &rest more) &rest alterations
                                &aux (n-matched-alterations 0))
  (labels ((make-case (type &rest actions)
             (apply #'make-case* type
                    (mapcar (lambda (action) `(,functoid ,action ,@more))
                            actions)))
           (make-case* (type &rest actions)
             (let* ((found (assoc type alterations :test 'equal))
                    (alteration (or (cdr found) '(:extend))))
               (when found
                 (incf n-matched-alterations))
               (ecase (car alteration)
                 (:override (list `(,type ,@(cdr alteration))))
                 (:extend   (list `(,type ,@actions ,@(cdr alteration))))
                 (:delete))))) ; no clause
    (prog1
      `(typecase ,obj
         ;; Until the compiler can learn how to efficiently compile jump tables
         ;; by widetag, test in descending order of popularity.
         ;; These two are in fact generally the most frequently occurring type.
         ,.(make-case 'cons `(car ,obj) `(cdr ,obj))
         ,.(make-case* 'instance
            `(let ((.l. (%instance-layout ,obj)))
               ;; Though we've bound %INSTANCE-LAYOUT to a variable,
               ;; pass the form %INSTANCE-LAYOUT to functoid
               ;; in case it wants to examine the form
               (,functoid (%instance-layout ,obj) ,@more)
               (do-instance-tagged-slot (.i. ,obj :layout .l. :pad nil)
                 (,functoid (%instance-ref ,obj .i.) ,@more))))
         (function
          (typecase ,obj
            ,.(make-case* 'closure
               `(,functoid (%closure-fun ,obj) ,@more)
               `(do-closure-values (.o. ,obj :include-extra-values t)
                  ;; FIXME: doesn't allow setf, but of course there is
                  ;; no closure-index-set anyway, so .O. might be unused
                  ;; if functoid is a macro that does nothing.
                  (,functoid .o. ,@more)))
            ,.(make-case* 'funcallable-instance
               `(let ((.l. (%funcallable-instance-layout ,obj)))
                  ;; As for INSTANCE, allow the functoid to see the access form
                  (,functoid (%funcallable-instance-layout ,obj) ,@more)
                  (,functoid (%funcallable-instance-fun ,obj) ,@more)
                  (ecase (layout-bitmap .l.)
                    (#.sb-kernel::+layout-all-tagged+
                     (loop for .i. from instance-data-start ; exclude layout
                           to (- (get-closure-length ,obj) funcallable-instance-info-offset)
                           do (,functoid (%funcallable-instance-info ,obj .i.) ,@more)))
                    (#b0110
                     ;; A pedantically correct kludge which shall remain unless need arises
                     ;; for more general partially unboxed FINs.
                     ;;  payload word 0 is raw (but looks like a fixnum, by design)
                     ;;  word 1 is the fin-fun which we already accounted for above
                     ;;  word 2 (info slot 0) is the only one that hasn't been processed.
                     ;;  words 3 and 4 are raw but looks like fixnums by accident.
                     (,functoid (%funcallable-instance-info ,obj 0) ,@more)))))
            .,(make-case 'function))) ; in case there was code provided for it
         (t
          ;; TODO: the generated code is pretty horrible. OTHER-POINTER-LOWTAG
          ;; is known at this point, but tested N times.
          (typecase ,obj
            ,.(make-case* 'simple-vector
               `(dotimes (.i. (length ,obj))
                  (,functoid (data-vector-ref ,obj .i.) ,@more)))
            ;; Fancy arrays aren't highly popular, but this case must precede ARRAY
            ;; because ARRAY weeds out all other arrays, namely the ones that
            ;; hold no pointers: simple-string, simple-bit-vector, etc.
            ,.(make-case '(satisfies array-header-p)
               `(%array-data ,obj)
               `(%array-displaced-p ,obj)
               `(%array-displaced-from ,obj))
            ,.(make-case 'array)
            ,.(make-case* 'symbol
               `(,functoid (%primitive sb-c:fast-symbol-global-value ,obj) ,@more)
               `(,functoid (symbol-info ,obj) ,@more)
               `(,functoid (symbol-name ,obj) ,@more)
               `(,functoid (symbol-package ,obj) ,@more)
               `(when (symbol-extra-slot-p ,obj)
                  (,functoid (symbol-extra ,obj) ,@more)))
            ,.(make-case 'fdefn
               `(fdefn-name ,obj)
               `(fdefn-fun ,obj)
               ;; While it looks like we could easily allow a pointer to a movable object
               ;; in the fdefn-raw-addr slot, it is not exactly trivial- at a bare minimum,
               ;; translating the raw-addr to a lispobj might have to be pseudoatomic,
               ;; since we don't know what object to pin when reconstructing it.
               ;; For simple-funs in dynamic space, it doesn't have to be pseudoatomic
               ;; because a reference to the interior of code pins the code.
               ;; Closure trampolines would be fine as well. That leaves funcallable instances
               ;; as the pain point. Those could go on pages of code as well, but see the
               ;; comment in conservative_root_p() in gencgc as to why that alone
               ;; would be inadequate- we require a properly tagged descriptor
               ;; to enliven any object other than code.
               #+immobile-code
               `(%make-lisp-obj
                 (alien-funcall (extern-alien "fdefn_callee_lispobj" (function unsigned unsigned))
                                (logandc2 (get-lisp-obj-address ,obj) lowtag-mask))))
            ,.(make-case* 'code-component
               `(loop for .i. from 2 below (code-header-words ,obj)
                      do (,functoid (code-header-ref ,obj .i.) ,@more)))
            ,.(make-case '(or float (complex float) bignum
                           #+sb-simd-pack simd-pack
                           #+sb-simd-pack-256 simd-pack-256
                           system-area-pointer)) ; nothing to do
            ,.(make-case 'weak-pointer `(weak-pointer-value ,obj))
            ,.(make-case 'ratio `(%numerator ,obj) `(%denominator ,obj))
            ;; Visitor won't be invoked on (COMPLEX float)
            ,.(make-case '(complex rational) `(%realpart ,obj) `(%imagpart ,obj))
            ;; Caller can do anything in the fallback case.
            ,.(make-case 't))))
      (when (> (length alterations) n-matched-alterations)
        (error "DO-REFERENCED-OBJECT usage error")))))

;;; Return T if and only if THIS references THAT.
;;; code-components are considered to reference their embedded
;;; simple-funs for this purpose; if THIS is a simple-fun, it is ignored.
(defun references-p (this that)
  (macrolet ((test (x) `(when (eq ,x that) (go win))))
    (tagbody
       (do-referenced-object (this test)
         (code-component
          :extend
          (dotimes (i (code-n-entries this))
            (let ((f (%code-entry-point this i)))
              (when (eq f that)
                (go win)))))
         (t
          :extend
          (case (widetag-of this)
            (#.value-cell-widetag
             (test (value-cell-ref this)))
            (#.filler-widetag)
            (t
             (bug "Unknown object type #x~x addr=~x"
                  (widetag-of this)
                  (get-lisp-obj-address this))))))
       (return-from references-p nil)
     win
       (return-from references-p t))))

;;; This interface allows one either to be agnostic of the referencing space,
;;; or specify exactly one space, but not specify a list of spaces.
;;; An upward-compatible change would be to assume a list, and call ENSURE-LIST.
(defun map-referencing-objects (fun space object)
  (declare (type (or (eql :all) spaces) space))
  (declare (dynamic-extent fun))
  (unless *ignore-after*
    (setq *ignore-after* (cons 1 2)))
  (let ((fun (%coerce-callable-to-fun fun)))
    (map-allocated-objects
     (lambda (referer widetag size)
       (declare (ignore widetag size))
       (when (and (valid-obj space referer) ; semi-bogus!
                  (references-p referer object))
         (funcall fun referer)))
     space)))

(defun list-referencing-objects (space object)
  (collect ((res))
    (map-referencing-objects
     (lambda (obj) (res obj)) space object)
    (res)))

;;;; ROOM

(defun room-minimal-info ()
  (multiple-value-bind (names name-width
                        used-bytes used-bytes-width
                        overhead-bytes)
      (loop for (nil name function) in +all-spaces+
            for (space-used-bytes space-overhead-bytes)
               = (multiple-value-list (funcall function))
            collect name into names
            collect space-used-bytes into used-bytes
            collect space-overhead-bytes into overhead-bytes
            maximizing (length name) into name-maximum
            maximizing space-used-bytes into used-bytes-maximum
            finally (return (values
                             names name-maximum
                             used-bytes (decimal-with-grouped-digits-width
                                         used-bytes-maximum)
                             overhead-bytes)))
    (loop for name in names
          for space-used-bytes in used-bytes
          for space-overhead-bytes in overhead-bytes
          do (format t "~V@<~A usage is:~> ~V:D bytes~@[ (~:D bytes ~
                        overhead)~].~%"
                     (+ name-width 10) name used-bytes-width space-used-bytes
                     space-overhead-bytes)))
  #+sb-thread
  (format t "Control and binding stack usage is for the current thread ~
             only.~%")
  (format t "Garbage collection is currently ~:[enabled~;DISABLED~].~%"
          *gc-inhibit*))

(defun room-intermediate-info ()
  (room-minimal-info)
  (memory-usage :count-spaces '(:dynamic #+immobile-space :immobile)
                :print-spaces t
                :cutoff 0.05f0
                :print-summary nil))

(defun room-maximal-info ()
  (let ((spaces '(:dynamic #+immobile-space :immobile :static)))
    (room-minimal-info)
    (memory-usage :count-spaces spaces)
    (dolist (space spaces)
      (instance-usage space :top-n 10))))

(defun room (&optional (verbosity :default))
  "Print to *STANDARD-OUTPUT* information about the state of internal
  storage and its management. The optional argument controls the
  verbosity of output. If it is T, ROOM prints out a maximal amount of
  information. If it is NIL, ROOM prints out a minimal amount of
  information. If it is :DEFAULT or it is not supplied, ROOM prints out
  an intermediate amount of information."
  (fresh-line)
  (ecase verbosity
    ((t)
     (room-maximal-info))
    ((nil)
     (room-minimal-info))
    (:default
     (room-intermediate-info)))
  (values))

#+nil ; for debugging
(defun show-dynamic-space-code (&optional (stream *standard-output*)
                                &aux (n-code-bytes 0)
                                     (total-pages next-free-page)
                                     (pages
                                      (make-array total-pages :element-type 'bit)))
  (flet ((dump-page (page-num)
           (format stream "~&Page ~D~%" page-num)
           (let ((where (+ dynamic-space-start (* page-num gencgc-card-bytes)))
                 (seen-filler nil))
             (loop
               (multiple-value-bind (obj type size)
                   (reconstitute-object (ash where (- n-fixnum-tag-bits)))
                 (when (= type code-header-widetag)
                   (incf n-code-bytes size))
                 (when (if (and (consp obj) (eq (car obj) 0) (eq (cdr obj) 0))
                           (if seen-filler
                               (progn (write-char #\. stream) nil)
                               (setq seen-filler t))
                           (progn (setq seen-filler nil) t))
                   (let ((*print-pretty* nil))
                     (format stream "~&  ~X ~4X ~S " where size obj)))
                 (incf where size)
                 (loop for index from page-num to (find-page-index (1- where))
                       do (setf (sbit pages index) 1)))
               (let ((next-page (find-page-index where)))
                 (cond ((= (logand where (1- gencgc-card-bytes)) 0)
                        (format stream "~&-- END OF PAGE --~%")
                        (return next-page))
                       ((eq next-page page-num))
                       (t
                        (setq page-num next-page seen-filler nil))))))))
    (let ((i 0))
      (loop while (< i total-pages)
            do (let ((type (ldb (byte 2 0) (slot (deref page-table i) 'flags))))
                 (if (= type 3)
                     (setq i (dump-page i))
                     (incf i)))))
    (let* ((n-pages (count 1 pages))
           (tot (* n-pages gencgc-card-bytes))
           (waste (- tot n-code-bytes)))
      (format t "~&Used-bytes=~D Pages=~D Waste=~D (~F%)~%"
              n-code-bytes n-pages waste
              (* 100 (/ waste tot))))))

#+nil ; for debugging
(defun show-immobile-spaces (which)
  (flet ((show (obj type size)
           (declare (ignore type size))
           (let ((*print-pretty* nil))
             (format t "~x: ~s~%" (get-lisp-obj-address obj) obj))))
    (when (or (eq which :fixed) (eq which :both))
      (format t "Fixedobj space~%==============~%")
      (map-objects-in-range #'show
        (%make-lisp-obj fixedobj-space-start)
        (%make-lisp-obj (sap-int *fixedobj-space-free-pointer*))))
    (when (or (eq which :variable) (eq which :both))
      (format t "Varyobj space~%=============~%")
      (map-objects-in-range #'show
        (%make-lisp-obj varyobj-space-start)
        (%make-lisp-obj (sap-int *varyobj-space-free-pointer*))))))

#+gencgc
(defun generation-of (object)
  (let* ((addr (get-lisp-obj-address object))
         (page (find-page-index addr)))
    (cond ((>= page 0) (slot (deref page-table page) 'gen))
          #+immobile-space
          ((immobile-space-addr-p addr)
           ;; SIMPLE-FUNs don't contain a generation byte
           (when (simple-fun-p object)
             (setq addr (get-lisp-obj-address (fun-code-header object))))
           (let ((sap (int-sap (logandc2 addr lowtag-mask))))
             (logand (if (fdefn-p object) (sap-ref-8 sap 1) (sap-ref-8 sap 3))
                     #xF))))))

;;; Show objects in a much simpler way than print-allocated-objects.
;;; Probably don't use this for generation 0 of dynamic space. Other spaces are ok.
;;; (And this is removed from the image; it's meant for developers)
#+gencgc
(defun show-generation-objs (gen space)
  (let ((*print-pretty* nil))
    (map-allocated-objects
     (lambda (obj type size)
       (declare (ignore type))
       (when (= (generation-of obj) gen)
         (format t "~x ~3x ~s~%" (get-lisp-obj-address obj) size obj)))
     space)))

;;; Unfortunately this is a near total copy of the test in gc.impure.lisp
(defun !ensure-genesis-code/data-separation ()
  #+gencgc
  (let* ((n-bits (+ next-free-page 10))
         (code-bits (make-array n-bits :element-type 'bit))
         (data-bits (make-array n-bits :element-type 'bit))
         (total-code-size 0))
    (map-allocated-objects
     (lambda (obj type size)
       (declare ((and fixnum (integer 1)) size))
       ;; M-A-O disables GC, therefore GET-LISP-OBJ-ADDRESS is safe
       (let ((obj-addr (get-lisp-obj-address obj))
             (array (cond ((= type code-header-widetag)
                           (incf total-code-size size)
                           code-bits)
                          (t
                           data-bits)))
             (other-array (cond ((= type code-header-widetag)
                                 data-bits)
                                (t
                                 code-bits))))
         ;; This is not the most efficient way to update the bit arrays,
         ;; but the simplest and clearest for sure. (The loop could avoided
         ;; if the current page is the same as the previously seen page)
         (loop for index from (find-page-index obj-addr)
               to (find-page-index (truly-the word
                                              (+ (logandc2 obj-addr lowtag-mask)
                                                 (1- size))))
               do (cond ((= (sbit other-array index) 1)
                         (format t "~&broken on page index ~d base ~x~%"
                                 index
                                 (+ dynamic-space-start (* index gencgc-card-bytes)))
                         (alien-funcall (extern-alien "ldb_monitor" (function void))))
                        (t
                         (setf (sbit array index) 1))))))
     :dynamic)))
;;; Because pseudo-static objects can not move nor be freed,
;;; this is a valid test that genesis separated code and data.
(!ensure-genesis-code/data-separation)

(defun hexdump (thing &optional (n-words nil wordsp)
                                ;; pass NIL explicitly if T crashes on you
                                (decode t))
  (multiple-value-bind (obj addr count)
      (if (integerp thing)
          (values nil thing (if wordsp n-words 1))
          (values
           thing
           (logandc2 (get-lisp-obj-address thing) lowtag-mask)
           (if wordsp
               n-words
               (if (and (typep thing 'code-component) (plusp (code-n-entries thing)))
                   ;; Display up through the first fun header
                   (+ (code-header-words thing)
                      (ash (%code-fun-offset thing 0) (- word-shift))
                      simple-fun-insts-offset)
                   ;; at most 16 words
                   (min 16 (ash (primitive-object-size thing) (- word-shift)))))))
    (with-pinned-objects (obj)
      (dotimes (i count)
        (let ((word (sap-ref-word (int-sap addr) (ash i word-shift))))
          (multiple-value-bind (lispobj ok) (if decode (make-lisp-obj word nil))
            (let ((*print-lines* 1)
                  (*print-pretty* t))
              (format t "~x: ~v,'0x~:[~; = ~S~]~%"
                      (+ addr (ash i word-shift))
                      (* 2 n-word-bytes)
                      word ok lispobj))))))))
#+sb-thread
(defun show-tls-map ()
  (let ((list
         (sort (sb-vm::list-allocated-objects
                :all
                :type sb-vm:symbol-widetag
                :test (lambda (x) (plusp (sb-kernel:symbol-tls-index x))))
               #'<
               :key #'sb-kernel:symbol-tls-index))
        (prev 0))
    (dolist (x list)
      (let ((n  (ash (sb-kernel:symbol-tls-index x) (- sb-vm:word-shift))))
        (when (and (> n sb-thread::tls-index-start)
                   (> n (1+ prev)))
          (format t "(unused)~%"))
        (format t "~5d = ~s~%" n x)
        (setq prev n)))))

(in-package "SB-C")
;;; As soon as practical in warm build it makes sense to add
;;; cold-allocation-point-fixups into the weak hash-table.
(defvar *!cold-allocation-point-fixups*)
(let ((hash-table (make-hash-table :test 'eq)))
  ;; Group by code component
  (loop for (code . offset) across *!cold-allocation-point-fixups*
        do (push offset (gethash code hash-table)))
  (dohash ((code list) hash-table)
    (setf (gethash code *allocation-point-fixups*)
          (convert-alloc-point-fixups code list))))
