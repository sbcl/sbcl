;;;; Identical Code Folding (similar to what might be done by a C linker)

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;

(in-package "SB-VM")

;;; Deduce equivalence between compiled code components so that all but
;;; one canonical representative of each can be eliminated.
;;;
;;; Unlike in C++ where this operation can be sometimes safe,
;;; and tools try to distinguish between safe and unsafe folding (see [*])
;;; in Lisp it is * NEVER * truly safe. But we can opt to do it anyway.
;;; The reason it is not safe is that we have no way of knowing whether
;;; some random bit of code will later execute something like:
;;;   (ASSERT (NOT (EQ (EVAL `#',(THING)) (FDEFINITION OTHERTHING))))
;;; to check some assumed invariant that would have been valid prior to folding
;;; but becomes invalid if function pointers are unexpectedly changed
;;; without the application's knowlege.
;;; (And needless to say, this affects EQ-based hashing which we take care of)
;;;
;;; [*] https://ai.google/research/pubs/pub36912
;;;

(defun apply-forwarding-map (map &optional print)
  (when print
    (let ((*print-pretty* nil))
      (dohash ((k v) map)
        (format t "~s = ~s~%" k v))))
  (flet ((forward (object)
           (typecase object
             (code-component (gethash object map object))
             (simple-fun
              (let* ((code (fun-code-header object))
                     (new (gethash code map)))
                (if new
                    (%primitive sb-c:compute-fun
                                new
                                (- (%fun-code-offset object)
                                   (ash (code-header-words new)
                                        word-shift)))
                    object)))
             (t object))))
    (map-allocated-objects
     (lambda (object widetag size &aux touchedp)
       (declare (ignore size))
       (macrolet ((rewrite (place &aux (accessor (if (listp place) (car place))))
                    ;; These two slots have no setters, but nor are
                    ;; they possibly affected by code folding.
                    (unless (member accessor '(symbol-name symbol-package))
                      `(let* ((oldval ,place) (newval (forward oldval)))
                         (unless (eq newval oldval)
                           ,(case accessor
                              (data-vector-ref
                               `(setf (svref ,@(cdr place)) newval touchedp t))
                              (value-cell-ref
                               ;; pinned already because we're iterating over the heap
                               ;; which disables GC, but maybe some day it won't.
                               `(with-pinned-objects (object)
                                  (setf (sap-ref-lispobj (int-sap (get-lisp-obj-address object))
                                                         (- (ash value-cell-value-slot word-shift)
                                                            other-pointer-lowtag))
                                        newval)))
                              (weak-pointer-value
                               `(let ((newval-gen (generation-of newval)))
                                  ;; Preserve GC invariant that a weak pointer
                                  ;; can't point to an object younger than itself.
                                  (cond ((and (fixnump newval-gen)
                                              (< newval-gen (generation-of object)))
                                         #+nil
                                         (warn "Can't update weak pointer ~s" object))
                                        (t
                                         (with-pinned-objects (object)
                                           (setf (sap-ref-lispobj
                                                  (int-sap (get-lisp-obj-address object))
                                                  (- (ash weak-pointer-value-slot word-shift)
                                                     other-pointer-lowtag))
                                                 newval))))))
                              (%primitive
                               (ecase (cadr place)
                                 (fast-symbol-global-value
                                  `(setf (symbol-global-value ,@(cddr place)) newval))))
                              (t
                               `(setf ,place newval))))))))
         (do-referenced-object (object rewrite)
           (simple-vector
            :extend
            (when (and (= (get-header-data object) vector-valid-hashing-subtype)
                       touchedp)
              (setf (svref object 1) 1)))
           (fdefn
            :override
            (let* ((oldval (fdefn-fun object))
                   (newval (forward oldval)))
              (unless (eq newval oldval)
                (setf (fdefn-fun object) newval))))
           (closure
            :override
            (let* ((oldval (%closure-fun object))
                   (newval (forward oldval)))
              (unless (eq newval oldval)
                (warn "Not able to forward a closure function yet: ~S" oldval)))
            (dotimes (i (1- (get-closure-length object)))
              (let* ((oldval (%closure-index-ref object i))
                     (newval (forward oldval)))
                (unless (eq newval oldval)
                  (with-pinned-objects (object)
                    (setf (sap-ref-lispobj (int-sap (get-lisp-obj-address object))
                                           (- (ash (+ i closure-info-offset) word-shift)
                                              fun-pointer-lowtag))
                                        newval))))))
           (ratio :override)
           ((complex rational) :override)
           (t
            :extend
            (case widetag
              (#.value-cell-widetag
               (rewrite (value-cell-ref object)))
              (t
               (bug "Unknown object type #x~x addr=~x"
                    widetag (get-lisp-obj-address object))))))))
     :all)))

(declaim (inline fun-entry))
(defun fun-entry (fun)
  #-(or x86 x86-64)
  (int-sap (+ (get-lisp-obj-address fun)
              (- fun-pointer-lowtag)
              (ash simple-fun-code-offset word-shift)))
  ;; The preceding case would actually work, but I'm anticipating a change
  ;; in which simple-fun objects are all contiguous in their code component,
  ;; followed by all the machine instructions for all the simple-funs.
  ;; If that change is done, then you must indirect through the SELF pointer
  ;; in order to get the correct starting address.
  ;; (Such change would probably be confined to x86[-64])
  #+(or x86 x86-64)
  (sap-ref-sap (int-sap (- (get-lisp-obj-address fun) fun-pointer-lowtag))
               (ash simple-fun-self-slot word-shift)))

;;; TODO: code needs to be considered equivalent in the abstract,
;;; not at the byte level. In particular, operations that encode relative
;;; jumps and such need to be considered equal if the absolute target
;;; is the same. This will likely entail running the disassembler.
(defun code-equivalent-p (obj1 obj2)
  (with-pinned-objects (obj1 obj2)
    (and (= (code-header-words obj1) (code-header-words obj2))
         (= (%code-code-size obj1) (%code-code-size obj2))
         (= (code-n-unboxed-data-bytes obj1)
            (code-n-unboxed-data-bytes obj2))
         ;; Compare boxed constants. (Ignore debug-info and fixups)
         (loop for i from code-constants-offset
               below (code-header-words obj1)
               always (eq (code-header-ref obj1 i) (code-header-ref obj2 i)))
         ;; Compare unboxed constants
         (let ((nwords (ceiling (code-n-unboxed-data-bytes obj1) n-word-bytes))
               (sap1 (code-instructions obj1))
               (sap2 (code-instructions obj2)))
           (dotimes (i nwords t)
             (unless (= (sap-ref-word sap1 (ash i word-shift))
                        (sap-ref-word sap2 (ash i word-shift)))
               (return nil))))
         ;; Compare instruction bytes
         (dotimes (i (code-n-entries obj1) t)
           (let* ((f1 (%code-entry-point obj1 i))
                  (f2 (%code-entry-point obj2 i))
                  (l1 (%simple-fun-text-len f1 i))
                  (l2 (%simple-fun-text-len f2 i)))
             (unless (and (= l1 l2)
                          (= 0 (alien-funcall
                                (extern-alien "memcmp"
                                              (function int system-area-pointer
                                                        system-area-pointer long))
                                (fun-entry f1)
                                (fun-entry f2)
                                l1)))
               (return nil)))))))

;;; Compute a key for binning purposes.
(defun compute-code-hash-key (code)
  (with-pinned-objects (code)
    (flet ((unboxed-bytes-digest ()
             ;; Mix at most 20 words of the instructions of each simple-fun.
             (let ((hash 0))
               (dotimes (i (code-n-entries code) hash)
                 (let* ((f (%code-entry-point code i))
                        (start (fun-entry f))
                        (len (%simple-fun-text-len f i)))
                   (dotimes (i (min 20 (ceiling len n-word-bytes)))
                     (setq hash (mix (logand (sap-ref-word
                                              start (ash i word-shift))
                                             most-positive-fixnum)
                                     hash))))))))
      (list* (code-header-words code)
             (unboxed-bytes-digest)
             (collect ((consts))
               ;; Ignore the fixups and the debug info
               (do ((i (1- (code-header-words code)) (1- i)))
                   ((< i code-constants-offset) (consts))
                 (consts (code-header-ref code i))))))))

(defun fold-identical-code (&key aggressive print)
  #+gencgc (gc :gen 7)
  ;; Pass 1: count code objects.  I'd like to enhance MAP-ALLOCATED-OBJECTS
  ;; to have a mode that scans only GC pages with that can hold code
  ;; (or any subset of page types). This is fine though.
  (let ((code-objects
         (let ((count 0))
           (map-allocated-objects
            (lambda (obj widetag size)
              (declare (ignore size))
              (when (and (eql widetag code-header-widetag)
                         (plusp (code-n-entries obj)))
                (incf count)))
            :all)
           (make-weak-vector count)))
        (referenced-objects
         (make-hash-table :test #'eq)))
    ;; Pass 2: collect them.
    (let ((i 0))
      (map-allocated-objects
       (lambda (obj widetag size)
         (declare (ignore size))
         (when (and (eql widetag code-header-widetag)
                    (plusp (code-n-entries obj)))
           (setf (aref code-objects i) obj)
           (incf i)))
       :all))
    (unless aggressive
      ;; Figure out which of those are referenced by any object
      ;; except for an fdefn.
      (flet ((visit (referent referer)
               (when (typep referent 'simple-fun)
                 (setq referent (fun-code-header referent)))
               (when (and (typep referent 'code-component)
                          (plusp (code-n-entries referent))
                          (not (gethash referent referenced-objects)))
                 (setf (gethash referent referenced-objects) referer))))
        ;; Scan the whole heap, and look at each object for pointers
        ;; to any code object.
        (map-allocated-objects
         (lambda (referer widetag size)
           (declare (ignore size))
           (unless (or (eql widetag fdefn-widetag)
                       (weak-vector-p referer))
             (do-referenced-object (referer visit referer)
               ;; maybe this should be the default fallback?
               (t
                :extend
                (case (widetag-of referer)
                  (#.value-cell-widetag
                   (visit (value-cell-ref referer) referer))
                  (t
                   (bug "Unknown object type #x~x ~s" widetag referer)))))))
         :all)))
    ;; Now place objects that possibly match into the same bin.
    (let ((bins (make-hash-table :test 'equal))
          (n 0))
      (dovector (x code-objects)
        (unless (gethash x referenced-objects)
          (incf n)
          (push x (gethash (compute-code-hash-key x) bins))))
      #+nil
      (format t "~d objects, ~d candidates, ~d bins~%"
              (length code-objects) n (hash-table-count bins))
      ;; Scan each bin that has more than one object in it
      ;; and check whether any are equivalent to one another.
      (let ((equiv-map (make-hash-table :test #'eq)))
        (dohash ((key objects) bins)
          (declare (ignore key))
          ;; Skip if there can not possibly be an equivalence class
          ;; with more than one thing in it.
          (when (cdr objects)
            (let (equivalences)
              (dolist (item objects)
                (let ((found (assoc item equivalences :test #'code-equivalent-p)))
                  (if found
                      (push item (cdr found))
                      (push (list item) equivalences))))
              (dolist (set equivalences)
                (when (cdr set) ; have two or more, so choose one as canonical
                  (let ((winner
                         (labels ((keyfn (x)
                                    #+64-bit
                                    (%code-serialno x)
                                    ;; Creation time is an estimate of order, but
                                    ;; frankly we shouldn't be storing times anyway,
                                    ;; as it causes irreproducible builds.
                                    ;; But I don't know what else to do.
                                    #-64-bit
                                    (sb-c::debug-source-compiled
                                     (sb-c::compiled-debug-info-source
                                      (%code-debug-info x))))
                                  (compare (a b) (if (< (keyfn a) (keyfn b)) a b)))
                           (reduce #'compare set))))
                    (dolist (obj (delete winner set))
                      (setf (gethash obj equiv-map) winner))))))))
        (apply-forwarding-map equiv-map print)))))
