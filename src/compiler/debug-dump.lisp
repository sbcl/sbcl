;;;; stuff that creates debugger information from the compiler's
;;;; internal data structures

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

(deftype byte-buffer () '(vector (unsigned-byte 8)))
(defvar *byte-buffer*)
(declaim (type byte-buffer *byte-buffer*))
(defvar *contexts*)
(declaim (type (vector t) *contexts*))
(defvar *local-call-context*)
(defvar *location-context* nil)

;;;; debug blocks

(deftype location-kind ()
  '(member :unknown-return :known-return :internal-error :non-local-exit
           :block-start :call-site :single-value-return :non-local-entry))

;;; The LOCATION-INFO structure holds the information what we need
;;; about locations which code generation decided were "interesting".
(defstruct (location-info
            (:constructor make-location-info (kind label vop context))
            (:copier nil))
  ;; The kind of location noted.
  (kind nil :type location-kind)
  (context nil)
  ;; The label pointing to the interesting code location.
  (label nil :type (or label index null))
  ;; The VOP that emitted this location (for node, save-set, ir2-block, etc.)
  (vop nil :type vop))

(defstruct (restart-location
            (:constructor make-restart-location (&optional label tn))
            (:copier nil))
  (label nil :type (or null label))
  (tn nil :type (or null tn) :read-only t))
(!set-load-form-method restart-location (:xc :target) :ignore-it)

;;; This is called during code generation in places where there is an
;;; "interesting" location: someplace where we are likely to end up
;;; in the debugger, and thus want debug info.
(defun note-debug-location (vop label kind &optional context)
  (declare (type vop vop) (type (or label null) label)
           (type location-kind kind))
  (let ((location (make-location-info kind label vop context)))
    (setf (ir2-block-locations (vop-block vop))
          (nconc (ir2-block-locations (vop-block vop))
                 (list location)))
    location))

(defun note-this-location (vop kind)
  "NOTE-THIS-LOCATION VOP Kind
  Note that the current code location is an interesting (to the debugger)
  location of the specified Kind. VOP is the VOP responsible for this code.
  This VOP must specify some non-null :SAVE-P value (perhaps :COMPUTE-ONLY) so
  that the live set is computed."
  (let ((lab (gen-label)))
    (emit-label lab)
    (note-debug-location vop lab kind *location-context*)))

(defun note-next-instruction (vop kind)
  "NOTE-NEXT-INSTRUCTION VOP Kind
   Similar to NOTE-THIS-LOCATION, except the use the location of the next
   instruction for the code location, wherever the scheduler decided to put
   it."
  (let ((loc (note-debug-location vop nil kind)))
    (emit-postit (lambda (segment posn)
                   (setf (location-info-label loc)
                         (- posn (segment-header-skew segment))))))
  (values))

(declaim (inline ir2-block-environment))
(defun ir2-block-environment (2block)
  (declare (type ir2-block 2block))
  (block-environment (ir2-block-block 2block)))

(defun make-lexenv-var-cache (lexenv)
  (or (lexenv-var-cache lexenv)
      (let ((cache (make-hash-table :test #'eq)))
        (labels ((populate (lexenv)
                   (loop for (nil . var) in (lexenv-vars lexenv)
                         when (lambda-var-p var)
                         do (setf (gethash var cache) t))
                   (let* ((lambda (lexenv-lambda lexenv))
                          (call-lexenv (and lambda
                                            (lambda-call-lexenv lambda))))
                     (cond ((not call-lexenv))
                           ((lexenv-var-cache call-lexenv)
                            (loop for var being each hash-key of (lexenv-var-cache call-lexenv)
                                  do (setf (gethash var cache) t)))
                           (t
                            (populate call-lexenv))))))
          (populate lexenv))
        (setf (lexenv-var-cache lexenv) cache))))

(defun leaf-visible-to-debugger-p (leaf node)
  (gethash leaf (make-lexenv-var-cache (node-lexenv node))))

(defun optional-leaf-p (leaf)
  (let ((home (lambda-var-home leaf)))
    (functional-kind-case home
      (external
       (let ((entry (lambda-entry-fun home)))
         (when (optional-dispatch-p entry)
           (let ((pos (position leaf (lambda-vars home))))
             (and pos
                  (>= (1- pos) (optional-dispatch-min-args entry))))))))))

;;; Type checks of the arguments in an external function happen before
;;; all the &optionals are initialized. PROPAGATE-TO-ARGS marks such
;;; locations with the entry point, which allows to deduce if an
;;; optional is alive at that point.
(defun optional-processed (leaf)
  (let ((home (lambda-var-home leaf)))
    (functional-kind-case home
      (external
       (let ((entry (lambda-entry-fun home)))
         (if (and (optional-dispatch-p entry)
                  *local-call-context*)
             (let ((pos (position leaf (lambda-vars home)))
                   (entry-pos (position *local-call-context*
                                        (optional-dispatch-entry-points entry)
                                        :key #'force)))
               (not (and pos
                         entry-pos
                         (>= (1- pos) (+ (optional-dispatch-min-args entry) entry-pos)))))
             t)))
      (t t))))

;;; Given a local conflicts vector and an IR2 block to represent the
;;; set of live TNs, and the VAR-LOCS hash-table representing the
;;; variables dumped, compute a bit-vector representing the set of
;;; live variables. If the TN is environment-live, we only mark it as
;;; live when it is in scope at NODE.
(defun compute-live-vars (live node block var-locs vop)
  (declare (type ir2-block block) (type local-tn-bit-vector live)
           (type hash-table var-locs) (type node node)
           (type (or vop null) vop)
           #-sb-xc-host (values simple-bit-vector))
  (let ((res (make-array (logandc2 (+ (hash-table-count var-locs) 7) 7)
                         :element-type 'bit
                         :initial-element 0))
        (spilled (gethash vop
                          (ir2-component-spilled-vops
                           (component-info *component-being-compiled*)))))
    (do-live-tns (tn live block)
      (let ((leaf (tn-leaf tn)))
        (when (and (lambda-var-p leaf)
                   (or (not (member (tn-kind tn)
                                    '(:environment :debug-environment)))
                       (leaf-visible-to-debugger-p leaf node))
                   (or (null spilled)
                       (not (member tn spilled)))
                   (optional-processed leaf))
          (let ((num (gethash leaf var-locs)))
            (when num

              (setf (sbit res num) 1))))))
    res))

;;; The PC for the location most recently dumped.
(defvar *previous-location*)
(declaim (type index *previous-location*))
(defvar *previous-live*)
(defvar *previous-form-number*)

(defun encode-restart-location (location x)
  (typecase x
    (restart-location
     (let ((offset (- (label-position (restart-location-label x))
                      location))
           (tn (restart-location-tn x))
           (registers-size #.(integer-length (sb-size (sb-or-lose 'sb-vm::registers)))))
       (if tn
           (the fixnum (logior (ash offset registers-size)
                               (tn-offset tn)))
           offset)))
    (cons (let ((last (cdr (last x))))
            (if (restart-location-p last)
                (let ((new (copy-list x)))
                  (setf (cdr (last new))
                        (encode-restart-location location last))
                  new)
                x)))
    (t
     x)))

(defun decode-restart-location (x)
  (declare (fixnum x))
  (let ((registers-size #.(integer-length (sb-size (sb-or-lose 'sb-vm::registers)))))
    (values (make-sc+offset sb-vm:descriptor-reg-sc-number
                            (ldb (byte registers-size 0) x))
            (ash x (- registers-size)))))

;;; Dump a compiled debug-location into *BYTE-BUFFER* that describes
;;; the code/source map and live info. If true, VOP is the VOP
;;; associated with this location, for use in determining whether TNs
;;; are spilled.
(defun dump-1-location (node block kind tlf-num label live var-locs vop
                        &optional context)
  (declare (type node node) (type ir2-block block)
           (type (or null local-tn-bit-vector) live)
           (type (or label index) label)
           (type location-kind kind) (type (or index null) tlf-num)
           (type hash-table var-locs) (type (or vop null) vop))
  (let* ((byte-buffer *byte-buffer*)
         (stepping (and (combination-p node)
                        (combination-step-info node)))
         (*local-call-context*
           (if (local-call-context-p context)
               (local-call-context-fun context)))
         (context (if (local-call-context-p context)
                      (local-call-context-var context)
                      context))
         (live (and live
                    (compute-live-vars live node block var-locs vop)))
         (anything-alive (and live
                              (find 1 live)))
         (equal-live (and anything-alive
                          (equal live *previous-live*)))

         (path (node-source-path node))
         (loc (if (fixnump label) label (label-position label)))
         (form-number (source-path-form-number path)))
    (vector-push-extend
     (logior
      (if context
          compiled-code-location-context
          0)
      (if stepping
          compiled-code-location-stepping
          0)
      (if (zerop form-number)
          compiled-code-location-zero-form-number
          0)
      (cond (equal-live
             (cond ((eql form-number *previous-form-number*)
                    ;; Repurpose this bit for equal-form-number since
                    ;; -equal-live and -live don't need to appear at the
                    ;; same time.
                    (setf form-number 0)
                    compiled-code-location-live)
                   (t
                    0)))
            (anything-alive
             compiled-code-location-live)
            (t
             0))
      (if equal-live
          compiled-code-location-equal-live
          0)
      (position-or-lose kind +compiled-code-location-kinds+))
     byte-buffer)
    (write-var-integer (- loc *previous-location*) byte-buffer)
    (setq *previous-location* loc)

    (unless tlf-num
      (write-var-integer (source-path-tlf-number path) byte-buffer))
    (unless (zerop form-number)
      (setf *previous-form-number* form-number)
      (write-var-integer form-number byte-buffer))

    (when (and anything-alive
               (not equal-live))
      (setf *previous-live* live)
      (write-packed-bit-vector live byte-buffer))
    (when stepping
      (write-var-string stepping byte-buffer))
    (when context
      (let ((context (encode-restart-location loc context)))
        (write-var-integer (or (position context *contexts* :test #'equal)
                               (vector-push-extend context *contexts*))
                           byte-buffer))))
  (values))

;;; Extract context info from a Location-Info structure and use it to
;;; dump a compiled code-location.
(defun dump-location-from-info (loc tlf-num var-locs)
  (declare (type location-info loc) (type (or index null) tlf-num)
           (type hash-table var-locs))
  (let ((vop (location-info-vop loc)))
    (dump-1-location (vop-node vop)
                     (vop-block vop)
                     (location-info-kind loc)
                     tlf-num
                     (location-info-label loc)
                     (vop-save-set vop)
                     var-locs
                     vop
                     (location-info-context loc)))
  (values))

;;; Scan all the blocks, determining if all locations are in the same
;;; TLF, and returning it or NIL.
(defun find-tlf-number (fun)
  (declare (type clambda fun))
  (let ((res (source-path-tlf-number (node-source-path (lambda-bind fun)))))
    (declare (type (or index null) res))
    (do-environment-ir2-blocks (2block (lambda-environment fun))
      (let ((block (ir2-block-block 2block)))
        (when (eq (block-info block) 2block)
          (unless (eql (source-path-tlf-number
                        (node-source-path
                         (block-start-node block)))
                       res)
            (setq res nil)))

        (dolist (loc (ir2-block-locations 2block))
          (unless (eql (source-path-tlf-number
                        (node-source-path
                         (vop-node (location-info-vop loc))))
                       res)
            (setq res nil)))))
    res))

;;; Dump out the number of locations and the locations for Block.
(defun dump-block-locations (block locations tlf-num var-locs)
  (declare (type cblock block) (list locations))
  (unless (and locations
               (eq (location-info-kind (first locations))
                   :non-local-entry))
      (let ((2block (block-info block)))
        (dump-1-location (block-start-node block)
                         2block :block-start tlf-num
                         (ir2-block-%label 2block)
                         (ir2-block-live-out 2block)
                         var-locs
                         nil)))
  (dolist (loc locations)
    (dump-location-from-info loc tlf-num var-locs))
  (values))

;;; Return a vector and an integer (or null) suitable for use as the
;;; BLOCKS and TLF-NUMBER in FUN's DEBUG-FUN.
(defun compute-debug-blocks (fun var-locs)
  (declare (type clambda fun) (type hash-table var-locs))
  (let ((*previous-location* 0)
        *previous-live*
        *previous-form-number*
        (tlf-num (find-tlf-number fun))
        (env (lambda-environment fun))
        (byte-buffer *byte-buffer*)
        prev-block
        locations
        elsewhere-locations)
    (setf (fill-pointer byte-buffer) 0)
    (do-environment-ir2-blocks (2block env)
      (let ((block (ir2-block-block 2block)))
        (when (eq (block-info block) 2block)
          (when prev-block
            (dump-block-locations prev-block (nreverse (shiftf locations nil))
                                  tlf-num var-locs))
          (setf prev-block block)))
      (dolist (loc (ir2-block-locations 2block))
        (if (label-elsewhere-p (location-info-label loc)
                               (location-info-kind loc))
            (push loc elsewhere-locations)
            (push loc locations))))

    (dump-block-locations prev-block (nreverse locations)
                          tlf-num var-locs)

    (when elsewhere-locations
      (dolist (loc (nreverse elsewhere-locations))
        (dump-location-from-info loc tlf-num var-locs)))
    (values (coerce byte-buffer '(simple-array (unsigned-byte 8) (*))) tlf-num)))

;;; Return DEBUG-SOURCE structure containing information derived from
;;; INFO.
(defun debug-source-for-info (info &key function)
  (declare (type source-info info))
  (let ((file-info (get-toplevelish-file-info info)))
    (multiple-value-call
        (if function 'sb-di::make-core-debug-source 'make-debug-source)
     :namestring (or *source-namestring*
                     (make-file-info-namestring
                      (let ((pathname
                             (case *name-context-file-path-selector*
                               (pathname (file-info-pathname file-info))
                               (truename (file-info-truename file-info)))))
                        (if (pathnamep pathname) pathname))
                      file-info))
      :created
      #+sb-xc-host (file-info-write-date file-info)
      #-sb-xc-host (let ((source-date-epoch (posix-getenv "SOURCE_DATE_EPOCH"))
                         (file-write-date (file-info-write-date file-info)))
                     (if source-date-epoch
                         (multiple-value-bind (val end)
                             (parse-integer source-date-epoch :junk-allowed t)
                           (if (and (= end (length source-date-epoch))
                                    (and val file-write-date)
                                    (< (+ val unix-to-universal-time) file-write-date))
                               (+ val unix-to-universal-time)
                               file-write-date))
                         file-write-date))
      :start-positions (coerce-to-smallest-eltype
                        (file-info-positions file-info))
     (if function
         (values :form (let ((direct-file-info (source-info-file-info info)))
                         (when (eq :lisp (file-info-%truename direct-file-info))
                           (elt (file-info-forms direct-file-info) 0)))
                 :function function)
         (values)))))

(defun smallest-element-type (integer negative)
  (let ((bits (max (+ (integer-length integer)
                      (if negative 1 0))
                   8)))
    (list (if negative
              'signed-byte
              'unsigned-byte)
          (if (= (logcount bits) 1) ;; power of two?
              bits
              ;; Next power of two
              (ash 1 (integer-length bits))))))

;;; Given an arbitrary sequence, coerce it to an unsigned vector if
;;; possible. Ordinarily we coerce it to the smallest specialized
;;; vector we can.
;;; During cross-compilation the in-memory representation is opaque -
;;; we don't care how it looks, but can recover the intended specialization.
(defun coerce-to-smallest-eltype (seq)
  (declare (type sequence seq))
  (let ((max-positive 0)
        (max-negative 0)
        (length 0))
    (flet ((frob (x)
             (typecase x
               ((integer 0)
                (when (>= x max-positive)
                  (setf max-positive x)))
               ((integer * -1)
                (let ((abs (- x)))
                  (when (>= abs max-negative)
                    (setf max-negative abs))))
               (t
                (return-from coerce-to-smallest-eltype
                  (logically-readonlyize
                   (coerce seq 'simple-vector)))))))
      (if (listp seq)
          (dolist (i seq)
            (incf length)     ; so not to traverse again to compute it
            (frob i))
          (dovector (i seq (setq length (length seq)))
            (frob i)))
      (if (zerop length)
          #()
          (logically-readonlyize
           (coerce seq
                   `(simple-array
                     ,(smallest-element-type (max max-positive
                                                  (1- max-negative))
                                             (plusp max-negative))
                     1)))))))

(defun compact-vector (sequence)
  (cond ((and (= (length sequence) 1)
              (not (typep (elt sequence 0) '(and vector (not string)))))
         (elt sequence 0))
        (t
         (coerce-to-smallest-eltype sequence))))

;;;; variables

;;; Return a SC+OFFSET describing TN's location.
(defun tn-sc+offset (tn)
  (declare (type tn tn))
  (make-sc+offset (sc-number (tn-sc tn))
                  (tn-offset tn)))

;;; Dump info to represent VAR's location being TN. ID is an integer
;;; that makes VAR's name unique in the function. BUFFER is the vector
;;; we stick the result in. If MINIMAL, we suppress name dumping, and
;;; set the minimal flag.
;;;
;;; The DEBUG-VAR is only marked as always-live if the TN is
;;; environment live and is an argument. If a :DEBUG-ENVIRONMENT TN,
;;; then we also exclude set variables, since the variable is not
;;; guaranteed to be live everywhere in that case.
(defun dump-1-var (fun var tn minimal buffer &optional name same-name-p)
  (declare (type lambda-var var) (type (or tn null) tn)
           (type clambda fun))
  (let* ((package (sb-xc:symbol-package name))
         (package-p (and package (not (eq package *package*))))
         (save-tn (and tn (tn-save-tn tn)))
         (kind (and tn (tn-kind tn)))
         (flags 0)
         (indirect (and (lambda-var-indirect var)
                        (not (lambda-var-explicit-value-cell var))
                        (neq (lambda-environment fun)
                             (lambda-environment (lambda-var-home var))))))
    (declare (type (and sb-xc:fixnum unsigned-byte) flags))
    (let ((info (lambda-var-arg-info var)))
      ;; &more vars need no name
      (when (and info
                 (memq (arg-info-kind info)
                       '(:more-context :more-count)))
        (setq minimal t)))
    (cond (minimal
           (setq flags (logior flags compiled-debug-var-minimal-p))
           (unless (and tn (tn-offset tn))
             (setq flags (logior flags compiled-debug-var-deleted-p))))
          (t
           (unless package
             (setq flags (logior flags compiled-debug-var-uninterned)))
           (when package-p
             (setq flags (logior flags compiled-debug-var-packaged)))))
    (when (and (or (eq kind :environment)
                   (and (eq kind :debug-environment)
                        (null (basic-var-sets var))))
               (not (gethash tn (ir2-component-spilled-tns
                                 (component-info *component-being-compiled*))))
               (or (eq (lambda-var-home var) fun)
                   (member var (environment-closure (lambda-environment fun))))
               (not (optional-leaf-p var))) ;; not always initialized
      (setq flags (logior flags compiled-debug-var-environment-live)))
    (when save-tn
      (setq flags (logior flags compiled-debug-var-save-loc-p)))
    (when indirect
      (setq flags (logior flags compiled-debug-var-indirect-p)))
    (when (and same-name-p (not minimal))
      (setq flags (logior flags compiled-debug-var-same-name-p)))
    (vector-push-extend flags buffer)
    (unless minimal
      (unless same-name-p
        (write-var-string (symbol-name name) buffer))
      (when package-p
        (write-var-string (sb-xc:package-name package) buffer)))

    (cond (indirect
           ;; Indirect variables live in the parent frame, and are
           ;; accessed through a saved frame pointer.
           ;; The first one/two sc-offsets are for the frame pointer,
           ;; the third is for the stack offset.
           (write-var-integer (tn-sc+offset tn) buffer)
           (when save-tn
             (write-var-integer (tn-sc+offset save-tn) buffer))
           (write-var-integer (tn-sc+offset (leaf-info var)) buffer))
          (t
           (if (and tn (tn-offset tn))
               (write-var-integer (tn-sc+offset tn) buffer)
               (aver minimal))
           (when save-tn
             (write-var-integer (tn-sc+offset save-tn) buffer)))))
  (values))

(defun leaf-principal-name (leaf)
  ;; If all the references are from the same substituted variable
  ;; use its name.
  ;; Helps with &key processing variables.
  (let ((refs (leaf-refs leaf)))
    (loop for ref in refs
          for name = (ref-%source-name ref)
          for first-name = name then first-name
          unless (eq name first-name)
          return (leaf-debug-name leaf)
          finally (return name))))

;;; Return a vector suitable for use as the DEBUG-FUN-VARS
;;; of FUN. LEVEL is the current DEBUG-INFO quality. VAR-LOCS is a
;;; hash table in which we enter the translation from LAMBDA-VARS to
;;; the relative position of that variable's location in the resulting
;;; vector.
(defun compute-vars (fun level var-locs)
  (declare (type clambda fun) (type hash-table var-locs))
  (collect ((vars))
    (labels ((frob-leaf (leaf tn gensym-p)
               (let ((name (leaf-principal-name leaf)))
                 (when (and name (leaf-refs leaf) (tn-offset tn)
                            (or gensym-p (cl:symbol-package name)))
                   (vars (list* name leaf tn)))))
             (frob-lambda (x gensym-p)
               (dolist (leaf (lambda-vars x))
                 (frob-leaf leaf (leaf-info leaf) gensym-p))))
      (frob-lambda fun t)
      (when (>= level 1)
        (dolist (x (ir2-environment-closure (environment-info (lambda-environment fun))))
          (let ((thing (car x)))
            (when (lambda-var-p thing)
              (frob-leaf thing (cdr x) (>= level 2)))))

        (dolist (let (lambda-lets fun))
          (frob-lambda let (>= level 2)))))

    (setf (fill-pointer *byte-buffer*) 0)
    (let ((sorted (sort (vars) #'string<
                        :key (lambda (x)
                               (symbol-name (car x)))))
          (prev-name nil)
          (i 0)
          ;; XEPs don't have any useful variables
          (minimal (functional-kind-eq fun external)))
      (declare (type index i))
      (loop for (name var . tn) in sorted
            do
            (dump-1-var fun var tn minimal *byte-buffer*
                        name
                        (and prev-name (eq prev-name name)))
            (setf prev-name name)
            (setf (gethash var var-locs) i)
            (incf i))
      (copy-seq *byte-buffer*))))

;;; Return a vector suitable for use as the DEBUG-FUN-VARS of
;;; FUN, representing the arguments to FUN in minimal variable format.
(defun compute-minimal-vars (fun)
  (declare (type clambda fun))
  (setf (fill-pointer *byte-buffer*) 0)
  (dolist (var (lambda-vars fun))
    (dump-1-var fun var (leaf-info var) t *byte-buffer*))
  (copy-seq *byte-buffer*))

;;; Return VAR's relative position in the function's variables (determined
;;; from the VAR-LOCS hashtable).  If VAR is deleted, then return DELETED.
(defun debug-location-for (var var-locs)
  (declare (type lambda-var var) (type hash-table var-locs))
  (let ((res (gethash var var-locs)))
    (cond (res)
          (t
           (aver (or (null (leaf-refs var))
                     (not (tn-offset (leaf-info var)))))
           'deleted))))

;;;; arguments/returns

;;; Return a vector to be used as the COMPILED-DEBUG-FUN-ARGS for FUN.
;;; If FUN is the MAIN-ENTRY for an optional dispatch, then look at
;;; the ARGLIST to determine the syntax, otherwise pretend all
;;; arguments are fixed.
;;;
;;; ### This assumption breaks down in EPs other than the main-entry,
;;; since they may or may not have supplied-p vars, etc.
(defun compute-args (fun var-locs)
  (declare (type clambda fun) (type hash-table var-locs))
  (collect ((res))
    (let ((od (lambda-optional-dispatch fun)))
      (if (and od (eq (optional-dispatch-main-entry od) fun))
          (let ((actual-vars (lambda-vars fun))
                (saw-optional nil))
            (labels ((one-arg (arg)
                       (let ((info (lambda-var-arg-info arg))
                             (actual (pop actual-vars)))
                         (cond (info
                                (case (arg-info-kind info)
                                  (:keyword
                                   (res (arg-info-key info)))
                                  (:rest
                                   (let ((more (arg-info-default info)))
                                     (cond ((and (consp more) (third more))
                                            (one-arg (first (arg-info-default info)))
                                            (one-arg (second (arg-info-default info)))
                                            (return-from one-arg))
                                           (more
                                            (setf (arg-info-default info) t)))
                                     (res 'rest-arg)))
                                  (:more-context
                                   (res 'more-arg))
                                  (:optional
                                   (unless saw-optional
                                     (res 'optional-args)
                                     (setq saw-optional t))))
                                (res (debug-location-for actual var-locs))
                                (when (arg-info-supplied-p info)
                                  (res 'supplied-p)
                                  (res (debug-location-for (pop actual-vars) var-locs))))
                                (t
                                 (res (debug-location-for actual var-locs)))))))
              (dolist (arg (optional-dispatch-arglist od))
                (one-arg arg))))
          (dolist (var (lambda-vars fun))
            (res (debug-location-for var var-locs)))))

    (coerce-to-smallest-eltype (res))))

;;; Return a vector of SC offsets describing FUN's return locations.
;;; (Must be known values return...)
(defun compute-debug-returns (fun)
  (coerce-to-smallest-eltype
   (mapcar #'tn-sc+offset
           (return-info-locations (tail-set-info (lambda-tail-set fun))))))

;;;; debug functions

;;; Return a C-D-F structure with all the mandatory slots filled in.
(defun dfun-from-fun (fun)
  (declare (type clambda fun))
  (let* ((2env (environment-info (lambda-environment fun)))
         (dispatch (lambda-optional-dispatch fun))
         (main-p (and dispatch
                      (eq fun (optional-dispatch-main-entry dispatch))))
         (kind (if main-p nil (ecase (functional-kind fun)
                                (#.(functional-kind-attributes nil) nil)
                                (#.(functional-kind-attributes optional) :optional)
                                (#.(functional-kind-attributes external) :external)
                                (#.(functional-kind-attributes toplevel) :toplevel)
                                (#.(functional-kind-attributes cleanup) :cleanup))))
         (name (leaf-debug-name fun))
         (name (if (consp name)
                   (case (car name)
                     ((xep tl-xep)
                      (aver (eql kind :external))
                      (second name))
                     (&optional-processor
                      (setf kind :optional)
                      (second name))
                     (&more-processor
                      (setf kind :more)
                      (second name))
                     (t
                      name))
                   name)))
    (make-compiled-debug-fun
     :name name
     :kind kind
     #-fp-and-pc-standard-save :return-pc
     #-fp-and-pc-standard-save (tn-sc+offset (ir2-environment-return-pc 2env))
     #-fp-and-pc-standard-save :return-pc-pass
     #-fp-and-pc-standard-save (tn-sc+offset (ir2-environment-return-pc-pass 2env))
     #-fp-and-pc-standard-save :old-fp
     #-fp-and-pc-standard-save (tn-sc+offset (ir2-environment-old-fp 2env))
     #-fp-and-pc-standard-save :lra-saved-pc
     #-fp-and-pc-standard-save (label-position (ir2-environment-lra-saved-pc 2env))
     #-fp-and-pc-standard-save :cfp-saved-pc
     #-fp-and-pc-standard-save (label-position (ir2-environment-cfp-saved-pc 2env))
     :closure-save
     (when (ir2-environment-closure-save-tn 2env)
       (tn-sc+offset (ir2-environment-closure-save-tn 2env)))
     #+unwind-to-frame-and-call-vop :bsp-save
     #+unwind-to-frame-and-call-vop
     (when (ir2-environment-bsp-save-tn 2env)
       (tn-sc+offset (ir2-environment-bsp-save-tn 2env)))
     :start-pc
     (label-position (ir2-environment-environment-start 2env))
     :elsewhere-pc
     (label-position (ir2-environment-elsewhere-start 2env)))))

;;; Return a complete C-D-F structure for FUN. This involves
;;; determining the DEBUG-INFO level and filling in optional slots as
;;; appropriate.
(defun compute-1-debug-fun (fun var-locs)
  (declare (type clambda fun) (type hash-table var-locs))
  (let* ((dfun (dfun-from-fun fun))
         (actual-level (policy (lambda-bind fun) compute-debug-fun))
         (level (cond #+sb-dyncount
                      (*collect-dynamic-statistics*
                       (max actual-level 2))
                      (actual-level))))
    (cond ((or (and (zerop level)
                    (let ((od (lambda-optional-dispatch fun)))
                      (or (not od)
                          (not (eq (optional-dispatch-main-entry od) fun)))))
               (eq (compiled-debug-fun-kind dfun) :more))
           (setf (compiled-debug-fun-vars dfun)
                 (compute-minimal-vars fun))
           (setf (compiled-debug-fun-arguments dfun) :minimal))
          (t
           (setf (compiled-debug-fun-vars dfun)
                 (compute-vars fun level var-locs))
           (setf (compiled-debug-fun-arguments dfun)
                 (compute-args fun var-locs))))

    (if (>= level 1)
        (multiple-value-bind (blocks tlf-num)
            (compute-debug-blocks fun var-locs)
          (setf (compiled-debug-fun-blocks dfun) blocks
                (compiled-debug-fun-tlf-number dfun) tlf-num))
        (setf (compiled-debug-fun-tlf-number dfun) (find-tlf-number fun)))

    (if (xep-p fun)
        (setf (compiled-debug-fun-returns dfun) :standard)
        (let ((info (tail-set-info (lambda-tail-set fun))))
          (when info
            (cond ((eq (return-info-kind info) :unknown)
                   (setf (compiled-debug-fun-returns dfun)
                         :standard))
                  ((eq (return-info-kind info) :unboxed))
                  ((/= level 0)
                   (setf (compiled-debug-fun-returns dfun)
                         (compute-debug-returns fun)))))))
    dfun))


;;;; Packed debug functions:

;;; Dump a packed binary representation of a DFUN into *BYTE-BUFFER*.
;;; PREV-START and START are the byte offsets in the code where the
;;; previous function started and where this one starts.
;;; PREV-ELSEWHERE is the previous function's elsewhere PC.
(defun dump-1-packed-dfun (dfun prev-start start prev-elsewhere)
  (declare (type compiled-debug-fun dfun)
           (type index prev-start start prev-elsewhere)
           (special *debug-component-name*))
  (let* ((name (compiled-debug-fun-name dfun))
         (same-name-p (eq name
                          *debug-component-name*)))
    (let ((options 0))
      (setf (ldb packed-debug-fun-kind-byte options)
            (position-or-lose (compiled-debug-fun-kind dfun)
                              packed-debug-fun-kinds))
      (setf (ldb packed-debug-fun-returns-byte options)
            (etypecase (compiled-debug-fun-returns dfun)
              ((member :standard) packed-debug-fun-returns-standard)
              ((member :fixed) packed-debug-fun-returns-fixed)
              (vector packed-debug-fun-returns-specified)))
      (vector-push-extend options *byte-buffer*))

    (let ((flags 0))
      (when (compiled-debug-fun-vars dfun)
        (setq flags (logior flags packed-debug-fun-variables-bit)))
      (when (compiled-debug-fun-blocks dfun)
        (setq flags (logior flags packed-debug-fun-blocks-bit)))
      (when (compiled-debug-fun-tlf-number dfun)
        (setq flags (logior flags packed-debug-fun-tlf-number-bit)))
      (unless (memq (compiled-debug-fun-arguments dfun) '(nil :minimal))
        (setq flags (logior flags packed-debug-fun-non-minimal-arguments-bit)))
      (when (compiled-debug-fun-closure-save dfun)
        (setq flags (logior flags packed-debug-fun-closure-save-loc-bit)))
      #+unwind-to-frame-and-call-vop
      (when (compiled-debug-fun-bsp-save dfun)
        (setq flags (logior flags packed-debug-fun-bsp-save-loc-bit)))
      (when same-name-p
        (setq flags (logior flags packed-debug-fun-previous-name)))
      (vector-push-extend flags *byte-buffer*))
    (unless same-name-p
      (setf *debug-component-name* name)
      (write-var-integer (or (position name *contexts* :test #'equal)
                             (vector-push-extend name *contexts*))
                         *byte-buffer*))

    (let ((vars (compiled-debug-fun-vars dfun)))
      (when vars
        (let ((len (length vars)))
          (write-var-integer len *byte-buffer*)
          (dotimes (i len)
            (vector-push-extend (aref vars i) *byte-buffer*)))))

    (let ((blocks (compiled-debug-fun-blocks dfun)))
      (when blocks
        (let ((len (length blocks)))
          (write-var-integer len *byte-buffer*)
          (dotimes (i len)
            (vector-push-extend (mask-field (byte 8 0) (aref blocks i))
                                *byte-buffer*)))))

    (when (compiled-debug-fun-tlf-number dfun)
      (write-var-integer (compiled-debug-fun-tlf-number dfun)
                         *byte-buffer*))

    (let ((arguments (compiled-debug-fun-arguments dfun)))
      (unless (memq arguments '(nil :minimal))
        (let ((len (length arguments)))
          (write-var-integer len *byte-buffer*)
          (dotimes (i len)
            (let ((argument (aref arguments i)))
              (case argument
                (deleted
                 (write-var-integer packed-debug-fun-arg-deleted
                                    *byte-buffer*))
                (supplied-p
                 (write-var-integer packed-debug-fun-arg-supplied-p
                                    *byte-buffer*))
                (optional
                 (write-var-integer packed-debug-fun-arg-optional
                                    *byte-buffer*))
                (rest
                 (write-var-integer packed-debug-fun-arg-rest
                                    *byte-buffer*))
                (more
                 (write-var-integer packed-debug-fun-arg-more
                                    *byte-buffer*))
                (otherwise
                 (cond ((integerp argument)
                        (write-var-integer (+ argument
                                              packed-debug-fun-arg-index-offset)
                                           *byte-buffer*))
                       ((keywordp argument)
                        (write-var-integer packed-debug-fun-key-arg-keyword
                                           *byte-buffer*)
                        (write-var-string (symbol-name argument)
                                          *byte-buffer*))
                       ((sb-xc:symbol-package argument)
                        (write-var-integer packed-debug-fun-key-arg-packaged
                                           *byte-buffer*)
                        (write-var-string (symbol-name argument)
                                          *byte-buffer*)
                        (write-var-string (sb-xc:package-name
                                           (sb-xc:symbol-package argument))
                                          *byte-buffer*))
                       (t
                        (write-var-integer packed-debug-fun-key-arg-uninterned
                                           *byte-buffer*)
                        (write-var-string (symbol-name argument)
                                          *byte-buffer*))))))))))

    (let ((returns (compiled-debug-fun-returns dfun)))
      (when (vectorp returns)
        (let ((len (length returns)))
          (write-var-integer len *byte-buffer*)
          (dotimes (i len)
            (write-var-integer (aref returns i) *byte-buffer*)))))

    #-fp-and-pc-standard-save
    (progn
      (write-var-integer (compiled-debug-fun-return-pc dfun)
                         *byte-buffer*)
      (write-var-integer (compiled-debug-fun-return-pc-pass dfun)
                         *byte-buffer*)
      (write-var-integer (compiled-debug-fun-old-fp dfun)
                         *byte-buffer*)
      (write-var-integer (compiled-debug-fun-lra-saved-pc dfun)
                         *byte-buffer*)
      (write-var-integer (compiled-debug-fun-cfp-saved-pc dfun)
                         *byte-buffer*))
    (when (compiled-debug-fun-closure-save dfun)
      (write-var-integer (compiled-debug-fun-closure-save dfun)
                         *byte-buffer*))
    #+unwind-to-frame-and-call-vop
    (when (compiled-debug-fun-bsp-save dfun)
      (write-var-integer (compiled-debug-fun-bsp-save dfun)
                         *byte-buffer*))
    (write-var-integer (- start prev-start) *byte-buffer*)
    (write-var-integer (- (compiled-debug-fun-start-pc dfun) start)
                       *byte-buffer*)
    (write-var-integer (- (compiled-debug-fun-elsewhere-pc dfun)
                          prev-elsewhere)
                       *byte-buffer*)))

;;; Return a byte-vector holding all the debug functions for a
;;; component in the packed binary PACKED-DEBUG-FUN format.
(defun compute-packed-debug-funs (dfuns)
  (declare (list dfuns))
  (setf (fill-pointer *byte-buffer*) 0)
  (let ((prev-start 0)
        (prev-elsewhere 0))
    (dolist (dfun dfuns)
      (let ((start (car dfun))
            (elsewhere (compiled-debug-fun-elsewhere-pc (cdr dfun))))
        (dump-1-packed-dfun (cdr dfun) prev-start start prev-elsewhere)
        (setq prev-start start prev-elsewhere elsewhere))))
  (logically-readonlyize (compress *byte-buffer*)))


;;;; full component dumping

(defun empty-fun-p (fun)
  (let* ((2block (block-info (lambda-block fun)))
         (start (ir2-block-start-vop 2block))
         (next (ir2-block-next 2block)))
    (and
     start
     (eq start (ir2-block-last-vop 2block))
     (eq (vop-name start) 'note-environment-start)
     next
     (neq (ir2-block-environment 2block)
          (ir2-block-environment next)))))

;;; Return a DEBUG-INFO structure describing COMPONENT. This has to be
;;; called after assembly so that source map information is available.
(defun debug-info-for-component (component)
  (declare (type component component))
  (let* ((dfuns nil)
         (simple-fun-headers
          ;; Compute all simple-fun metadata and store into a simple-vector
          (let* ((entries (ir2-component-entries (component-info component)))
                 (nfuns (length entries))
                 (i (* sb-vm:code-slots-per-simple-fun nfuns))
                 (v (make-array i)))
            (dolist (e entries v)
              ;; Process in reverse order of ENTRIES.
              (decf i sb-vm:code-slots-per-simple-fun)
              (setf (svref v (+ i sb-vm:simple-fun-name-slot)) (entry-info-name e)
                    (svref v (+ i sb-vm:simple-fun-arglist-slot)) (entry-info-arguments e)
                    (svref v (+ i sb-vm:simple-fun-source-slot)) (entry-info-form/doc e)
                    (svref v (+ i sb-vm:simple-fun-info-slot)) (entry-info-type/xref e)))))
         (var-locs (make-hash-table :test 'eq))
         (*byte-buffer* (make-array 10
                                    :element-type '(unsigned-byte 8)
                                    :fill-pointer 0
                                    :adjustable t))
         (*contexts* (make-array 10
                                 :fill-pointer 0
                                 :adjustable t))
         (lambdas (sort (copy-list (component-lambdas component))
                        #'<
                        :key (lambda (lambda)
                               (label-position (block-label (lambda-block lambda))))))
         (name (loop for lambda in lambdas
                     for entry = (leaf-info lambda)
                     when entry
                     return
                     (entry-info-name entry)))
         (*debug-component-name* name))
    (declare (special *debug-component-name*))
    (dolist (lambda lambdas)
      (unless (empty-fun-p lambda)
        (clrhash var-locs)
        (push (cons (label-position (block-label (lambda-block lambda)))
                    (compute-1-debug-fun lambda var-locs))
              dfuns)))
    (let ((map (compute-packed-debug-funs (nreverse dfuns)))
          (contexts (compact-vector *contexts*)))
      #+sb-xc-host
      (!make-compiled-debug-info name *package* map contexts simple-fun-headers)
      #-sb-xc-host
      (let ((di (%make-instance (+ (sb-kernel::type-dd-length compiled-debug-info)
                                   (length simple-fun-headers)))))
        (setf (%instance-layout di) #.(find-layout 'compiled-debug-info)
              ;; The fixed slots except for SOURCE are declared readonly
              (%instance-ref di (get-dsd-index compiled-debug-info name)) name
              (%instance-ref di (get-dsd-index compiled-debug-info source)) nil
              (%instance-ref di (get-dsd-index compiled-debug-info package)) *package*
              (%instance-ref di (get-dsd-index compiled-debug-info fun-map)) map
              (%instance-ref di (get-dsd-index compiled-debug-info contexts)) contexts)
        (let ((i (get-dsd-index compiled-debug-info rest)))
          (dovector (x simple-fun-headers di)
            (setf (%instance-ref di i) x)
            (incf i)))))))

;;; Write BITS out to BYTE-BUFFER in backend byte order. The length of
;;; BITS must be evenly divisible by eight.
(defun write-packed-bit-vector (bits byte-buffer)
  (declare (type simple-bit-vector bits) (type byte-buffer byte-buffer))

  ;; Enforce constraint from CMU-CL-era comment.
  (aver (zerop (mod (length bits) 8)))

  (multiple-value-bind (initial step done)
      (ecase *backend-byte-order*
        (:little-endian (values 0  1  8))
        (:big-endian    (values 7 -1 -1)))
    (let ((shift initial)
          (byte 0))
      (dotimes (i (length bits))
        (let ((int (aref bits i)))
          (setf byte (logior byte (ash int shift)))
          (incf shift step))
        (when (= shift done)
          (vector-push-extend byte byte-buffer)
          (setf shift initial
                byte 0)))
      (unless (= shift initial)
        (vector-push-extend byte byte-buffer))))
  (values))

(declaim (ftype (sfunction ((and byte-buffer (not simple-array)))
                           (or (simple-array (unsigned-byte 8) 1)
                               (simple-array (signed-byte 8) 1)))
                compress)
         (ftype (sfunction ((or (simple-array (unsigned-byte 8) 1)
                                (simple-array (signed-byte 8) 1)))
                           (simple-array (unsigned-byte 8) 1))
                decompress))

#-(and sb-core-compression (not sb-xc-host))
(progn
  (defun compress (vector)
    #-sb-xc-host (%shrink-vector (%array-data vector) (length vector))
    #+sb-xc-host (coerce vector '(simple-array (unsigned-byte 8) 1)))

  (defun decompress (vector)
    vector))

#+(and sb-core-compression (not sb-xc-host))
(progn
  (defun compress (vector)
    (with-alien ((compress-vector (function int (* char) size-t) :extern "compress_vector"))
      (let* ((data (truly-the (simple-array * (*)) (%array-data vector))))
        (with-pinned-objects (data)
          (alien-funcall compress-vector (int-sap (get-lisp-obj-address data)) (length vector)))
        data)))

  (defun decompress (vector)
    (if (typep vector '(simple-array (signed-byte 8) (*)))
        (with-alien ((decompress-vector (function (* unsigned-char) (* char) (* size-t)) :extern "decompress_vector")
                     (size size-t))
          (let* ((pointer
                   (with-pinned-objects (vector)
                     (alien-funcall decompress-vector (int-sap (get-lisp-obj-address vector)) (addr size))))
                 (length size)
                 (new (make-array length :element-type '(unsigned-byte 8))))
            (loop for i below length
                  do (setf (aref new i) (deref pointer i)))
            (free-alien pointer)
            new))
        vector)))
