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
    (case (functional-kind home)
      (:external
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
    (case (functional-kind home)
      (:external
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
(declaim (ftype (sfunction (local-tn-bit-vector node ir2-block hash-table (or null vop))
                           simple-bit-vector)
                compute-live-vars))
(defun compute-live-vars (live node block var-locs vop)
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
(defun dump-1-location (node block kind label live var-locs vop
                        &optional context)
  (declare (type node node) (type ir2-block block)
           (type (or null local-tn-bit-vector) live)
           (type (or label index) label)
           (type location-kind kind)
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
         (form-number (source-path-form-number path))
         (context (if (opaque-box-p context)
                      (opaque-box-value context)
                      context)))
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
(defun dump-location-from-info (loc var-locs)
  (declare (type location-info loc)
           (type hash-table var-locs))
  (let ((vop (location-info-vop loc)))
    (dump-1-location (vop-node vop)
                     (vop-block vop)
                     (location-info-kind loc)
                     (location-info-label loc)
                     (vop-save-set vop)
                     var-locs
                     vop
                     (location-info-context loc)))
  (values))

;;; Dump out the number of locations and the locations for Block.
(defun dump-block-locations (block locations var-locs)
  (declare (type cblock block) (list locations))
  (unless (and locations
               (eq (location-info-kind (first locations))
                   :non-local-entry))
      (let ((2block (block-info block)))
        (dump-1-location (block-start-node block)
                         2block :block-start
                         (ir2-block-%label 2block)
                         (ir2-block-live-out 2block)
                         var-locs
                         nil)))
  (dolist (loc locations)
    (dump-location-from-info loc var-locs))
  (values))

;;; Return a vector and an integer (or null) suitable for use as the
;;; BLOCKS and TLF-NUMBER in FUN's DEBUG-FUN.
(defun compute-debug-blocks (fun var-locs)
  (declare (type clambda fun) (type hash-table var-locs))
  (let ((*previous-location* 0)
        *previous-live*
        *previous-form-number*
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
                                  var-locs))
          (setf prev-block block)))
      (dolist (loc (ir2-block-locations 2block))
        (if (label-elsewhere-p (location-info-label loc)
                               (location-info-kind loc))
            (push loc elsewhere-locations)
            (push loc locations))))

    (dump-block-locations prev-block (nreverse locations) var-locs)

    (when elsewhere-locations
      (dolist (loc (nreverse elsewhere-locations))
        (dump-location-from-info loc var-locs)))
    ;; lz-compress accept any array of octets and returns a simple-array
    (logically-readonlyize (lz-compress byte-buffer))))

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
     :created (file-info-write-date file-info)
     (if function
         (values :form (let ((direct-file-info (source-info-file-info info)))
                         (when (eq :lisp (file-info-truename direct-file-info))
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
  (let* ((save-tn (and tn (tn-save-tn tn)))
         (kind (and tn (tn-kind tn)))
         (flags 0)
         (info (lambda-var-arg-info var))
         (indirect (and (lambda-var-indirect var)
                        (not (lambda-var-explicit-value-cell var))
                        (neq (lambda-environment fun)
                             (lambda-environment (lambda-var-home var)))))
         ;; Keep this condition in sync with PARSE-COMPILED-DEBUG-VARS
         (large-fixnums (>= (integer-length most-positive-fixnum) 62))
         more)
    (declare (type (and sb-xc:fixnum unsigned-byte) flags))
    (when minimal
      (setq flags (logior flags compiled-debug-var-minimal-p))
      (unless (and tn (tn-offset tn))
        (setq flags (logior flags compiled-debug-var-deleted-p))))
    (when (and (or (eq kind :environment)
                   (and (eq kind :debug-environment)
                        (null (basic-var-sets var))))
               (not (gethash tn (ir2-component-spilled-tns
                                 (component-info *component-being-compiled*))))
               (lexenv-contains-lambda fun
                                       (lambda-lexenv (lambda-var-home var)))
               (not (optional-leaf-p var))) ;; not always initialized
      (setq flags (logior flags compiled-debug-var-environment-live)))
    (when save-tn
      (setq flags (logior flags compiled-debug-var-save-loc-p)))
    (when indirect
      (setq flags (logior flags compiled-debug-var-indirect-p)))
    (when info
      (case (arg-info-kind info)
        (:more-context
         (setq flags (logior flags compiled-debug-var-more-context-p)
               more t))
        (:more-count
         (setq flags (logior flags compiled-debug-var-more-count-p)
               more t))))
    (when (and same-name-p
               (not (or more minimal)))
      (setf flags (logior flags compiled-debug-var-same-name-p)))
    (when large-fixnums
      (cond (indirect
             (setf (ldb (byte 27 8) flags) (tn-sc+offset tn))
             (when save-tn
               (setf (ldb (byte 27 35) flags) (tn-sc+offset save-tn))))
            (t
             (if (and tn (tn-offset tn))
                 (setf (ldb (byte 27 8) flags) (tn-sc+offset tn))
                 (aver minimal))
             (when save-tn
               (setf (ldb (byte 27 35) flags) (tn-sc+offset save-tn))))))
    (vector-push-extend flags buffer)
    (unless (or minimal
                same-name-p
                more) ;; &more vars need no name
      ;; Dumping uninterned symbols as debug var names is kinda silly.
      ;; Reconstruction of the name on fasl load produces a new gensym anyway.
      ;; So rather than waste symbol space, just dump such symbols as strings,
      ;; and PARSE-COMPILED-DEBUG-VARS can create the interned symbol.
      ;; This reduces core size by omitting zillions of symbols whose names
      ;; are spelled the same.
      (vector-push-extend (if (cl:symbol-package name) name (string name)) buffer))

    (cond (indirect
           ;; Indirect variables live in the parent frame, and are
           ;; accessed through a saved frame pointer.
           ;; The first one/two sc-offsets are for the frame pointer,
           ;; the third is for the stack offset.
           (unless large-fixnums
             (vector-push-extend (tn-sc+offset tn) buffer)
             (when save-tn
               (vector-push-extend (tn-sc+offset save-tn) buffer)))
           (vector-push-extend (tn-sc+offset (leaf-info var)) buffer))
          ((not large-fixnums)
           (if (and tn (tn-offset tn))
               (vector-push-extend (tn-sc+offset tn) buffer)
               (aver minimal))
           (when save-tn
             (vector-push-extend (tn-sc+offset save-tn) buffer)))))
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

    (let ((sorted (sort (vars) #'string<
                        :key (lambda (x)
                               (symbol-name (car x)))))
          (prev-name nil)
          (i 0)
          (buffer (make-array 0 :fill-pointer 0 :adjustable t))
          ;; XEPs don't have any useful variables
          (minimal (eq (functional-kind fun) :external)))
      (declare (type index i))
      (loop for (name var . tn) in sorted
            do
            (dump-1-var fun var tn minimal buffer
                        name
                        (and prev-name (eq prev-name name)))
            (setf prev-name name)
            (setf (gethash var var-locs) i)
            (incf i))
      (compact-vector buffer))))

;;; Return a vector suitable for use as the DEBUG-FUN-VARS of
;;; FUN, representing the arguments to FUN in minimal variable format.
(defun compute-minimal-vars (fun)
  (declare (type clambda fun))
  (let ((buffer (make-array 0 :fill-pointer 0 :adjustable t)))
    (dolist (var (lambda-vars fun))
      (dump-1-var fun var (leaf-info var) t buffer))
    (compact-vector buffer)))

;;; Return VAR's relative position in the function's variables (determined
;;; from the VAR-LOCS hashtable).  If VAR is deleted, then return DEBUG-INFO-VAR-DELETED.
(defun debug-location-for (var var-locs)
  (declare (type lambda-var var) (type hash-table var-locs))
  (let ((res (gethash var var-locs)))
    (cond (res)
          (t
           (aver (or (null (leaf-refs var))
                     (not (tn-offset (leaf-info var)))))
           debug-info-var-deleted))))

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
                                     (res debug-info-var-rest)))
                                  (:more-context
                                   (res debug-info-var-more))
                                  (:optional
                                   (unless saw-optional
                                     (res debug-info-var-optional)
                                     (setq saw-optional t))))
                                (res (debug-location-for actual var-locs))
                                (when (arg-info-supplied-p info)
                                  (res debug-info-var-supplied-p)
                                  (res (debug-location-for (pop actual-vars) var-locs))))
                                (t
                                 (res (debug-location-for actual var-locs)))))))
              (dolist (arg (optional-dispatch-arglist od))
                (one-arg arg))))
          (dolist (var (lambda-vars fun))
            (res (debug-location-for var var-locs)))))

    (compact-vector (res))))

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
         (kind (if main-p nil (functional-kind fun)))
         (name (leaf-debug-name fun))
         (name (if (consp name)
                   (case (car name)
                     ((xep tl-xep)
                      (aver (eq kind :external))
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
    (funcall (compiled-debug-fun-ctor kind)
             :name name
             #-fp-and-pc-standard-save :return-pc
             #-fp-and-pc-standard-save (tn-sc+offset (ir2-environment-return-pc 2env))
             #-fp-and-pc-standard-save :return-pc-pass
             #-fp-and-pc-standard-save (tn-sc+offset (ir2-environment-return-pc-pass 2env))
             #-fp-and-pc-standard-save :old-fp
             #-fp-and-pc-standard-save (tn-sc+offset (ir2-environment-old-fp 2env))
             :encoded-locs
             (cdf-encode-locs
              (label-position (ir2-environment-environment-start 2env))
              (label-position (ir2-environment-elsewhere-start 2env))
              (source-path-form-number (node-source-path (lambda-bind fun)))
              (label-position (block-label (lambda-block fun)))
              (when (ir2-environment-closure-save-tn 2env)
                (tn-sc+offset (ir2-environment-closure-save-tn 2env)))
              #+unwind-to-frame-and-call-vop
              (when (ir2-environment-bsp-save-tn 2env)
                (tn-sc+offset (ir2-environment-bsp-save-tn 2env)))
              #-fp-and-pc-standard-save
              (label-position (ir2-environment-lra-saved-pc 2env))
              #-fp-and-pc-standard-save
              (label-position (ir2-environment-cfp-saved-pc 2env))))))

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
    (when (>= level 1)
      (setf (compiled-debug-fun-blocks dfun)
            (compute-debug-blocks fun var-locs)))
    (if (xep-p fun)
        (setf (compiled-debug-fun-returns dfun) :standard)
        (let ((info (tail-set-info (lambda-tail-set fun))))
          (when info
            (cond ((eq (return-info-kind info) :unknown)
                   (setf (compiled-debug-fun-returns dfun)
                         :standard))
                  ((/= level 0)
                   (setf (compiled-debug-fun-returns dfun)
                         (compute-debug-returns fun)))))))
    dfun))

;;;; full component dumping

;;; Compute the full form function map.
(defun compute-debug-fun-map (sorted)
  (declare (list sorted))
  (loop for (fun next) on sorted
        do (setf (compiled-debug-fun-next fun) next))
  (car sorted))

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
  (let ((dfuns nil)
        (var-locs (make-hash-table :test 'eq))
        (*byte-buffer* (make-array 10
                                   :element-type '(unsigned-byte 8)
                                   :fill-pointer 0
                                   :adjustable t))
        (*contexts* (make-array 10
                                :fill-pointer 0
                                :adjustable t))
        component-tlf-num)
    (dolist (lambda (component-lambdas component))
      (unless (empty-fun-p lambda)
       (clrhash var-locs)
       (let ((tlf-num (source-path-tlf-number
                       (node-source-path (lambda-bind lambda)))))
         (if component-tlf-num
             (aver (or (block-compile *compilation*)
                       (= component-tlf-num tlf-num)))
             (setf component-tlf-num tlf-num))
         (push (compute-1-debug-fun lambda var-locs) dfuns))))
    (let* ((sorted (sort dfuns #'< :key #'compiled-debug-fun-offset))
           (fun-map (compute-debug-fun-map sorted)))
      (make-compiled-debug-info
       ;; COMPONENT-NAME is often not useful, and sometimes completely fubar.
       ;; Function names, on the other hand, are seldom unhelpful,
       ;; so if there's only one function, pick that as the component name.
       ;; Otherwise preserve whatever crummy name was already assigned.
       :name (let* ((2comp (component-info component))
                    (entries (sb-c::ir2-component-entries 2comp)))
               (or (and (not (cdr entries))
                        (sb-c::entry-info-name (car entries)))
                   (component-name component)))
       :fun-map fun-map
       :tlf-num+offset (pack-tlf-num+offset
                        component-tlf-num
                        (and component-tlf-num
                             (aref (file-info-positions
                                    (source-info-file-info *source-info*))
                                   component-tlf-num)))
       :contexts (compact-vector *contexts*)))))

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
