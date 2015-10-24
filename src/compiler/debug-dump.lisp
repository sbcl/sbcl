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

(in-package "SB!C")

(deftype byte-buffer () '(vector (unsigned-byte 8)))
(defvar *byte-buffer*)
(declaim (type byte-buffer *byte-buffer*))

;;;; debug blocks

(deftype location-kind ()
  '(member :unknown-return :known-return :internal-error :non-local-exit
           :block-start :call-site :single-value-return :non-local-entry
           :step-before-vop))

;;; The LOCATION-INFO structure holds the information what we need
;;; about locations which code generation decided were "interesting".
(defstruct (location-info
            (:constructor make-location-info (kind label vop))
            (:copier nil))
  ;; The kind of location noted.
  (kind nil :type location-kind)
  ;; The label pointing to the interesting code location.
  (label nil :type (or label index null))
  ;; The VOP that emitted this location (for node, save-set, ir2-block, etc.)
  (vop nil :type vop))

;;; This is called during code generation in places where there is an
;;; "interesting" location: someplace where we are likely to end up
;;; in the debugger, and thus want debug info.
(defun note-debug-location (vop label kind)
  (declare (type vop vop) (type (or label null) label)
           (type location-kind kind))
  (let ((location (make-location-info kind label vop)))
    (setf (ir2-block-locations (vop-block vop))
          (nconc (ir2-block-locations (vop-block vop))
                 (list location)))
    location))

#!-sb-fluid (declaim (inline ir2-block-physenv))
(defun ir2-block-physenv (2block)
  (declare (type ir2-block 2block))
  (block-physenv (ir2-block-block 2block)))

(defun leaf-visible-to-debugger-p (leaf node)
  (or (rassoc leaf (lexenv-vars (node-lexenv node)))
      (labels ((visible-in-call-lexenv (lambda)
                 (when lambda
                   (let ((call-lexenv (lambda-call-lexenv lambda)))
                     (and call-lexenv
                          (or (rassoc leaf (lexenv-vars call-lexenv))
                              (visible-in-call-lexenv (lexenv-lambda call-lexenv))))))))
        (visible-in-call-lexenv (lexenv-lambda (node-lexenv node))))))

;;; Given a local conflicts vector and an IR2 block to represent the
;;; set of live TNs, and the VAR-LOCS hash-table representing the
;;; variables dumped, compute a bit-vector representing the set of
;;; live variables. If the TN is environment-live, we only mark it as
;;; live when it is in scope at NODE.
(defun compute-live-vars (live node block var-locs vop)
  (declare (type ir2-block block) (type local-tn-bit-vector live)
           (type hash-table var-locs) (type node node)
           (type (or vop null) vop))
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
                       (not (member tn spilled))))
          (let ((num (gethash leaf var-locs)))
            (when num
              (setf (sbit res num) 1))))))
    res))

;;; The PC for the location most recently dumped.
(defvar *previous-location*)
(declaim (type index *previous-location*))

;;; Dump a compiled debug-location into *BYTE-BUFFER* that describes
;;; the code/source map and live info. If true, VOP is the VOP
;;; associated with this location, for use in determining whether TNs
;;; are spilled.
(defun dump-1-location (node block kind tlf-num label live var-locs vop)
  (declare (type node node) (type ir2-block block)
           (type (or null local-tn-bit-vector) live)
           (type (or label index) label)
           (type location-kind kind) (type (or index null) tlf-num)
           (type hash-table var-locs) (type (or vop null) vop))

  (let ((byte-buffer *byte-buffer*))
    (vector-push-extend
     (position-or-lose kind *compiled-code-location-kinds*)
     byte-buffer)

    (let ((loc (if (fixnump label) label (label-position label))))
      (write-var-integer (- loc *previous-location*) byte-buffer)
      (setq *previous-location* loc))

    (let ((path (node-source-path node)))
      (unless tlf-num
        (write-var-integer (source-path-tlf-number path) byte-buffer))
      (write-var-integer (source-path-form-number path) byte-buffer))

    (if live
        (write-packed-bit-vector (compute-live-vars live node block var-locs vop)
                                 byte-buffer)
        (write-packed-bit-vector
         (make-array (logandc2 (+ (hash-table-count var-locs) 7) 7)
                     :initial-element 0
                     :element-type 'bit)
         byte-buffer))

    (write-var-string (or (and (typep node 'combination)
                               (combination-step-info node))
                          "")
                      byte-buffer))
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
                     vop))
  (values))

;;; Scan all the blocks, determining if all locations are in the same
;;; TLF, and returning it or NIL.
(defun find-tlf-number (fun)
  (declare (type clambda fun))
  (let* ((source-path (node-source-path (lambda-bind fun)))
         (res (source-path-tlf-number source-path)))
    (declare (type (or index null) res))
    (do-physenv-ir2-blocks (2block (lambda-physenv fun))
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
    (values res (source-path-form-number source-path))))

;;; Dump out the number of locations and the locations for Block.
(defun dump-block-locations (block locations tlf-num var-locs)
  (declare (type cblock block) (list locations))
  (if (and locations
           (eq (location-info-kind (first locations))
               :non-local-entry))
      (write-var-integer (length locations) *byte-buffer*)
      (let ((2block (block-info block)))
        (write-var-integer (+ (length locations) 1) *byte-buffer*)
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
  (multiple-value-bind (tlf-num form-number) (find-tlf-number fun)
   (let ((*previous-location* 0)
         (physenv (lambda-physenv fun))
         (byte-buffer *byte-buffer*)
         prev-block
         locations
         elsewhere-locations)
     (setf (fill-pointer byte-buffer) 0)
     (do-physenv-ir2-blocks (2block physenv)
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
       (write-var-integer (length elsewhere-locations) byte-buffer)
       (dolist (loc (nreverse elsewhere-locations))
         (push loc locations)
         (dump-location-from-info loc tlf-num var-locs)))

     (values (!make-specialized-array (length byte-buffer) '(unsigned-byte 8)
                                      byte-buffer)
             tlf-num form-number))))

;;; Return DEBUG-SOURCE structure containing information derived from
;;; INFO.
(defun debug-source-for-info (info &key function)
  (declare (type source-info info))
  (let ((file-info (get-toplevelish-file-info info)))
    (make-debug-source
     :compiled (source-info-start-time info)

     :namestring (or *source-namestring*
                     (make-file-info-namestring
                      (if (pathnamep (file-info-name file-info))
                          (file-info-name file-info))
                      file-info))
     :created (file-info-write-date file-info)
     :source-root (file-info-source-root file-info)
     :start-positions (coerce-to-smallest-eltype
                       (file-info-positions file-info))

     :form (let ((direct-file-info (source-info-file-info info)))
             (when (eq :lisp (file-info-name direct-file-info))
               (elt (file-info-forms direct-file-info) 0)))
     :function function)))

;;; Given an arbitrary sequence, coerce it to an unsigned vector if
;;; possible. Ordinarily we coerce it to the smallest specialized
;;; vector we can.
;;; During cross-compilation the in-memory representation is opaque -
;;; we don't care how it looks, but can recover the intended specialization.

(defun coerce-to-smallest-eltype (seq)
  (let ((maxoid 0) (length 0))
    (flet ((frob (x)
             (if (typep x 'unsigned-byte)
                 (when (>= x maxoid)
                   (setf maxoid x))
                 (return-from coerce-to-smallest-eltype
                   (coerce seq 'simple-vector)))))
      (if (listp seq)
          (dolist (i seq)
            (incf length) ; so not to traverse again to compute it
            (frob i))
          (dovector (i seq (setq length (length seq)))
            (frob i)))
      (let ((specializer (etypecase maxoid
                           ((unsigned-byte 8) '(unsigned-byte 8))
                           ((unsigned-byte 16) '(unsigned-byte 16))
                           ((unsigned-byte 32) '(unsigned-byte 32))
                           ((unsigned-byte 64) '(unsigned-byte 64)))))
        ;; formerly (coerce seq `(simple-array ,specializer (*)))
        ;; plus a kludge for cross-compilation. This is nicer.
        (!make-specialized-array length specializer seq)))))

;;;; variables

;;; Return a SC-OFFSET describing TN's location.
(defun tn-sc-offset (tn)
  (declare (type tn tn))
  (make-sc-offset (sc-number (tn-sc tn))
                  (tn-offset tn)))

(defun lambda-ancestor-p (maybe-ancestor maybe-descendant)
  (declare (type clambda maybe-ancestor)
           (type (or clambda null) maybe-descendant))
  (loop
     (when (eq maybe-ancestor maybe-descendant)
       (return t))
     (setf maybe-descendant (lambda-parent maybe-descendant))
     (when (null maybe-descendant)
       (return nil))))

;;; Dump info to represent VAR's location being TN. ID is an integer
;;; that makes VAR's name unique in the function. BUFFER is the vector
;;; we stick the result in. If MINIMAL, we suppress name dumping, and
;;; set the minimal flag.
;;;
;;; The DEBUG-VAR is only marked as always-live if the TN is
;;; environment live and is an argument. If a :DEBUG-ENVIRONMENT TN,
;;; then we also exclude set variables, since the variable is not
;;; guaranteed to be live everywhere in that case.
(defun dump-1-var (fun var tn id minimal buffer)
  (declare (type lambda-var var) (type (or tn null) tn) (type index id)
           (type clambda fun))
  (let* ((name (leaf-debug-name var))
         (save-tn (and tn (tn-save-tn tn)))
         (kind (and tn (tn-kind tn)))
         (flags 0)
         (info (lambda-var-arg-info var))
         (indirect (and (lambda-var-indirect var)
                        (not (lambda-var-explicit-value-cell var))
                        (neq (lambda-physenv fun)
                             (lambda-physenv (lambda-var-home var))))))
    (declare (type index flags))
    (when minimal
      (setq flags (logior flags compiled-debug-var-minimal-p))
      (unless (and tn (tn-offset tn))
        (setq flags (logior flags compiled-debug-var-deleted-p))))
    (when (and (or (eq kind :environment)
                   (and (eq kind :debug-environment)
                        (null (basic-var-sets var))))
               (not (gethash tn (ir2-component-spilled-tns
                                 (component-info *component-being-compiled*))))
               (lambda-ancestor-p (lambda-var-home var) fun))
      (setq flags (logior flags compiled-debug-var-environment-live)))
    (when save-tn
      (setq flags (logior flags compiled-debug-var-save-loc-p)))
    (unless (or (zerop id) minimal)
      (setq flags (logior flags compiled-debug-var-id-p)))
    (when indirect
      (setq flags (logior flags compiled-debug-var-indirect-p)))
    (when info
      (case (arg-info-kind info)
        (:more-context
         (setq flags (logior flags compiled-debug-var-more-context-p)))
        (:more-count
         (setq flags (logior flags compiled-debug-var-more-count-p)))))
    #!+64-bit
    (cond (indirect
           (setf (ldb (byte 27 8) flags) (tn-sc-offset tn))
           (when save-tn
             (setf (ldb (byte 27 35) flags) (tn-sc-offset save-tn))))
          (t
           (if (and tn (tn-offset tn))
               (setf (ldb (byte 27 8) flags) (tn-sc-offset tn))
               (aver minimal))
           (when save-tn
             (setf (ldb (byte 27 35) flags) (tn-sc-offset save-tn)))))
    (vector-push-extend flags buffer)
    (unless minimal
      (vector-push-extend name buffer)
      (unless (zerop id)
        (vector-push-extend id buffer)))

    (cond (indirect
           ;; Indirect variables live in the parent frame, and are
           ;; accessed through a saved frame pointer.
           ;; The first one/two sc-offsets are for the frame pointer,
           ;; the third is for the stack offset.
           #!-64-bit
           (vector-push-extend (tn-sc-offset tn) buffer)
           #!-64-bit
           (when save-tn
             (vector-push-extend (tn-sc-offset save-tn) buffer))
           (vector-push-extend (tn-sc-offset (leaf-info var)) buffer))
          #!-64-bit
          (t
           (if (and tn (tn-offset tn))
               (vector-push-extend (tn-sc-offset tn) buffer)
               (aver minimal))
           (when save-tn
             (vector-push-extend (tn-sc-offset save-tn) buffer)))))
  (values))

;;; Return a vector suitable for use as the DEBUG-FUN-VARS
;;; of FUN. LEVEL is the current DEBUG-INFO quality. VAR-LOCS is a
;;; hash table in which we enter the translation from LAMBDA-VARS to
;;; the relative position of that variable's location in the resulting
;;; vector.
(defun compute-vars (fun level var-locs)
  (declare (type clambda fun) (type hash-table var-locs))
  (collect ((vars))
    (labels ((frob-leaf (leaf tn gensym-p)
               (let ((name (leaf-debug-name leaf)))
                 (when (and name (leaf-refs leaf) (tn-offset tn)
                            (or gensym-p (symbol-package name)))
                   (vars (cons leaf tn)))))
             (frob-lambda (x gensym-p)
               (dolist (leaf (lambda-vars x))
                 (frob-leaf leaf (leaf-info leaf) gensym-p))))
      (frob-lambda fun t)
      (when (>= level 2)
        (dolist (x (ir2-physenv-closure (physenv-info (lambda-physenv fun))))
          (let ((thing (car x)))
            (when (lambda-var-p thing)
              (frob-leaf thing (cdr x) (= level 3)))))

        (dolist (let (lambda-lets fun))
          (frob-lambda let (= level 3)))))

    (let ((sorted (sort (vars) #'string<
                        :key (lambda (x)
                               (symbol-name (leaf-debug-name (car x))))))
          (prev-name nil)
          (id 0)
          (i 0)
          (buffer (make-array 0 :fill-pointer 0 :adjustable t)))
      (declare (type (or simple-string null) prev-name)
               (type index id i))
      (dolist (x sorted)
        (let* ((var (car x))
               (name (symbol-name (leaf-debug-name var))))
          (cond ((and prev-name (string= prev-name name))
                 (incf id))
                (t
                 (setq id 0  prev-name name)))
          (dump-1-var fun var (cdr x) id nil buffer)
          (setf (gethash var var-locs) i)
          (incf i)))
      (coerce buffer 'simple-vector))))

;;; Return a vector suitable for use as the DEBUG-FUN-VARS of
;;; FUN, representing the arguments to FUN in minimal variable format.
(defun compute-minimal-vars (fun)
  (declare (type clambda fun))
  (let ((buffer (make-array 0 :fill-pointer 0 :adjustable t)))
    (dolist (var (lambda-vars fun))
      (dump-1-var fun var (leaf-info var) 0 t buffer))
    (coerce buffer 'simple-vector)))

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
   (mapcar (lambda (loc)
             (tn-sc-offset loc))
           (return-info-locations (tail-set-info (lambda-tail-set fun))))))

;;;; debug functions

;;; Return a C-D-F structure with all the mandatory slots filled in.
(defun dfun-from-fun (fun)
  (declare (type clambda fun))
  (let* ((2env (physenv-info (lambda-physenv fun)))
         (dispatch (lambda-optional-dispatch fun))
         (main-p (and dispatch
                      (eq fun (optional-dispatch-main-entry dispatch)))))
    (make-compiled-debug-fun
     :name (leaf-debug-name fun)
     :kind (if main-p nil (functional-kind fun))
     #!-fp-and-pc-standard-save :return-pc
     #!-fp-and-pc-standard-save (tn-sc-offset (ir2-physenv-return-pc 2env))
     #!-fp-and-pc-standard-save :old-fp
     #!-fp-and-pc-standard-save (tn-sc-offset (ir2-physenv-old-fp 2env))
     :start-pc (label-position (ir2-physenv-environment-start 2env))
     :elsewhere-pc (label-position (ir2-physenv-elsewhere-start 2env))
     :closure-save (when (ir2-physenv-closure-save-tn 2env)
                     (tn-sc-offset (ir2-physenv-closure-save-tn 2env)))
     #!+unwind-to-frame-and-call-vop
     :bsp-save
     #!+unwind-to-frame-and-call-vop
     (when (ir2-physenv-bsp-save-tn 2env)
       (tn-sc-offset (ir2-physenv-bsp-save-tn 2env))))))

;;; Return a complete C-D-F structure for FUN. This involves
;;; determining the DEBUG-INFO level and filling in optional slots as
;;; appropriate.
(defun compute-1-debug-fun (fun var-locs)
  (declare (type clambda fun) (type hash-table var-locs))
  (let* ((dfun (dfun-from-fun fun))
         (actual-level (policy (lambda-bind fun) compute-debug-fun))
         (level (if #!+sb-dyncount *collect-dynamic-statistics*
                    #!-sb-dyncount nil
                    (max actual-level 2)
                    actual-level))
         (toplevel-p (eq :toplevel (compiled-debug-fun-kind dfun))))
    (cond ((or (zerop level) toplevel-p))
          ((and (<= level 1)
                (let ((od (lambda-optional-dispatch fun)))
                  (or (not od)
                      (not (eq (optional-dispatch-main-entry od) fun)))))
           (setf (compiled-debug-fun-vars dfun)
                 (compute-minimal-vars fun))
           (setf (compiled-debug-fun-arguments dfun) :minimal))
          (t
           (setf (compiled-debug-fun-vars dfun)
                 (compute-vars fun level var-locs))
           (setf (compiled-debug-fun-arguments dfun)
                 (compute-args fun var-locs))))

    (if (and (>= level 2) (not toplevel-p))
        (multiple-value-bind (blocks tlf-num form-number)
            (compute-debug-blocks fun var-locs)
          (setf (compiled-debug-fun-blocks dfun) blocks
                (compiled-debug-fun-tlf-number dfun) tlf-num
                (compiled-debug-fun-form-number dfun) form-number))
        (multiple-value-bind (tlf-num form-number) (find-tlf-number fun)
          (setf (compiled-debug-fun-tlf-number dfun) tlf-num
                (compiled-debug-fun-form-number dfun) form-number)))
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

;;; Compute the full form (simple-vector) function map.
(defun compute-debug-fun-map (sorted)
  (declare (list sorted))
  (let* ((len (1- (* (length sorted) 2)))
         (funs-vec (make-array len)))
    (do ((i -1 (+ i 2))
         (sorted sorted (cdr sorted)))
        ((= i len))
      (declare (fixnum i))
      (let ((dfun (car sorted)))
        (unless (minusp i)
          (setf (svref funs-vec i) (car dfun)))
        (setf (svref funs-vec (1+ i)) (cdr dfun))))
    funs-vec))

;;; Return a DEBUG-INFO structure describing COMPONENT. This has to be
;;; called after assembly so that source map information is available.
(defun debug-info-for-component (component)
  (declare (type component component))
  (let ((dfuns nil)
        (var-locs (make-hash-table :test 'eq))
        (*byte-buffer* (make-array 10
                                   :element-type '(unsigned-byte 8)
                                   :fill-pointer 0
                                   :adjustable t)))
    (dolist (lambda (component-lambdas component))
      (clrhash var-locs)
      (push (cons (label-position (block-label (lambda-block lambda)))
                  (compute-1-debug-fun lambda var-locs))
            dfuns))
    (let* ((sorted (sort dfuns #'< :key #'car))
           (fun-map (compute-debug-fun-map sorted)))
      (make-compiled-debug-info :name (component-name component)
                                :fun-map fun-map))))

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
