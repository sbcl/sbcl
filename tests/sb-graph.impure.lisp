(require 'sb-graph)
(require 'sb-posix)
(require 'uiop)

;;; These are tests for the sb-graph contrib module. Due to the nature
;;; of the features creating a bunch of files, I relocated the tests
;;; to here.

(defmacro with-graph (&body forms)
  `(compile-forms-as-file-with-tracing ',forms))
(defun compile-forms-as-file-with-tracing (forms)
  (declare (type list forms))
  (let* ((sb-c::*compile-trace-targets* (cons :sb-graph sb-c::*compile-trace-targets*))
         (dir (pathname (concatenate 'string (scratch-file-name) "/"))))
    (sb-posix:mkdir dir #b111111111)
    (let ((lisp (merge-pathnames (pathname-name (pathname (scratch-file-name "lisp")))
                                 dir))
          (trace (merge-pathnames (pathname-name (pathname (scratch-file-name "trace")))
                                  dir))
          (fasl (merge-pathnames (pathname-name (pathname (scratch-file-name "fasl")))
                                 dir)))
      (with-open-file (f lisp :direction :output)
        (dolist (form forms)
          (prin1 form f)))
      (let ((res (progn (compile-file lisp :output-file fasl :trace-file trace) t)))
        (uiop:delete-directory-tree (pathname dir) :validate t)
        res))))

(with-test (:name :compile-some-forms-with-graphing
            :skipped-on (:not :sb-devel))
  (with-graph
      (defpackage :sb-graph-test
        (:shadow :stream)
        (:use :cl :cl-user)
        (:export :hook :disable-hook :enable-hook :unhook :hook-enabled
         :make-graph :make-and-dfs :save-graph :graph :render-graph :expand-codename
         :interactively-graph :output :expand :dfs-add))

    (in-package :sb-graph-test)

    (defmacro hook (fun lambda-list &body body)
      (let ((ll (gensym))
            (f (gensym))
            (orig (gensym)))
        `(let ((,f ',fun))
           (when (nth-value 1 (gethash ',f *hook-enabled*))
             (unhook ,fun))
           (setf (gethash ,f *hook-enabled*) t)
           (sb-int::encapsulate ,f 'hook
                                (lambda (,orig &rest ,ll)
                                  (when (hook-enabled ,fun)
                                    (destructuring-bind ,lambda-list ,ll
                                      (block hook
                                        ,@body)))
                                  (apply ,orig ,ll))))))
    (defmacro disable-hook (fun)
      (let ((f (gensym)))
        `(let ((,f ',fun))
           (when (nth-value 1 (gethash ,f *hook-enabled*))
             (setf (gethash ',fun *hook-enabled*) nil)))))
    (defmacro enable-hook (fun)
      (let ((f (gensym)))
        `(let ((,f ',fun))
           (when (nth-value 1 (gethash ,f *hook-enabled*))
             (setf (gethash ',fun *hook-enabled*) t)))))
    (defmacro unhook (fun)
      (let ((f (gensym)))
        `(let ((,f ',fun))
           (when (nth-value 1 (gethash ,f *hook-enabled*))
             (sb-int::unencapsulate ,f 'hook)
             (remhash ,f *hook-enabled*)))))
    (defmacro hook-enabled (fun)
      `(gethash ',fun *hook-enabled*))
    (defun make-graph ()
      (make-instance 'graph
                     :stream (make-string-output-stream)
                     :dfs-table (make-hash-table :test 'eq :size 63)
                     :obj-table (make-hash-table :test 'eq :size 63)
                     :codename-table (make-hash-table :test 'equal :size 63)
                     :codename-number 0))

    (defun make-and-dfs (object distance)
      (let ((graph (make-graph)))
        (dfs-add graph distance object)
        graph))

    (defun save-graph (str filename)
      (with-open-file (s filename :direction :output :if-does-not-exist :create :if-exists :supersede)
        (write-string str s)))

    ;; dfs-table: obj -> T
    ;; obj-table: obj -> codename
    ;; codename-table: codename -> obj
    (defclass graph ()
      ((stream :initarg :stream :accessor stream)
       (dfs-table :initarg :dfs-table :reader dfs-table)
       (obj-table :initarg :obj-table :reader obj-table)
       (codename-table :initarg :codename-table :reader codename-table)
       (codename-number :initarg :codename-number :accessor codename-number)))

    (defmethod render-graph (graph)
      (get-output-stream-string (stream graph))
      (write-string (format nil "digraph {~%") (stream graph))
      (maphash #'(lambda (k v) (declare (ignore v)) (edges graph k)) (dfs-table graph))
      (write-string "}" (stream graph))
      (get-output-stream-string (stream graph)))

    ;; RENDER-GRAPH goes through all the nodes in DFS-TABLE, so we add the
    ;; node corresponding to the given codename to the graph's DFS-TABLE
    (defmethod expand-codename (graph codename)
      (setf (gethash (gethash codename (codename-table graph))
                     (dfs-table graph))
            t))

    (defun get-node-from-codename (graph codename)
      (gethash codename (codename-table graph)))

    ;; creates a new codename, ties it to this object, then returns it.
    (defun add-to-code-tables (graph object)
      (if (nth-value 1 (gethash object (obj-table graph)))
          (gethash object (obj-table graph))
          (let ((new-codename (let ((res (format nil "~X" (codename-number graph))))
                                (incf (codename-number graph))
                                res)))
            (setf (gethash object (obj-table graph))
                  new-codename
                  (gethash new-codename (codename-table graph))
                  object)
            new-codename)))

    (let ((curr-graph nil)
          (curr-file nil))
      (defun interactively-graph (graph &optional (filename nil))
        (setf curr-graph graph)
        (setf curr-file filename))

      (defun output ()
        (if curr-file
            (save-graph (render-graph curr-graph) curr-file)
            (render-graph curr-graph)))

      (defun expand (codename)
        (expand-codename curr-graph codename)
        (when curr-file
          (output)))

      (defun get-node (codename)
        (get-node-from-codename curr-graph codename)))))
