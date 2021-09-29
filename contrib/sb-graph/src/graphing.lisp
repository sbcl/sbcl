(in-package :sb-graph)

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

(defmethod unexpand-codename (graph codename)
   (remhash (gethash codename (codename-table graph))
                  (dfs-table graph)))

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

  (defun unexpand (codename)
    (unexpand-codename curr-graph codename)
    (when curr-file
      (output)))

  (defun get-node (codename)
    (get-node-from-codename curr-graph codename)))


;; modify-str-plist returns a new plist which is identical except the
;; value associated with KEY has been replaced by what is returned by
;; funcalling func on the value
(defun modify-str-plist (plist key func)
  (assert (= 0 (mod (length plist) 2)))
  (apply #'nconc
         (loop with ck = nil
               with cv = nil
               while (setf ck (car plist)
                           cv (cadr plist))
               collect (let ((v (if (string= key ck)
                                    (list ck (funcall func cv))
                                    (list ck cv))))
                         (setf plist (cddr plist))
                         v))))

;; this is because graphviz doesn't allow nodes to have more than 16k
;; of text in them
(defun clamp (str)
  (if (< 16300 (length str))
      (subseq str 0 16300)
      str))

;; This was copied from the common lisp cookbook.
(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part
is replaced with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
          for old-pos = 0 then (+ pos part-length)
          for pos = (search part string
                            :start2 old-pos
                            :test test)
          do (write-string string out
                           :start old-pos
                           :end (or pos (length string)))
          when pos do (write-string replacement out)
          while pos)))

(defmethod edge ((graph graph) from to &rest options)
  (assert (= 0 (mod (length options) 2)))
  (format (stream graph) "\"{~A} ~A\" -> \"{~A} ~A\"[~A];~%"
          (if (gethash from (obj-table graph))
              (gethash from (obj-table graph))
              (add-to-code-tables graph from))
          (replace-all (replace-all (clamp (display from)) "\"" "'") "\\" "\\\\")
          (if (gethash to (obj-table graph))
              (gethash to (obj-table graph))
              (add-to-code-tables graph to))
          (replace-all (replace-all (clamp (display to)) "\"" "'") "\\" "\\\\")
          (apply #'concatenate
                 (cons 'string
                       (loop with ck = nil
                             with cv = nil
                             while (setf ck (car options)
                                         cv (cadr options))
                             collect (let ((v (format nil "~A=\"~A\"" ck cv)))
                                       (setf options (cddr options))
                                       v))))))

;; this is overriding the edge so that we can not render CTRANs
(defmethod edge ((graph graph) from (to sb-c::ctran) &rest options)
  (apply #'edge (nconc (list graph from (sb-c::ctran-next to))
                       (modify-str-plist options "label"
                                         (lambda (x)
                                           (concatenate 'string x (format nil "[ctran: ~A]" (sb-c::ctran-kind to)))))
                       (unless (find-if (lambda (x) (string= x "color")) options)
                         '("color" "blue")))))


(defmethod edge ((graph graph) from (to list) &rest options)
  (let ((counter 0))
    (mapc #'(lambda (x)
              (apply #'edge
                     (nconc (list graph from x)
                            (modify-str-plist
                             options "label"
                             #'(lambda (x)
                                 (let ((res (format nil "~A[# ~A]"
                                                    x counter)))
                                   (incf counter)
                                   res))))))
          to)))

;; display goes from an object to the string representation that'll be
;; inside the graph nodes

(defmethod display ((c sb-c::component))
  (format nil "COMPONENT: '~S'"
          (sb-c::component-name c)))

;; The sxhash is required to not end up with a single CBLOCK node
(defmethod display ((b sb-c::cblock))
  (format nil "CBLOCK"))

(defmethod display ((ctran sb-c::ctran))
  (error "Trying to display a CTRAN, this shouldn't happen"))

(defmethod display ((cl sb-c::clambda))
  (format nil "CLAMBDA:~%%debug-name: ~A~%source-name: ~A~%kind: ~A"
          (sb-c::lambda-%debug-name cl)
          (sb-c::lambda-%source-name cl)
          (sb-c::lambda-kind cl)))

(defmethod display ((cr sb-c::creturn))
  (format nil "CRETURN:~%result-type: ~A"
          (sb-c::return-result-type cr)))

(defmethod display ((bind sb-c::bind))
  (format nil "BIND"))

(defmethod display ((ref sb-c::ref))
  (format nil "REF:~%derived-type: ~A"
          (sb-c::ref-derived-type ref)))

(defmethod display ((comb sb-c::combination))
  (format nil "COMBINATION:~%kind: ~A~%info: ~A"
          (sb-c::combination-kind comb)
          (sb-c::combination-info comb)))

(defmethod display ((lvar sb-c::lvar))
  (format nil "LVAR:~%%derived-type: ~A~%dynamic-extent: ~A"
          (sb-c::lvar-%derived-type lvar)
          (sb-c::lvar-dynamic-extent lvar)))

(defmethod display ((const sb-c::constant))
  (format nil "CONSTANT:~%value: ~A"
          (sb-c::constant-value const)))

(defmethod display ((lamvar sb-c::lambda-var))
  (format nil "LAMBDA-VAR:~%arg-info: ~A~%flags: ~A"
          (sb-c::lambda-var-arg-info lamvar)
          (sb-c::lambda-var-flags lamvar)))

(defmethod display ((entry sb-c::entry))
  (format nil "ENTRY"))

(defmethod display (obj)
  (format nil "NOT SUPPORTED YET:~% ~A" obj))


(defmethod edges ((graph graph) (objects list))
  (mapc #'(lambda (o) (edges graph o)) objects))

(defmethod edges ((graph graph) (component sb-c::component))
  (edge graph component (sb-c::component-head component) "label" "head")
  (edge graph component (sb-c::component-tail component) "label" "tail"))

(defmethod edges ((graph graph) (cblock sb-c::cblock))
  (edge graph cblock (sb-c::block-component cblock) "label" "component")
  (edge graph cblock (sb-c::block-succ cblock) "label" "succ")
  (edge graph cblock (sb-c::block-pred cblock) "label" "pred")
  (edge graph cblock (sb-c::block-start cblock) "label" "start"))

;; Unfinished
(defmethod edges ((graph graph) (cl sb-c::clambda))
  (edge graph cl (sb-c::lambda-home cl) "label" "home")
  (edge graph cl (sb-c::lambda-vars cl) "label" "vars"))

(defmethod edges ((graph graph) (cr sb-c::creturn))
  (edge graph cr (sb-c::return-lambda cr) "label" "lambda")
  (edge graph cr (sb-c::return-result cr) "label" "result"))

;; this is just a dummy function to skip the CTRAN
(defmethod edges ((graph graph) (ct sb-c::ctran)))

(defmethod edges ((graph graph) (ref sb-c::ref))
  (edge graph ref (sb-c::ref-leaf ref) "label" "leaf")
  (edge graph ref (sb-c::ref-next ref) "label" "next")
  (edge graph ref (sb-c::ref-lvar ref) "label" "lvar"))

(defmethod edges ((graph graph) (bind sb-c::bind))
  (edge graph bind (sb-c::bind-lambda bind) "label" "lambda")
  (edge graph bind (sb-c::bind-next bind) "label" "next" "color" "green")
  (edge graph bind (sb-c::bind-prev bind) "label" "prev" "color" "red"))

(defmethod edges ((graph graph) (comb sb-c::combination))
  (edge graph comb (sb-c::combination-fun comb) "label" "fun")
  (edge graph comb (sb-c::combination-args comb) "label" "args"))

(defmethod edges ((graph graph) (lvar sb-c::lvar))
  (edge graph lvar (sb-c::lvar-dest lvar) "label" "dest" "color" "brown")
  (edge graph lvar (sb-c::lvar-uses lvar) "label" "uses"))

(defmethod edges ((graph graph) (lamvar sb-c::lambda-var))
  (edge graph lamvar (sb-c::lambda-var-home lamvar) "label" "home")
  (edge graph lamvar (sb-c::lambda-var-sets lamvar) "label" "sets"))

(defmethod edges ((graph graph) (entry sb-c::entry))
  (edge graph entry (sb-c::entry-exits entry) "label" "exits")
  (edge graph entry (sb-c::entry-cleanup entry) "label" "cleanup")
  (edge graph entry (sb-c::entry-next entry) "label" "next")
  (edge graph entry (sb-c::entry-prev entry) "label" "prev"))

(defmethod edges ((graph graph) object))

;; the name is a "pun" of the words 'unique' and 'graph'
(defmacro unig ((graph obj) &body body)
  (let ((g (gensym))
        (o (gensym)))
    `(let ((,g ,graph)
           (,o ,obj))
       (unless (nth-value 1 (gethash ,o (dfs-table ,g)))
         (setf (gethash ,o (dfs-table ,g)) t)
         ,@body))))

(defmethod dfs-add ((graph graph) (distance integer) (objects list))
  (mapc #'(lambda (o) (dfs-add graph distance o)) objects))

;; a component should be rendered as a subgraph
(defmethod dfs-add ((graph graph) (distance integer) (component sb-c::component))
  (when (> distance 0)
    (decf distance)
    (unig (graph component)
      (dfs-add graph distance (sb-c::component-head component))
      (dfs-add graph distance (sb-c::component-tail component)))))

(defmethod dfs-add ((graph graph) (distance integer) (cblock sb-c::cblock))
  (when (> distance 0)
    (decf distance)
    (unig (graph cblock)
      (dfs-add graph distance (sb-c::block-component cblock))
      (dfs-add graph distance (sb-c::block-succ cblock))
      (dfs-add graph distance (sb-c::block-pred cblock))
      (dfs-add graph distance (sb-c::block-start cblock)))))

;; Unfinished
(defmethod dfs-add ((graph graph) (distance integer) (cl sb-c::clambda))
  (when (> distance 0)
    (decf distance)
    (unig (graph cl)
      (dfs-add graph distance (sb-c::lambda-home cl))
      (dfs-add graph distance (sb-c::lambda-vars cl)))))

(defmethod dfs-add ((graph graph) (distance integer) (cr sb-c::creturn))
  (when (> distance 0)
    (decf distance)
    (unig (graph cr)
      (dfs-add graph distance (sb-c::return-lambda cr))
      (dfs-add graph distance (sb-c::return-result cr)))))

;; (defmethod dfs-add ((graph graph) (distance integer) (node sb-c::node))
;;   )

;; this is just a dummy function to skip the CTRAN. Note that it
;; doesn't decf distance.
(defmethod dfs-add ((graph graph) (distance integer) (ct sb-c::ctran))
  (when (> distance 0)
    (unig (graph ct)
      (dfs-add graph distance (sb-c::ctran-next ct)))))

(defmethod dfs-add ((graph graph) (distance integer) (ref sb-c::ref))
  (when (> distance 0)
    (decf distance)
    (unig (graph ref)
      (dfs-add graph distance (sb-c::ref-leaf ref))
      (dfs-add graph distance (sb-c::ref-next ref))
      (dfs-add graph distance (sb-c::ref-lvar ref)))))

(defmethod dfs-add ((graph graph) (distance integer) (bind sb-c::bind))
  (when (> distance 0)
    (decf distance)
    (unig (graph bind)
      (dfs-add graph distance (sb-c::bind-lambda bind))
      (dfs-add graph distance (sb-c::bind-next bind))
      (dfs-add graph distance (sb-c::bind-prev bind)))))

(defmethod dfs-add ((graph graph) (distance integer) (comb sb-c::combination))
  (when (> distance 0)
    (decf distance)
    (unig (graph comb)
      (dfs-add graph distance (sb-c::combination-fun comb))
      (dfs-add graph distance (sb-c::combination-args comb)))))

(defmethod dfs-add ((graph graph) (distance integer) (lvar sb-c::lvar))
  (when (> distance 0)
    (decf distance)
    (unig (graph lvar)
      (dfs-add graph distance (sb-c::lvar-dest lvar))
      (dfs-add graph distance (sb-c::lvar-uses lvar)))))

(defmethod dfs-add ((graph graph) (distance integer) (lamvar sb-c::lambda-var))
  (when (> distance 0)
    (decf distance)
    (unig (graph lamvar)
      (dfs-add graph distance (sb-c::lambda-var-home lamvar))
      (dfs-add graph distance (sb-c::lambda-var-sets lamvar)))))

(defmethod dfs-add ((graph graph) (distance integer) (entry sb-c::entry))
  (when (> distance 0)
    (decf distance)
    (unig (graph entry)
      (dfs-add graph distance (sb-c::entry-exits entry))
      (dfs-add graph distance (sb-c::entry-cleanup entry))
      (dfs-add graph distance (sb-c::entry-next entry))
      (dfs-add graph distance (sb-c::entry-prev entry)))))

(defmethod dfs-add ((graph graph) (distance integer) object)
  (when (> distance 0)
    (decf distance)
    (unig (graph object))))
