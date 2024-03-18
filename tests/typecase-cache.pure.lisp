(unless (cl:gethash 'sb-c:jump-table sb-c::*backend-template-names*)
  (invoke-restart 'run-tests::skip-file))

(defstruct animal)
(defstruct (goat (:include animal)))
(defstruct (kid (:include goat)))
(defstruct fruit)
(defstruct (tree (:copier nil)))
(defstruct (apple (:include fruit)))
(defstruct (pair (:include fruit)))
(defstruct (kons (:constructor kons (kar kdr))) kar kdr)
(defstruct (pear (:include kons)))
(defstruct (peach (:include apple)))

;; TODO: this typecase is still suboptimal because after we've looked up
;; the relevant clause index, the answer should just be a constant.
;; I thought it already did that but I guess not?
(defun f (x)
  (typecase x
    (apple 'computer)
    (fruit 'jam)
    (pear 'pair)
    (tree 3)
    ((or hash-table kid) 'peach)
    (kons 'pear)
    ((or peach pathname) 'yup)))

(compile 'f)
(defstruct (macintosh (:include apple)))
(defstruct (mulberry (:include tree)))

(with-test (:name :cached-typecase)
  (assert (eq (f (make-mulberry)) 3))
  (assert (eq (f (make-macintosh)) 'computer))
  (assert (eq (f #p"file.name") 'yup)))

(defvar *cell*  (car (ctu:find-code-constants #'f :type '(cons sb-pcl::cache))))
(assert (sb-pcl::cache-p (car *cell*)))

(dotimes (i 100)
  (let ((form `(defstruct (,(intern (format nil "TUR~D" i)) (:Include fruit)))))
    (eval form)
    (assert (eq (f (funcall (intern (format nil "MAKE-TUR~D" i)))) 'jam))))
(with-test (:name :expand-cache)
  (assert (>= (sb-pcl::cache-statistics (car *cell*)) 100)))
