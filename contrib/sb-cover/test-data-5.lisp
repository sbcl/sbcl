(declaim (optimize sb-c:store-coverage-data))

(defun outer ()
  (catch 'tag
    (inner)
    (print "returned from inner")))

(defun inner ()
  (print "inside inner")
  (thrower)
  (print "after thrower"))

(defun thrower ()
  (throw 'tag :good))
