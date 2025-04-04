;;;; test of the pretty-printer

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, the test
;;;; files (like this one) were written from scratch after the fork
;;;; from CMU CL.
;;;;
;;;; This software is in the public domain and is provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for
;;;; more information.

(with-test (:name :pprint-logical-block-#=nil)
  (assert (not (search "#1=" (write-to-string '(let () (let () x))
                                              :circle t
                                              :pretty t)))))

(with-test (:name :pprint-list-spaces)
  (let* ((list (make-list 50 :initial-element #\Space))
         (string (write-to-string list :pretty t :escape t)))
    (assert (equal (read-from-string string) list))
    (assert (= (count #\Newline string) 2))))

(with-test (:name :pprint-vector-spaces)
  (let* ((vector (coerce (make-list 50 :initial-element #\Space) 'vector))
         (string (write-to-string vector :pretty t :escape t)))
    (assert (equalp (read-from-string string) vector))
    (assert (= (count #\Newline string) 2))))

(with-test (:name :pprint-random-spaces)
  (dotimes (i 100)
    (let* ((list (loop repeat 1000 if (= (random 2) 1) collect #\Space else collect #\Tab))
           (lstring (write-to-string list :pretty t :escape t))
           (lread (read-from-string lstring))
           (vector (coerce list 'vector))
           (vstring (write-to-string vector :pretty t :escape t))
           (vread (read-from-string vstring)))
      (assert (equal list lread))
      (assert (equalp vector vread)))))

(with-test (:name :logical-block-unrelated-object)
  (assert (equal (with-output-to-string (s)
                   (let ((*print-circle* t))
                     (pprint-logical-block (s nil) (princ '(#1=(1) #1#) s))))
                 "(#1=(1) #1#)")))
