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

#+sb-unicode
(with-test (:name (:unicode-casing)
                  :skipped-on '(not :sb-unicode))
  (labels ((str (&rest chars)
             (coerce chars 'string))
           (test-fn (fn locale pairs)
             (loop for (a . b) in pairs do
                  (assert (string= (funcall fn (apply #'str a) :locale locale)
                                   (funcall fn (apply #'str b) :locale locale))))))
    (test-fn
     #'sb-unicode:uppercase nil
     '(((#\i) . (#\I)) ((#\a) . (#\a)) ((#\U+DF) . (#\s #\s))
       ((#\GREEK_SMALL_LETTER_SIGMA) . (#\GREEK_SMALL_LETTER_FINAL_SIGMA))))
    (test-fn
     #'sb-unicode:lowercase nil
     '(((#\I) . (#\i)) ((#\U+03A3) . (#\U+03C3)) ((#\a #\U+03A3) . (#\a #\U+03C2))
       ((#\a #\U+03A3 #\U+0308) . (#\a #\U+03C2 #\U+0308))
       ((#\Space #\U+03A3 #\Space) . (#\Space #\U+03C3 #\Space))
       ((#\U+03A3 #\U+03A3) . (#\U+03C3 #\U+03C2))))
    (test-fn
     #'sb-unicode:uppercase :tr
     '(((#\U+0131) . (#\I))
       ((#\i) . (#\U+0130))))
    (test-fn
     #'sb-unicode:lowercase :tr
     '(((#\I) . (#\U+0131))
       ((#\U+130) . (#\i))
       ((#\I #\COMBINING_DOT_ABOVE) . (#\i))))
    (test-fn
     #'sb-unicode:uppercase :lt
     '(((#\i #\U+0307) . (#\I))))
    (test-fn
     #'sb-unicode:lowercase :lt
     '(((#\I #\U+0301) . (#\i #\U+0307 #\U+0301))))))

(with-test (:name (:cl-case-invertibility))
  (loop for i from 0 below char-code-limit
     for char = (code-char i)
     do
       (when (upper-case-p char)
         (assert (char= char (char-upcase (char-downcase char)))))
       (when (lower-case-p char)
         (assert (char= char (char-downcase (char-upcase char)))))))

(with-test (:name (:basic-confusable-detection))
  (assert (sb-unicode:confusable-p "l0" "1O"))
  (assert (sb-unicode:confusable-p "\"" "''"))
  (assert (not (sb-unicode:confusable-p "a" "A")))
  (assert (not (sb-unicode:confusable-p "" "<")))
  #+sb-unicode
  (assert (sb-unicode:confusable-p
           (coerce '(#\a #\COMBINING_RING_ABOVE) 'string)
           (string #\LATIN_SMALL_LETTER_A_WITH_RING_ABOVE))))
