(in-package :sb-bsd-sockets)

;;; This courtesy of Pierre Mai in comp.lang.lisp 08 Jan 1999 00:51:44 +0100
;;; Message-ID: <87lnjebq0f.fsf@orion.dent.isdn.cs.tu-berlin.de>

(defun split (string &optional max (ws '(#\Space #\Tab)))
  "Split `string' along whitespace as defined by the sequence `ws'.
The whitespace is elided from the result.  The whole string will be
split, unless `max' is a non-negative integer, in which case the
string will be split into `max' tokens at most, the last one
containing the whole rest of the given `string', if any."
  (flet ((is-ws (char) (find char ws)))
    (loop for start = (position-if-not #'is-ws string)
          then (position-if-not #'is-ws string :start index)
          for index = (and start
                           (if (and max (= (1+ word-count) max))
                               nil
                             (position-if #'is-ws string :start start)))
          while start
          collect (subseq string start index)
          count 1 into word-count
          while index)))

