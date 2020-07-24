;;;; integer-specific (quite possibly FIXNUM-specific or
;;;; machine-word-specific) transforms

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

;;;; RANDOM in various integer cases

(deftransform random ((limit &optional state)
                      ((integer 1 #.(ash 1 sb-vm:n-word-bits)) &optional *))
  "transform to a sample no wider than CPU word"
  (let ((type (lvar-type limit)))
    (if (numeric-type-p type)
        (let ((limit-high (numeric-type-high (lvar-type limit))))
          (aver limit-high)
          (if (<= limit-high (1+ most-positive-fixnum))
              '(%inclusive-random-fixnum (1- limit)
                                         (or state *random-state*))
              '(%inclusive-random-integer (1- limit)
                                          (or state *random-state*))))
        (give-up-ir1-transform "too-wide inferred type for LIMIT argument"))))

;;; Boxing the argument to RANDOM (and often the return value as well)
;;; could be quite expensive in speed, while inlining every RANDOM
;;; call could be very expensive in code space, so use policy to
;;; decide.
(deftransform %inclusive-random-integer
    ((inclusive-limit state) (* *) * :policy (> speed space))
  ;; By the way, some natural special cases (notably when the user is
  ;; asking for a full %RANDOM-WORD) could be expanded to much simpler
  ;; code (with no test and loop) if someone finds it important.
  '(let ((n-bits (integer-length inclusive-limit)))
    (%inclusive-random-integer-accept-reject (%random-bits n-bits state)
     inclusive-limit)))
