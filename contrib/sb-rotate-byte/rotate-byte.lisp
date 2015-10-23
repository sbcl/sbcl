(in-package "SB-ROTATE-BYTE")

(defun rotate-byte (count bytespec integer)
  "Rotates a field of bits within INTEGER; specifically, returns an
integer that contains the bits of INTEGER rotated COUNT times
leftwards within the byte specified by BYTESPEC, and elsewhere
contains the bits of INTEGER."
  (rotate-byte count bytespec integer))

(defun %rotate-byte (count size pos integer)
  (let ((count (nth-value 1 (round count size)))
        (mask (1- (ash 1 size))))
    (logior (logand integer (lognot (ash mask pos)))
            (let ((field (logand (ash mask pos) integer)))
              (logand (ash mask pos)
                      (if (> count 0)
                          (logior (ash field count)
                                  (ash field (- count size)))
                          (logior (ash field count)
                                  (ash field (+ count size)))))))))

(defun %unsigned-32-rotate-byte (count integer)
  ;; inhibit transforms
  (declare (notinline %rotate-byte))
  (%rotate-byte count 32 0 integer))

#+64-bit
(defun %unsigned-64-rotate-byte (count integer)
  (%unsigned-64-rotate-byte count integer))
