;;;; Generate RTF out of a regular text file, splitting
;;;; paragraphs on empty lines.
;;;;
;;;; Used to generate License.rtf out of COPYING for the
;;;; Windows installer.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(defun read-text (pathname)
  (let ((pars (list nil)))
    (with-open-file (f pathname :external-format :ascii)
      (loop for line = (read-line f nil)
            for text = (string-trim '(#\Space #\Tab) line)
            while line
            when (plusp (length text))
            do (setf (car pars)
                     (if (car pars)
                         (concatenate 'string (car pars) " " text)
                         text))
            else
            do (push nil pars)))
    (nreverse pars)))

(defun write-rtf (pars pathname)
  (with-open-file (f pathname :direction :output :external-format :ascii
                     :if-exists :supersede)
    ;; \rtf0 = RTF 1.0
    ;; \ansi = character set
    ;; \deffn = default font
    ;; \fonttbl = font table
    ;; \fs = font size in half-points
    (format f "{\\rtf1\\ansi~
                \\deffn0~
                {\\fonttbl\\f0\\fswiss Helvetica;}~
                \\fs20~
                ~{~A\\par\\par ~}}"      ; each par used to end with
                                         ; ~%, but resulting Rtf looks
                                         ; strange (WinXP, WiX 3.0.x,
                                         ; ?)
                         pars)))
