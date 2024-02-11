;;;; a simple huffman encoder/decoder, used to compress unicode
;;;; character names.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

(defstruct (huffman-node (:constructor make-huffman-node (key weight))
                         (:copier nil))
  (key nil :read-only t)
  (weight nil :read-only t))

(defstruct (huffman-pair
             (:include huffman-node)
             (:copier nil)
             (:constructor make-huffman-pair
                           (left right &aux
                                 (key   (concatenate 'string
                                                     (huffman-node-key left)
                                                     (huffman-node-key right)))
                                 (weight (+ (huffman-node-weight left)
                                            (huffman-node-weight right))))))
  (left nil :read-only t)
  (right nil :read-only t))
(declaim (freeze-type huffman-node))

(defun huffman-weights (corpus)
  (let ((weight-table (make-hash-table :test #'equal)))
    (loop for string in corpus
       do (loop for char across string
             do (incf (gethash char weight-table 0))))
    (let (alist)
      (maphash (lambda (char weight)
                 (push (make-huffman-node (string char) weight) alist))
               weight-table)
      (sort alist #'string< :key #'huffman-node-key))))

(defun make-huffman-tree (corpus)
  (labels ((merge-table (table)
             (setf table (stable-sort table #'< :key #'huffman-node-weight))
             (push (make-huffman-pair (pop table) (pop table))
                   table)
             (if (second table)
                 (merge-table table)
                 (car table)))
           (finish-tree (tree)
             (if (huffman-pair-p tree)
                 (list (huffman-node-key tree)
                       (finish-tree (huffman-pair-left tree))
                       (finish-tree (huffman-pair-right tree)))
                 (huffman-node-key tree))))
    (finish-tree (merge-table (huffman-weights corpus)))))

;; won't need this eval-when after moving this file into warm build
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; actually 88 (for now), but leave some wiggle room
  (defconstant longest-unicode-char-name 96))
;;; The :ALL-CHAR-NAMES test in character.pure.lisp shows that this decoder
;;; is faster than and much less consy than the old one.
;;; (time (...)) old way
;;;   0.540 seconds of real time
;;;  741,593,840 bytes consed
;;; new way
;;;   0.128 seconds of real time
;;;  13,414,128 bytes consed

(defun huffman-decode (tree code output)
  (declare (type (or (simple-base-string #.longest-unicode-char-name) null) output)
           (integer code))
  ;; The highest 1 bit acts only to demarcate the end of the encoding.
  ;; Therefore the number of data bits in the encoding is 1 fewer than that.
  (let ((nbits (1- (integer-length code)))
        (buffer (make-array longest-unicode-char-name :element-type 'base-char))
        (bufpos longest-unicode-char-name))
   (declare (dynamic-extent buffer))
   (declare (type (mod 500) nbits)) ; the longest encoding is 385 bits for now
   (labels ((choose (branch)
              (destructuring-bind (left right) (cdr branch)
                (if (logbitp (decf nbits) code) right left)))
            (decode (branch)
              (when (zerop nbits)
                (error "Invalid Huffman-code: ~S" code))
              (let ((next (choose branch)))
                 (cond ((consp next)
                        (decode next))
                       (t
                        (when (> nbits 1) (decode tree))
                        (setf (char buffer (decf bufpos))
                              (char (the (simple-base-string 1) next) 0)))))))
     (decode tree)
     (cond (output
            (replace output buffer :start2 bufpos)
            (- (length output) bufpos)) ; return number of significant chars
           (t
            (subseq buffer bufpos))))))

(defun huffman-match (char node)
  (if (consp node)
      (find char (the string (car node)) :test #'equal)
      (eql char (character node))))

(defun huffman-encode (string tree)
  (let ((code 1))
    (labels ((encode (bit char tree)
               (when bit
                 (setf code (+ (ash code 1) bit)))
               (if (consp tree)
                   (destructuring-bind (key left right) tree
                     (declare (ignore key))
                     (cond ((huffman-match char left)
                            (encode 0 char left))
                           ((huffman-match char right)
                            (encode 1 char right))
                           (t
                            ;; unknown
                            (return-from huffman-encode nil))))
                   (unless (huffman-match char tree)
                     (error "Error encoding ~S (bad tree)." char)))))
      (loop for char across string
         do (encode nil char tree))
      code)))
