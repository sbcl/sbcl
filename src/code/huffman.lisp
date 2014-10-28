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

(in-package "SB!IMPL")

(defstruct (huffman-node (:constructor make-huffman-node (key weight)))
  key weight)

(defstruct (huffman-pair
             (:include huffman-node)
             (:constructor make-huffman-pair
                           (left right &aux
                                 (key   (concatenate 'string
                                                     (huffman-node-key left)
                                                     (huffman-node-key right)))
                                 (weight (+ (huffman-node-weight left)
                                            (huffman-node-weight right))))))
  left right)

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

(defun huffman-decode (code tree)
  (let ((original code))
   (labels ((pop-bit ()
              (let* ((bits (integer-length code))
                     (bit (ldb (byte 1 (- bits 2)) code)))
                (setf code (dpb 1 (byte 1 (- bits 2))
                                (ldb (byte (- bits 1) 0) code)))
                bit))
            (choose (branch)
              (destructuring-bind (key left right) branch
                  (declare (ignore key))
                (if (zerop (pop-bit))
                    left
                    right)))
            (decode (branch)
              (when (zerop code)
                (error "Invalid Huffman-code: ~S" original))
              (let ((next (choose branch)))
                 (cond ((consp next)
                        (decode next))
                       ((< 1 code)
                        (concatenate 'string next (decode tree)))
                       (t
                        next)))))
     (decode tree))))

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
