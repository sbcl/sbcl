;;;; This file is for testing bivalent stream functionality, using
;;;; test machinery which might have side effects (e.g.  executing
;;;; DEFUN, writing files).  Note that the tests here might reach into
;;;; unexported functionality, and should not be used as a guide for
;;;; users.

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

;;; Test character decode restarts.

(defun bvector (&rest elements)
  (make-array (length elements) :element-type '(unsigned-byte 8)
              :initial-contents elements))

(defun cvector (&rest elements)
  (make-array (length elements) :element-type 'character
              :initial-contents elements))

(defmacro with-bivalent-io-setup ((file) &body body)
  (declare (ignore file))
  (let ((file-var (gensym)))
    `(let ((,file-var (scratch-file-name)))
       (unwind-protect
            (macrolet
                ((with-stream ((stream &rest args &key &allow-other-keys) &body body)
                   `(with-open-file (,stream ,',file-var ,@args
                                             :element-type :default :external-format :utf-8)
                      ,@body)))
              ,@body)
         (when (probe-file ,file-var)
           (delete-file ,file-var))))))

(defun assert-roundtrip (write-call read-call result expected)
  (unless (equalp result expected)
    (error "~@<Writing via ~S and reading back via ~S produced ~S, ~
            expected ~S.~@:>"
           write-call read-call result expected)))

(defun zero-string (n) (make-string n :initial-element #\nul))

(defvar *read/write-sequence-pairs*
  `(;; List source and destination sequence.
    ((65)       ()         ,(list 0)        ()         1 (#\A))
    ((#\B)      ()         ,(list 0)        ()         1 (#\B))
    ((#x7e)     ()         ,(list 0)        ()         1 (,(code-char #x7e)))
    ((66 #\C)   ()         ,(list 0 0)      ()         2 (#\B #\C))
    ((#\B 67)   ()         ,(list 0 0)      ()         2 (#\B #\C))
    ((#\B #\C)  (:start 1) ,(list 0)        ()         1 (#\C))
    ((#\B #\C)  (:end 1)   ,(list 0)        ()         1 (#\B))
    ((#\B)      ()         ,(list 0 0)      (:start 1) 2 (0 #\B))
    ((#\B)      ()         ,(list 0 0)      (:end 1)   1 (#\B 0))
    ;; Vector source sequence.
    (#(65)      ()         ,(list 0)        ()         1 (#\A))
    (#(#\B)     ()         ,(list 0)        ()         1 (#\B))
    (#(#x7e)    ()         ,(list 0)        ()         1 (,(code-char #x7e)))
    (#(66 #\C)  ()         ,(list 0 0)      ()         2 (#\B #\C))
    (#(#\B 67)  ()         ,(list 0 0)      ()         2 (#\B #\C))
    (#(#\B #\C) (:end 1)   ,(list 0)        ()         1 (#\B))
    (#(#\B #\C) (:start 1) ,(list 0)        ()         1 (#\C))
    ;; String source sequence.
    ("A"        ()         ,(list 0)        ()         1 (#\A))
    ("B"        ()         ,(list 0)        ()         1 (#\B))
    ("BC"       (:start 1) ,(list 0)        ()         1 (#\C))
    ("BC"       (:end 1)   ,(list 0)        ()         1 (#\B))
    ;; Generic vector destination sequence.
    (#(65)      ()         ,(vector 0)      ()         1 #(#\A))
    (#(#\B)     ()         ,(vector 0)      ()         1 #(#\B))
    (#(#x7e)    ()         ,(vector 0)      ()         1 #(,(code-char #x7e)))
    (#(66 #\C)  ()         ,(vector 0 0)    ()         2 #(#\B #\C))
    (#(#\B 67)  ()         ,(vector 0 0)    ()         2 #(#\B #\C))
    (#(#\B)     ()         ,(vector 0 0)    (:end 1)   1 #(#\B 0))
    (#(#\B)     ()         ,(vector 0 0)    (:start 1) 2 #(0 #\B))
    ;; Byte-vector destination sequence.
    (#(65)      ()         ,(bvector 0)     ()         1 #(65))
    (#(#\B)     ()         ,(bvector 0)     ()         1 #(66))
    (#(#xe0)    ()         ,(bvector 0)     ()         1 #(#xe0))
    (#(66 #\C)  ()         ,(bvector 0 0)   ()         2 #(66 67))
    (#(#\B 67)  ()         ,(bvector 0 0)   ()         2 #(66 67))
    (#(#\B)     ()         ,(bvector 0 0)   (:end 1)   1 #(66 0))
    (#(#\B)     ()         ,(bvector 0 0)   (:start 1) 2 #(0 66))
    ;; Character-vector destination sequence.
    (#(65)      ()         ,(cvector #\_)     ()         1 #(#\A))
    (#(#\B)     ()         ,(cvector #\_)     ()         1 #(#\B))
    (#(#x7e)    ()         ,(cvector #\_)     ()         1 #(,(code-char #x7e)))
    (#(66 #\C)  ()         ,(cvector #\_ #\_) ()         2 #(#\B #\C))
    (#(#\B 67)  ()         ,(cvector #\_ #\_) ()         2 #(#\B #\C))
    (#(#\B)     ()         ,(cvector #\_ #\_) (:end 1)   1 #(#\B #\_))
    (#(#\B)     ()         ,(cvector #\_ #\_) (:start 1) 2 #(#\_ #\B))
    ;; String destination sequence.
    (#(65)      ()         ,(zero-string 1)   ()         1 "A")
    (#(#\B)     ()         ,(zero-string 1)   ()         1 "B")
    (#(66 #\C)  ()         ,(zero-string 2)   ()         2 "BC")
    (#(#\B 67)  ()         ,(zero-string 2)   ()         2 "BC")
    (#(#\B)     ()         ,(zero-string 2)   (:end 1)   1 ,(coerce '(#\B #\Nul) 'string))
    (#(#\B)     ()         ,(zero-string 2)   (:start 1) 2 ,(coerce '(#\Nul #\B) 'string))))

(defun do-writes (stream pairs)
  (loop :for (sequence args) :in pairs
     :do (apply #'write-sequence sequence stream args)))

(defun do-reads (stream pairs)
  (loop :for (source source-args into into-args
              expected-position expected-sequence) :in pairs
     :do (let ((into/old (copy-seq into))
               (position (apply #'read-sequence into stream into-args)))
           (unless (= position expected-position)
             (error "~@<~S returned ~S, expected ~S.~@:>"
                    `(read-sequence ,into/old ,@into-args)
                    position expected-position))
           (assert-roundtrip `(write-sequence ,source ,@source-args)
                             `(read-sequence ,into/old ,@into-args)
                             into expected-sequence))))

(with-test (:name (stream :bivalent :roundtrip :element))
  (let ((pairs '((write-byte 65   read-char #\A)
                 (write-char #\B  read-byte 66)
                 (write-byte #xe0 read-byte #xe0)
                 (write-char #\C  read-char #\C))))
    (with-bivalent-io-setup ("bivalent-stream-test.txt")
      (with-stream (stream :direction :output :if-exists :supersede)
        (loop :for (function argument) :in pairs
           :do (funcall function argument stream)))

      (with-stream (stream :direction :input)
        (loop :for (write-function write-arg read-function expected) :in pairs
           :do (let ((result (funcall read-function stream)))
                 (assert-roundtrip `(,write-function ,write-arg)
                                   `(,read-function)
                                   result expected)))))))

(with-test (:name (stream :bivalent :roundtrip sequence))
  (with-bivalent-io-setup ("bivalent-stream-test.txt")
    ;; Write sequence.
    (with-stream (stream :direction :output :if-exists :supersede)
      (do-writes stream *read/write-sequence-pairs*))
    ;; Read sequence and compare.
    (with-stream (stream :direction :input)
      (do-reads stream *read/write-sequence-pairs*))))

(defvar *synonym-stream-stream*)
(with-test (:name (stream :bivalent :roundtrip sequence synonym-stream))
  (with-bivalent-io-setup ("bivalent-stream-test.txt")
    ;; Write sequence.
    (with-stream (stream :direction :output :if-exists :supersede)
      (let ((*synonym-stream-stream* stream)
            (stream (make-synonym-stream '*synonym-stream-stream*)))
        (do-writes stream *read/write-sequence-pairs*)))
    ;; Read sequence and compare.
    (with-stream (stream :direction :input)
      (let ((*synonym-stream-stream* stream)
            (stream (make-synonym-stream '*synonym-stream-stream*)))
       (do-reads stream *read/write-sequence-pairs*)))))

(with-test (:name (stream :bivalent :roundtrip sequence broadcast-stream))
  (with-bivalent-io-setup ("bivalent-stream-test.txt")
    ;; Write sequence.
    (with-stream (stream :direction :output :if-exists :supersede)
      (let ((stream (make-broadcast-stream stream)))
        (do-writes stream *read/write-sequence-pairs*)))
    ;; Read sequence and compare.
    (with-stream (stream :direction :input)
      (do-reads stream *read/write-sequence-pairs*))))

(with-test (:name (stream :bivalent :roundtrip sequence echo-stream))
  (with-bivalent-io-setup ("bivalent-stream-test.txt")
    ;; Write sequence.
    (with-stream (stream :direction :output :if-exists :supersede)
      (do-writes stream *read/write-sequence-pairs*))
    ;; Read sequence and compare.
    (with-stream (stream :direction :input)
      (let ((stream (make-echo-stream stream (make-broadcast-stream))))
        (do-reads stream *read/write-sequence-pairs*)))))

(with-test (:name (stream :bivalent :roundtrip sequence two-way-stream))
  (with-bivalent-io-setup ("bivalent-stream-test.txt")
    ;; Write sequence.
    (with-stream (stream :direction :output :if-exists :supersede)
      (let ((stream (make-two-way-stream (make-concatenated-stream) stream)))
        (do-writes stream *read/write-sequence-pairs*)))
    ;; Read sequence and compare.
    (with-stream (stream :direction :input)
      (let ((stream (make-two-way-stream stream (make-broadcast-stream))))
        (do-reads stream *read/write-sequence-pairs*)))))

(with-test (:name (stream :bivalent synonym-stream *standard-input* *standard-output*))
  (assert (eq (sb-impl::stream-element-mode *standard-input*) :bivalent))
  (assert (eq (sb-impl::stream-element-mode *standard-output*) :bivalent)))

(with-test (:name (stream :bivalent :no-unknown-type-condition))
  (assert-no-signal
   (with-bivalent-io-setup ("bivalent-stream-test.txt")
     (with-stream (stream :direction :output :if-exists :supersede)))
   sb-kernel:parse-unknown-type))
