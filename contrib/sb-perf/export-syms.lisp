;;; SBCL integration with Linux perf jitdump interface
;;; https://gist.github.com/lukego/e0192eab0091ccb6c149bd98f59ee7bf

(defpackage :sb-perf
  (:use :common-lisp)
  (:export #:write-perfmap
           #:write-jitdump))
(in-package :sb-perf)


;;;; perf map

(defun code-short-name (code)
  (let* ((debug (sb-kernel:%code-debug-info code))
         (name (and (typep debug 'sb-c::compiled-debug-info)
                    (sb-c::compiled-debug-info-name (sb-kernel:%code-debug-info code)))))
    (if name (remove #\newline (prin1-to-string name)))))


(defun write-perfmap ()
  (with-open-file (s (format nil "/tmp/perf-~d.map" (sb-posix:getpid))
                     :direction :output :if-exists :supersede
                     :if-does-not-exist :create)
    (sb-vm:map-code-objects (lambda (code)
                              (let ((name (code-short-name code)))
                                (when name
                                  (format s "~x ~x ~a~%"
                                          (sb-sys:sap-int (sb-kernel:code-instructions code))
                                          (sb-kernel:%code-code-size code)
                                          (substitute #\_ #\Space name))))))))


;;;; jitdump

(defvar *jitdump* nil
  "Output stream writing to jitdump log file.")

(defvar *mmap* nil
  "mmap() of the dump file for perf to see.")

(defun timestamp ()
  "Return 64-bit monotonic timestamp."
  (multiple-value-bind (sec ns) (sb-unix::clock-gettime 1)
    (dpb ns (byte 32 32) sec)))

(defun filename ()
  (format nil "/tmp/jit-~a.dump" (sb-posix:getpid)))

(defun le-bytes (value bytes)
  (loop for i below bytes
        for v = value then (ash v -8)
        collect (logand v #xff)))

(defun u32 (value)
  "Write one 32-bit unsigned little-endian VALUE."
  (write-sequence (le-bytes value 4)
                  *jitdump*))

(defun u64 (value)
  "Write one 64-bit unsigned VALUE."
  (write-sequence (le-bytes value 8)
                  *jitdump*))

(defun bin (sap len)
  "Write a sequence of LEN bytes from memory address SAP."
  (loop for i from 0 below len
        do (write-byte (sb-sys:sap-ref-8 sap i) *jitdump*)))

(defun str (string)
  "Write STRING with a null-terminator."
  (multiple-value-bind (bytes len)
      (sb-alien:make-alien-string string
                                  :external-format :utf-8
                                  :null-terminate t)
    (unwind-protect (bin (sb-alien:alien-sap bytes)
                         len)
      (sb-alien:free-alien bytes))))

(defun write-file-header ()
  "Write the Jitdump file header."
  (u32 #x4A695444 )                     ; Magic
  (u32 1)                               ; Version
  (u32 (+ 4 4 4 4 4 4 8 8))             ; Total size
  (u32 62)                              ; ELF Mach = x86-64 (XXX)
  (u32 0)                               ; Pad
  (u32 (sb-posix:getpid))               ; Pid
  (u64 (timestamp))                     ; Timestamp
  (u64 0))                              ; Flags

(defun write-record-header (&key id content-size (timestamp (timestamp)))
  "Write a Jitdump record header."
  (u32 id)
  (u32 (+ 4 4 8 content-size))
  (u64 timestamp))

(defun write-load (&key name id address size
                         (pid (sb-posix:getpid))
                         (tid (sb-thread:thread-os-tid sb-thread:*current-thread*)))
  "Write a Jitdump LOAD event."
  (write-record-header :id 0
                       :content-size (+ 4 4 8 8 8 8 (1+ (length name)) size)
                       :timestamp (get-universal-time))
  (u32 pid)                          ; PID
  (u32 tid)                          ; Thread ID
  (u64 (sb-sys:sap-int address))     ; VMA - address of function start
  (u64 (sb-sys:sap-int address))     ; Code address
  (u64 size)                         ; Code size
  (u64 id)                           ; Unique ID
  (str name)                         ; Function name
  (bin address size))                ; Raw machine code

(defun code-info (code)
  (let* ((id (sb-kernel:%code-serialno code))
         (debug-info (sb-kernel:%code-debug-info code))
         (name (and (typep debug-info 'sb-c::compiled-debug-info)
                    (sb-c::compiled-debug-info-name debug-info))))
    (list :name (if (null name)
                    "?"
                    (let ((*package* (find-package :keyword)))
                      (prin1-to-string name)))
          :id id
          :address (sb-kernel:code-instructions code)
          :size (sb-kernel:%code-code-size code))))

(defun write-jitdump ()
  (with-open-file (*jitdump* (filename)
                             :element-type '(unsigned-byte 8)
                             :direction :output :if-exists :supersede :if-does-not-exist :create)
    (write-file-header)
    (sb-vm:map-code-objects (lambda (code)
                              (apply #'write-load (code-info code)))))
  (when *mmap*
    (sb-posix:munmap (car *mmap*) (cdr *mmap*))
    (setf *mmap* nil))
  ;; Needs to be open for reading only
  (with-open-file (*jitdump* (filename)
                             :element-type '(unsigned-byte 8))
    (let* ((size (file-length *jitdump*))
           (pagesize 4096) ;; TODO: (sysconf PAGESIZE)
           (rounded-size (* pagesize (ceiling size pagesize)))
           (addr (sb-posix:mmap (sb-sys:int-sap 0)
                                rounded-size
                                (logior sb-posix:prot-read sb-posix:prot-exec)
                                (logior sb-posix:map-private)
                                (sb-sys:fd-stream-fd *jitdump*)
                                0)))
      (setf *mmap* (cons addr rounded-size)))))


#+(or)
(write-perfmap)
#+(or)
(write-jitdump)
