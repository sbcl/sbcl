;;; -*- lisp -*-

;;; This code is in the public domain.

;;; The cmucl implementation of simple-streams was done by Paul Foley,
;;; who placed the code in the public domain.  Sbcl port by Rudi
;;; Schlatte.

;;; TODO (Rudi 2003-05-12): The contents of this file, along with
;;; constants.lisp, should presumably end up somewhere else, either in
;;; sbcl itself or in sb-posix.

(in-package "SB-UNIX")


(export '(prot-read prot-write prot-exec prot-none
          map-shared map-private map-fixed
          unix-mmap unix-munmap
          unix-mlock unix-munlock))


(defun unix-mmap (addr length prot flags fd offset)
  (declare (type (or null system-area-pointer) addr)
           (type (unsigned-byte 32) length)
           (type (integer 1 7) prot)
           (type (unsigned-byte 32) flags)
           (type (or null unix-fd) fd)
           (type (signed-byte 32) offset))
  (let ((result (alien-funcall (extern-alien "mmap"
                                             (function system-area-pointer
                                                       system-area-pointer
                                                       size-t int int int
                                                       off-t))
                               (or addr (sb-sys:int-sap 0)) length prot flags
                               (or fd -1) offset)))
    ;; FIXME (Rudi 2003-05-12) : here, we assume that a sap is 32
    ;; bits.  Revisit during the 64-bit port.  #XFFFFFFFF is (void
    ;; *)-1, which is the charming return value of mmap on failure.
    (if (= (sb-sys:sap-int result) #XFFFFFFFF)
        (values nil (get-errno))
        result)))

(defun unix-munmap (start length)
  (declare (type system-area-pointer start)
           (type (unsigned-byte 32) length))
  (void-syscall ("munmap" system-area-pointer size-t) start length))

(defun unix-mlock (addr length)
  (declare (type system-area-pointer addr)
	   (type (unsigned-byte 32) length))
  (void-syscall ("mlock" system-area-pointer size-t) addr length))

(defun unix-munlock (addr length)
  (declare (type system-area-pointer addr)
	   (type (unsigned-byte 32) length))
  (void-syscall ("munlock" system-area-pointer size-t) addr length))



