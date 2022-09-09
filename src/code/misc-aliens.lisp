;;;; assorted alien definitions

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

;;; Declare each of the free space pointers (except dynamic) as an alien var
;;; with darwin-jit, READ-ONLY-SPACE-START is a constant from genesis
;;; Maybe this whole file should go in sb-kernel to avoid sb-kernel::
#-darwin-jit
(define-alien-variable ("READ_ONLY_SPACE_START" sb-vm:read-only-space-start) sb-kernel::os-vm-size-t)
(define-alien-variable ("read_only_space_free_pointer"
                        sb-vm:*read-only-space-free-pointer*)
    system-area-pointer)

;;; STATIC-SPACE-START is a constant from genesis
(define-alien-variable ("static_space_free_pointer" sb-vm:*static-space-free-pointer*)
  system-area-pointer)

(define-alien-variable ("DYNAMIC_SPACE_START" sb-vm:dynamic-space-start) sb-kernel::os-vm-size-t)
;;; Dynamic doesn't really have a "free pointer" but it's the upper bound on space usage.
(declaim (inline dynamic-space-free-pointer))
(defun dynamic-space-free-pointer ()
  (sap+ (int-sap sb-vm:dynamic-space-start)
        ;; not sure why next_free_page is 'sword_t' instead of 'uword_t' !
        (truly-the (signed-byte 64)
                   (* (extern-alien "next_free_page" signed) sb-vm:gencgc-page-bytes))))

#+immobile-space
(progn
(define-symbol-macro sb-vm:alien-linkage-table-space-start
    (extern-alien "ALIEN_LINKAGE_TABLE_SPACE_START" unsigned))
(define-alien-variable ("TEXT_SPACE_START" sb-vm:text-space-start) unsigned-long))

#+darwin-jit
(define-alien-variable ("static_code_space_free_pointer" sb-vm:*static-code-space-free-pointer*)
  system-area-pointer)

(declaim (inline memmove))
(define-alien-routine ("memmove" memmove) void ; BUG: technically returns void*
  (dest (* char))
  (src (* char))
  (n sb-unix::size-t))

(defun copy-ub8-to-system-area (src src-offset dst dst-offset length)
  (with-pinned-objects (src)
    (memmove (sap+ dst dst-offset) (sap+ (vector-sap src) src-offset) length))
  (values))

(defun copy-ub8-from-system-area (src src-offset dst dst-offset length)
  (with-pinned-objects (dst)
    (memmove (sap+ (vector-sap dst) dst-offset) (sap+ src src-offset) length))
  (values))

(defun system-area-ub8-copy (src src-offset dst dst-offset length)
  (memmove (sap+ dst dst-offset) (sap+ src src-offset) length)
  (values))

(define-alien-routine ("os_get_errno" get-errno) int)
(setf (documentation 'get-errno 'function)
      "Return the value of the C library pseudo-variable named \"errno\".")

;;; Decode errno into a string.
#-win32
(defun strerror (&optional (errno (get-errno)))
  (alien-funcall (extern-alien "strerror" (function c-string int)) errno))

#+win32
(defun strerror (&optional (errno (sb-win32:get-last-error)))
  (sb-win32:format-system-message errno))

;;; This gives you a way to call the low-level debugger in situations
;;; where something is broken, though the REPL works. One example is that
;;; when the cold core first starts up, you can't compile alien accessors,
;;; so an alien-funcall at the REPL would fail thusly:
;;; 0: ("undefined function" ...) ; EVAL-IN-NATIVE-ENVIRONMENT
;;; 1: (EVAL (LAMBDA (SB-ALIEN::SAP SB-ALIEN::OFFSET IGNORE) ...))
;;; 2: (SB-ALIEN::COERCE-TO-INTERPRETED-FUNCTION (LAMBDA (...)))
;;; 3: (SB-ALIEN-INTERNALS:%ALIEN-VALUE #.(SB-SYS:INT-SAP #X50000BB0) ...)
;;;
;;; or more generally, the compiler could be messed up for whatever reason.
(defun sb-vm:ldb-monitor ()
  (alien-funcall (extern-alien "ldb_monitor" (function void))))
