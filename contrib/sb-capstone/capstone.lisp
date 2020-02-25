;; This file makes a Common Lisp interface for Capstone.
;; We extract the API from https://github.com/aquynh/capstone/blob/master/include/capstone.h
;; We follow the file order and names as much as possible

(defpackage "SB-CAPSTONE"
  (:use :common-lisp)
  (:use :sb-alien)
  (:export
   ;; Constants
   #:cs-arch-arm
   #:cs-arch-arm64
   #:cs-arch-mips
   #:cs-arch-x86
   #:cs-arch-ppc
   #:cs-arch-sparc
   #:cs-arch-sysz
   #:cs-arch-xcore
   #:cs-arch-m68k
   #:cs-arch-tms320c64x
   #:cs-arch-m680x
   #:cs-arch-evm
   #:cs-arch-max
   #:cs-arch-all
   #:cs-api-major
   #:cs-api-minor
   #:cs-next-version
   #:cs-version-major
   #:cs-version-minor
   #:cs-version-extra
   #:cs-mnemonic-size
   #:cs-support-diet
   #:cs-support-x86-reduce
   #:cs-mode-little-endian
   #:cs-mode-arm
   #:cs-mode-16
   #:cs-mode-32
   #:cs-mode-64
   #:cs-mode-thumb
   #:cs-mode-mclass
   #:cs-mode-v8
   #:cs-mode-micro
   #:cs-mode-mips3
   #:cs-mode-mips32r6
   #:cs-mode-mips2
   #:cs-mode-v9
   #:cs-mode-qpx
   #:cs-mode-m68k-000
   #:cs-mode-m68k-010
   #:cs-mode-m68k-020
   #:cs-mode-m68k-030
   #:cs-mode-m68k-040
   #:cs-mode-m68k-060
   #:cs-mode-big-endian
   #:cs-mode-mips32
   #:cs-mode-mips64
   #:cs-mode-m680x-6301
   #:cs-mode-m680x-6309
   #:cs-mode-m680x-6800
   #:cs-mode-m680x-6801
   #:cs-mode-m680x-6805
   #:cs-mode-m680x-6808
   #:cs-mode-m680x-6809
   #:cs-mode-m680x-6811
   #:cs-mode-m680x-cpu12
   #:cs-mode-m680x-hcs08
   #:cs-opt-invalid
   #:cs-opt-syntax
   #:cs-opt-detail
   #:cs-opt-mode
   #:cs-opt-mem
   #:cs-opt-skipdata
   #:cs-opt-skipdata-setup
   #:cs-opt-mnemonic
   #:cs-opt-unsigned
   #:cs-opt-off
   #:cs-opt-on
   #:cs-opt-syntax-default
   #:cs-opt-syntax-intel
   #:cs-opt-syntax-att
   #:cs-opt-syntax-noregname
   #:cs-opt-syntax-masm
   #:cs-op-invalid
   #:cs-op-reg
   #:cs-op-imm
   #:cs-op-mem
   #:cs-op-fp
   #:cs-ac-invalid
   #:cs-ac-read
   #:cs-ac-write
   #:cs-grp-invalid
   #:cs-grp-jump
   #:cs-grp-call
   #:cs-grp-ret
   #:cs-grp-int
   #:cs-grp-iret
   #:cs-grp-privilege
   #:cs-grp-branch-relative
   #:cs-err-ok
   #:cs-err-mem
   #:cs-err-arch
   #:cs-err-handle
   #:cs-err-csh
   #:cs-err-mode
   #:cs-err-option
   #:cs-err-detail
   #:cs-err-memsetup
   #:cs-err-version
   #:cs-err-diet
   #:cs-err-skipdata
   #:cs-err-x86-att
   #:cs-err-x86-intel
   #:cs-err-x86-masm
   ;; Macros
   #:cs-make-version
   ;; Types
   #:cs-opt-mem
   #:cs-opt-mnem
   #:cs-detail
   #:cs-insn
   ;; Slots
   #:insn-id
   #:insn-addr
   #:insn-size
   #:insn-bytes
   #:insn-mnemonic
   #:insn-operands
   #:insn-detail
   ;; Capstone functions
   #:cs-open
   #:cs-close
   #:cs-malloc
   #:cs-option
   #:cs-disasm-iter
   #:cs-free
   ;; Helper function
   #:cs-open-for-target))

(in-package "SB-CAPSTONE")
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (sb-int:system-package-p *package*) t))

(defun try-load-shared-object (pathname)
  (handler-case
      (load-shared-object pathname :dont-save t)
    (error ()
      nil)))

(defun load-capstone ()
  (if (try-load-shared-object "libcapstone.so")
      (pushnew :sb-capstone *features*)
      (warn "Capstone not loaded.")))

(load-capstone)

;; Exported most of the constants, with the intention of making a binding
;; as complete as possible, while only mapping capstone.h (main header file)

(defconstant cs-api-major 4)
(defconstant cs-api-minor 0)

(defconstant cs-next-version 4)

(defconstant cs-version-major cs-api-major)
(defconstant cs-version-minor cs-api-minor)
(defconstant cs-version-extra 0)

(defmacro cs-make-version (major minor) (+ (ash major 8) minor))
(defconstant cs-mnemonic-size 32)

(defconstant cs-arch-arm 0)
(defconstant cs-arch-arm64 1)
(defconstant cs-arch-mips 2)
(defconstant cs-arch-x86 3)
(defconstant cs-arch-ppc 4)
(defconstant cs-arch-sparc 5)
(defconstant cs-arch-sysz 6)
(defconstant cs-arch-xcore 7)
(defconstant cs-arch-m68k 8)
(defconstant cs-arch-tms320c64x 9)
(defconstant cs-arch-m680x 10)
(defconstant cs-arch-evm 11)
(defconstant cs-arch-max 12)
(defconstant cs-arch-all #xffff)

(defconstant cs-support-diet (1+ cs-arch-all))
(defconstant cs-support-x86-reduce (+ 2 cs-arch-all))

(defconstant cs-mode-little-endian 0)
(defconstant cs-mode-arm 0)
(defconstant cs-mode-16 (ash 1 1))
(defconstant cs-mode-32 (ash 1 2))
(defconstant cs-mode-64 (ash 1 3))
(defconstant cs-mode-thumb (ash 1 4))
(defconstant cs-mode-mclass (ash 1 5))
(defconstant cs-mode-v8 (ash 1 6))
(defconstant cs-mode-micro (ash 1 4))
(defconstant cs-mode-mips3 (ash 1 5))
(defconstant cs-mode-mips32r6 (ash 1 6))
(defconstant cs-mode-mips2 (ash 1 7))
(defconstant cs-mode-v9 (ash 1 4))
(defconstant cs-mode-qpx (ash 1 4))
(defconstant cs-mode-m68k-000 (ash 1 1))
(defconstant cs-mode-m68k-010 (ash 1 2))
(defconstant cs-mode-m68k-020 (ash 1 3))
(defconstant cs-mode-m68k-030 (ash 1 4))
(defconstant cs-mode-m68k-040 (ash 1 5))
(defconstant cs-mode-m68k-060 (ash 1 6))
(defconstant cs-mode-big-endian (ash 1 31))
(defconstant cs-mode-mips32 cs-mode-32)
(defconstant cs-mode-mips64 cs-mode-64)
(defconstant cs-mode-m680x-6301 (ash 1 1))
(defconstant cs-mode-m680x-6309 (ash 1 2))
(defconstant cs-mode-m680x-6800 (ash 1 3))
(defconstant cs-mode-m680x-6801 (ash 1 4))
(defconstant cs-mode-m680x-6805 (ash 1 5))
(defconstant cs-mode-m680x-6808 (ash 1 6))
(defconstant cs-mode-m680x-6809 (ash 1 7))
(defconstant cs-mode-m680x-6811 (ash 1 8))
(defconstant cs-mode-m680x-cpu12 (ash 1 9))
(defconstant cs-mode-m680x-hcs08 (ash 1 10))

(define-alien-type cs-opt-mem
    (struct nil
            (malloc int)
            (calloc int)
            (realloc int)
            (free int)
            (vsnprintf int)))

(define-alien-type cs-opt-mnem
    (struct nil
            (id int)
            (mnemonic int)))

(defconstant cs-opt-invalid 0)
(defconstant cs-opt-syntax 1)
(defconstant cs-opt-detail 2)
(defconstant cs-opt-mode 3)
(defconstant cs-opt-mem 4)
(defconstant cs-opt-skipdata 5)
(defconstant cs-opt-skipdata-setup 6)
(defconstant cs-opt-mnemonic 7)
(defconstant cs-opt-unsigned 8)

(defconstant cs-opt-off 0)
(defconstant cs-opt-on 1)
(defconstant cs-opt-syntax-default 0)
(defconstant cs-opt-syntax-intel 1)
(defconstant cs-opt-syntax-att 2)
(defconstant cs-opt-syntax-noregname 3)
(defconstant cs-opt-syntax-masm 4)

(defconstant cs-op-invalid 0)
(defconstant cs-op-reg 1)
(defconstant cs-op-imm 2)
(defconstant cs-op-mem 3)
(defconstant cs-op-fp 4)

(defconstant cs-ac-invalid 0)
(defconstant cs-ac-read (ash 1 0))
(defconstant cs-ac-write (ash 1 1))

(defconstant cs-grp-invalid 0)
(defconstant cs-grp-jump 1)
(defconstant cs-grp-call 2)
(defconstant cs-grp-ret 3)
(defconstant cs-grp-int 4)
(defconstant cs-grp-iret 5)
(defconstant cs-grp-privilege 6)
(defconstant cs-grp-branch-relative 7)

;; cs-detail is not fully mapped
(define-alien-type cs-detail
    (struct nil
            (regs-read (array int 12))
            (regs-read-count int)
            (regs-write (array int 20))
            (regs-write-count int)
            (groups (array int 8))
            (groups-count int)
            ;; FIXME: insert a union of unmapped structs
            ;; architecture dependent structs (cs_x86, cs_arm64, etc)
            ))

(define-alien-type cs-insn
    (struct nil
            (insn-id int)
            (insn-addr unsigned)
            (insn-size short)
            (insn-bytes (array char 16))
            (insn-mnemonic (array char 32))
            (insn-operands (array char 160))
            (insn-detail (* cs-detail))))

(defconstant cs-err-ok  0)
(defconstant cs-err-mem  1)
(defconstant cs-err-arch  2)
(defconstant cs-err-handle  3)
(defconstant cs-err-csh 4)
(defconstant cs-err-mode  5)
(defconstant cs-err-option  6)
(defconstant cs-err-detail  7)
(defconstant cs-err-memsetup  8)
(defconstant cs-err-version  9)
(defconstant cs-err-diet  10)
(defconstant cs-err-skipdata  11)
(defconstant cs-err-x86-att  12)
(defconstant cs-err-x86-intel  13)
(defconstant cs-err-x86-masm  14)

;; The handle returned by cs-open will be represented as being of type unsigned

(define-alien-routine cs-open int (arch int) (mode (integer 64)) (handle unsigned :out))

(define-alien-routine cs-version unsigned (major int :out) (minor int :out))

(define-alien-routine cs-close int (handle unsigned :in-out))

(define-alien-routine cs-free void (insn (* cs-insn)) (count unsigned))

(define-alien-routine cs-option int (handle unsigned) (cs-opt-type int) (value unsigned))

(define-alien-routine cs-malloc (* cs-insn) (handle unsigned))

(define-alien-routine cs-disasm-iter
    (boolean 8)
  (handle unsigned)
  (code unsigned :in-out)
  (size unsigned :in-out)
  (address unsigned :in-out)
  (insn (* cs-insn)))


(defun get-cs-arch (target)
  (case (car target)
    (:x86 cs-arch-x86)
    (:x86-64 cs-arch-x86)
    (:ppc cs-arch-ppc)
    (:ppc64 cs-arch-ppc)
    (:mips cs-arch-mips)
    (:arm cs-arch-arm)
    (:arm64 cs-arch-arm64)
    (:sparc cs-arch-sparc)
    (otherwise
     (error "Unknown arch for capstone"))))

(defun get-cs-mode (target)
  (flet ((cs-endiannes (endiannes)
           (ecase endiannes
             (:little-endian cs-mode-little-endian)
             (:big-endian cs-mode-big-endian)))
         (cs-wordsize (arch)
           (ecase arch
             (:x86 cs-mode-32)
             (:x86-64 cs-mode-64)
             (:ppc cs-mode-32)
             (:ppc64 cs-mode-64)
             (:mips cs-mode-32)
             (:arm cs-mode-32)
             (:arm64 cs-mode-64)
             (:sparc cs-mode-32))))
    (+ (cs-endiannes (cadr target)) (cs-wordsize (car target)))))

;; The function expects a list of SBCL features, the first one being the architecture, the second one being the endiannes
;; an example target would be (:x86-64 :little-endian)

(defun cs-open-for-target (target)
  (cs-open (get-cs-arch target)
           (get-cs-mode target)))
