;;;; disassembler structures not needed in cross-compilation host

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!DISASSEM")

(defstruct (storage-info (:copier nil))
  (groups nil :type list)               ; alist of (name . location-group)
  (debug-vars #() :type vector))

(defstruct (segment (:conc-name seg-)
                    (:constructor %make-segment)
                    (:copier nil))
  ;; the object that should be pinned when calling the sap-maker
  (object nil)
  (sap-maker (missing-arg) :type (function () system-area-pointer))
  ;; Length in bytes of the range of memory covered by this segment.
  (length 0 :type disassem-length)
  (virtual-location 0 :type address)
  (storage-info nil :type (or null storage-info))
  (code nil :type (or null code-component))
  ;; the byte offset beyond CODE-INSTRUCTIONS of CODE which
  ;; corresponds to offset 0 in this segment
  (initial-offset 0 :type index)
  ;; number of bytes to print as literal data without disassembling.
  ;; will always be 0 for any segment whose initial-offset is nonzero
  (initial-raw-bytes 0 :type index)
  (hooks nil :type list))

;;; All state during disassembly. We store some seemingly redundant
;;; information so that we can allow garbage collect during disassembly and
;;; not get tripped up by a code block being moved...
(defstruct (disassem-state (:conc-name dstate-)
                           (:constructor %make-dstate
                               (alignment argument-column fun-hooks))
                           (:copier nil))
  ;; offset of current pos in segment
  (cur-offs 0 :type offset)
  ;; offset of next position
  (next-offs 0 :type offset)
  ;; a sap pointing to our segment
  (segment-sap (int-sap 0) :type system-area-pointer)
  ;; the current segment
  (segment nil :type (or null segment))
  ;; to avoid buffer overrun at segment end, we might need to copy bytes
  ;; here first because sap-ref-dchunk reads a fixed length.
  (scratch-buf (make-array 8 :element-type '(unsigned-byte 8)))
  ;; what to align to in most cases
  (alignment sb!vm:n-word-bytes :type alignment)
  (byte-order sb!c:*backend-byte-order*
              :type (member :big-endian :little-endian))
  ;; for user code to track decoded bits, cleared each time after a
  ;; non-prefix instruction is processed
  (inst-properties 0 :type fixnum)
  (filtered-values (make-array max-filtered-value-index)
                   :type filtered-value-vector)
  ;; to avoid consing decoded values, a prefilter can keep a chain
  ;; of objects in these slots. The objects returned here
  ;; are reusable for the next instruction.
  (filtered-arg-pool-in-use)
  (filtered-arg-pool-free)
  ;; used for prettifying printing
  (addr-print-len nil :type (or null (integer 0 20)))
  (argument-column 0 :type column)
  ;; to make output look nicer
  (output-state :beginning
                :type (member :beginning
                              :block-boundary
                              nil))

  ;; alist of (address . label-number)
  (labels nil :type list)
  ;; same as LABELS slot data, but in a different form
  (label-hash (make-hash-table) :type hash-table)
  ;; list of function
  (fun-hooks nil :type list)

  ;; alist of (address . label-number), popped as it's used
  (cur-labels nil :type list)
  ;; OFFS-HOOKs, popped as they're used
  (cur-offs-hooks nil :type list)

  ;; for the current location
  (notes nil :type list)

  ;; currently active source variables
  (current-valid-locations nil :type (or null (vector bit))))

(declaim (freeze-type disassem-state))
(defmethod print-object ((dstate disassem-state) stream)
  (print-unreadable-object (dstate stream :type t)
    (format stream
            "+~W~@[ in ~S~]"
            (dstate-cur-offs dstate)
            (dstate-segment dstate))))

;;; Return the absolute address of the current instruction in DSTATE.
(defun dstate-cur-addr (dstate)
  (the address (+ (seg-virtual-location (dstate-segment dstate))
                  (dstate-cur-offs dstate))))

;;; Return the absolute address of the next instruction in DSTATE.
(defun dstate-next-addr (dstate)
  (the address (+ (seg-virtual-location (dstate-segment dstate))
                  (dstate-next-offs dstate))))
