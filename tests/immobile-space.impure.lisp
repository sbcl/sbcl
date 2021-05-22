
#-immobile-space (sb-ext:exit :code 104)

;;; If an instance was allocated but its layout not stored yet
;;; it could crash
(defun alloc-layoutless-instances ()
  (list (sb-vm::alloc-immobile-fixedobj
         6 (logior (ash 5 sb-vm:instance-length-shift) sb-vm:instance-widetag))
        (sb-vm::alloc-immobile-fixedobj
         6 (logior (ash 5 sb-vm:n-widetag-bits) sb-vm:funcallable-instance-widetag))))
(compile 'alloc-layoutless-instances)
;;; the first GC should WP some pages but might not crash
(dotimes (i 1000 (gc)) (alloc-layoutless-instances))
;;; the second one was more likely to crash
(dotimes (i 1000 (gc)) (alloc-layoutless-instances))

(defstruct trythis a)

;;; Assign a bitmap that is not the special case for "all tagged"
;;; but does correctly indicate 1 tagged slot.
(let* ((l (sb-kernel:wrapper-friend (sb-kernel:find-layout 'trythis)))
       (slot (1- (sb-kernel:%instance-length l))))
  (assert (eql (sb-kernel:%raw-instance-ref/signed-word l slot)
               sb-kernel:+layout-all-tagged+))
  (sb-kernel:%raw-instance-set/word l slot 1))

(defun ll-alloc ()
  ;; This must be in its own function because the vop preserves no registers
  ;; when calling to C.
  (values(sb-sys:%primitive
            sb-vm::alloc-immobile-fixedobj
            8 ; an unused sized class
            2 ; physical words
            (logior (ash 1 sb-vm:instance-length-shift)
                    sb-vm:instance-widetag))))
(compile 'll-alloc) ; low level allocator
(defun make ()
  (let ((inst (ll-alloc)))
    (setf (sb-kernel:%instance-wrapper (truly-the trythis inst))
          (sb-kernel:find-layout 'trythis))
    (setf (trythis-a inst) (copy-seq "Hello"))
    inst))

(setf (extern-alien "verify_gens" char) 0)
(compile 'make)
(defglobal things (loop repeat 5 collect (make)))
;;; promote THINGS to gen 1 so that we can make them
;;; point to something younger.
(gc :gen 1)
(assert (eql (sb-kernel:generation-of (car things)) 1))
(setf (trythis-a (car things)) "wat")

;;; This next GC doesn't incur a bug (though that's maybe surprising),
;;; but the final one would if this one leaves a page protection bit
;;; in a wrong state such that an old->young pointer is missed next time.
(gc :gen 2)
(print things)
(setf (trythis-a (car things)) "anewstring")
(gc)
