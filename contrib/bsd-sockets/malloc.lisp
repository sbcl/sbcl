(in-package :bsd-sockets-internal)

(defun malloc (size)
  "Allocate foreign memory in some way that allows the garbage collector to free it later.  Note that memory allocated this way does not count as `consed' for the purposes of deciding when to gc, so explicitly calling EXT:GC occasionally would be a good idea if you use it a lot"
  ;; we can attach finalizers to any object, and they'll be called on
  ;; the next gc after the object no longer has references.  We can't
  ;; however make the finalizer close over the object, or it'll never
  ;; have no references.  I experimentally determined that (sap-alien
  ;; (alien-sap f)) is not EQ to f, so we can do it that way
  (let* ((memory (make-alien (unsigned 8) size))
         (alias (sap-alien (alien-sap memory)
                                 (* (unsigned 8)))))
    (sb-ext:finalize memory
                     (lambda ()
                       (free-alien alias)))))

