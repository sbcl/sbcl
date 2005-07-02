;;;; support for threads in the target machine common to uni- and
;;;; multithread systems

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!THREAD")

(defstruct (thread (:constructor %make-thread))
  name
  %sap)

(def!method print-object ((thread thread) stream)
  (if (thread-name thread)
      (print-unreadable-object (thread stream :type t :identity t)
        (prin1 (thread-name thread) stream))
      (print-unreadable-object (thread stream :type t :identity t)
        ;; body is empty => there is only one space between type and
        ;; identity
        ))
  thread)

(defun thread-state (thread)
  (let ((state
	 (sb!sys:sap-int
	  (sb!sys:sap-ref-sap (thread-%sap thread)
			      (* sb!vm::thread-state-slot
				 sb!vm::n-word-bytes)))))
    (ecase state
      (#.(sb!vm:fixnumize 0) :starting)
      (#.(sb!vm:fixnumize 1) :running)
      (#.(sb!vm:fixnumize 2) :suspended)
      (#.(sb!vm:fixnumize 3) :dead))))

(defun %set-thread-state (thread state)
  (setf (sb!sys:sap-ref-sap (thread-%sap thread)
                            (* sb!vm::thread-state-slot
                               sb!vm::n-word-bytes))
        (sb!sys:int-sap
	  (ecase state
	    (:starting #.(sb!vm:fixnumize 0))
	    (:running #.(sb!vm:fixnumize 1))
	    (:suspended #.(sb!vm:fixnumize 2))
	    (:dead #.(sb!vm:fixnumize 3))))))

(defun thread-alive-p (thread)
  (not (eq :dead (thread-state thread))))

;; A thread is eligible for gc iff it has finished and there are no
;; more references to it. This list is supposed to keep a reference to
;; all running threads.
(defvar *all-threads* ())
(defvar *all-threads-lock* (make-mutex :name "all threads lock"))

(defun list-all-threads ()
  (with-mutex (*all-threads-lock*)
    (copy-list *all-threads*)))

(declaim (inline current-thread-sap))
(defun current-thread-sap ()
  (sb!vm::current-thread-offset-sap sb!vm::thread-this-slot))

(declaim (inline current-thread-sap-id))
(defun current-thread-sap-id ()
  (sb!sys:sap-int
   (sb!vm::current-thread-offset-sap sb!vm::thread-os-thread-slot)))

(defun init-initial-thread ()
  (let ((initial-thread (%make-thread :name "initial thread"
                                      :%sap (current-thread-sap))))
    (setq *current-thread* initial-thread)
    ;; Either *all-threads* is empty or it contains exactly one thread
    ;; in case we are in reinit since saving core with multiple
    ;; threads doesn't work.
    (setq *all-threads* (list initial-thread))))
