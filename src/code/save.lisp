;;;; Dump the current Lisp image into a core file. Also contains
;;;; various high-level initialization stuff: loading init files and
;;;; parsing environment variables.
;;;;
;;;; (All the real work is done by C.)

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

;;;; SAVE-LISP-AND-DIE itself

(sb!alien:define-alien-routine "save" (sb!alien:boolean)
  (file sb!alien:c-string)
  (initial-fun (sb!alien:unsigned #.sb!vm:n-word-bits)))

;;; FIXME: When this is run without the PURIFY option,
;;; it seems to save memory all the way up to the high-water mark,
;;; not just what's currently used; and then after loading the
;;; image to make a running Lisp, the memory never gets reclaimed.
;;; (But with the PURIFY option it seems to work OK.)
(defun save-lisp-and-die (core-file-name &key
					 (toplevel #'toplevel-init)
					 (purify t)
					 (root-structures ())
					 (environment-name "auxiliary"))
  #!+sb-doc
  "Save a \"core image\", i.e. enough information to restart a Lisp
  process later in the same state, in the file of the specified name.

  This implementation is not as polished and painless as you might like: 
    * It corrupts the current Lisp image enough that the current process
      needs to be killed afterwards.
    * There is absolutely no binary compatibility of core images between
      different runtime support programs. Even runtimes built from the same
      sources at different times are treated as incompatible for this purpose.
  This isn't because we like it this way, but just because there don't
  seem to be good quick fixes for either limitation and no one has been
  sufficiently motivated to do lengthy fixes.

  The following &KEY arguments are defined:
    :TOPLEVEL
       The function to run when the created core file is resumed.
       The default function handles command line toplevel option
       processing and runs the top level read-eval-print loop. This
       function should not return.
    :PURIFY
       If true (the default), do a purifying GC which moves all dynamically
       allocated objects into static space so that they stay pure. This takes
       somewhat longer than the normal GC which is otherwise done, but it's
       only done once, and subsequent GC's will be done less often and will
       take less time in the resulting core file. See the PURIFY function.
    :ROOT-STRUCTURES
       This should be a list of the main entry points in any newly loaded
       systems. This need not be supplied, but locality and/or GC performance
       may be better if they are. Meaningless if :PURIFY is NIL. See the
       PURIFY function.
    :ENVIRONMENT-NAME
       This is also passed to the PURIFY function when :PURIFY is T.
       (rarely used)

  The save/load process changes the values of some global variables:
    *STANDARD-OUTPUT*, *DEBUG-IO*, etc.
      Everything related to open streams is necessarily changed, since
      the OS won't let us preserve a stream across save and load.
    *DEFAULT-PATHNAME-DEFAULTS*
      This is reinitialized to reflect the working directory where the
      saved core is loaded."

  ;; FIXME (sb!mp::shutdown-multi-processing)
  ;; FIXME: What is this for? Explain.
  (when (fboundp 'cancel-finalization)
    (cancel-finalization sb!sys:*tty*))
  ;; FIXME: Would it be possible to unmix the PURIFY logic from this
  ;; function, and just do a GC :FULL T here? (Then if the user wanted
  ;; a PURIFYed image, he'd just run PURIFY immediately before calling
  ;; SAVE-LISP-AND-DIE.)
  (if purify
      (purify :root-structures root-structures
	      :environment-name environment-name)
      #!-gencgc (gc) #!+gencgc (gc :full t))
  ;; FIXME: Wouldn't it be more correct to go through this list backwards
  ;; instead of forwards?
  (dolist (f *before-save-initializations*)
    (funcall f))
  (flet ((restart-lisp ()
           (handling-end-of-the-world
	     (reinit)
	     (dolist (f *after-save-initializations*)
	       (funcall f))
	     (funcall toplevel))))
    ;; FIXME: Perhaps WITHOUT-GCING should be wrapped around the
    ;; LET as well, to avoid the off chance of an interrupt triggering
    ;; GC and making our saved RESTART-LISP address invalid?
    (without-gcing
      (save (unix-namestring core-file-name nil)
	    (get-lisp-obj-address #'restart-lisp)))))
