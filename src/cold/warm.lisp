;;;; "warm initialization": initialization which comes after cold init

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "COMMON-LISP-USER")

;;;; general warm init compilation policy

(proclaim '(optimize (compilation-speed 1)
		     (debug #+sb-show 2 #-sb-show 1)
		     (inhibit-warnings 2)
		     (safety 1)
		     (space 1)
		     (speed 2)))

;;;; KLUDGE: Compile and load files which really belong in cold load but are
;;;; here for various unsound reasons. We handle them here, before the package
;;;; hacking below, because they use the SB!FOO cold package name convention
;;;; instead of the SB-FOO final package name convention (since they really
;;;; do belong in cold load and will hopefully make it back there reasonably
;;;; soon). -- WHN 19991207

(dolist (stem '(;; FIXME: The files here from outside the src/pcl/ directory
		;; probably belong in cold load instead of warm load. They
		;; ended up here as a quick hack to work around the
		;; consequences of my misunderstanding how ASSEMBLE-FILE works
		;; when I wrote the cold build code. The cold build code
		;; expects only one FASL filename per source file, when it
		;; turns out we really need one FASL file for ASSEMBLE-FILE
		;; output and another for COMPILE-FILE output. It would
		;; probably be good to redo the cold build code so that the
		;; COMPILE-FILE stuff generated here can be loaded at the same
		;; time as the ASSEMBLE-FILE stuff generated there.
		"src/assembly/target/assem-rtns"
		"src/assembly/target/array"
		"src/assembly/target/arith"
		"src/assembly/target/alloc"))
  ;; KLUDGE: Cut-and-paste programming, the sign of a true professional.:-|
  ;; (Hopefully this will go away as we move the files above into cold load.)
  ;; -- WHN 19991214
  (let ((fullname (concatenate 'string stem ".lisp")))
    (sb!int:/show "about to compile" fullname)
    (multiple-value-bind
	(compiled-truename compilation-warnings-p compilation-failure-p)
	(compile-file fullname)
      (declare (ignore compilation-warnings-p))
      (sb!int:/show "done compiling" fullname)
      (if compilation-failure-p
	  (error "COMPILE-FILE of ~S failed." fullname)
	  (unless (load compiled-truename)
	    (error "LOAD of ~S failed." compiled-truename))))))

;;;; package hacking

;;; Our cross-compilation host is out of the picture now, so we no longer need
;;; to worry about collisions between our package names and cross-compilation
;;; host package names, so now is a good time to rename any package with a
;;; bootstrap-only name SB!FOO to its permanent name SB-FOO.
;;;
;;; (In principle it might be tidier to do this when dumping the cold image in
;;; genesis, but in practice the logic might be a little messier because
;;; genesis dumps both symbols and packages, and we'd need to make that dumped
;;; symbols were renamed in the same way as dumped packages. Or we could do it
;;; in cold init, but it's easier to experiment with and debug things here in
;;; warm init than in cold init, so we do it here instead.)
(let ((boot-prefix "SB!")
      (perm-prefix "SB-"))
  (dolist (package (list-all-packages))
    (let ((old-package-name (package-name package)))
      (when (and (>= (length old-package-name) (length boot-prefix))
		 (string= boot-prefix old-package-name
			  :end2 (length boot-prefix)))
	(let ((new-package-name (concatenate 'string
					     perm-prefix
					     (subseq old-package-name
						     (length boot-prefix)))))
	  (rename-package package
			  new-package-name
			  (package-nicknames package)))))))

;;; KLUDGE: This is created here (instead of in package-data-list.lisp-expr)
;;; because it doesn't have any symbols in it, so even if it's
;;; present at cold load time, genesis thinks it's unimportant
;;; and doesn't dump it. There's gotta be a better way, but for now
;;; I'll just do it here. (As noted below, I'd just as soon have this
;;; go away entirely, so I'm disinclined to fiddle with it too much.)
;;; -- WHN 19991206
;;;
;;; FIXME: Why do slot accessor names need to be interned anywhere? For
;;; low-level debugging? Perhaps this should go away, or at least
;;; be optional, controlled by SB-SHOW or something.
(defpackage "SB-SLOT-ACCESSOR-NAME"
  (:use))

;;;; compiling and loading more of the system

;;; KLUDGE: In SBCL, almost all in-the-flow-of-control package hacking has
;;; gone away in favor of package setup controlled by tables. However, that
;;; mechanism isn't smart enough to handle shadowing, and since this shadowing
;;; is inherently a non-ANSI KLUDGE anyway (i.e. there ought to be no
;;; difference between e.g. CL:CLASS and SB-PCL:CLASS) there's not much
;;; point in trying to polish it by implementing a non-KLUDGEy way of
;;; setting it up. -- WHN 19991203
(let ((*package* (the package (find-package "SB-PCL"))))
  (shadow '(;; CLASS itself and operations thereon
	    "CLASS" "CLASS-NAME" "CLASS-OF" "FIND-CLASS"
	    ;; some system classes
	    "BUILT-IN-CLASS" "STANDARD-CLASS" "STRUCTURE-CLASS"))
  ;; Of the shadowing symbols above, these are external symbols in CMU CL ca.
  ;; 19991203. I'm not sure what's the basis of the decision to export some and
  ;; not others; we'll just follow along..
  (export (mapcar #'intern '("CLASS-NAME" "CLASS-OF" "FIND-CLASS"))))

;;; FIXME: CMU CL's pclcom.lisp had extra optional stuff wrapped around
;;; COMPILE-PCL, at least some of which we should probably have too:
;;;
;;; (with-compilation-unit
;;;     (:optimize '(optimize (debug #+(and (not high-security) small) .5
;;;				  #-(or high-security small) 2
;;;				  #+high-security 3)
;;;			   (speed 2) (safety #+(and (not high-security) small) 0
;;;					     #-(or high-security small) 2
;;;					     #+high-security 3)
;;;			   (inhibit-warnings 2))
;;;      :optimize-interface '(optimize-interface #+(and (not high-security) small)
;;; (safety 1)
;;;					       #+high-security (safety 3))
;;;      :context-declarations
;;;      '((:external (declare (optimize-interface (safety #-high-security 2 #+high-
;;; security 3)
;;;						(debug #-high-security 1 #+high-s
;;; ecurity 3))))
;;;	((:or :macro (:match "$EARLY-") (:match "$BOOT-"))
;;;	(declare (optimize (speed 0))))))
;;;
;;; FIXME: This has mutated into a hack which crudely duplicates
;;; functionality from the existing mechanism to load files from
;;; stems-and-flags.lisp-expr, without being quite parallel. (E.g.
;;; object files end up alongside the source files instead of ending
;;; up in parallel directory trees.) Maybe we could merge the
;;; filenames here into stems-and-flags.lisp-expr with some new flag
;;; (perhaps :WARM) to indicate that the files should be handled not
;;; in cold load but afterwards. Alternatively, we could call
(dolist (stem '(
		;; CLOS, derived from the PCL reference implementation
		;;
		;; This PCL build order is based on a particular
		;; linearization of the declared build order
		;; dependencies from the old PCL defsys.lisp
		;; dependency database.
		"src/pcl/walk"
		"src/pcl/iterate"
		"src/pcl/early-low"
		"src/pcl/macros"
		"src/pcl/low"
		"src/pcl/fin"
		"src/pcl/defclass"
		"src/pcl/defs"
		"src/pcl/fngen"
		"src/pcl/cache"
		"src/pcl/dlisp"
		"src/pcl/dlisp2"
		"src/pcl/boot"
		"src/pcl/vector"
		"src/pcl/slots-boot"
		"src/pcl/combin"
		"src/pcl/dfun"
		"src/pcl/fast-init"
		"src/pcl/braid"
		"src/pcl/dlisp3"
		"src/pcl/generic-functions"
		"src/pcl/slots"
		"src/pcl/init"
		"src/pcl/std-class"
		"src/pcl/cpl"
		"src/pcl/fsc"
		"src/pcl/methods"
		"src/pcl/fixup"
		"src/pcl/defcombin"
		"src/pcl/ctypes"
		"src/pcl/construct"
		"src/pcl/env"
		"src/pcl/documentation"
		"src/pcl/print-object"
		"src/pcl/precom1"
		"src/pcl/precom2"
		;; functionality which depends on CLOS
		"src/code/force-delayed-defbangmethods"
		;; other functionality not needed for cold init, moved
		;; to warm init to reduce peak memory requirement in
		;; cold init
		"src/code/describe" ; FIXME: should be byte compiled
		"src/code/inspect" ; FIXME: should be byte compiled
		"src/code/profile"
		"src/code/ntrace"
		"src/code/foreign"
		"src/code/run-program"
		;; Code derived from PCL's pre-ANSI DESCRIBE-OBJECT
		;; facility is still used in our ANSI DESCRIBE
		;; facility, and should be compiled and loaded after
		;; our DESCRIBE facility is compiled and loaded.
		"src/pcl/describe" ; FIXME: should probably be byte compiled
		;; FIXME: What about Gray streams? e.g. "gray-streams.lisp"
		;; and "gray-streams-class.lisp"?
		))
  (let ((fullname (concatenate 'string stem ".lisp")))
    (sb-int:/show "about to compile" fullname)
    (multiple-value-bind
	(compiled-truename compilation-warnings-p compilation-failure-p)
	(compile-file fullname)
      (declare (ignore compilation-warnings-p))
      (sb-int:/show "done compiling" fullname)
      (cond (compilation-failure-p
	     (error "COMPILE-FILE of ~S failed." fullname))
	    (t
	     (unless (load compiled-truename)
	       (error "LOAD of ~S failed." compiled-truename))
	     (sb-int:/show "done loading" compiled-truename))))))

;;;; setting package documentation

;;; While we were running on the cross-compilation host, we tried to
;;; be portable and not overwrite the doc strings for the standard
;;; packages. But now the cross-compilation host is only a receding
;;; memory, and we can have our way with the doc strings.
(sb-int:/show "setting package documentation")
#+sb-doc (setf (documentation (find-package "COMMON-LISP") t)
"public: home of symbols defined by the ANSI language specification")
#+sb-doc (setf (documentation (find-package "COMMON-LISP-USER") t)
	       "public: the default package for user code and data")
#+sb-doc (setf (documentation (find-package "KEYWORD") t)
	       "public: home of keywords")

;;; KLUDGE: It'd be nicer to do this in the table with the other
;;; non-standard packages. -- WHN 19991206
#+sb-doc (setf (documentation (find-package "SB-SLOT-ACCESSOR-NAME") t)
	       "private: home of CLOS slot accessor internal names")

;;; FIXME: There doesn't seem to be any easy way to get package doc strings
;;; through the cold boot process. They need to be set somewhere. Maybe the
;;; easiest thing to do is to read them out of package-data-list.lisp-expr
;;; now?

;;;; restoring compilation policy to neutral values in preparation for
;;;; SAVE-LISP-AND-DIE as final SBCL core

(sb-int:/show "setting compilation policy to neutral values")
(proclaim '(optimize (compilation-speed 1)
		     (debug 1)
		     (inhibit-warnings 1)
		     (safety 1)
		     (space 1)
		     (speed 1)))

;;; FIXME: It would be good to unintern stuff we will no longer need
;;; before we go on to PURIFY. E.g.
;;;  * various PCL stuff like INITIAL-CLASSES-AND-WRAPPERS; and
;;;  * *BUILT-IN-CLASSES* (which can't actually be freed by UNINTERN at
;;;    this point, since it passed through another PURIFY earlier
;;;    at cold init time).
