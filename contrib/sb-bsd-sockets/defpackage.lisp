(defpackage "SB-BSD-SOCKETS-INTERNAL"
  (:nicknames "SOCKINT")
  (:shadow close listen)
  #+cmu (:shadowing-import-from "CL" with-array-data)
  #+sbcl (:shadowing-import-from "SB-KERNEL" with-array-data)
  #+cmu (:use "COMMON-LISP" "ALIEN" "SYSTEM" "EXT" "C-CALL")
  #+sbcl (:use "COMMON-LISP" "SB-ALIEN" #+nil "SB-SYSTEM" "SB-EXT" "SB-C-CALL"))

;;; SBCL changes a lot of package prefixes.  To avoid littering the
;;; code with conditionals, we use the SBCL package prefixes
;;; throughout.  This means that we need to create said packages
;;; first, if we're using CMUCL

;;; One thing that this exercise really has made clear is just how much
;;; of the alien stuff is scattered around the cmucl package space
;;; seemingly at random.  Hmm.

#+cmu
(eval-when (:compile-toplevel :load-toplevel)
  (defun add-package-nickname (name nickname)
    (let ((p (find-package name)))
      (rename-package p (package-name p)
                      (cons nickname (package-nicknames name)))))
  (add-package-nickname "EXT" "SB-EXT")
  (add-package-nickname "ALIEN" "SB-ALIEN")
  (add-package-nickname "UNIX" "SB-UNIX")
  (add-package-nickname "C-CALL" "SB-C-CALL")
  (add-package-nickname "KERNEL" "SB-KERNEL")
  (add-package-nickname "SYSTEM" "SB-SYS"))

(defpackage "SB-BSD-SOCKETS"
  (:export socket local-socket inet-socket
           make-local-socket make-inet-socket
           socket-bind socket-accept socket-connect
           socket-send socket-receive socket-recv
           socket-name socket-peername socket-listen
           socket-close socket-file-descriptor
           socket-family socket-protocol socket-open-p
           socket-type socket-make-stream get-protocol-by-name

           get-host-by-name get-host-by-address
           host-ent
           host-ent-addresses host-ent-address
           host-ent-aliases host-ent-name
           name-service-error
           ;; not sure if these are really good names or not
           netdb-internal-error
           netdb-success-error
           host-not-found-error
           try-again-error
           no-recovery-error

           ;; all socket options are also exported, by code in
           ;; sockopt.lisp

           socket-error

           ;; other errno-based socket errors are exported by code in
           ;; sockets.lisp

           make-inet-address

           non-blocking-mode
           )
  (:use "COMMON-LISP" "SB-BSD-SOCKETS-INTERNAL")
  (:import-from "SB-INT" "UNSUPPORTED-OPERATOR" "FEATUREP")
  (:documentation
   "

A thinly-disguised BSD socket API for SBCL.  Ideas stolen from the BSD
socket API for C and Graham Barr's IO::Socket classes for Perl.

We represent sockets as CLOS objects, and rename a lot of methods and
arguments to fit Lisp style more closely.

"
   ))

#||

<h2>Contents</h2>

<ol>
<li> General concepts
<li> Methods applicable to all <a href="#socket">sockets</a>
<li> <a href="#sockopt">Socket Options</a>
<li> Methods applicable to a particular subclass
<ol>
<li> <a href="#internet">INET-SOCKET</a> - Internet Protocol (TCP, UDP, raw) sockets
<li> Methods on <a href="#LOCAL-SOCKET">LOCAL-SOCKET</a> - Local-domain sockets
</ol>
<li> <a href="#name-service">Name resolution</a> (DNS, /etc/hosts, &amp;c)
</ol>

<h2>General concepts</h2>

<p>Most of the functions are modelled on the BSD socket API.  BSD sockets
are widely supported, portably <i>("portable" by Unix standards, at least)</i>
available on a variety of systems, and documented.  There are some
differences in approach where we have taken advantage of some of the more useful features of Common Lisp - briefly

<ul>
<li> Where the C API would typically return -1 and set errno, we
signal an error.  All the errors are subclasses of SOCKET-CONDITION
and generally correspond one for one with possible <tt>errno</tt> values

<li> We use multiple return values in many places where the C API would use
pass-by-reference values

<li> We can often avoid supplying an explicit <i>length</i> argument to
functions because we already know how long the argument is.

<li> IP addresses and ports are represented in slightly friendlier fashion
than "network-endian integers".  See the section on <a href="#internet"
>Internet domain</a> sockets for details.
</ul>


|#

(in-package :sb-bsd-sockets)

(defmethod asdf:hyperdocumentation
    ((package (eql #.*package*)) symbol kind)
  (declare (ignore kind))
  (format nil "file://~A#~A"
          #.(namestring
             (merge-pathnames "index.html"
                              (or *load-pathname* *compile-file-pathname*)))
          symbol))
