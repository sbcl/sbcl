;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-LOCKLESS")

(sb-xc:defstruct (list-node
             (:conc-name nil)
             (:constructor %make-sentinel-node ())
             (:copier nil))
  ;; Logically the type of NEXT is _always_ LIST-NODE, but the bits might appear
  ;; as though they represent a fixnum. So FIXNUM has to be in the :TYPE declaration
  ;; or else the compiler will make incorrect inferences particularly with
  ;; regard to MAKE-MAKED-REF which uses %MAKE-LISP-OBJ to remove lowtag bits.
    (%node-next +tail+ :type (or fixnum list-node)))

;;; We look for +TAIL+ in IMMEDIATE-CONSTANT-SC.
#+sb-xc-host
(progn (defstruct list-node)
       (defvar +tail+ (make-list-node)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; This constant is magic and is assigned by genesis
  (setf (info :variable :kind '+tail+) :constant))

;;; Specialized list variants will be created for
;;;  fixnum, integer, real, string, generic "comparable"
;;; but the node type and list type is the same regardless of key type.
(sb-xc:defstruct (linked-list
                   (:constructor %make-lfl
                                 (head inserter deleter finder inequality equality))
                   (:conc-name list-))
  (head       nil :type list-node :read-only t)
  (inserter   nil :type function :read-only t)
  (deleter    nil :type function :read-only t)
  (finder     nil :type function :read-only t)
  (inequality nil :type function :read-only t)
  (equality   nil :type function :read-only t))

;; NODE-HASH is a fixnum. Negatives probably don't do the right thing
(defconstant +hash-nbits+ sb-vm:n-positive-fixnum-bits)
;;; The more bins there are, the more dummy nodes.
;;; Dummy nodes constitute extra space overhead.
(defparameter *desired-elts-per-bin* 4)
(sb-xc:defstruct (split-ordered-list
                   (:include linked-list)
                   (:conc-name so-)
                   (:copier nil)
                   (:constructor %%make-split-ordered-list
                    (head hashfun inserter deleter finder
                     inequality equality valuesp)))
  (count 0 :type sb-ext:word)
  (threshold 0 :type sb-ext:word)
  (hashfun #'error :type (sfunction (t) (and fixnum unsigned-byte)))
  (bins '(#() . 1) :type (cons simple-vector (integer 1 (#.+hash-nbits+))))
  (elts-per-bin *desired-elts-per-bin* :type (integer 1 20))
  ;; If VALUESP is NIL, then this is a set, otherwise it is a map.
  (valuesp nil))

;;; A split-order dummy node has only a hash. Hashes of dummy nodes are unique.
;;; The least-significant-bit in the hash of a dummy node is 0, and in an ordinary
;;; node it is 1, so that there always exists a dummy node whose hash is less
;;; than that of an ordinary node.
(sb-xc:defstruct (so-node (:conc-name nil)
                          (:include list-node)
                          (:copier nil)
                          (:constructor %make-so-dummy-node (node-hash)))
  (node-hash 0 :type fixnum :read-only t))
;;; An ordinary node has a key and hash. Keys are unique but hashes can be nonunique,
;;; though uniqueness will improve efficiency of the algorithms.
;;; The ordering is primarily based on hash, using key as a tiebreaker,
;;; so there is a total ordering even if there are hash collisions.
(sb-xc:defstruct (so-key-node
                   (:conc-name nil)
                   (:include so-node)
                   (:copier nil)
                   (:constructor %make-so-set-node (node-hash so-key)))
  (so-key (missing-arg) :read-only t))
(sb-xc:defstruct (so-data-node
                   (:conc-name nil)
                   (:include so-key-node)
                   (:copier nil)
                   (:constructor %make-so-map-node (node-hash so-key so-data)))
  (so-data nil))

;;; Nodes inserted into the list held in *FINALIZER-STORE* get their %INSTANCE-LAYOUT
;;; changed to this mainly for a GC heap invariant check.
(sb-xc:defstruct (finalizer-node
                   (:conc-name nil)
                   (:include so-data-node)
                   (:copier nil)))
