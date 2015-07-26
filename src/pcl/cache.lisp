;;;; the basics of the PCL wrapper cache mechanism

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

;;;; This software is derived from software originally released by Xerox
;;;; Corporation. Copyright and release statements follow. Later modifications
;;;; to the software are in the public domain and are provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for more
;;;; information.

;;;; copyright information from original PCL sources:
;;;;
;;;; Copyright (c) 1985, 1986, 1987, 1988, 1989, 1990 Xerox Corporation.
;;;; All rights reserved.
;;;;
;;;; Use and copying of this software and preparation of derivative works based
;;;; upon this software are permitted. Any distribution of this software or
;;;; derivative works must comply with all applicable United States export
;;;; control laws.
;;;;
;;;; This software is made available AS IS, and Xerox Corporation makes no
;;;; warranty about the software, its performance or its conformity to any
;;;; specification.

;;;; Note: as of SBCL 1.0.6.3 it is questionable if cache.lisp can
;;;; anymore be considered to be "derived from software originally
;;;; released by Xerox Corporation", as at that time the whole cache
;;;; implementation was essentially redone from scratch.

(in-package "SB-PCL")

;;;; Public API:
;;;;
;;;;   fill-cache
;;;;   probe-cache
;;;;   make-cache
;;;;   map-cache
;;;;   emit-cache-lookup
;;;;   copy-cache
;;;;   hash-table-to-cache
;;;;
;;;; This is a thread and interrupt safe reimplementation loosely
;;;; based on the original PCL cache by Kickzales and Rodrigues,
;;;; as described in "Efficient Method Dispatch in PCL".
;;;;
;;;; * Writes to cache are made atomic using compare-and-swap on
;;;;   wrappers. Wrappers are never moved or deleted after they have
;;;;   been written: to clean them out the cache need to be copied.
;;;;
;;;; * Copying or expanding the cache drops out incomplete and invalid
;;;;   lines.
;;;;
;;;; * Since the cache is used for memoization only we don't need to
;;;;   worry about which of simultaneous replacements (when expanding
;;;;   the cache) takes place: the losing one will have its work
;;;;   redone later. This also allows us to drop entries when the
;;;;   cache is about to grow insanely huge.
;;;;
;;;; The cache is essentially a specialized hash-table for layouts, used
;;;; for memoization of effective methods, slot locations, and constant
;;;; return values.
;;;;
;;;; Subsequences of the cache vector are called cache lines.
;;;;
;;;; The cache vector uses the symbol SB-PCL::..EMPTY.. as a sentinel
;;;; value, to allow storing NILs in the vector as well.

(defstruct (cache (:constructor %make-cache)
                  (:copier %copy-cache))
  ;; Number of keys the cache uses.
  (key-count 1 :type (integer 1 (#.call-arguments-limit)))
  ;; True if we store values in the cache.
  (value)
  ;; Number of vector elements a single cache line uses in the vector.
  ;; This is always a power of two, so that the vector length can be both
  ;; an exact multiple of this and a power of two.
  (line-size 1 :type (integer 1 #.most-positive-fixnum))
  ;; Cache vector, its length is always both a multiple of line-size
  ;; and a power of two. This is so that we can calculate
  ;;   (mod index (length vector))
  ;; using a bitmask.
  (vector #() :type simple-vector)
  ;; The bitmask used to calculate
  ;;   (mod (* line-size line-hash) (length vector))).
  (mask 0 :type fixnum)
  ;; Current probe-depth needed in the cache.
  (depth 0 :type index)
  ;; Maximum allowed probe-depth before the cache needs to expand.
  (limit 0 :type index))

(defun compute-cache-mask (vector-length line-size)
  ;; Since both vector-length and line-size are powers of two, we
  ;; can compute a bitmask such that
  ;;
  ;;  (logand <mask> <combined-layout-hash>)
  ;;
  ;; is "morally equal" to
  ;;
  ;;  (mod (* <line-size> <combined-layout-hash>) <vector-length>)
  ;;
  ;; This is it: (1- vector-length) is #b111... of the approriate size
  ;; to get the MOD, and (- line-size) gives right the number of zero
  ;; bits at the low end.
  (logand (1- vector-length) (- line-size)))

(defun cache-statistics (cache)
  (let* ((vector (cache-vector cache))
         (size (length vector))
         (line-size (cache-line-size cache))
         (total-lines (/ size line-size)))
    (values (loop for i from 0 by line-size below size
                  count (neq (svref vector i) '..empty..))
            total-lines (cache-depth cache) (cache-limit cache))))

;;; Don't allocate insanely huge caches: this is 4096 lines for a
;;; value cache with 8-15 keys -- probably "big enough for anyone",
;;; and 16384 lines for a commonplace 2-key value cache.
(defconstant +cache-vector-max-length+ (expt 2 16))

;;; Compute the maximum allowed probe depth as a function of cache size.
;;; Cache size refers to number of cache lines, not the length of the
;;; cache vector.
;;;
;;; FIXME: It would be nice to take the generic function optimization
;;; policy into account here (speed vs. space.)
(declaim (inline compute-limit))
(defun compute-limit (size)
  (ceiling (sqrt (sqrt size))))

;;; Returns VALUE if it is not ..EMPTY.., otherwise executes ELSE:
(defmacro non-empty-or (value else)
  (with-unique-names (n-value)
    `(let ((,n-value ,value))
       (if (eq ,n-value '..empty..)
           ,else
           ,n-value))))

;;; Fast way to check if a thing found at the position of a cache key is one:
;;; it is always either a wrapper, or the ..EMPTY.. symbol.
(declaim (inline cache-key-p))
(defun cache-key-p (thing)
  (not (symbolp thing)))

;;; Atomically update the current probe depth of a cache.
(defun note-cache-depth (cache depth)
  (loop for old = (cache-depth cache)
        while (and (< old depth)
                   (not (eq old (compare-and-swap (cache-depth cache)
                                                  old depth))))))

;;; Compute the starting index of the next cache line in the cache vector.
(declaim (inline next-cache-index))
(defun next-cache-index (mask index line-size)
  (declare (type (unsigned-byte #.sb-vm:n-word-bits) index line-size mask))
  (logand mask (+ index line-size)))

;;; Returns the hash-value for layout, or executes ELSE if the layout
;;; is invalid.
(defmacro hash-layout-or (layout else)
  (with-unique-names (n-hash)
    `(let ((,n-hash (layout-clos-hash ,layout)))
       (if (zerop ,n-hash)
           ,else
           ,n-hash))))

;;; Compute cache index for the cache and a list of layouts.
(declaim (inline compute-cache-index))
(defun compute-cache-index (cache layouts)
  (let ((index (hash-layout-or (car layouts)
                               (return-from compute-cache-index nil))))
    (declare (fixnum index))
    (dolist (layout (cdr layouts))
      (mixf index (hash-layout-or layout (return-from compute-cache-index nil))))
    ;; align with cache lines
    (logand index (cache-mask cache))))

;;; Emit code that does lookup in cache bound to CACHE-VAR using
;;; layouts bound to LAYOUT-VARS. Go to MISS-TAG on event of a miss or
;;; invalid layout. Otherwise, if VALUE-VAR is non-nil, set it to the
;;; value found. (VALUE-VAR is non-nil only when CACHE-VALUE is true.)
;;;
;;; In other words, produces inlined code for COMPUTE-CACHE-INDEX when
;;; number of keys and presence of values in the cache is known
;;; beforehand.
(defun emit-cache-lookup (cache-var layout-vars miss-tag value-var)
  (with-unique-names (probe n-vector n-depth n-mask
                      MATCH-WRAPPERS EXIT-WITH-HIT)
    (let* ((num-keys (length layout-vars))
           (pointer
            ;; We don't need POINTER if the cache has 1 key and no value,
            ;; or if FOLD-INDEX-ADDRESSING is supported, in which case adding
            ;; a constant to the base index for each cell-ref yields better code.
            #-(or x86 x86-64)
            (when (or (> num-keys 1) value-var) (make-symbol "PTR")))
           (line-size (power-of-two-ceiling (+ num-keys (if value-var 1 0)))))
      `(let ((,n-mask (cache-mask ,cache-var))
             (,probe (hash-layout-or ,(car layout-vars) (go ,miss-tag))))
         (declare (index ,probe))
         ,@(mapcar (lambda (layout-var)
                     `(mixf ,probe (hash-layout-or ,layout-var (go ,miss-tag))))
                   (cdr layout-vars))
         ;; align with cache lines
         (setf ,probe (logand ,probe ,n-mask))
         (let ((,n-depth (cache-depth ,cache-var))
               (,n-vector (cache-vector ,cache-var))
               ,@(when pointer `((,pointer ,probe))))
           (declare (index ,n-depth ,@(when pointer (list pointer))))
           (tagbody
            ,MATCH-WRAPPERS
            (when (and ,@(loop for layout-var in layout-vars
                               for i from 0
                               collect
                             (if pointer
                                 `(prog1 (eq ,layout-var
                                             (svref ,n-vector ,pointer))
                                    (incf ,pointer))
                                 `(eq ,layout-var
                                      (svref ,n-vector
                                             (the index (+ ,probe ,i)))))))
              ,@(when value-var
                 `((setf ,value-var
                         (non-empty-or (svref ,n-vector
                                              ,(or pointer
                                                   `(the index
                                                         (+ ,probe ,num-keys))))
                                       (go ,miss-tag)))))
              (go ,EXIT-WITH-HIT))
            (when (zerop ,n-depth) (go ,miss-tag))
            (decf ,n-depth)
            (setf ,probe (next-cache-index ,n-mask ,probe ,line-size))
            ,@(if pointer `((setf ,pointer ,probe)))
            (go ,MATCH-WRAPPERS)
            ,EXIT-WITH-HIT))))))

;;; Probes CACHE for LAYOUTS.
;;;
;;; Returns two values: a boolean indicating a hit or a miss, and a secondary
;;; value that is the value that was stored in the cache if any.
(defun probe-cache (cache layouts)
  (declare (optimize speed))
  (unless (consp layouts)
    (setf layouts (list layouts)))
  (let ((vector (cache-vector cache))
        (key-count (cache-key-count cache))
        (line-size (cache-line-size cache))
        (mask (cache-mask cache)))
    (flet ((probe-line (base)
             (declare (optimize (sb-c::type-check 0)))
             (tagbody
                (loop for offset of-type index from 0 below key-count
                      for layout in layouts do
                      (unless (eq layout (svref vector (+ base offset)))
                        ;; missed
                        (go :miss)))
                ;; all layouts match!
                (let ((value (when (cache-value cache)
                               (non-empty-or (svref vector (+ base key-count))
                                             (go :miss)))))
                  (return-from probe-cache (values t value)))
              :miss
                (return-from probe-line (next-cache-index mask base line-size)))))
      (declare (ftype (function (index) (values index &optional)) probe-line))
      (let ((index (compute-cache-index cache layouts)))
        (when index
          (loop repeat (1+ (cache-depth cache))
                do (setf index (probe-line index)))))))
  (values nil nil))

;;; Tries to write LAYOUTS and VALUE at the cache line starting at
;;; the index BASE. Returns true on success, and false on failure.
(defun try-update-cache-line (cache base layouts value)
  (declare (index base))
  (let ((vector (cache-vector cache))
        (new (pop layouts)))
    ;; If we unwind from here, we will be left with an incomplete
    ;; cache line, but that is OK: next write using the same layouts
    ;; will fill it, and reads will treat an incomplete line as a
    ;; miss -- causing it to be filled.
    (loop for old = (compare-and-swap (svref vector base) '..empty.. new)  do
          (when (and (cache-key-p old) (not (eq old new)))
            ;; The place was already taken, and doesn't match our key.
            (return-from try-update-cache-line nil))
          (unless layouts
            ;; All keys match or successfully saved, save our value --
            ;; just smash it in. Until the first time it is written
            ;; there is ..EMPTY.. here, which probes look for, so we
            ;; don't get bogus hits. This is necessary because we want
            ;; to be able store arbitrary values here for use with
            ;; constant-value dispatch functions.
            (when (cache-value cache)
              (setf (svref vector (1+ base)) value))
            (return-from try-update-cache-line t))
          (setf new (pop layouts))
          (incf base))))

;;; Tries to write LAYOUTS and VALUE somewhere in the cache. Returns
;;; true on success and false on failure, meaning the cache is too
;;; full.
(defun try-update-cache (cache layouts value)
  (let ((index (or (compute-cache-index cache layouts)
                   ;; At least one of the layouts was invalid: just
                   ;; pretend we updated the cache, and let the next
                   ;; read pick up the mess.
                   (return-from try-update-cache t)))
        (line-size (cache-line-size cache))
        (mask (cache-mask cache)))
    (declare (index index))
    (loop for depth from 0 upto (cache-limit cache) do
          (when (try-update-cache-line cache index layouts value)
            (note-cache-depth cache depth)
            (return-from try-update-cache t))
          (setf index (next-cache-index mask index line-size)))))

;;; Constructs a new cache.
(defun make-cache (&key (key-count (missing-arg)) (value (missing-arg))
                   (size 1))
  (let* ((line-size (power-of-two-ceiling (+ key-count (if value 1 0))))
         (adjusted-size (power-of-two-ceiling size))
         (length (* adjusted-size line-size)))
    (if (<= length +cache-vector-max-length+)
        (%make-cache :key-count key-count
                     :line-size line-size
                     :vector (make-array length :initial-element '..empty..)
                     :value value
                     :mask (compute-cache-mask length line-size)
                     :limit (compute-limit adjusted-size))
        ;; Make a smaller one, then
        (make-cache :key-count key-count :value value :size (ceiling size 2)))))

;;;; Copies and expands the cache, dropping any invalidated or
;;;; incomplete lines.
(defun copy-and-expand-cache (cache layouts value)
  (let ((copy (%copy-cache cache))
        (length (length (cache-vector cache)))
        (drop-random-entries nil))
    (declare (index length))
    (when (< length +cache-vector-max-length+)
      (setf length (* 2 length)))
    (tagbody
     :again
       ;; Blow way the old vector first, so a GC potentially triggered by
       ;; MAKE-ARRAY can collect it.
       (setf (cache-vector copy) #()
             (cache-vector copy) (make-array length :initial-element '..empty..)
             (cache-depth copy) 0
             (cache-mask copy) (compute-cache-mask length (cache-line-size cache))
             (cache-limit copy) (compute-limit (/ length (cache-line-size cache))))
       ;; First insert the new one -- if we don't do this first and
       ;; the cache has reached its maximum size we may end up looping
       ;; in FILL-CACHE.
       (unless (try-update-cache copy layouts value)
         (bug "Could not insert ~S:~S to supposedly empty ~S." layouts value copy))
       (map-cache (if drop-random-entries
                      ;; The cache is at maximum size, and all entries
                      ;; do not fit in. Drop a random ~50% of entries,
                      ;; to make space for new ones. This needs to be
                      ;; random, since otherwise we might get in a
                      ;; rut: add A causing B to drop, then add B
                      ;; causing A to drop... repeat ad nauseam,
                      ;; spending most of the time here instead of
                      ;; doing real work. 50% because if we drop too
                      ;; few we need to do this almost right away
                      ;; again, and if we drop too many, we need to
                      ;; recompute more then we'd like.
                      ;; _Experimentally_ 50% seems to perform the
                      ;; best, but it would be nice to have a proper
                      ;; analysis...
                      (randomly-punting-lambda (layouts value)
                        (try-update-cache copy layouts value))
                      (lambda (layouts value)
                        (unless (try-update-cache copy layouts value)
                          ;; Didn't fit -- expand the cache, or drop
                          ;; a few unlucky ones.
                          (if (< length +cache-vector-max-length+)
                              (setf length (* 2 length))
                              (setf drop-random-entries t))
                          (go :again))))
                  cache))
    copy))

(defun cache-has-invalid-entries-p (cache)
  (let ((vector (cache-vector cache))
        (line-size (cache-line-size cache))
        (key-count (cache-key-count cache))
        (mask (cache-mask cache))
        (index 0))
    (loop
      ;; Check if the line is in use, and check validity of the keys.
      (let ((key1 (svref vector index)))
        (when (cache-key-p key1)
          (if (zerop (layout-clos-hash key1))
              ;; First key invalid.
              (return-from cache-has-invalid-entries-p t)
              ;; Line is in use and the first key is valid: check the rest.
              (loop for offset from 1 below key-count
                    do (let ((thing (svref vector (+ index offset))))
                         (when (or (not (cache-key-p thing))
                                   (zerop (layout-clos-hash thing)))
                           ;; Incomplete line or invalid layout.
                           (return-from cache-has-invalid-entries-p t)))))))
      ;; Line empty of valid, onwards.
      (setf index (next-cache-index mask index line-size))
      (when (zerop index)
        ;; wrapped around
        (return-from cache-has-invalid-entries-p nil)))))

(defun hash-table-to-cache (table &key value key-count)
  (let ((cache (make-cache :key-count key-count :value value
                           :size (hash-table-count table))))
    (maphash (lambda (class value)
               (setq cache (fill-cache cache (class-wrapper class) value)))
             table)
    cache))

;;; Inserts VALUE to CACHE keyd by LAYOUTS. Expands the cache if
;;; necessary, and returns the new cache.
(defun fill-cache (cache layouts value)
  (labels
      ((%fill-cache (cache layouts value expand)
         (cond ((try-update-cache cache layouts value)
                cache)
               ((and (not expand) (cache-has-invalid-entries-p cache))
                ;; Don't expand yet: maybe there will be enough space if
                ;; we just drop the invalid entries.
                (%fill-cache (copy-cache cache) layouts value t))
               (t
                (copy-and-expand-cache cache layouts value)))))
    (%fill-cache cache (ensure-list layouts) value nil)))

;;; Calls FUNCTION with all layouts and values in cache.
(defun map-cache (function cache)
  (let* ((vector (cache-vector cache))
         (key-count (cache-key-count cache))
         (valuep (cache-value cache))
         (line-size (cache-line-size cache))
         (mask (cache-mask cache))
         (fun (if (functionp function)
                  function
                  (fdefinition function)))
         (index 0))
    (tagbody
     :map
       (let ((layouts
              (loop for offset from 0 below key-count
                    collect (non-empty-or (svref vector (+ offset index))
                                          (go :next)))))
         (let ((value (when valuep
                        (non-empty-or (svref vector (+ index key-count))
                                      (go :next)))))
           ;; Let the callee worry about invalid layouts
           (funcall fun layouts value)))
     :next
       (setf index (next-cache-index mask index line-size))
       (unless (zerop index)
         (go :map))))
  cache)

;;; Copying a cache without expanding it is very much like mapping it:
;;; we need to be carefull because there may be updates while we are
;;; copying it, and we don't want to copy incomplete entries or invalid
;;; ones.
(defun copy-cache (cache)
  (let* ((vector (cache-vector cache))
         (copy (make-array (length vector) :initial-element '..empty..))
         (line-size (cache-line-size cache))
         (key-count (cache-key-count cache))
         (valuep (cache-value cache))
         (mask (cache-mask cache))
         (size (/ (length vector) line-size))
         (index 0)
         (depth 0))
    (tagbody
     :copy
       (let ((layouts (loop for offset from 0 below key-count
                            collect (non-empty-or (svref vector (+ index offset))
                                                  (go :next)))))
         ;; Check validity & compute primary index.
         (let ((primary (or (compute-cache-index cache layouts)
                            (go :next))))
           ;; Check & copy value.
           (when valuep
             (setf (svref copy (+ index key-count))
                   (non-empty-or (svref vector (+ index key-count))
                                 (go :next))))
           ;; Copy layouts.
           (loop for offset from 0 below key-count do
                 (setf (svref copy (+ index offset)) (pop layouts)))
           ;; Update probe depth.
           (let ((distance (/ (- index primary) line-size)))
             (setf depth (max depth (if (minusp distance)
                                        ;; account for wrap-around
                                        (+ distance size)
                                        distance))))))
     :next
       (setf index (next-cache-index mask index line-size))
       (unless (zerop index)
         (go :copy)))
    (%make-cache :vector copy
                 :depth depth
                 :key-count (cache-key-count cache)
                 :line-size line-size
                 :value valuep
                 :mask mask
                 :limit (cache-limit cache))))

;;;; For debugging & collecting statistics.

(defun map-all-caches (function)
  (dolist (p (list-all-packages))
    (do-symbols (s p)
      (when (eq p (symbol-package s))
        (dolist (name (list s
                            `(setf ,s)
                            (slot-reader-name s)
                            (slot-writer-name s)
                            (slot-boundp-name s)))
          (when (fboundp name)
            (let ((fun (fdefinition name)))
              (when (typep fun 'generic-function)
                (let ((cache (gf-dfun-cache fun)))
                  (when cache
                    (funcall function name cache)))))))))))

(defun check-cache-consistency (cache)
  (let ((table (make-hash-table :test 'equal)))
    (map-cache (lambda (layouts value)
                 (declare (ignore value))
                 (if (gethash layouts table)
                     (cerror "Check futher."
                             "Multiple appearances of ~S." layouts)
                     (setf (gethash layouts table) t)))
               cache)))
