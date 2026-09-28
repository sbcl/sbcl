;;;; Minimal perfect hash function generator for sets of (UNSIGNED-BYTE 32)

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

;;; This is a port of a reduced-functionality copy of Bob Jenkins' perfect
;;; hash generator (perfect.c and perfhex.c, which he placed in the public
;;; domain; see https://burtleburtle.net/bob/hash/perfect.html), which used
;;; to live in the C runtime. Being written in portable Lisp, it runs in the
;;; cross-compilation host just the same as in the target, so the
;;; cross-compiler doesn't need to record the hash functions it generates.
;;;
;;; It produces exactly the same text as the C code did, so that the comments
;;; tagging each way of computing the hash, which the tests use for coverage,
;;; still appear. Keys are held in the order of the C code's linked list,
;;; which is the reverse of the order of the input.
;;;
;;; The keys are hashed to a pair (A,B) such that the pair is distinct for all
;;; keys, then the final perfect hash is (A XOR SCRAMBLE[TAB[B]]). SCRAMBLE is
;;; a predetermined mapping of 0..255 into 0..SMAX-1, and TAB is filled in so
;;; as to make the hash perfect: first the values of TAB used by more than one
;;; key, trying all possible values for each; then treating each remaining
;;; unmapped key and each unused hash value as the two sides of a bipartite
;;; graph, by finding augmenting paths (Tarjan, "Data Structures and Network
;;; Algorithms"). For small sets of keys, it is often possible to skip all
;;; that and find a short expression computing the hash directly.

(defconstant phash-use-scramble 4096) ; use SCRAMBLE if BLEN >= this
(defconstant phash-scramble-len (ash 1 16))
(defconstant phash-retry-initkey 2048) ; tries at finding distinct (A,B)
(defconstant phash-retry-hex 200) ; tries at making a perfect hash

;;; C's shifts on uint32_t. Shift counts are taken modulo 32 as the hardware
;;; does, which only matters in cases that C leaves undefined.
(defmacro phash<< (x n) `(logand (ash ,x (logand ,n 31)) #xFFFFFFFF))
(defmacro phash>> (x n) `(ash ,x (- (logand ,n 31))))

;;; Format CONTROL, which uses the C directives %d and %x.
(defun phash-sprintf (control &rest args)
  (declare (dynamic-extent args))
  (with-output-to-string (stream)
    (let ((i 0) (n (length control)))
      (loop
        (when (>= i n) (return))
        (let ((char (char control i)))
          (cond ((char= char #\%)
                 (let ((arg (logand (pop args) #xFFFFFFFF)))
                   (ecase (char control (1+ i))
                     (#\d (format stream "~D" (if (logbitp 31 arg) (- arg (ash 1 32)) arg)))
                     (#\x (format stream "~(~X~)" arg))))
                 (incf i 2))
                (t
                 (write-char char stream)
                 (incf i))))))))

;;; Return the ceiling of the log base 2 of VAL.
(defun phash-log2 (val)
  (let ((i 0))
    (loop (when (>= (ash 1 i) val) (return i))
          (incf i))))

;;; Compute p(x), where p is a permutation of 0..(1<<nbits)-1.
;;; permute(0)=0. This is intended and useful.
(defun phash-permute (x nbits)
  (declare (type (unsigned-byte 32) x) (type (integer 0 31) nbits))
  (let ((mask (1- (ash 1 nbits)))
        (const2 (+ 1 (floor nbits 2)))
        (const3 (+ 1 (floor nbits 3)))
        (const4 (+ 1 (floor nbits 4)))
        (const5 (+ 1 (floor nbits 5))))
    (dotimes (i 20 x)
      (setq x (logand (+ x (phash<< x const2)) mask))
      (setq x (logxor x (ash x (- const3))))
      (setq x (logand (+ x (phash<< x const4)) mask))
      (setq x (logxor x (ash x (- const5)))))))

;;; SCRAMBLE depends only on log2(SMAX), so it's computed once for each.
;;; Only SCRAMBLE[0..max(SMAX,256)-1] is ever read: AUGMENT tries values below
;;; SMAX (or below 256 when BLEN >= PHASH-USE-SCRAMBLE, in which case SMAX is
;;; at least that large anyway), and the output only has the first 256.
(defglobal *phash-scramble-tables* nil)
(defun phash-scramble-table (smax)
  (let* ((nbits (phash-log2 smax))
         (tables (or *phash-scramble-tables*
                     (setq *phash-scramble-tables* (make-array 33 :initial-element nil)))))
    (or (svref tables nbits)
        (let* ((n (min (max smax 256) phash-scramble-len))
               (table (make-array n :element-type '(unsigned-byte 32))))
          (dotimes (i n)
            (setf (aref table i) (phash-permute i nbits)))
          ;; A race here is benign: both threads compute the same table.
          (setf (svref tables nbits) table)))))

(declaim (inline phash-distinct3 phash-testfour))
(defun phash-distinct3 (x y z)
  (and (/= x y) (/= x z) (/= y z)))
;;; Check whether A,B,C,D are some permutation of 0,1,2,3.
(defun phash-testfour (a b c d)
  (= (logxor (ash 1 a) (ash 1 b) (ash 1 c) (ash 1 d)) #xf))

;;; Store code into the vector LINES as a PHASH-SPRINTF control string and
;;; its arguments. They're only formatted once a hash is found, since HEXN
;;; rewrites them on every attempt.
(defmacro phash-line (n control &rest args)
  `(setf (svref lines ,n) (list ,control ,@args)))

;;; Find a perfect hash when there are only three keys. Max 6 instructions.
;;; A minimal perfect hash needs to xor one of 0,1,2,3 afterwards to cause
;;; the hole to land on 3. Return the number of LINES used.
(defun phash-hexthree (a b c lowbit highbit minimal lines)
  (declare (type (unsigned-byte 32) a b c lowbit highbit)
           (type simple-vector lines))
  (flet ((try (x y z plain adjusted &rest args)
           (when (phash-distinct3 x y z)
             (if (or (not minimal) (and (/= x 3) (/= y 3) (/= z 3)))
                 (setf (svref lines 0) (cons plain args))
                 (setf (svref lines 0)
                       (list* adjusted (append args (list (logxor x y z 3))))))
             (return-from phash-hexthree 1))))
    ;; one instruction
    (try (logand a 3) (logand b 3) (logand c 3)
         "(& val 3) ;h3a" "(^ (& val 3) %d) ;h3b")
    (try (ash a -30) (ash b -30) (ash c -30)
         "(>> val %d) ;h3c" "(^ (>> val %d) %d) ;h3d" 30)
    ;; two instructions
    (loop for i from 0 below highbit
          do (try (logand (ash a (- i)) 3) (logand (ash b (- i)) 3)
                  (logand (ash c (- i)) 3)
                  "(& (>> val %d) 3) ;h3e" "(^ (& (>> val %d) 3) %d) ;h3f" i))
    ;; three instructions
    (loop for i from 0 to highbit
          do (try (logand (+ a (phash>> a i)) 3) (logand (+ b (phash>> b i)) 3)
                  (logand (+ c (phash>> c i)) 3)
                  "(& (+ val (>> val %d)) 3) ;h3g"
                  "(^ (& (+ val (>> val %d)) 3) %d) ;h3h" i))
    ;; Four instructions: this always works. If the three values
    ;; are distinct, there are two bits which distinguish them.
    (loop for i from lowbit to highbit
          do (loop for j from i to highbit
                   do (flet ((f (x) (logand (logxor (phash>> x i) (phash>> x j)) 3)))
                        (try (f a) (f b) (f c)
                             "(& (^ (>> val %d) (>> val %d)) 3) ;h3i"
                             "(^ (& (^ (>> val %d) (>> val %d)) 3) %d) ;h3j"
                             i j))))
    (error "bug in hexthree")))

;;; Find a perfect hash when there are only four keys. Max 10 instructions.
;;; It is automatically minimal. Return the number of LINES used.
(defun phash-hexfour (a b c d lowbit highbit diffbits lines)
  (declare (type (unsigned-byte 32) a b c d lowbit highbit diffbits)
           (type simple-vector lines))
  (let ((used 1))
    (macrolet ((testfour-each ((var) expr)
                 ;; EXPR computes the hash of VAR for each key
                 `(phash-testfour ,@(mapcar (lambda (key) `(let ((,var ,key)) ,expr))
                                            '(a b c d))))
               (try (expr control &rest args)
                 `(when (testfour-each (v) ,expr)
                    (phash-line 0 ,control ,@args)
                    (return-from phash-hexfour used))))
      ;; one instruction
      (when (= (logand diffbits 3) 3)
        (try (logand v 3) "(& val 3) ;h4a"))
      (when (= (logand (ash diffbits -30) 3) 3)
        (try (ash v -30) "(>> val %d) ;h4b" 30))
      ;; two instructions
      (loop for i from lowbit below highbit
            when (= (logand (phash>> diffbits i) 3) 3)
            do (try (logand (phash>> v i) 3) "(& (>> val %d) 3) ;h4c" i))
      ;; three instructions (linear with the number of diffbits)
      (when (/= (logand diffbits 3) 0)
        (loop for i from lowbit to highbit
              when (/= (logand (phash>> diffbits i) 3) 0)
              do (try (logand (+ v (phash>> v i)) 3)
                      "(& (+ val (>> val %d)) 3) ;h4d" i)
                 (try (logand (- v (phash>> v i)) 3)
                      "(& (- val (>> val %d)) 3) ;h4e" i)
                 ;; h4f: ((val>>k)-val)&3: redundant with h4e
                 (try (logand (logxor v (phash>> v i)) 3)
                      "(& (^ val (>> val %d)) 3) ;h4g" i)))
      ;; four instructions (linear with the number of diffbits)
      (when (/= (logand diffbits 3) 0)
        (loop for i from lowbit to highbit
              do (when (and (logtest (phash>> diffbits i) 1)
                            (logtest diffbits 2))
                   (try (logxor (logand v 3) (logand (phash>> v i) 1))
                        "(^ (& val 3) (& (>> val %d) 1)) ;h4h" i)
                   (try (logxor (logand v 2) (logand (phash>> v i) 1))
                        "(^ (& val 2) (& (>> val %d) 1)) ;h4i" i))
                 (when (and (logtest (phash>> diffbits i) 2)
                            (logtest diffbits 1))
                   (try (logxor (logand v 3) (logand (phash>> v i) 2))
                        "(^ (& val 3) (& (>> val %d) 2)) ;h4j" i)
                   (try (logxor (logand v 1) (logand (phash>> v i) 2))
                        "(^ (& val 1) (& (>> val %d) 2)) ;h4k" i))))
      ;; four instructions (quadratic in the number of diffbits).
      ;; The C code this was ported from uses A's bits for the second
      ;; term of every key, so that's done here too.
      (loop for i from lowbit to highbit
            when (= (logand (phash>> diffbits i) 1) 1)
            do (loop for j from lowbit to highbit
                     when (/= (logand (phash>> diffbits j) 3) 0)
                     do (let ((aj (phash>> a j)))
                          (try (logand (+ (phash>> v i) aj) 3)
                               "(& (+ (>> val %d) (>> val %d)) 3) ;h4l" i j)
                          (try (logand (- (phash>> v i) aj) 3)
                               "(& (- (>> val %d) (>> val %d)) 3) ;h4m" i j)
                          (try (logand (logxor (phash>> v i) aj) 3)
                               "(& (^ (>> val %d) (>> val %d)) 3) ;h4n" i j))))
      ;; five instructions (quadratic in the number of diffbits)
      (loop for i from lowbit to highbit
            when (logtest (phash>> diffbits i) 1)
            do (loop for j from lowbit to highbit
                     do (when (/= (logand (phash>> diffbits j) 3) 0)
                          (try (logxor (logand (phash>> v j) 3) (logand (phash>> v i) 1))
                               "(^ (& (>> val %d) 3) (& (>> val %d) 1)) ;h4o" j i)
                          (try (logxor (logand (phash>> v j) 2) (logand (phash>> v i) 1))
                               "(^ (& (>> val %d) 2) (& (>> val %d) 1)) ;h4p" j i))
                        (cond ((= i 0)
                               (try (logand (logxor (phash>> v j) (phash<< v 1)) 3)
                                    "(& (^ (>> val %d) (<< val 1)) 3) ;h4q" j))
                              ((= i 1)
                               (try (logxor (logand (phash>> v j) 3) (logand v 2))
                                    "(^ (& (>> val %d) 3) (& val 2)) ;h4r" j))
                              (t
                               (try (logxor (logand (phash>> v j) 3)
                                            (logand (phash>> v (1- i)) 2))
                                    "(^ (& (>> val %d) 3) (& (>> val %d) 2)) ;h4s"
                                    j (1- i))))
                        (try (logxor (logand (phash>> v j) 1) (logand (phash>> v i) 2))
                             "(^ (& (>> val %d) 1) (& (>> val %d) 2)) ;h4t" j i)))
      ;; OK, bring out the big guns. There exist three bits i,j,k
      ;; which distinguish a,b,c,d. i^(j<<1)^(k*q) is guaranteed to
      ;; work for some q in {0,1,2,3}, proven by exhaustive search
      ;; of all (8 choose 4) cases. Find three such bits and try the
      ;; 4 cases. Some cases below may duplicate some cases above, so
      ;; that what is below is guaranteed to work no matter what was
      ;; attempted above. The generated hash is at most 10 instructions.
      (flet ((bitof (x n) (logand (phash>> x n) 1)))
        (let* ((i (loop for i from lowbit below 32
                        when (/= (bitof c i) (bitof d i)) return i
                        finally (return 32)))
               (j (flet ((f (x j) (logxor (bitof x i) (ash (bitof x j) 1))))
                    (loop for j from lowbit below 32
                          when (phash-distinct3 (f b j) (f c j) (f d j)) return j
                          finally (return 32))))
               (k (flet ((f (x k)
                           (logxor (bitof x i) (ash (bitof x j) 1) (ash (bitof x k) 2))))
                    (loop for k from lowbit below 32
                          when (let ((w (f a k)) (x (f b k)) (y (f c k)) (z (f d k)))
                                 (and (phash-distinct3 x y z)
                                      (/= w x) (/= w y) (/= w z)))
                          return k
                          finally (return 32))))
               m n o)
          (when (or (= i 32) (= j 32) (= k 32))
            (error "bug in hexfour: i ~D j ~D k ~D" i j k))
          ;; if any bit has two 1s and two 0s, make that bit o
          (cond ((/= (+ (bitof a i) (bitof b i) (bitof c i) (bitof d i)) 2)
                 (setq m j n k o i))
                ((/= (+ (bitof a j) (bitof b j) (bitof c j) (bitof d j)) 2)
                 (setq m i n k o j))
                (t
                 (setq m i n j o k)))
          (when (> m n) (rotatef m n)) ; guarantee m < n
          ;; seven instructions, multiply bit o by 1
          (when (testfour-each (v)
                  (logxor (logand (logxor (phash>> v m) (phash>> v o)) 1)
                          (logand (phash>> v (1- n)) 2)))
            (when (> m o) (rotatef m o)) ; make sure m < o and m < n
            (if (= m 0)
                (phash-line 0 "(^ (& (^ val (>> val %d)) 1) (& (>> val %d) 2)) ;1"
                      o (1- n))
                (phash-line 0 "(^ (& (^ (>> val %d) (>> val %d)) 1) (& (>> val %d) 2)) ;2"
                      m o (1- n)))
            (return-from phash-hexfour used))
          ;; six to seven instructions, multiply bit o by 2
          (when (testfour-each (v)
                  (logxor (logand (phash>> v m) 1)
                          (ash (logand (logxor (phash>> v n) (phash>> v o)) 1) 1)))
            (when (= m (1- o)) (rotatef n o)) ; make m==n-1 if possible
            (cond ((= m 0)
                   (phash-line 0 "(^ (& val 1) (& (^ (>> val %d) (>> val %d)) 2)) ;3"
                         (1- n) (1- o)))
                  ((= o 0)
                   (phash-line 0 "(^ (& (>> val %d) 2) (& (^ (>> val %d) val) 1)) ;4"
                         (1- m) n))
                  (t
                   (phash-line 0 "(^ (& (>> val %d) 1) (& (^ (>> val %d) (>> val %d)) 2)) ;5"
                         m (1- n) (1- o))))
            (return-from phash-hexfour used))
          ;; multiplying by 3 is a pain: seven or eight instructions
          (when (testfour-each (v)
                  (logxor (logand (phash>> v m) 1) (logand (phash>> v (1- n)) 2)
                          (logand (phash>> v o) 1) (ash (logand (phash>> v o) 1) 1)))
            (setq used 2)
            (phash-line 0 "(let ((b (& (>> val %d) 1))) ;6" o)
            (cond ((and (= m (1- o)) (= m 0))
                   (phash-line 1 "(^ (& val 3) (& (>> val %d) 2) b) ;7" (1- n)))
                  ((= m (1- o))
                   (phash-line 1 "(^ (& (>> val %d) 3) (& (>> val %d) 2) b) ;8" m (1- n)))
                  ((and (= m (1- n)) (= m 0))
                   (phash-line 1 "(^ (& val 3) b (<< b 1)) ;9"))
                  ((= m (1- n))
                   (phash-line 1 "(^ (& (>> val %d) 3) b (<< b 1)) ;10" m))
                  ((and (= o (1- n)) (= m 0))
                   (phash-line 1 "(^ (& val 1) (& (>> val %d) 3) (<< b 1)) ;11" o))
                  ((= o (1- n))
                   (phash-line 1 "(^ (& (>> val %d) 1) (& (>> val %d) 3) (<< b 1)) ;12" m o))
                  ((and (/= m (1- o)) (/= m (1- n)) (/= o (1- m)) (/= o (1- n)))
                   (setq used 3)
                   (phash-line 0 "(let ((newval (& val #x%x)))"
                         (logxor (ash 1 m) (ash 1 n) (ash 1 o)))
                   (if (= o 0)
                       (phash-line 1 "(let ((b (u32- newval))) ;13")
                       (phash-line 1 "(let ((b (u32- (>> newval %d)))) ;14" o))
                   (if (= m 0)
                       (phash-line 2 "(& (^ newval (>> newval %d) b) 3) ;15" (1- n))
                       (phash-line 2 "(& (^ (>> newval %d) (>> newval %d) b) 3) ;16"
                             m (1- n))))
                  ((= o (1- m))
                   (cond ((= o 0) (phash-line 0 "(let ((b (& (<< val 1) 2))) ;17"))
                         ((= o 1) (phash-line 0 "(let ((b (& val 2))) ;18"))
                         (t (phash-line 0 "(let ((b (& (>> val %d) 2))) ;19" (1- o))))
                   (if (= o 0)
                       (phash-line 1 "(^ (& val 3) (& (>> val %d) 1) b) ;20" n)
                       (phash-line 1 "(^ (& (>> val %d) 3) (& (>> val %d) 1) b) ;21" o n)))
                  (t
                   (phash-line 1 "(^ (& (>> val %d) 1) (& (>> val %d) 2) b (<< b 1)); h4ax"
                         m (1- n))))
            (return-from phash-hexfour used))
          ;; five instructions, multiply bit o by 0, covered before the big guns
          (when (testfour-each (v)
                  (logxor (logand (phash>> v m) 1) (logand (phash>> v (1- n)) 2)))
            (phash-line 0 "(^ (& (>> val %d) 1) (& (>> val %d) 2)) ;h4v" m (1- n))
            (return-from phash-hexfour used))))
      (error "bug in hexfour"))))

;;; Try to find a perfect hash when there are five to eight keys. We can't
;;; deterministically find a perfect hash, but there's a reasonable chance
;;; we'll get lucky. Return the number of LINES used, or NIL.
(defun phash-hexeight (hash key-a lowbit highbit minimal lines)
  (declare (type (simple-array (unsigned-byte 32) (*)) hash key-a)
           (type (unsigned-byte 32) lowbit highbit)
           (type simple-vector lines))
  (let* ((nkeys (length hash))
         ;; hash values which should never be used
         (badmask (if minimal (logxor (1- (ash 1 8)) (1- (ash 1 nkeys))) 0)))
    ;; Test if KEY-A is distinct and in range for all keys.
    (flet ((testeight ()
             (let ((mask badmask))
               (dotimes (k nkeys t)
                 (let ((bit (ash 1 (aref key-a k))))
                   (when (logtest mask bit) (return nil))
                   (setq mask (logior mask bit)))))))
      (macrolet ((try (expr control &rest args)
                   (let ((key (gensym "KEY")))
                     `(progn
                        (dotimes (,key nkeys)
                          (setf (aref key-a ,key) (let ((v (aref hash ,key))) ,expr)))
                        (when (testeight)
                          (phash-line 0 ,control ,@args)
                          (return-from phash-hexeight 1))))))
        ;; one instruction
        (try (logand v 7) "(& val 7)")
        ;; two instructions
        (loop for i from lowbit to (- highbit 2)
              do (try (logand (phash>> v i) 7) "(& (>> val %d) 7)" i))
        ;; four instructions
        (loop for i from lowbit to highbit
              do (loop for j from (1+ i) to highbit
                       do (if (= i 0)
                              (try (logand (+ (phash>> v i) (phash>> v j)) 7)
                                   "(& (+ val (>> val %d)) 7)" j)
                              (try (logand (+ (phash>> v i) (phash>> v j)) 7)
                                   "(& (+ (>> val %d) (>> val %d)) 7)" i j))
                          (if (= i 0)
                              (try (logand (logxor (phash>> v i) (phash>> v j)) 7)
                                   "(& (^ val (>> val %d)) 7)" j)
                              (try (logand (logxor (phash>> v i) (phash>> v j)) 7)
                                   "(& (^ (>> val %d) (>> val %d)) 7)" i j))
                          (if (= i 0)
                              (try (logand (- (phash>> v i) (phash>> v j)) 7)
                                   "(& (- val (>> val %d)) 7)" j)
                              (try (logand (- (phash>> v i) (phash>> v j)) 7)
                                   "(& (- (>> val %d) (>> val %d)) 7)" i j))))
        ;; six instructions
        (loop for i from lowbit to highbit
              do (loop for j from (1+ i) to highbit
                       do (loop for k from (1+ j) to highbit
                                do (try (logand (+ (phash>> v i) (phash>> v j)
                                                   (phash>> v k))
                                                7)
                                        "(& (+ (>> val %d) (>> val %d) (>> val %d)) 7)"
                                        i j k))))
        nil))))

;;; Try to find a perfect hash function for INPUT, a vector of distinct keys.
;;; If MINIMAL, the hash ranges over 0..N-1 for N keys, otherwise over the
;;; power-of-2-ceiling of N. FAST should make the generator try less hard.
;;; Return a string, which when read is a list of forms computing the hash of
;;; VAL, or NIL if no hash could be found. Unless COMMENTS, strip comments.
(defun generate-perfect-hash-sexpr (input &optional (minimal t) (fast nil) (comments t))
  (declare (type (simple-array (unsigned-byte 32) (*)) input))
  (let* ((nkeys (length input))
         (hash (let ((v (make-array nkeys :element-type '(unsigned-byte 32))))
                 (dotimes (i nkeys v)
                   (setf (aref v i) (aref input (- nkeys 1 i))))))
         (key-a (make-array nkeys :element-type '(unsigned-byte 32) :initial-element 0))
         (key-b (make-array nkeys :element-type '(unsigned-byte 32) :initial-element 0))
         (key-nextb (make-array nkeys :element-type '(signed-byte 32) :initial-element -1))
         ;; the generated code for the final hash, assuming the initial hash
         ;; is done: see PHASH-LINE. NIL for an empty line.
         (lines (make-array 10 :initial-element nil))
         (used 0)
         (lowbit 0)   ; lowest bit where any key differs
         (highbit 0)  ; highest bit where any key differs
         (diffbits 0) ; bits which differ for some key
         ;; state machine used by HEXN
         (state 0) (state-j 0) (state-k 0)
         ;; the table indexed by B
         (tabb-val (make-array 0 :element-type '(unsigned-byte 16)))
         (tabb-list (make-array 0 :element-type '(signed-byte 32)))
         (tabb-listlen (make-array 0 :element-type '(unsigned-byte 32)))
         (tabb-water (make-array 0 :element-type '(unsigned-byte 32)))
         ;; the key having each hash value, indexed by hash
         (tabh (make-array 0 :element-type '(signed-byte 32)))
         ;; the queue of Bs used by AUGMENT, which is the spanning tree
         (tabq-b (make-array 0 :element-type '(signed-byte 32)))
         (tabq-parent (make-array 0 :element-type '(unsigned-byte 32)))
         (tabq-newval (make-array 0 :element-type '(unsigned-byte 16)))
         (tabq-oldval (make-array 0 :element-type '(unsigned-byte 16))))
    (declare (type (simple-array (unsigned-byte 32) (*)) hash key-a key-b
                   tabb-listlen tabb-water tabq-parent)
             (type (simple-array (signed-byte 32) (*)) key-nextb tabb-list tabh tabq-b)
             (type (simple-array (unsigned-byte 16) (*)) tabb-val tabq-newval tabq-oldval)
             (type simple-vector lines)
             (type (unsigned-byte 32) used lowbit highbit diffbits state state-j state-k))
    (macrolet ((do-keys ((var) &body body)
                 `(dotimes (,var nkeys) ,@body)))
      (labels ((setlow ()
                 (let ((first (aref hash 0)))
                   (setq diffbits 0)
                   (do-keys (k)
                     (setq diffbits (logior diffbits (logxor first (aref hash k))))))
                 (setq lowbit (let ((i 0))
                                (loop (when (or (= i 32) (logbitp i diffbits)) (return i))
                                      (incf i))))
                 (setq highbit (let ((i 31))
                                 (loop (when (or (= i 0) (logbitp i diffbits)) (return i))
                                       (decf i)))))
               ;; Guns aren't enough. Bring out the Bomb. Use TAB.
               ;; This finds the initial (A,B) when we need to use TAB, producing
               ;; a different (A,B) every time it's called, trying all reasonable
               ;; cases, fastest first. The initial mix can be filled into LINES
               ;; starting at 1. The final hash (at line 7) is (A ^ TAB[B]) or
               ;; (A ^ SCRAMBLE[TAB[B]]).
               (hexn (salt alen blen)
                 (declare (type (unsigned-byte 32) salt alen blen))
                 (let ((alog (phash-log2 alen))
                       (blog (phash-log2 blen)))
                   (loop
                     (ecase state
                       (1 ; a = val>>30; b=val&3
                        (do-keys (k)
                          (let ((v (aref hash k)))
                            (setf (aref key-a k) (phash>> (phash<< v (- 32 (1+ highbit))) (- 32 alog))
                                  (aref key-b k) (logand (phash>> v lowbit) (1- blen)))))
                        (if (= lowbit 0)
                            (phash-line 5 "(let ((b (& val #x%x)))" (1- blen))
                            (phash-line 5 "(let ((b (& (>> val %d) #x%x)))" lowbit (1- blen)))
                        (if (= (1+ highbit) 32)
                            (phash-line 6 "(let ((a (>> val %d)))" (- 32 alog))
                            (phash-line 6 "(let ((a (>> (<< val %d) %d)))" (- 32 (1+ highbit)) (- 32 alog)))
                        (incf state)
                        (return))
                       (2 ; a = val&3; b=val>>30
                        (do-keys (k)
                          (let ((v (aref hash k)))
                            (setf (aref key-a k) (logand (phash>> v lowbit) (1- alen))
                                  (aref key-b k) (phash>> (phash<< v (- 32 (1+ highbit))) (- 32 blog)))))
                        (if (= (1+ highbit) 32)
                            (phash-line 5 "(let ((b (>> val %d)))" (- 32 blog))
                            (phash-line 5 "(let ((b (>> (<< val %d) %d)))" (- 32 (1+ highbit)) (- 32 blog)))
                        (if (= lowbit 0)
                            (phash-line 6 "(let ((a (& val #x%x)))" (1- alen))
                            (phash-line 6 "(let ((a (& (>> val %d) #x%x)))" lowbit (1- alen)))
                        (incf state)
                        (return))
                       ;; states 3,4,5:
                       ;;   for (k=lowbit; k<=highbit; ++k)
                       ;;     for (j=lowbit; j<=highbit; ++j)
                       ;;       b = (val>>j)&3;
                       ;;       a = (val<<k)>>30;
                       (3
                        (setq state-k lowbit state-j lowbit)
                        (incf state))
                       (4
                        (cond ((not (< state-j highbit))
                               (incf state))
                              (t
                               (do-keys (k)
                                 (let ((v (aref hash k)))
                                   (setf (aref key-b k) (logand (phash>> v state-j) (1- blen))
                                         (aref key-a k) (phash>> (phash<< v (- 32 state-k 1))
                                                                 (- 32 alog)))))
                               (cond ((= state-j 0)
                                      (phash-line 5 "(let ((b (& val #x%x)))" (1- blen)))
                                     ((= (+ blog state-j) 32)
                                      (phash-line 5 "(let ((b  (>> val %d)))" state-j))
                                     (t
                                      (phash-line 5 "(let ((b (& (>> val %d) #x%x)))" state-j (1- blen))))
                               (if (= (- 32 state-k 1) 0)
                                   (phash-line 6 "(let ((a (>> val %d)))" (- 32 alog))
                                   (phash-line 6 "(let ((a (>> (<< val %d) %d)))"
                                         (- 32 state-k 1) (- 32 alog)))
                               (loop (unless (< (incf state-j) highbit) (return))
                                     (when (> (logand (phash>> diffbits state-j) (1- blen)) 2)
                                       (return)))
                               (return))))
                       (5
                        (loop (unless (< (incf state-k) highbit) (return))
                              (when (> (logand (phash>> (phash<< diffbits (- 32 state-k 1)) alog)
                                               (1- alen))
                                       0)
                                (return)))
                        (cond ((not (< state-k highbit))
                               (incf state))
                              (t
                               (setq state-j lowbit state 4))))
                       ;; states 6,7,8:
                       ;;   for (k=0; k<UB4BITS-alog; ++k)
                       ;;     for (j=0; j<UB4BITS-blog; ++j)
                       ;;       val = val+f(salt);
                       ;;       val ^= (val >> 16);
                       ;;       val += (val << 8);
                       ;;       val ^= (val >> 4);
                       ;;       b = (val >> j) & 3;
                       ;;       a = (val + (val << k)) >> 30;
                       (6
                        (setq state-k 0 state-j 0)
                        (incf state))
                       (7 ; Just do something that will surely work
                        (cond
                          ((not (<= state-j (- 32 blog)))
                           (incf state))
                          (t
                           (let ((addk (logand (* #x9e3779b9 salt) #xFFFFFFFF))
                                 (span (- (1+ highbit) lowbit)))
                             (do-keys (k)
                               (let ((val (logand (+ (aref hash k) addk) #xFFFFFFFF)))
                                 (when (> span 16)
                                   (setq val (logxor val (ash val -16))))
                                 (when (> span 8)
                                   (setq val (logand (+ val (phash<< val 8)) #xFFFFFFFF)))
                                 (setq val (logxor val (ash val -4)))
                                 (setf (aref key-b k) (logand (phash>> val state-j) (1- blen))
                                       (aref key-a k)
                                       (if (= state-k 0)
                                           (phash>> val (- 32 alog))
                                           (phash>> (logand (+ val (phash<< val state-k)) #xFFFFFFFF)
                                                    (- 32 alog))))))
                             (phash-line 1 "(+= val #x%x)" addk)
                             (when (> span 16)
                               (phash-line 2 "(^= val (>> val 16))"))
                             (when (> span 8)
                               (phash-line 3 "(+= val (<< val 8))"))
                             (phash-line 4 "(^= val (>> val 4))")
                             (if (= state-j 0)
                                 (phash-line 5 "(let ((b (& val #x%x)))" (1- blen))
                                 (phash-line 5 "(let ((b (& (>> val %d) #x%x)))" state-j (1- blen)))
                             (if (= state-k 0)
                                 (phash-line 6 "(let ((a (>> val %d)))" (- 32 alog))
                                 (phash-line 6 "(let ((a (>> (u32+ val (<< val %d)) %d)))"
                                       state-k (- 32 alog)))
                             (incf state-j)
                             (return)))))
                       (8
                        (incf state-k)
                        (if (not (<= state-k (- 32 alog)))
                            (incf state)
                            (setq state-j 0 state 7)))
                       (9
                        (setq state 6))))))
               ;; Initialize (A,B) when keys are integers. Return T if we found
               ;; a perfect hash and no more work is needed.
               (inithex (alen blen salt)
                 (when (< nkeys 3)
                   (error "Can't generate a perfect hash of fewer than 3 keys"))
                 (setlow)
                 (case nkeys
                   (3 (setq used (phash-hexthree (aref hash 0) (aref hash 1) (aref hash 2)
                                                 lowbit highbit minimal lines))
                      t)
                   (4 (setq used (phash-hexfour (aref hash 0) (aref hash 1) (aref hash 2)
                                                (aref hash 3) lowbit highbit diffbits lines))
                      t)
                   (t
                    (when (and (<= nkeys 8) (= salt 1)) ; first time through
                      (let ((n (phash-hexeight hash key-a lowbit highbit minimal lines)))
                        (when n ; got lucky, don't need TAB
                          (setq used n)
                          (return-from inithex t))))
                    (when (= salt 1)
                      (setq used 8 state 1 state-j 0 state-k 0)
                      (fill lines nil :end 8))
                    (if (< blen phash-use-scramble)
                        (phash-line 7 "(^ a (aref tab b))")
                        (phash-line 7 "(^ a (aref scramble (aref tab b)))"))
                    (hexn salt alen blen)
                    nil)))
               ;; Put keys in TABB according to KEY-B, and check if the initial
               ;; hash might work. COMPLETE means to finish despite collisions.
               (inittab (blen complete)
                 (let ((nocollision t))
                   (fill tabb-val 0 :end blen)
                   (fill tabb-list -1 :end blen)
                   (fill tabb-listlen 0 :end blen)
                   (fill tabb-water 0 :end blen)
                   ;; Two keys with the same (a,b) guarantees a collision
                   (do-keys (k)
                     (let ((b (aref key-b k)))
                       (do ((other (aref tabb-list b) (aref key-nextb other)))
                           ((< other 0))
                         (when (= (aref key-a k) (aref key-a other))
                           (setq nocollision nil)
                           (when (= (aref hash k) (aref hash other))
                             (error "Duplicate perfect hash key ~X" (aref hash k)))
                           (unless complete
                             (return-from inittab nil))))
                       (incf (aref tabb-listlen b))
                       (setf (aref key-nextb k) (aref tabb-list b)
                             (aref tabb-list b) k)))
                   nocollision))
               ;; Run a hash function on the key to get A and B. Return
               ;;   0: didn't find distinct (a,b) for all keys
               ;;   1: found distinct (a,b) for all keys, put keys in TABB
               ;;   2: found a perfect hash, no need to do any more work
               (initkey (alen blen salt)
                 (cond ((inithex alen blen salt) 2)
                       ((inittab blen nil) 1)
                       (t 0)))
               (allocate-tabb (blen)
                 (setq tabb-val (make-array blen :element-type '(unsigned-byte 16))
                       tabb-list (make-array blen :element-type '(signed-byte 32))
                       tabb-listlen (make-array blen :element-type '(unsigned-byte 32))
                       tabb-water (make-array blen :element-type '(unsigned-byte 32))
                       tabq-b (make-array (1+ blen) :element-type '(signed-byte 32))
                       tabq-parent (make-array (1+ blen) :element-type '(unsigned-byte 32))
                       tabq-newval (make-array (1+ blen) :element-type '(unsigned-byte 16))
                       tabq-oldval (make-array (1+ blen) :element-type '(unsigned-byte 16))))
               ;; Find a mapping that makes this a perfect hash.
               (perfect (blen smax scramble)
                 (declare (type (simple-array (unsigned-byte 32) (*)) scramble))
                 (let ((hsize (length tabh))
                       (maxkeys (loop for i below blen maximize (aref tabb-listlen i))))
                   ;; clear any state from previous attempts
                   (fill tabh -1)
                   (fill tabq-b -1)
                   (fill tabq-parent 0)
                   (fill tabq-newval 0)
                   (fill tabq-oldval 0)
                   (labels
                       ;; Try to apply an augmenting path. With ROLLBACK, undo it.
                       ((apply-path (tail rollback)
                          (let ((child (1- tail)) parent)
                            (loop
                              (when (= child 0) (return t))
                              (setq parent (aref tabq-parent child))
                              (let ((pb (aref tabq-b parent)))
                                ;; erase old hash values
                                (let ((stabb (aref scramble (aref tabb-val pb))))
                                  (do ((key (aref tabb-list pb) (aref key-nextb key)))
                                      ((< key 0))
                                    (let ((hash (logxor (aref key-a key) stabb)))
                                      ;; The C code could read past the end of TABH
                                      ;; here, but not find the key there.
                                      (when (and (< hash hsize) (= key (aref tabh hash)))
                                        (setf (aref tabh hash) -1)))))
                                ;; change the value, which changes the hashes
                                ;; of all of the parent's siblings
                                (setf (aref tabb-val pb) (if rollback
                                                             (aref tabq-oldval child)
                                                             (aref tabq-newval child)))
                                ;; set new hash values
                                (let ((stabb (aref scramble (aref tabb-val pb))))
                                  (do ((key (aref tabb-list pb) (aref key-nextb key)))
                                      ((< key 0))
                                    (let ((hash (logxor (aref key-a key) stabb)))
                                      (cond (rollback
                                             (unless (= parent 0) ; root never had a hash
                                               (setf (aref tabh hash) key)))
                                            ((>= (aref tabh hash) 0)
                                             ;; very rare: roll back any changes
                                             (apply-path tail t)
                                             (return-from apply-path nil))
                                            (t
                                             (setf (aref tabh hash) key)))))))
                              (setq child parent))))
                        ;; Add ITEM to the mapping. Construct a spanning tree of
                        ;; Bs with ITEM as root, where each parent can have all
                        ;; its hashes changed (by some new val) with at most one
                        ;; collision, and each child is the B of that collision.
                        ;; The path from ITEM to a B that can be remapped with no
                        ;; collision is an augmenting path.
                        (augment (item highwater)
                          (let ((limit (if (< blen phash-use-scramble) smax 256))
                                (highhash (if minimal nkeys smax))
                                (trans (or (not fast) minimal))
                                (tail 1))
                            (setf (aref tabq-b 0) item)
                            (do ((q 0 (1+ q)))
                                ((>= q tail) nil)
                              (when (and (not trans) (= q 1))
                                (return nil)) ; don't do transitive closure
                              (let ((myb (aref tabq-b q)))
                                (dotimes (i limit)
                                  (let ((childb -1)
                                        (stabb (aref scramble i)))
                                    (when
                                        (do ((key (aref tabb-list myb) (aref key-nextb key)))
                                            ((< key 0) t)
                                          (let ((hash (logxor (aref key-a key) stabb)))
                                            (when (>= hash highhash) ; out of bounds
                                              (return nil))
                                            (let ((childkey (aref tabh hash)))
                                              (when (>= childkey 0)
                                                (let ((hitb (aref key-b childkey)))
                                                  (cond ((>= childb 0)
                                                         ;; hit at most one child b
                                                         (when (/= childb hitb) (return nil)))
                                                        (t
                                                         (setq childb hitb)
                                                         ;; already explored
                                                         (when (= (aref tabb-water childb) highwater)
                                                           (return nil)))))))))
                                      ;; add CHILDB to the queue of reachable things
                                      (when (>= childb 0)
                                        (setf (aref tabb-water childb) highwater))
                                      (setf (aref tabq-b tail) childb
                                            (aref tabq-newval tail) (logand i #xFFFF)
                                            (aref tabq-oldval tail) (aref tabb-val myb)
                                            (aref tabq-parent tail) q)
                                      (incf tail)
                                      (when (< childb 0)
                                        ;; found an I with no collisions?
                                        ;; try to apply the augmenting path
                                        (when (apply-path tail nil)
                                          (return-from augment t))
                                        (decf tail))))))))))
                     ;; In descending order by number of keys, map all Bs
                     (loop for j from maxkeys downto 1
                           do (dotimes (i blen)
                                (when (= (aref tabb-listlen i) j)
                                  (unless (augment i (1+ i))
                                    (return-from perfect nil)))))
                     t)))
               ;; Guess initial values for ALEN and BLEN, possibly altering SMAX.
               (initalen (smax)
                 (let (alen blen)
                   (cond
                     ((not minimal)
                      (when (and fast (> (* 5 nkeys) (* 4 smax)))
                        (setq smax (* smax 2)))
                      ;; The C code this was ported from used the uninitialized
                      ;; blen to compute alen when smax > 131072.
                      (setq alen smax)
                      (setq blen (cond ((< smax 32) smax)
                                       ((<= (floor smax 4) (ash 1 14))
                                        (cond ((<= (* 100 nkeys) (* 56 smax)) (floor smax 32))
                                              ((<= (* 100 nkeys) (* 74 smax)) (floor smax 16))
                                              (t (floor smax 8))))
                                       (t
                                        (cond ((<= (* 10 nkeys) (* 6 smax)) (floor smax 16))
                                              ((<= (* 10 nkeys) (* 8 smax)) (floor smax 8))
                                              (t (floor smax 4))))))
                      (when (and fast (< blen (floor smax 8)))
                        (setq blen (floor smax 8)))
                      (setq alen (max alen 1) blen (max blen 1)))
                     (t
                      (let ((log (phash-log2 smax))
                            (five-eighths (<= (* 8 nkeys) (* 5 smax)))) ; nkeys <= smax*5/8
                        (cond
                          ((= log 0) (setq alen 1 blen 1))
                          ((<= log 8) (setq alen (floor smax 2) blen (floor smax 2)))
                          ((<= log 17)
                           (cond (fast
                                  (setq alen (floor smax 2) blen (floor smax 4)))
                                 ((< (floor smax 4) phash-use-scramble)
                                  (setq alen (if (<= (* 100 nkeys) (* 52 smax))
                                                 (floor smax 8)
                                                 (floor smax 4))
                                        blen alen))
                                 (t
                                  (setq alen (cond (five-eighths (floor smax 8))
                                                   ((<= (* 4 nkeys) (* 3 smax)) (floor smax 4))
                                                   (t (floor smax 2)))
                                        ;; always give the small size a shot
                                        blen (floor smax 4)))))
                          ((= log 18)
                           (if fast
                               (setq alen (floor smax 2) blen (floor smax 2))
                               (setq alen (floor smax 8) ; never require the multiword hash
                                     blen (if five-eighths (floor smax 4) (floor smax 2)))))
                          ((<= log 20)
                           (setq alen (if five-eighths (floor smax 8) (floor smax 2))
                                 blen (if five-eighths (floor smax 4) (floor smax 2))))
                          (t
                           (setq alen (floor smax 2) blen (floor smax 2)))))))
                   (values alen blen smax)))
               ;; Try to find a perfect hash function. Return BLEN, SMAX and
               ;; SCRAMBLE, or NIL if no perfect hash could be found.
               (findhash ()
                 (multiple-value-bind (alen blen smax)
                     (initalen (ash 1 (phash-log2 nkeys)))
                   (let ((scramble (phash-scramble-table smax))
                         (maxalen (if minimal (floor smax 2) smax))
                         (bad-initkey 0)
                         (bad-perfect 0)
                         (trysalt 1))
                     (setq tabh (make-array (if minimal nkeys smax)
                                            :element-type '(signed-byte 32)))
                     (allocate-tabb blen)
                     (loop
                       (let ((rslinit (initkey alen blen trysalt)))
                         (cond
                           ((= rslinit 2)
                            ;; INITKEY actually found a perfect hash,
                            ;; not just distinct (A,B)
                            (return (values 0 smax scramble)))
                           ((= rslinit 0) ; didn't find distinct (a,b)
                            (when (>= (incf bad-initkey) phash-retry-initkey)
                              ;; Try to put more bits in (A,B) to make distinct
                              ;; (A,B) more likely
                              (cond ((< alen maxalen)
                                     (setq alen (* alen 2)))
                                    ((< blen smax)
                                     (setq blen (* blen 2))
                                     (allocate-tabb blen))
                                    (t
                                     (inittab blen t) ; check for duplicates
                                     (return nil)))
                              (setq bad-initkey 0 bad-perfect 0)))
                           ;; Given distinct (A,B) for all keys, build a perfect hash
                           ((not (perfect blen smax scramble))
                            (when (>= (incf bad-perfect) phash-retry-hex)
                              (cond ((< blen smax)
                                     (setq blen (* blen 2))
                                     (allocate-tabb blen)
                                     ;; we know this salt got distinct (A,B)
                                     (decf trysalt))
                                    (t
                                     (return nil)))
                              (setq bad-perfect 0)))
                           (t
                            (return (values blen smax scramble)))))
                       (incf trysalt))))))
        (multiple-value-bind (blen smax scramble) (findhash)
          (when blen
            (emit-perfect-hash-sexpr blen smax scramble tabb-val lines used comments)))))))

;;; Write the hash function found by GENERATE-PERFECT-HASH-SEXPR.
(defun emit-perfect-hash-sexpr (blen smax scramble tab lines used comments)
  (declare (type (simple-array (unsigned-byte 32) (*)) scramble)
           (type (simple-array (unsigned-byte 16) (*)) tab)
           (type simple-vector lines))
  (with-output-to-string (stream)
    (let ((extra-parens 0))
      (write-char #\( stream)
      (when (>= blen phash-use-scramble)
        ;; A way to make the 1-byte values in TAB bigger
        (write-string "(let ((scramble #a((256) (unsigned-byte " stream)
        (let ((per-line (if (> smax 65536) 4 8)))
          (write-string (if (> smax 65536) "32)" "16)") stream)
          (dotimes (i 256)
            (format stream " #x~(~X~)" (aref scramble i))
            (when (= (mod i per-line) (1- per-line))
              (terpri stream))))
        (format stream ")))~%")
        (incf extra-parens))
      (when (> blen 0)
        ;; small adjustments to A to make values distinct
        (format stream "(let ((tab #a((~D) (unsigned-byte ~A" blen
                (if (or (<= smax 256) (>= blen phash-use-scramble)) "8)" "16)"))
        (dotimes (i blen)
          (format stream " ~D" (if (< blen phash-use-scramble)
                                   (aref scramble (aref tab i))
                                   (aref tab i))))
        (format stream ")))~%")
        (incf extra-parens))
      (let ((indent 0) (newline nil) (comment nil)
            (more-indent (if (> blen 0) 2 0)))
        (dotimes (i used)
          (let ((line (awhen (svref lines i) (apply #'phash-sprintf it))))
            (when line
              (when newline (terpri stream))
              (dotimes (i (+ indent more-indent)) (write-char #\space stream))
              (setq comment (position #\; line))
              (cond ((and comment (not comments)) ; strip the comment
                     (write-string line stream
                                   :end (if (char= (char line (1- comment)) #\space)
                                            (1- comment)
                                            comment))
                     (setq comment nil))
                    (t
                     (write-string line stream)))
              ;; Delay the newline so we prettily close all the parens on the last line.
              (setq newline t)
              (when (and (>= (length line) 4) (string= line "(let" :end1 4))
                (incf indent)))))
        (when comment (terpri stream))
        (dotimes (i (+ indent 1 extra-parens)) (write-char #\) stream))
        (terpri stream)))))
