#-x86-64 (invoke-restart 'run-tests::skip-file)

(defun foofill (v elt start end)
  (declare (simple-vector v) (sb-int:index start end))
  ;; FILL with :START and :END could, but currently do not,
  ;; always transform into VECTOR-FILL/T.
  ;; That's silly and we should fix it.
  ;; (Also we should fix that word-sized vectors could - but don't -
  ;; use that assembly routine)
  (values (sb-sys:%primitive sb-vm::vector-fill/t v elt start end)))
(compile 'foofill)

(defun exercise-foofill (item)
  ;; Test across multiple vector lengths. We can reasonably trust that the
  ;; "REP STOSQ" code path works - it's literally 1 machine instruction after
  ;; computing the start and length - so we only exercise vector fill operations
  ;; which are smaller than our threshold of 350 elements or more.
  (dolist (vec-len '(0 1 2 7 8 9 15 16 17 23 24 25 31 32 33 47 48 49 64 100))
    ;; Test even 'start' (16-byte aligned, 0 peel) and odd 'start' (8-byte unaligned, 1 peel)
    (dotimes (start (1+ vec-len))
      ;; Test all possible counts (exercising 0, 1, 2, 3+ unroll loops and 0..7 tail stores)
      (loop for end from start to vec-len
            do
          (let* ((sentinel (list ':sentinel))
                 (v (make-array vec-len :initial-element sentinel))
                 (res (foofill v item start end)))
            ;; 1. Verify return value
            (assert (eq res v))
            ;; 2. Verify filled region
            (loop for i from start below end do
              (unless (eq (svref v i) item)
                (error "Filled region mismatch at index ~A for len=~A [~A, ~A]: got ~S, expected ~S"
                       i vec-len start end (svref v i) item)))
            ;; 3. Verify prefix + suffix untouched
            (loop for i from 0 below start do
              (unless (eq (svref v i) sentinel)
                (error "Prefix corrupted at index ~A for len=~A [~A, ~A]"
                       i vec-len start end)))
            (loop for i from end below vec-len do
              (unless (eq (svref v i) sentinel)
                (error "Suffix corrupted at index ~A for len=~A [~A, ~A]"
                       i vec-len start end))))))))

(with-test (:name :vector-fill-unrolled)
  (exercise-foofill 42)
  (exercise-foofill '(a-list))
  (exercise-foofill 'a-symbol))
