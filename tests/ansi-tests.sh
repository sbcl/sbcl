#!/bin/sh -e

if [ ! -e ansi-test ]; then
   git clone --depth 1 https://github.com/sbcl/ansi-test.git
fi


cd ansi-test
rm -fr sandbox/scratch
../../run-sbcl.sh --disable-ldb --lose-on-corruption \
                  --no-userinit --no-sysinit \
                  --load gclload1.lsp --load gclload2.lsp \
                  --eval '(setf *default-pathname-defaults* (truename #P"sandbox/"))' \
                  --eval '(in-package :cl-test)' \
                  --eval '(disable-note :nil-vectors-are-strings)' \
                  --eval '(time (do-tests))' \
                  --eval '(let* ((expected (list* "APROPOS-LIST.ERROR.2" "APROPOS.ERROR.2" "COMPILE-FILE.2"
 "DEFINE-COMPILER-MACRO.8" "DESTRUCTURING-BIND.ERROR.10"
 "EXP.ERROR.10" "EXP.ERROR.11" "EXP.ERROR.8"
 "EXP.ERROR.9" "EXPT.ERROR.10" "EXPT.ERROR.11" "EXPT.ERROR.8" "EXPT.ERROR.9"
 "FORMAT.A.29" "FORMAT.A.57" "FORMAT.A.58" "FORMAT.B.27" "FORMAT.B.28"
 "FORMAT.B.29" "FORMAT.D.27" "FORMAT.D.28" "FORMAT.D.29" "FORMAT.F.45"
 "FORMAT.F.46" "FORMAT.F.46B" "FORMAT.F.5" "FORMAT.F.8" "FORMAT.O.27"
 "FORMAT.O.28" "FORMAT.O.29" "FORMAT.R.37" "FORMAT.R.38" "FORMAT.S.29"
 "FORMAT.E.1" "FORMAT.E.2" "FORMAT.E.6" "FORMAT.E.20"
 "FORMAT.X.27" "FORMAT.X.28" "FORMAT.X.29" "FORMATTER.A.57" "FORMATTER.A.58"
 "FORMATTER.B.27" "FORMATTER.B.28" "FORMATTER.B.29" "FORMATTER.D.27"
 "FORMATTER.D.28" "FORMATTER.D.29" "FORMATTER.F.45" "FORMATTER.F.46"
 "FORMATTER.F.46B" "FORMATTER.O.27" "FORMATTER.O.28" "FORMATTER.O.29"
 "FORMATTER.R.37" "FORMATTER.R.38" "FORMATTER.X.27" "FORMATTER.X.28"
 "FORMATTER.X.29" "LOOP.1.39" "LOOP.1.40" "LOOP.1.41" "LOOP.1.42" "LOOP.1.43"
 "MACROLET.36" "MAKE-CONDITION.3" "MAKE-CONDITION.4"
 "MAKE-PATHNAME-ERROR-ABSOLUTE-WILD-INFERIORS-BACK"
 "MAKE-PATHNAME-ERROR-RELATIVE-WILD-INFERIORS-BACK"
 "PPRINT-LOGICAL-BLOCK.ERROR.1" "PPRINT-LOGICAL-BLOCK.ERROR.1-UNSAFE"
 "PPRINT-LOGICAL-BLOCK.ERROR.3" "PPRINT-LOGICAL-BLOCK.ERROR.3-UNSAFE"
 "PRINT-LEVEL.8" "PRINT-LEVEL.9" "PRINT.BACKQUOTE.RANDOM.1"
 "PRINT.BACKQUOTE.RANDOM.10" "PRINT.BACKQUOTE.RANDOM.11"
 "PRINT.BACKQUOTE.RANDOM.13" "PRINT.BACKQUOTE.RANDOM.14"
 "PRINT.BACKQUOTE.RANDOM.2" "PRINT.BACKQUOTE.RANDOM.3"
 "PRINT.BACKQUOTE.RANDOM.4" "PRINT.BACKQUOTE.RANDOM.5" "PROCLAIM.ERROR.7"
 "SHIFTF.7"
 "SXHASH.17" "SXHASH.18" "SXHASH.19" "PRINT-STRUCTURE.1"
 "UNION.FOLD.1" "SUBTYPEP-COMPLEX.8"
 (append #+win32 (list "PROBE-FILE.4")
         #+x86 (list "CIS.4")
         #+(and arm64 (not darwin))
           (list "EXP.ERROR.4" "EXP.ERROR.5" "EXP.ERROR.6" "EXP.ERROR.7" "EXPT.ERROR.4"
                 "EXPT.ERROR.5" "EXPT.ERROR.6" "EXPT.ERROR.7")
         #-sb-unicode (list "MISC.638")
         (if (member :sb-fasteval sb-impl:+internal-features+)
             (list "INTERSECTION.FOLD.1" "UNION.FOLD.1" "SET-DIFFERENCE.FOLD.1"
                   "SET-EXCLUSIVE-OR.FOLD.1"
                   "ALL-STRUCTURE-CLASSES-ARE-SUBTYPES-OF-STRUCTURE-OBJECT.2" "TRACE.8")
             (list "MAP.48" "SYMBOL-FUNCTION.ERROR.5"))

         #+sb-unicode (list "BOTH-CASE-P.2" "CHAR-DOWNCASE.2" "CHAR-UPCASE.2"))))
                         (failing (remove "FORMAT.E.26"
                                          (mapcar (function string) regression-test:*failed-tests*)
                                          :test (function equal)))
                         #+sb-devel
                         (failing (remove "COMMON-LISP-PACKAGE-NICKNAMES" failing :test (function equal)))
                         (diff1 (set-difference failing  expected :test (function equal)))
                         (diff2 (set-difference expected failing :test (function equal))))
   (cond ((or diff1 diff2)
           (format t "Difference ~@[added ~a~] ~@[removed ~a~]~%" diff1 diff2)
           (sb-ext:exit :code 1))
         ((sb-ext:exit))))'
