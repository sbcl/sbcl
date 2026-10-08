(in-package :sb-manual)

(define-concept @interning-symbols (:title "interning symbols"
                                    :keys (("interning" "symbols")
                                           ("symbols," "interning"))))

(define-concept @package-lock (:title "package lock"
                               :keys (("package" "lock")
                                      ("lock," "package"))))

(define-concept @unicode (:title "Unicode" :keys ("Unicode")))

(define-concept @nfkc (:title "NFKC"
                       :keys ("NFKC"
                              "normalization form compatibility composition")))

(define-concept ~unbound-slot (:keys (("unbound" "slot")
                                      ("slot," "unbound"))))

(define-concept ~character-name (:keys (("character" "name")
                                        ("name" "of character"))))

(define-concept @hash-table (:title "hash table"
                             :keys (("hash" "table"))))

(define-concept @actual-source (:title "actual source"
                                :keys (("actual" "source")
                                       ("source," "actual"))))

(define-concept @original-source (:title "original source"
                                  :keys (("original" "source")
                                         ("source," "original"))))

(define-concept @processing-path (:title "processing path"
                                  :keys (("processing" "path"))))

(define-concept @macroexpansion (:title "macroexpansion"
                                 :keys ("macroexpansion")))

(define-concept ~source-transform (:keys (("source" "transform"))))


;;;; Optimization qualities

(define-concept @compilation-speed (:title "COMPILATION-SPEED"
                                    :keys (~compilation-speed)))

(define-concept ~compilation-speed
    (:keys (("compilation-speed," "optimization quality")
            ("optimization quality" "compilation-speed"))))

(define-concept ~debug (:keys (("debug," "optimization quality")
                               ("optimization quality" "debug"))))

(define-concept @debug (:title "DEBUG" :keys (~debug)))

(define-concept ~safety (:keys (("safety," "optimization quality")
                                ("optimization quality" "safety"))))

(define-concept @safety (:title "SAFETY" :keys (~safety)))

(define-concept ~space (:keys (("space," "optimization quality")
                               ("optimization quality" "space"))))

(define-concept @space (:title "SPACE" :keys (~space)))

(define-concept ~speed (:keys (("speed," "optimization quality")
                                ("optimization quality" "speed"))))

(define-concept @speed (:title "SPEED" :keys (~speed)))


(define-concept @tail-recursion (:title "tail recursion"
                                 :keys (("tail" "recursion")
                                        ("recursion," "tail"))))

(define-concept @tail-recursive (:title "tail recursive"
                                 :keys (@tail-recursion)))

(define-concept @interrupt (:title "interrupt"
                            :keys ("interrupt")))

(define-concept ~run-time-error (:keys (("run-time" "error")
                                        ("error," "run-time"))))

(define-concept @basic-block (:title "basic block"
                              :keys ("basic block"
                                     ("block," "basic"))))

(define-concept @block-start (:title "block start"
                              :keys (("block," "start location"))))

(define-concept @semi-inline (:title "semi inline"
                              :keys (("inline," "semi")
                                     ("semi-inline"))))

(define-concept @external-format (:title "external format"
                              :keys (("external" "format")
                                     ("format," "external"))))

(define-concept @generational-gc
    (:title "generational GC"
     :keys (("garbage collector," "generational")
            ("generational" "garbage collector"))))

(define-concept @conservative-gc
    (:title "conservative GC"
     :keys (("garbage collector," "conservative")
            ("conservative" "garbage collector"))))

(define-concept @declaration (:title "declaration"
                              :keys ("declaration")))

(define-concept @logical-pathname (:title "logical pathname"
                                   :keys (("logical" "pathname")
                                          ("pathname," "logical"))))

(define-concept ~disabling-ldb (:keys (("disabling" "LDB")
                                       ("LDB," "disabling"))))

(define-concept ~enabling-ldb (:keys (("enabling" "LDB")
                                      ("LDB," "enabling"))))
(define-concept ~repl (:keys ("Read-Eval-Print Loop" "REPL")))

(define-concept @repl (:title "REPL" :keys (~repl)))
