;;;; Support for koi8-r encoding.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
(in-package "SB!IMPL")

(define-unibyte-mapper
    koi8-r->code-mapper
    code->koi8-r-mapper
  (#x80 #x2500)  ;  BOX DRAWINGS LIGHT HORIZONTAL
  (#x81 #x2502)  ;  BOX DRAWINGS LIGHT VERTICAL
  (#x82 #x250C)  ;  BOX DRAWINGS LIGHT DOWN AND RIGHT
  (#x83 #x2510)  ;  BOX DRAWINGS LIGHT DOWN AND LEFT
  (#x84 #x2514)  ;  BOX DRAWINGS LIGHT UP AND RIGHT
  (#x85 #x2518)  ;  BOX DRAWINGS LIGHT UP AND LEFT
  (#x86 #x251C)  ;  BOX DRAWINGS LIGHT VERTICAL AND RIGHT
  (#x87 #x2524)  ;  BOX DRAWINGS LIGHT VERTICAL AND LEFT
  (#x88 #x252C)  ;  BOX DRAWINGS LIGHT DOWN AND HORIZONTAL
  (#x89 #x2534)  ;  BOX DRAWINGS LIGHT UP AND HORIZONTAL
  (#x8a #x253C)  ;  BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL
  (#x8b #x2580)  ;  UPPER HALF BLOCK
  (#x8c #x2584)  ;  LOWER HALF BLOCK
  (#x8d #x2588)  ;  FULL BLOCK
  (#x8e #x258C)  ;  LEFT HALF BLOCK
  (#x8f #x2590)  ;  RIGHT HALF BLOCK
  (#x90 #x2591)  ;  LIGHT SHADE
  (#x91 #x2592)  ;  MEDIUM SHADE
  (#x92 #x2593)  ;  DARK SHADE
  (#x93 #x2320)  ;  TOP HALF INTEGRAL
  (#x94 #x25A0)  ;  BLACK SQUARE
  (#x95 #x2219)  ;  BULLET OPERATOR
  (#x96 #x221A)  ;  SQUARE ROOT
  (#x97 #x2248)  ;  ALMOST EQUAL TO
  (#x98 #x2264)  ;  LESS-THAN OR EQUAL TO
  (#x99 #x2265)  ;  GREATER-THAN OR EQUAL TO
  (#x9a #x00A0)  ;  NO-BREAK SPACE
  (#x9b #x2321)  ;  BOTTOM HALF INTEGRAL
  (#x9c #x00B0)  ;  DEGREE SIGN
  (#x9d #x00B2)  ;  SUPERSCRIPT TWO
  (#x9e #x00B7)  ;  MIDDLE DOT
  (#x9f #x00F7)  ;  DIVISION SIGN
  (#xa0 #x2550)  ;  BOX DRAWINGS DOUBLE HORIZONTAL
  (#xa1 #x2551)  ;  BOX DRAWINGS DOUBLE VERTICAL
  (#xa2 #x2552)  ;  BOX DRAWINGS DOWN SINGLE AND RIGHT DOUBLE
  (#xa3 #x0451)  ;  CYRILLIC SMALL LETTER IO
  (#xa4 #x2553)  ;  BOX DRAWINGS DOWN DOUBLE AND RIGHT SINGLE
  (#xa5 #x2554)  ;  BOX DRAWINGS DOUBLE DOWN AND RIGHT
  (#xa6 #x2555)  ;  BOX DRAWINGS DOWN SINGLE AND LEFT DOUBLE
  (#xa7 #x2556)  ;  BOX DRAWINGS DOWN DOUBLE AND LEFT SINGLE
  (#xa8 #x2557)  ;  BOX DRAWINGS DOUBLE DOWN AND LEFT
  (#xa9 #x2558)  ;  BOX DRAWINGS UP SINGLE AND RIGHT DOUBLE
  (#xaa #x2559)  ;  BOX DRAWINGS UP DOUBLE AND RIGHT SINGLE
  (#xab #x255A)  ;  BOX DRAWINGS DOUBLE UP AND RIGHT
  (#xac #x255B)  ;  BOX DRAWINGS UP SINGLE AND LEFT DOUBLE
  (#xad #x255C)  ;  BOX DRAWINGS UP DOUBLE AND LEFT SINGLE
  (#xae #x255D)  ;  BOX DRAWINGS DOUBLE UP AND LEFT
  (#xaf #x255E)  ;  BOX DRAWINGS VERTICAL SINGLE AND RIGHT DOUBLE
  (#xb0 #x255F)  ;  BOX DRAWINGS VERTICAL DOUBLE AND RIGHT SINGLE
  (#xb1 #x2560)  ;  BOX DRAWINGS DOUBLE VERTICAL AND RIGHT
  (#xb2 #x2561)  ;  BOX DRAWINGS VERTICAL SINGLE AND LEFT DOUBLE
  (#xb3 #x0401)  ;  CYRILLIC CAPITAL LETTER IO
  (#xb4 #x2562)  ;  BOX DRAWINGS VERTICAL DOUBLE AND LEFT SINGLE
  (#xb5 #x2563)  ;  BOX DRAWINGS DOUBLE VERTICAL AND LEFT
  (#xb6 #x2564)  ;  BOX DRAWINGS DOWN SINGLE AND HORIZONTAL DOUBLE
  (#xb7 #x2565)  ;  BOX DRAWINGS DOWN DOUBLE AND HORIZONTAL SINGLE
  (#xb8 #x2566)  ;  BOX DRAWINGS DOUBLE DOWN AND HORIZONTAL
  (#xb9 #x2567)  ;  BOX DRAWINGS UP SINGLE AND HORIZONTAL DOUBLE
  (#xba #x2568)  ;  BOX DRAWINGS UP DOUBLE AND HORIZONTAL SINGLE
  (#xbb #x2569)  ;  BOX DRAWINGS DOUBLE UP AND HORIZONTAL
  (#xbc #x256A)  ;  BOX DRAWINGS VERTICAL SINGLE AND HORIZONTAL DOUBLE
  (#xbd #x256B)  ;  BOX DRAWINGS VERTICAL DOUBLE AND HORIZONTAL SINGLE
  (#xbe #x256C)  ;  BOX DRAWINGS DOUBLE VERTICAL AND HORIZONTAL
  (#xbf #x00A9)  ;  COPYRIGHT SIGN
  (#xc0 #x044E)  ;  CYRILLIC SMALL LETTER YU
  (#xc1 #x0430)  ;  CYRILLIC SMALL LETTER A
  (#xc2 #x0431)  ;  CYRILLIC SMALL LETTER BE
  (#xc3 #x0446)  ;  CYRILLIC SMALL LETTER TSE
  (#xc4 #x0434)  ;  CYRILLIC SMALL LETTER DE
  (#xc5 #x0435)  ;  CYRILLIC SMALL LETTER IE
  (#xc6 #x0444)  ;  CYRILLIC SMALL LETTER EF
  (#xc7 #x0433)  ;  CYRILLIC SMALL LETTER GHE
  (#xc8 #x0445)  ;  CYRILLIC SMALL LETTER HA
  (#xc9 #x0438)  ;  CYRILLIC SMALL LETTER I
  (#xca #x0439)  ;  CYRILLIC SMALL LETTER SHORT I
  (#xcb #x043A)  ;  CYRILLIC SMALL LETTER KA
  (#xcc #x043B)  ;  CYRILLIC SMALL LETTER EL
  (#xcd #x043C)  ;  CYRILLIC SMALL LETTER EM
  (#xce #x043D)  ;  CYRILLIC SMALL LETTER EN
  (#xcf #x043E)  ;  CYRILLIC SMALL LETTER O
  (#xd0 #x043F)  ;  CYRILLIC SMALL LETTER PE
  (#xd1 #x044F)  ;  CYRILLIC SMALL LETTER YA
  (#xd2 #x0440)  ;  CYRILLIC SMALL LETTER ER
  (#xd3 #x0441)  ;  CYRILLIC SMALL LETTER ES
  (#xd4 #x0442)  ;  CYRILLIC SMALL LETTER TE
  (#xd5 #x0443)  ;  CYRILLIC SMALL LETTER U
  (#xd6 #x0436)  ;  CYRILLIC SMALL LETTER ZHE
  (#xd7 #x0432)  ;  CYRILLIC SMALL LETTER VE
  (#xd8 #x044C)  ;  CYRILLIC SMALL LETTER SOFT SIGN
  (#xd9 #x044B)  ;  CYRILLIC SMALL LETTER YERU
  (#xda #x0437)  ;  CYRILLIC SMALL LETTER ZE
  (#xdb #x0448)  ;  CYRILLIC SMALL LETTER SHA
  (#xdc #x044D)  ;  CYRILLIC SMALL LETTER E
  (#xdd #x0449)  ;  CYRILLIC SMALL LETTER SHCHA
  (#xde #x0447)  ;  CYRILLIC SMALL LETTER CHE
  (#xdf #x044A)  ;  CYRILLIC SMALL LETTER HARD SIGN
  (#xe0 #x042E)  ;  CYRILLIC CAPITAL LETTER YU
  (#xe1 #x0410)  ;  CYRILLIC CAPITAL LETTER A
  (#xe2 #x0411)  ;  CYRILLIC CAPITAL LETTER BE
  (#xe3 #x0426)  ;  CYRILLIC CAPITAL LETTER TSE
  (#xe4 #x0414)  ;  CYRILLIC CAPITAL LETTER DE
  (#xe5 #x0415)  ;  CYRILLIC CAPITAL LETTER IE
  (#xe6 #x0424)  ;  CYRILLIC CAPITAL LETTER EF
  (#xe7 #x0413)  ;  CYRILLIC CAPITAL LETTER GHE
  (#xe8 #x0425)  ;  CYRILLIC CAPITAL LETTER HA
  (#xe9 #x0418)  ;  CYRILLIC CAPITAL LETTER I
  (#xea #x0419)  ;  CYRILLIC CAPITAL LETTER SHORT I
  (#xeb #x041A)  ;  CYRILLIC CAPITAL LETTER KA
  (#xec #x041B)  ;  CYRILLIC CAPITAL LETTER EL
  (#xed #x041C)  ;  CYRILLIC CAPITAL LETTER EM
  (#xee #x041D)  ;  CYRILLIC CAPITAL LETTER EN
  (#xef #x041E)  ;  CYRILLIC CAPITAL LETTER O
  (#xf0 #x041F)  ;  CYRILLIC CAPITAL LETTER PE
  (#xf1 #x042F)  ;  CYRILLIC CAPITAL LETTER YA
  (#xf2 #x0420)  ;  CYRILLIC CAPITAL LETTER ER
  (#xf3 #x0421)  ;  CYRILLIC CAPITAL LETTER ES
  (#xf4 #x0422)  ;  CYRILLIC CAPITAL LETTER TE
  (#xf5 #x0423)  ;  CYRILLIC CAPITAL LETTER U
  (#xf6 #x0416)  ;  CYRILLIC CAPITAL LETTER ZHE
  (#xf7 #x0412)  ;  CYRILLIC CAPITAL LETTER VE
  (#xf8 #x042C)  ;  CYRILLIC CAPITAL LETTER SOFT SIGN
  (#xf9 #x042B)  ;  CYRILLIC CAPITAL LETTER YERU
  (#xfa #x0417)  ;  CYRILLIC CAPITAL LETTER ZE
  (#xfb #x0428)  ;  CYRILLIC CAPITAL LETTER SHA
  (#xfc #x042D)  ;  CYRILLIC CAPITAL LETTER E
  (#xfd #x0429)  ;  CYRILLIC CAPITAL LETTER SHCHA
  (#xfe #x0427)  ;  CYRILLIC CAPITAL LETTER CHE
  (#xff #x042A)) ;  CYRILLIC CAPITAL LETTER HARD SIGN

(declaim (inline get-koi8-r-bytes))
(defun get-koi8-r-bytes (string pos end)
  (declare (optimize speed (safety 0))
           (type simple-string string)
           (type array-range pos end))
  (get-latin-bytes #'identity :koi8-r string pos end))

(defun string->koi8-r (string sstart send null-padding)
  (declare (optimize speed (safety 0))
           (type simple-string string)
           (type array-range sstart send))
  (values (string->latin% string sstart send #'get-koi8-r-bytes null-padding)))

(defmacro define-koi8-r->string* (accessor type)
  (declare (ignore type))
  (let ((name (make-od-name 'koi8-r->string* accessor)))
    `(progn
      (defun ,name (string sstart send array astart aend)
        (,(make-od-name 'latin->string* accessor) string sstart send array astart aend #'identity)))))
(instantiate-octets-definition define-koi8-r->string*)

(defmacro define-koi8-r->string (accessor type)
  (declare (ignore type))
  `(defun ,(make-od-name 'koi8-r->string accessor) (array astart aend)
    (,(make-od-name 'latin->string accessor) array astart aend #'identity)))
(instantiate-octets-definition define-koi8-r->string)

(pushnew '((:koi8-r :|koi8-r| :koi8r)
           koi8-r->string-aref string->koi8-r)
         *external-format-functions* :test #'equal)

;;; for fd-stream.lisp

(define-external-format (:koi8-r :|koi8-r|)
    1 t
    (let ((koi8-r-byte (code->koi8-r-mapper bits)))
      (if koi8-r-byte
          (setf (sap-ref-8 sap tail) koi8-r-byte)
          (stream-encoding-error-and-handle stream bits)))
    (code-char (koi8-r->code-mapper byte)))
