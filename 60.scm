;; TODO: License. From Gauche. Written by Shiro Kawai?

(test-section "srfi-60")

(use srfi-60)
(test-module 'srfi-60)

;; Most procedures are builtin and tested in test/numbers.scm.

(test* "bitwise-if" #b01100110
       (bitwise-if #b10101100 #b00110101 #b11001010))

(for-each (^(n r) (test* (format "log2-binary-factors(~a)" n) r
                         (log2-binary-factors n)))
          '(0  1  2  3  4  5  6  7  8  9  10  11  12  13  14  15  16
              -1 -2 -3 -4 -5 -6 -7 -8 -9 -10 -11 -12 -13 -14 -15 -16)
          '(-1 0  1  0  2  0  1  0  3  0   1   0   2   0   1   0   4
               0  1  0  2  0  1  0  3  0   1   0   2   0   1   0   4))

(define (test-rotate-bit-field n c s e r)
  (test* (format "rotate-bit-field(~s,~a,~a,~a)" (number->string n 2) c s e) r
         (number->string (rotate-bit-field n c s e) 2)))
(test-rotate-bit-field #b0100 3 4 0 "100") ; trivial path
(test-rotate-bit-field #b0100 3 0 4 "10")
(test-rotate-bit-field #b0100 -1 0 4 "10")
(test-rotate-bit-field #b0100 10 0 4 "1")
(test-rotate-bit-field #b110100100010000 -1 5 9 "110100010010000")
(test-rotate-bit-field #b110100100010000 1 5 9 "110100000110000")

(define (test-reverse-bit-field n s e r)
  (test* (format "reverse-bit-field(~s,~a,~a)" (number->string n 2) s e) r
         (number->string (reverse-bit-field n s e) 2)))
(test-reverse-bit-field #xa7 8 0 "10100111")
(test-reverse-bit-field #xa7 0 8 "11100101")
(test-reverse-bit-field #xa7 1 5 "10111001")

(test* "integer->list" '(#t #f #f #t) (integer->list 9))
(test* "integer->list" '(#f #f #t #f #f #t) (integer->list 9 6))
(test* "list->integer" 9 (list->integer '(#t #f #f #t)))
(test* "list->integer" 9 (list->integer '(#f #f #t #f #f #t)))
;; tests bignum path
(test* "list->integer" (+ (expt 2 63) (expt 2 62) (expt 2 31) (expt 2 30) 1)
       (list->integer '(#t #t #f #f #f #f #f #f
                        #f #f #f #f #f #f #f #f
                        #f #f #f #f #f #f #f #f
                        #f #f #f #f #f #f #f #f
                        #t #t #f #f #f #f #f #f
                        #f #f #f #f #f #f #f #f
                        #f #f #f #f #f #f #f #f
                        #f #f #f #f #f #f #f #t)))

(test* "booleans->integer" 9 (booleans->integer #f #f #t #f #f #t))
