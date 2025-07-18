(define pinfty +inf.0)
(define minfty -inf.0)
(define nanval +nan.0)

;(define pinfty 1.0e307)
;(define minfty -1.0e307)
;(define nanval 12345.0)

;;;; Basic tests of (rnrs bytevectors).

(define (iota count . o)
  (let ((start (if (pair? o) (car o) 0))
        (step (if (and (pair? o) (pair? (cdr o))) (cadr o) 1)))
    (let lp ((i count) (res '()))
      (if (<= i 0)
          res
          (lp (- i 1) (cons (+ start (* (- i 1) step)) res))))))

;;; Larceny tests

(define-syntax test
  (syntax-rules (=>)
   ((test id exp => result)
    (begin
      (display "Try ")
      (display id)
      (display ":")
      (display 'exp)
      (if (not (equal? exp result))
        (begin (newline)
          (display "*****BUG*****")
          (newline)
          (display "Expected: ")
          (display result)
          (display " Result: ")
          (display exp)
          (newline))
        (begin (display " OK")
               (newline)))))))
 

(define (basic-bytevector-tests)
  (call-with-current-continuation
   (lambda (exit)
     (let ()
       (test 1 (or (eq? (native-endianness) 'big)
                 (eq? (native-endianness) 'little)) => #t)

       (test 2 (bytevector? (vector)) => #f)
       (test 3 (bytevector? (make-bytevector 3)) => #t)

       (test 4 (bytevector-length (make-bytevector 44)) => 44)

       (test 5 (let ((b1 (make-bytevector 16 -127))
                   (b2 (make-bytevector 16 255)))
               (list
                (bytevector-s8-ref b1 0)
                (bytevector-u8-ref b1 0)
                (bytevector-s8-ref b2 0)
                (bytevector-u8-ref b2 0))) => '(-127 129 -1 255))

       (test 6 (let ((b (make-bytevector 16 -127)))
               (bytevector-s8-set! b 0 -126)
               (bytevector-u8-set! b 1 246)
               (list
                (bytevector-s8-ref b 0)
                (bytevector-u8-ref b 0)
                (bytevector-s8-ref b 1)
                (bytevector-u8-ref b 1))) => '(-126 130 -10 246))

       (let ()
         (define b (make-bytevector 16 -127))
         (bytevector-uint-set! b 0 (- (expt 2 128) 3) (endianness little) 16)

         (test 7 (bytevector-uint-ref b 0 (endianness little) 16)
               => #xfffffffffffffffffffffffffffffffd)

         (test 8 (bytevector-sint-ref b 0 (endianness little) 16) => -3)

         (test 9 (bytevector->u8-list b)
               => '(253 255 255 255 255 255 255 255
                   255 255 255 255 255 255 255 255))

         (bytevector-uint-set! b 0 (- (expt 2 128) 3) (endianness big) 16)

         (test 10 (bytevector-uint-ref b 0 (endianness big) 16)
               => #xfffffffffffffffffffffffffffffffd)

         (test 11 (bytevector-sint-ref b 0 (endianness big) 16) => -3)

         (test 12 (bytevector->u8-list b)
               => '(255 255 255 255 255 255 255 255
                   255 255 255 255 255 255 255 253)))

       (let ()
         (define b
           (u8-list->bytevector
            '(255 255 255 255 255 255 255 255
              255 255 255 255 255 255 255 253)))

         (test 13 (bytevector-u16-ref b 14 (endianness little)) => 65023)

         (test 14 (bytevector-s16-ref b 14 (endianness little)) => -513)

         (test 15 (bytevector-u16-ref b 14 (endianness big)) => 65533)

         (test 16 (bytevector-s16-ref b 14 (endianness big)) => -3)

         (bytevector-u16-set! b 0 12345 (endianness little))

         (test 17 (bytevector-u16-ref b 0 (endianness little)) => 12345)

         (bytevector-u16-native-set! b 0 12345)

         (test 18 (bytevector-u16-native-ref b 0) => 12345))

       (let ()
         (define b
           (u8-list->bytevector
            '(255 255 255 255 255 255 255 255
              255 255 255 255 255 255 255 253)))

         (test 19 (bytevector-u32-ref b 12 (endianness little)) => 4261412863)

         (test 20 (bytevector-s32-ref b 12 (endianness little)) => -33554433)

         (test 21 (bytevector-u32-ref b 12 (endianness big)) => 4294967293)

         (test 22 (bytevector-s32-ref b 12 (endianness big)) => -3))

       (let ()
         (define b
           (u8-list->bytevector
            '(255 255 255 255 255 255 255 255
              255 255 255 255 255 255 255 253)))

         (test 23 (bytevector-u64-ref b 8 (endianness little))
               => 18302628885633695743)

         (test 24 (bytevector-s64-ref b 8 (endianness little))
               => -144115188075855873)

         (test 25 (bytevector-u64-ref b 8 (endianness big))
               => 18446744073709551613)

         (test 26 (bytevector-s64-ref b 8 (endianness big)) => -3))

       (let ()
         (define b1 (u8-list->bytevector '(255 2 254 3 255)))
         (define b2 (u8-list->bytevector '(255 3 254 2 255)))
         (define b3 (u8-list->bytevector '(255 3 254 2 255)))
         (define b4 (u8-list->bytevector '(255 3 255)))

         (test 27 (bytevector=? b1 b2) => #f)
         (test 28 (bytevector=? b2 b3) => #t)
         (test 29 (bytevector=? b3 b4) => #f)
         (test 30 (bytevector=? b4 b3) => #f))

       (let ()
         (define b
           (u8-list->bytevector
            '(63 240 0 0 0 0 0 0)))

         (test 31 (bytevector-ieee-single-ref b 4 'little) => 0.0)

         (test 32 (bytevector-ieee-double-ref b 0 'big) => 1.0)

         (bytevector-ieee-single-native-set! b 4 3.0)

         (test 33 (bytevector-ieee-single-native-ref b 4) => 3.0)

         (bytevector-ieee-double-native-set! b 0 5.0)

         (test 34 (bytevector-ieee-double-native-ref b 0) => 5.0)

         (bytevector-ieee-double-set! b 0 1.75 'big)

         (test 35 (bytevector->u8-list b) => '(63 252 0 0 0 0 0 0))) ; depends on ieee routines

       (let ((b (make-bytevector 7 12)))
         (bytevector-fill! b 127)
         (test 36 (bytevector->u8-list b) => '(127 127 127 127 127 127 127)))

       (let ((b (u8-list->bytevector '(1 2 3 4 5 6 7 8))))
         (r6rs:bytevector-copy! b 0 b 3 4)
         (test 37 (bytevector->u8-list b) => '(1 2 3 1 2 3 4 8))
         (test 38 (bytevector=? b (bytevector-copy b)) => #t))

       (let ((b (u8-list->bytevector '(1 2 3 255 1 2 1 2))))
         (test 39 (bytevector->sint-list b (endianness little) 2)
               => '(513 -253 513 513))
         (test 40 (bytevector->uint-list b (endianness little) 2)
               => '(513 65283 513 513)))))))


(define (ieee-bytevector-tests)

  (define (roundtrip x getter setter! k endness)
    (let ((b (make-bytevector 100)))
      (setter! b k x endness)
      (getter b k endness)))

  (define (->single x)
    (roundtrip
     x bytevector-ieee-single-ref bytevector-ieee-single-set! 0 'big))

  (define (->double x)
    (roundtrip
     x bytevector-ieee-double-ref bytevector-ieee-double-set! 0 'big))

  ; Single precision, offset 0, big-endian

  (test 41 (roundtrip
         pinfty
         bytevector-ieee-single-ref bytevector-ieee-single-set! 0 'big)
        => pinfty)

  (test 42 (roundtrip
         minfty
         bytevector-ieee-single-ref bytevector-ieee-single-set! 0 'big)
        => minfty)

  (test 43 (let ((x (roundtrip
                  nanval
                  bytevector-ieee-single-ref bytevector-ieee-single-set!
                  0 'big)))
          (= x x))
        => #f)

  (test 44 (roundtrip
         1e10
         bytevector-ieee-single-ref bytevector-ieee-single-set! 0 'big)
        => 1e10)

  (test 45 (roundtrip
         -0.2822580337524414
         bytevector-ieee-single-ref bytevector-ieee-single-set! 0 'big)
        => -0.2822580337524414)

  ; Single precision, offset 0, little-endian

  (test 46 (roundtrip
         pinfty
         bytevector-ieee-single-ref bytevector-ieee-single-set! 0 'little)
        => pinfty)

  (test 47 (roundtrip
         minfty
         bytevector-ieee-single-ref bytevector-ieee-single-set! 0 'little)
        => minfty)

  (test 48 (let ((x (roundtrip
                  nanval
                  bytevector-ieee-single-ref bytevector-ieee-single-set!
                  0 'little)))
          (= x x))
        => #f)

  (test 49 (roundtrip
         1e10
         bytevector-ieee-single-ref bytevector-ieee-single-set! 0 'little)
        => 1e10)

  (test 50 (roundtrip
         -0.2822580337524414
         bytevector-ieee-single-ref bytevector-ieee-single-set! 0 'little)
        => -0.2822580337524414)

  ; Single precision, offset 1, big-endian

  (test 51 (roundtrip
         pinfty
         bytevector-ieee-single-ref bytevector-ieee-single-set! 1 'big)
        => pinfty)

  (test 52 (roundtrip
         minfty
         bytevector-ieee-single-ref bytevector-ieee-single-set! 1 'big)
        => minfty)

  (test 53 (let ((x (roundtrip
                  nanval
                  bytevector-ieee-single-ref bytevector-ieee-single-set!
                  1 'big)))
          (= x x))
        => #f)

  (test 54 (roundtrip
         1e10
         bytevector-ieee-single-ref bytevector-ieee-single-set! 1 'big)
        => 1e10)

  (test 55 (roundtrip
         -0.2822580337524414
         bytevector-ieee-single-ref bytevector-ieee-single-set! 1 'big)
        => -0.2822580337524414)

  ; Single precision, offset 1, little-endian

  (test 56 (roundtrip
         pinfty
         bytevector-ieee-single-ref bytevector-ieee-single-set! 1 'little)
        => pinfty)

  (test 57 (roundtrip
         minfty
         bytevector-ieee-single-ref bytevector-ieee-single-set! 1 'little)
        => minfty)

  (test 58 (let ((x (roundtrip
                  nanval
                  bytevector-ieee-single-ref bytevector-ieee-single-set!
                  1 'little)))
          (= x x))
        => #f)

  (test 59 (roundtrip
         1e10
         bytevector-ieee-single-ref bytevector-ieee-single-set! 1 'little)
        => 1e10)

  (test 60 (roundtrip
         -0.2822580337524414
         bytevector-ieee-single-ref bytevector-ieee-single-set! 1 'little)
        => -0.2822580337524414)

  ; Single precision, offset 2, big-endian

  (test 61 (roundtrip
         pinfty
         bytevector-ieee-single-ref bytevector-ieee-single-set! 2 'big)
        => pinfty)

  (test 62 (roundtrip
         minfty
         bytevector-ieee-single-ref bytevector-ieee-single-set! 2 'big)
        => minfty)

  (test 63 (let ((x (roundtrip
                  nanval
                  bytevector-ieee-single-ref bytevector-ieee-single-set!
                  2 'big)))
          (= x x))
        => #f)

  (test 64 (roundtrip
         1e10
         bytevector-ieee-single-ref bytevector-ieee-single-set! 2 'big)
        => 1e10)

  (test 65 (roundtrip
         -0.2822580337524414
         bytevector-ieee-single-ref bytevector-ieee-single-set! 2 'big)
        => -0.2822580337524414)

  ; Single precision, offset 2, little-endian

  (test 66 (roundtrip
         pinfty
         bytevector-ieee-single-ref bytevector-ieee-single-set! 2 'little)
        => pinfty)

  (test 67 (roundtrip
         minfty
         bytevector-ieee-single-ref bytevector-ieee-single-set! 2 'little)
        => minfty)

  (test 68 (let ((x (roundtrip
                  nanval
                  bytevector-ieee-single-ref bytevector-ieee-single-set!
                  2 'little)))
          (= x x))
        => #f)

  (test 69 (roundtrip
         1e10
         bytevector-ieee-single-ref bytevector-ieee-single-set! 2 'little)
        => 1e10)

  (test 70 (roundtrip
         -0.2822580337524414
         bytevector-ieee-single-ref bytevector-ieee-single-set! 2 'little)
        => -0.2822580337524414)

  ; Single precision, offset 3, big-endian

  (test 71 (roundtrip
         pinfty
         bytevector-ieee-single-ref bytevector-ieee-single-set! 3 'big)
        => pinfty)

  (test 72 (roundtrip
         minfty
         bytevector-ieee-single-ref bytevector-ieee-single-set! 3 'big)
        => minfty)

  (test 73 (let ((x (roundtrip
                  nanval
                  bytevector-ieee-single-ref bytevector-ieee-single-set!
                  3 'big)))
          (= x x))
        => #f)

  (test 74 (roundtrip
         1e10
         bytevector-ieee-single-ref bytevector-ieee-single-set! 3 'big)
        => 1e10)

  (test 75 (roundtrip
         -0.2822580337524414
         bytevector-ieee-single-ref bytevector-ieee-single-set! 3 'big)
        => -0.2822580337524414)

  ; Single precision, offset 3, little-endian

  (test 76 (roundtrip
         pinfty
         bytevector-ieee-single-ref bytevector-ieee-single-set! 3 'little)
        => pinfty)

  (test 77 (roundtrip
         minfty
         bytevector-ieee-single-ref bytevector-ieee-single-set! 3 'little)
        => minfty)

  (test 78 (let ((x (roundtrip
                  nanval
                  bytevector-ieee-single-ref bytevector-ieee-single-set!
                  3 'little)))
          (= x x))
        => #f)

  (test 79 (roundtrip
         1e10
         bytevector-ieee-single-ref bytevector-ieee-single-set! 3 'little)
        => 1e10)

  (test 80 (roundtrip
         -0.2822580337524414
         bytevector-ieee-single-ref bytevector-ieee-single-set! 3 'little)
        => -0.2822580337524414)

  ; Double precision, offset 0, big-endian

  (test 81 (roundtrip
         pinfty
         bytevector-ieee-double-ref bytevector-ieee-double-set! 0 'big)
        => pinfty)

  (test 82 (roundtrip
         minfty
         bytevector-ieee-double-ref bytevector-ieee-double-set! 0 'big)
        => minfty)

  (test 83 (let ((x (roundtrip
                  nanval
                  bytevector-ieee-double-ref bytevector-ieee-double-set!
                  0 'big)))
          (= x x))
        => #f)

  (test 84 (roundtrip
         1e10
         bytevector-ieee-double-ref bytevector-ieee-double-set! 0 'big)
        => 1e10)

  (test 85 (roundtrip
         -0.2822580337524414
         bytevector-ieee-double-ref bytevector-ieee-double-set! 0 'big)
        => -0.2822580337524414)

  ; Double precision, offset 0, little-endian

  (test 86 (roundtrip
         pinfty
         bytevector-ieee-double-ref bytevector-ieee-double-set! 0 'little)
        => pinfty)

  (test 87 (roundtrip
         minfty
         bytevector-ieee-double-ref bytevector-ieee-double-set! 0 'little)
        => minfty)

  (test 88 (let ((x (roundtrip
                  nanval
                  bytevector-ieee-double-ref bytevector-ieee-double-set!
                  0 'little)))
          (= x x))
        => #f)

  (test 89 (roundtrip
         1e10
         bytevector-ieee-double-ref bytevector-ieee-double-set! 0 'little)
        => 1e10)

  (test 90 (roundtrip
         -0.2822580337524414
         bytevector-ieee-double-ref bytevector-ieee-double-set! 0 'little)
        => -0.2822580337524414)

  ; Double precision, offset 1, big-endian

  (test 91 (roundtrip
         pinfty
         bytevector-ieee-double-ref bytevector-ieee-double-set! 1 'big)
        => pinfty)

  (test 92 (roundtrip
         minfty
         bytevector-ieee-double-ref bytevector-ieee-double-set! 1 'big)
        => minfty)

  (test 93 (let ((x (roundtrip
                  nanval
                  bytevector-ieee-double-ref bytevector-ieee-double-set!
                  1 'big)))
          (= x x))
        => #f)

  (test 94 (roundtrip
         1e10
         bytevector-ieee-double-ref bytevector-ieee-double-set! 1 'big)
        => 1e10)

  (test 95 (roundtrip
         -0.2822580337524414
         bytevector-ieee-double-ref bytevector-ieee-double-set! 1 'big)
        => -0.2822580337524414)

  ; Double precision, offset 1, little-endian

  (test 96 (roundtrip
         pinfty
         bytevector-ieee-double-ref bytevector-ieee-double-set! 1 'little)
        => pinfty)

  (test 97 (roundtrip
         minfty
         bytevector-ieee-double-ref bytevector-ieee-double-set! 1 'little)
        => minfty)

  (test 98 (let ((x (roundtrip
                  nanval
                  bytevector-ieee-double-ref bytevector-ieee-double-set!
                  1 'little)))
          (= x x))
        => #f)

  (test 99 (roundtrip
         1e10
         bytevector-ieee-double-ref bytevector-ieee-double-set! 1 'little)
        => 1e10)

  (test 100 (roundtrip
         -0.2822580337524414
         bytevector-ieee-double-ref bytevector-ieee-double-set! 1 'little)
        => -0.2822580337524414)

  ; Double precision, offset 2, big-endian

  (test 101 (roundtrip
         pinfty
         bytevector-ieee-double-ref bytevector-ieee-double-set! 2 'big)
        => pinfty)

  (test 102 (roundtrip
         minfty
         bytevector-ieee-double-ref bytevector-ieee-double-set! 2 'big)
        => minfty)

  (test 103 (let ((x (roundtrip
                  nanval
                  bytevector-ieee-double-ref bytevector-ieee-double-set!
                  2 'big)))
          (= x x))
        => #f)

  (test 104 (roundtrip
         1e10
         bytevector-ieee-double-ref bytevector-ieee-double-set! 2 'big)
        => 1e10)

  (test 105 (roundtrip
         -0.2822580337524414
         bytevector-ieee-double-ref bytevector-ieee-double-set! 2 'big)
        => -0.2822580337524414)

  ; Double precision, offset 2, little-endian

  (test 106 (roundtrip
         pinfty
         bytevector-ieee-double-ref bytevector-ieee-double-set! 2 'little)
        => pinfty)

  (test 107 (roundtrip
         minfty
         bytevector-ieee-double-ref bytevector-ieee-double-set! 2 'little)
        => minfty)

  (test 108 (let ((x (roundtrip
                  nanval
                  bytevector-ieee-double-ref bytevector-ieee-double-set!
                  2 'little)))
          (= x x))
        => #f)

  (test 109 (roundtrip
         1e10
         bytevector-ieee-double-ref bytevector-ieee-double-set! 2 'little)
        => 1e10)

  (test 110 (roundtrip
         -0.2822580337524414
         bytevector-ieee-double-ref bytevector-ieee-double-set! 2 'little)
        => -0.2822580337524414)

  ; Double precision, offset 3, big-endian

  (test 111 (roundtrip
         pinfty
         bytevector-ieee-double-ref bytevector-ieee-double-set! 3 'big)
        => pinfty)

  (test 112 (roundtrip
         minfty
         bytevector-ieee-double-ref bytevector-ieee-double-set! 3 'big)
        => minfty)

  (test 113 (let ((x (roundtrip
                  nanval
                  bytevector-ieee-double-ref bytevector-ieee-double-set!
                  3 'big)))
          (= x x))
        => #f)

  (test 114 (roundtrip
         1e10
         bytevector-ieee-double-ref bytevector-ieee-double-set! 3 'big)
        => 1e10)

  (test 115 (roundtrip
         -0.2822580337524414
         bytevector-ieee-double-ref bytevector-ieee-double-set! 3 'big)
        => -0.2822580337524414)

  ; Double precision, offset 3, little-endian

  (test 116 (roundtrip
         pinfty
         bytevector-ieee-double-ref bytevector-ieee-double-set! 3 'little)
        => pinfty)

  (test 117 (roundtrip
         minfty
         bytevector-ieee-double-ref bytevector-ieee-double-set! 3 'little)
        => minfty)

  (test 118 (let ((x (roundtrip
                  nanval
                  bytevector-ieee-double-ref bytevector-ieee-double-set!
                  3 'little)))
          (= x x))
        => #f)

  (test 119 (roundtrip
         1e10
         bytevector-ieee-double-ref bytevector-ieee-double-set! 3 'little)
        => 1e10)

  (test 120 (roundtrip
         -0.2822580337524414
         bytevector-ieee-double-ref bytevector-ieee-double-set! 3 'little)
        => -0.2822580337524414)

  ; Denormalized numbers.

  (do ((x 1.0 (* .5 x)))
      ((= x 0.0))
    (let ((y (->single x)))
      (if (and (> y 0.0)
               (not (= x y)))
          (begin (write (list 'inaccurate-single-conversion: x '=> y))
                 (newline)))))

  (do ((x 1.0 (* .5 x)))
      ((= x 0.0))
    (let ((y (->double x)))
      (if (not (= x y))
          (begin (write (list 'inaccurate-double-conversion: x '=> y))
                 (newline))))))




(display "basic bytevector tests")
(newline)
(basic-bytevector-tests)

(display "ieee bytevector tests")
(newline)
(ieee-bytevector-tests)

;;;; From Guile tests

;;; Some of the tests in here are examples taken from the R6RS Standard
;;; Libraries document.

(define-syntax with-test-prefix (syntax-rules ()
  ((_ what body-form ...)
   (begin
     (display what)
     (newline)
     body-form ...))))

(define-syntax pass-if (syntax-rules ()
  ((pass-if num what expr)
   (begin
     (display "Try ")
     (display num)
     (display ": ")
     (display 'expr)
     (if (not expr)
       (begin
         (display "***** BUG ******")
         (newline)
         (display 'expr)
         (display " failed")
         (newline))
       (begin
         (display " OK")
         (newline)))))))

(define-syntax pass-if-equal (syntax-rules ()
  ((_ num what expr1 expr2) (pass-if num what (equal? expr1 expr2)))))

(with-test-prefix "2.2 General Operations"

  (pass-if 122 "native-endianness"
    (not (not (memq (native-endianness) '(big little)))))

  (pass-if 123 "make-bytevector"
    (and (bytevector? (make-bytevector 20))
         (bytevector? (make-bytevector 20 3))))

  (pass-if 124 "bytevector-length"
    (= (bytevector-length (make-bytevector 20)) 20))

  (pass-if 125 "bytevector=?"
    (and (bytevector=? (make-bytevector 20 7)
                       (make-bytevector 20 7))
         (not (bytevector=? (make-bytevector 20 7)
                            (make-bytevector 20 0)))))

  ;; This failed prior to Guile 2.0.12.
  ;; See <http://bugs.gnu.org/19027>.
  (pass-if-equal 125.1 "bytevector-fill! with fill 255"
      (u8-list->bytevector '(255 255 255 255))
    (let ((bv (make-bytevector 4)))
      (bytevector-fill! bv 255)
      bv))

  (pass-if 126 "r6rs:bytevector-copy! overlapping"
    ;; See <http://debbugs.gnu.org/10070>.
    (let ((b (u8-list->bytevector '(1 2 3 4 5 6 7 8))))
      (r6rs:bytevector-copy! b 0 b 3 4)
      (bytevector->u8-list b)
      (bytevector=? b (u8-list->bytevector '(1 2 3 1 2 3 4 8))))))

(with-test-prefix "2.3 Operations on Bytes and Octets"

  (pass-if 127 "bytevector-{u8,s8}-ref"
    (equal? '(-127 129 -1 255)
            (let ((b1 (make-bytevector 16 -127))
                  (b2 (make-bytevector 16 255)))
              (list (bytevector-s8-ref b1 0)
                    (bytevector-u8-ref b1 0)
                    (bytevector-s8-ref b2 0)
                    (bytevector-u8-ref b2 0)))))

  (pass-if 128 "bytevector-{u8,s8}-set!"
    (equal? '(-126 130 -10 246)
            (let ((b (make-bytevector 16 -127)))

              (bytevector-s8-set! b 0 -126)
              (bytevector-u8-set! b 1 246)

              (list (bytevector-s8-ref b 0)
                    (bytevector-u8-ref b 0)
                    (bytevector-s8-ref b 1)
                    (bytevector-u8-ref b 1)))))

  (pass-if 129 "bytevector->u8-list"
    (let ((lst '(1 2 3 128 150 255)))
      (equal? lst
              (bytevector->u8-list
               (let ((b (make-bytevector 6)))
                 (for-each (lambda (i v)
                             (bytevector-u8-set! b i v))
                           (iota 6)
                           lst)
                 b)))))

  (pass-if 130 "u8-list->bytevector"
    (let ((lst '(1 2 3 128 150 255)))
      (equal? lst
              (bytevector->u8-list (u8-list->bytevector lst)))))

  (pass-if 131 "bytevector-uint-{ref,set!} [small]"
    (let ((b (make-bytevector 15)))
      (bytevector-uint-set! b 0 #x1234
                            (endianness little) 2)
      (equal? (bytevector-uint-ref b 0 (endianness big) 2)
              #x3412)))

  (pass-if 132 "bytevector-uint-set! [large]"
    (let ((b (make-bytevector 16)))
      (bytevector-uint-set! b 0 (- (expt 2 128) 3)
                            (endianness little) 16)
      (equal? (bytevector->u8-list b)
              '(253 255 255 255 255 255 255 255
                255 255 255 255 255 255 255 255))))

  (pass-if 133 "bytevector-uint-{ref,set!} [large]"
    (let ((b (make-bytevector 120)))
      (bytevector-uint-set! b 0 (- (expt 2 128) 3)
                            (endianness little) 16)
      (equal? (bytevector-uint-ref b 0 (endianness little) 16)
              #xfffffffffffffffffffffffffffffffd)))

  (pass-if 134 "bytevector-sint-ref [small]"
    (let ((b (u8-list->bytevector '(#xff #xf0 #xff))))
      (= (bytevector-sint-ref b 0 (endianness big) 2)
              (bytevector-sint-ref b 1 (endianness little) 2)
              -16)))

  (pass-if 135 "bytevector-sint-ref [large]"
    (let ((b (make-bytevector 50)))
      (bytevector-uint-set! b 0 (- (expt 2 128) 3)
                            (endianness little) 16)
      (equal? (bytevector-sint-ref b 0 (endianness little) 16)
              -3)))

  (pass-if 136 "bytevector-sint-set! [small]"
    (let ((b (make-bytevector 3)))
      (bytevector-sint-set! b 0 -16 (endianness big) 2)
      (bytevector-sint-set! b 1 -16 (endianness little) 2)
      (equal? (bytevector->u8-list b)
	      '(#xff #xf0 #xff))))

  (pass-if 137 "equal?"
    (let ((bv1 (u8-list->bytevector (iota 123)))
          (bv2 (u8-list->bytevector (iota 123))))
      (equal? bv1 bv2))))

(with-test-prefix "2.4 Operations on Integers of Arbitrary Size"

  (pass-if 138 "bytevector->sint-list"
    (let ((b (u8-list->bytevector '(1 2 3 255 1 2 1 2))))
      (equal? (bytevector->sint-list b (endianness little) 2)
              '(513 -253 513 513))))

  (pass-if 139 "bytevector->uint-list"
    (let ((b (u8-list->bytevector '(2 1 255 3 2 1 2 1))))
      (equal? (bytevector->uint-list b (endianness big) 2)
              '(513 65283 513 513))))

  (pass-if 140 "bytevector->uint-list [empty]"
    (let ((b (make-bytevector 0)))
      (null? (bytevector->uint-list b (endianness big) 2))))

  (pass-if 141 "{sint,uint}-list->bytevector"
    (let ((b1 (sint-list->bytevector '(513 -253 513 513)
                                     (endianness little) 2))
          (b2 (uint-list->bytevector '(513 65283 513 513)
                                     (endianness little) 2))
          (b3 (u8-list->bytevector '(1 2 3 255 1 2 1 2))))
      (and (bytevector=? b1 b2)
           (bytevector=? b2 b3))))

  (pass-if 142 "sint-list->bytevector [limits]"
           (bytevector=? (sint-list->bytevector '(-32768 32767)
                                                (endianness big) 2)
                         (let ((bv (make-bytevector 4)))
                           (bytevector-u8-set! bv 0 #x80)
                           (bytevector-u8-set! bv 1 #x00)
                           (bytevector-u8-set! bv 2 #x7f)
                           (bytevector-u8-set! bv 3 #xff)
                           bv))))

(with-test-prefix "2.5 Operations on 16-Bit Integers"

  (pass-if 143 "bytevector-u16-ref"
    (let ((b (u8-list->bytevector
              '(255 255 255 255 255 255 255 255
                255 255 255 255 255 255 255 253))))
      (and (equal? (bytevector-u16-ref b 14 (endianness little))
                   #xfdff)
           (equal? (bytevector-u16-ref b 14 (endianness big))
                   #xfffd))))

  (pass-if 144 "bytevector-s16-ref"
    (let ((b (u8-list->bytevector
              '(255 255 255 255 255 255 255 255
                255 255 255 255 255 255 255 253))))
      (and (equal? (bytevector-s16-ref b 14 (endianness little))
                   -513)
           (equal? (bytevector-s16-ref b 14 (endianness big))
                   -3))))

  (pass-if 145 "bytevector-s16-ref [unaligned]"
    (let ((b (u8-list->bytevector '(#xff #xf0 #xff))))
      (equal? (bytevector-s16-ref b 1 (endianness little))
	      -16)))

  (pass-if 146 "bytevector-{u16,s16}-ref"
    (let ((b (make-bytevector 2)))
      (bytevector-u16-set! b 0 44444 (endianness little))
      (and (equal? (bytevector-u16-ref b 0 (endianness little))
                   44444)
           (equal? (bytevector-s16-ref b 0 (endianness little))
                   (- 44444 65536)))))

  (pass-if 147 "bytevector-native-{u16,s16}-{ref,set!}"
    (let ((b (make-bytevector 2)))
      (bytevector-u16-native-set! b 0 44444)
      (and (equal? (bytevector-u16-native-ref b 0)
                   44444)
           (equal? (bytevector-s16-native-ref b 0)
                   (- 44444 65536)))))

  (pass-if 148 "bytevector-s16-{ref,set!} [unaligned]"
    (let ((b (make-bytevector 3)))
      (bytevector-s16-set! b 1 -77 (endianness little))
      (equal? (bytevector-s16-ref b 1 (endianness little))
	      -77))))

(with-test-prefix "2.6 Operations on 32-bit Integers"

  (pass-if 149 "bytevector-u32-ref"
    (let ((b (u8-list->bytevector
              '(255 255 255 255 255 255 255 255
                255 255 255 255 255 255 255 253))))
      (and (equal? (bytevector-u32-ref b 12 (endianness little))
                   #xfdffffff)
           (equal? (bytevector-u32-ref b 12 (endianness big))
                   #xfffffffd))))

  (pass-if 150 "bytevector-s32-ref"
    (let ((b (u8-list->bytevector
              '(255 255 255 255 255 255 255 255
                255 255 255 255 255 255 255 253))))
      (and (equal? (bytevector-s32-ref b 12 (endianness little))
                   -33554433)
           (equal? (bytevector-s32-ref b 12 (endianness big))
                   -3))))

  (pass-if 151 "bytevector-{u32,s32}-ref"
    (let ((b (make-bytevector 4)))
      (bytevector-u32-set! b 0 2222222222 (endianness little))
      (and (equal? (bytevector-u32-ref b 0 (endianness little))
                   2222222222)
           (equal? (bytevector-s32-ref b 0 (endianness little))
                   (- 2222222222 (expt 2 32))))))

  (pass-if 152 "bytevector-{u32,s32}-native-{ref,set!}"
    (let ((b (make-bytevector 4)))
      (bytevector-u32-native-set! b 0 2222222222)
      (and (equal? (bytevector-u32-native-ref b 0)
                   2222222222)
           (equal? (bytevector-s32-native-ref b 0)
                   (- 2222222222 (expt 2 32)))))))

(with-test-prefix "2.7 Operations on 64-bit Integers"

  (pass-if 153 "bytevector-u64-ref"
    (let ((b (u8-list->bytevector
              '(255 255 255 255 255 255 255 255
                255 255 255 255 255 255 255 253))))
      (and (equal? (bytevector-u64-ref b 8 (endianness little))
                   #xfdffffffffffffff)
           (equal? (bytevector-u64-ref b 8 (endianness big))
                   #xfffffffffffffffd))))

  (pass-if 154 "bytevector-s64-ref"
    (let ((b (u8-list->bytevector
              '(255 255 255 255 255 255 255 255
                255 255 255 255 255 255 255 253))))
      (and (equal? (bytevector-s64-ref b 8 (endianness little))
                   -144115188075855873)
           (equal? (bytevector-s64-ref b 8 (endianness big))
                   -3))))

  (pass-if 155 "bytevector-{u64,s64}-ref"
    (let ((b (make-bytevector 8))
          (big 9333333333333333333))
      (bytevector-u64-set! b 0 big (endianness little))
      (and (equal? (bytevector-u64-ref b 0 (endianness little))
                   big)
           (equal? (bytevector-s64-ref b 0 (endianness little))
                   (- big (expt 2 64))))))

  (pass-if 156 "bytevector-{u64,s64}-native-{ref,set!}"
    (let ((b (make-bytevector 8))
          (big 9333333333333333333))
      (bytevector-u64-native-set! b 0 big)
      (and (equal? (bytevector-u64-native-ref b 0)
                   big)
           (equal? (bytevector-s64-native-ref b 0)
                   (- big (expt 2 64))))))

  (pass-if 157 "ref/set! with zero"
     (let ((b (make-bytevector 8)))
       (bytevector-s64-set! b 0 -1 (endianness big))
       (bytevector-u64-set! b 0  0 (endianness big))
       (= 0 (bytevector-u64-ref b 0 (endianness big))))))

(with-test-prefix "2.8 Operations on IEEE-754 Representations"

  (pass-if 158 "single, little endian"
    ;; http://bugs.gnu.org/11310
    (let ((b (make-bytevector 4)))
      (bytevector-ieee-single-set! b 0 1.0 (endianness little))
      (equal? (u8-list->bytevector '(0 0 128 63)) b)))

  (pass-if 159 "single, big endian"
    ;; http://bugs.gnu.org/11310
    (let ((b (make-bytevector 4)))
      (bytevector-ieee-single-set! b 0 1.0 (endianness big))
      (equal? (u8-list->bytevector '(63 128 0 0)) b)))

  (pass-if 160 "bytevector-ieee-single-native-{ref,set!}"
    (let ((b (make-bytevector 4))
          (number 3.00))
      (bytevector-ieee-single-native-set! b 0 number)
      (equal? (bytevector-ieee-single-native-ref b 0)
              number)))

  (pass-if 161 "bytevector-ieee-single-{ref,set!}"
    (let ((b (make-bytevector 8))
          (number 3.14))
      (bytevector-ieee-single-set! b 0 number (endianness little))
      (bytevector-ieee-single-set! b 4 number (endianness big))
      (equal? (bytevector-ieee-single-ref b 0 (endianness little))
              (bytevector-ieee-single-ref b 4 (endianness big)))))

   (pass-if 162 "bytevector-ieee-single-{ref,set!} [unaligned]"
    (let ((b (make-bytevector 9))
          (number 3.14))
      (bytevector-ieee-single-set! b 1 number (endianness little))
      (bytevector-ieee-single-set! b 5 number (endianness big))
      (equal? (bytevector-ieee-single-ref b 1 (endianness little))
              (bytevector-ieee-single-ref b 5 (endianness big)))))

  (pass-if 163 "double, little endian"
    ;; http://bugs.gnu.org/11310
    (let ((b (make-bytevector 8)))
      (bytevector-ieee-double-set! b 0 1.0 (endianness little))
      (equal? (u8-list->bytevector '(0 0 0 0 0 0 240 63)) b)))

  (pass-if 164 "double, big endian"
    ;; http://bugs.gnu.org/11310
    (let ((b (make-bytevector 8)))
      (bytevector-ieee-double-set! b 0 1.0 (endianness big))
      (equal? (u8-list->bytevector '(63 240 0 0 0 0 0 0)) b)))

  (pass-if 165 "bytevector-ieee-double-native-{ref,set!}"
    (let ((b (make-bytevector 8))
          (number 3.14))
      (bytevector-ieee-double-native-set! b 0 number)
      (equal? (bytevector-ieee-double-native-ref b 0)
              number)))

  (pass-if 166 "bytevector-ieee-double-{ref,set!}"
    (let ((b (make-bytevector 16))
          (number 3.14))
      (bytevector-ieee-double-set! b 0 number (endianness little))
      (bytevector-ieee-double-set! b 8 number (endianness big))
      (equal? (bytevector-ieee-double-ref b 0 (endianness little))
              (bytevector-ieee-double-ref b 8 (endianness big))))))

(with-test-prefix "2.9 Operations on Strings"

  (pass-if 167 "string->utf8"
    (let* ((str  "hello, world")
           (utf8 (string->utf8 str)))
      (and (bytevector? utf8)
           (= (bytevector-length utf8)
              (string-length str))
           (equal? (string->list str)
                   (map integer->char (bytevector->u8-list utf8))))))

  (pass-if 168 "string->utf8 [latin-1]"
    (let* ((str  "hé, ça va bien ?")
           (utf8 (string->utf8 str)))
      (and (bytevector? utf8)
           (= (bytevector-length utf8)
              (+ 2 (string-length str))))))

  (pass-if 169 "string->utf16"
    (let* ((str   "hello, world")
           (utf16 (string->utf16 str)))
      (and (bytevector? utf16)
           (= (bytevector-length utf16)
              (* 2 (string-length str)))
           (equal? (string->list str)
                   (map integer->char
                        (bytevector->uint-list utf16
                                               (endianness big) 2))))))

  (pass-if 170 "string->utf16 [little]"
    (let* ((str   "hello, world")
           (utf16 (string->utf16 str (endianness little))))
      (and (bytevector? utf16)
           (= (bytevector-length utf16)
              (* 2 (string-length str)))
           (equal? (string->list str)
                   (map integer->char
                        (bytevector->uint-list utf16
                                               (endianness little) 2))))))


  (pass-if 171 "string->utf32"
    (let* ((str   "hello, world")
           (utf32 (string->utf32 str)))
      (and (bytevector? utf32)
           (= (bytevector-length utf32)
              (* 4 (string-length str)))
           (equal? (string->list str)
                   (map integer->char
                        (bytevector->uint-list utf32
                                               (endianness big) 4))))))

  (pass-if 172 "string->utf32 [Greek]"
    (let* ((str   "Ἄνεμοι")
           (utf32 (string->utf32 str)))
      (and (bytevector? utf32)
           (equal? (bytevector->uint-list utf32 (endianness big) 4)
                   '(#x1f0c #x3bd #x3b5 #x3bc #x3bf #x3b9)))))

  (pass-if 173 "string->utf32 [little]"
    (let* ((str   "hello, world")
           (utf32 (string->utf32 str (endianness little))))
      (and (bytevector? utf32)
           (= (bytevector-length utf32)
              (* 4 (string-length str)))
           (equal? (string->list str)
                   (map integer->char
                        (bytevector->uint-list utf32
                                               (endianness little) 4))))))

  (pass-if 174 "utf8->string"
    (let* ((utf8  (u8-list->bytevector (map char->integer
                                            (string->list "hello, world"))))
           (str   (utf8->string utf8)))
      (and (string? str)
           (= (string-length str)
              (bytevector-length utf8))
           (equal? (string->list str)
                   (map integer->char (bytevector->u8-list utf8))))))

  (pass-if 175 "utf8->string [latin-1]"
    (let* ((utf8  (string->utf8 "hé, ça va bien ?"))
           (str   (utf8->string utf8)))
      (and (string? str)
           (= (string-length str)
              (- (bytevector-length utf8) 2)))))

  (pass-if-equal 175.1 "utf8->string [replacement character]"
      '(104 105 65533)
    (map char->integer
         (string->list (utf8->string (u8-list->bytevector '(104 105 239 191 189))))))

  (pass-if 176 "utf16->string"
    (let* ((utf16  (uint-list->bytevector (map char->integer
                                               (string->list "hello, world"))
                                          (endianness big) 2))
           (str   (utf16->string utf16)))
      (and (string? str)
           (= (* 2 (string-length str))
              (bytevector-length utf16))
           (equal? (string->list str)
                   (map integer->char
                        (bytevector->uint-list utf16 (endianness big)
                                               2))))))

  (pass-if 177 "utf16->string [little]"
    (let* ((utf16  (uint-list->bytevector (map char->integer
                                               (string->list "hello, world"))
                                          (endianness little) 2))
           (str   (utf16->string utf16 (endianness little))))
      (and (string? str)
           (= (* 2 (string-length str))
              (bytevector-length utf16))
           (equal? (string->list str)
                   (map integer->char
                        (bytevector->uint-list utf16 (endianness little)
                                               2))))))
  (pass-if 178 "utf32->string"
    (let* ((utf32  (uint-list->bytevector (map char->integer
                                               (string->list "hello, world"))
                                          (endianness big) 4))
           (str   (utf32->string utf32)))
      (and (string? str)
           (= (* 4 (string-length str))
              (bytevector-length utf32))
           (equal? (string->list str)
                   (map integer->char
                        (bytevector->uint-list utf32 (endianness big)
                                               4))))))

  (pass-if 179 "utf32->string [little]"
    (let* ((utf32  (uint-list->bytevector (map char->integer
                                               (string->list "hello, world"))
                                          (endianness little) 4))
           (str   (utf32->string utf32 (endianness little))))
      (and (string? str)
           (= (* 4 (string-length str))
              (bytevector-length utf32))
           (equal? (string->list str)
                   (map integer->char
                        (bytevector->uint-list utf32 (endianness little)
                                               4)))))))


