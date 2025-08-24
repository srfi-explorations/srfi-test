
(test-begin "srfi-4")

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

(test-begin "1.1 Basic Bytevector Tests")

(test-equal (or (eq? (native-endianness) 'big)
                (eq? (native-endianness) 'little)) #t)

(test-equal (bytevector? (vector)) #f)
(test-equal (bytevector? (make-bytevector 3)) #t)

(test-equal (bytevector-length (make-bytevector 44)) 44)

(test-equal (let ((b1 (make-bytevector 16 127))
                  (b2 (make-bytevector 16 255)))
              (list
                (bytevector-s8-ref b1 0)
                (bytevector-u8-ref b1 0)
                (bytevector-s8-ref b2 0)
                (bytevector-u8-ref b2 0)))
            '(127 129 -1 255))

(test-equal (let ((b (make-bytevector 16 127)))
              (bytevector-s8-set! b 0 -126)
              (bytevector-u8-set! b 1 246)
              (list
                (bytevector-s8-ref b 0)
                (bytevector-u8-ref b 0)
                (bytevector-s8-ref b 1)
                (bytevector-u8-ref b 1)))
            '(-126 130 -10 246))

(let ()
  (define b (make-bytevector 16 127))
  (bytevector-uint-set! b 0 (- (expt 2 128) 3) (endianness little) 16)

  (test-equal (bytevector-uint-ref b 0 (endianness little) 16)
              #xfffffffffffffffffffffffffffffffd)

  (test-equal (bytevector-sint-ref b 0 (endianness little) 16) -3)

  (test-equal (bytevector->u8-list b)
              '(253 255 255 255 255 255 255 255
                255 255 255 255 255 255 255 255))

  (bytevector-uint-set! b 0 (- (expt 2 128) 3) (endianness big) 16)

  (test-equal (bytevector-uint-ref b 0 (endianness big) 16)
              #xfffffffffffffffffffffffffffffffd)

  (test-equal (bytevector-sint-ref b 0 (endianness big) 16) -3)

  (test-equal (bytevector->u8-list b)
              '(255 255 255 255 255 255 255 255
                255 255 255 255 255 255 255 )))

(let ()
  (define b
    (u8-list->bytevector
      '(255 255 255 255 255 255 255 255
        255 255 255 255 255 255 255 253)))

  (test-equal (bytevector-u16-ref b 14 (endianness little)) 65023)

  (test-equal (bytevector-s16-ref b 14 (endianness little)) -513)

  (test-equal (bytevector-u16-ref b 14 (endianness big)) 65533)

  (test-equal (bytevector-s16-ref b 14 (endianness big)) -3)

  (bytevector-u16-set! b 0 12345 (endianness little))

  (test-equal (bytevector-u16-ref b 0 (endianness little)) 12345)

  (bytevector-u16-native-set! b 0 12345)

  (test-equal (bytevector-u16-native-ref b 0) 12345))

(let ()
  (define b
    (u8-list->bytevector
      '(255 255 255 255 255 255 255 255
        255 255 255 255 255 255 255 253)))

  (test-equal (bytevector-u32-ref b 12 (endianness little)) 4261412863)

  (test-equal (bytevector-s32-ref b 12 (endianness little)) -33554433)

  (test-equal (bytevector-u32-ref b 12 (endianness big)) 4294967293)

  (test-equal (bytevector-s32-ref b 12 (endianness big)) -3))

(let ()
  (define b
    (u8-list->bytevector
      '(255 255 255 255 255 255 255 255
        255 255 255 255 255 255 255 253)))

  (test-equal (bytevector-u64-ref b 8 (endianness little))
              18302628885633695743)

  (test-equal (bytevector-s64-ref b 8 (endianness little))
              -144115188075855873)

  (test-equal (bytevector-u64-ref b 8 (endianness big))
              18446744073709551613)

  (test-equal (bytevector-s64-ref b 8 (endianness big)) -3))

(let ()
  (define b1 (u8-list->bytevector '(255 2 254 3 255)))
  (define b2 (u8-list->bytevector '(255 3 254 2 255)))
  (define b3 (u8-list->bytevector '(255 3 254 2 255)))
  (define b4 (u8-list->bytevector '(255 3 255)))

  (test-equal (bytevector=? b1 b2) #f)
  (test-equal (bytevector=? b2 b3) #t)
  (test-equal (bytevector=? b3 b4) #f)
  (test-equal (bytevector=? b4 b3) #f))

(let ()
  (define b
    (u8-list->bytevector
      '(63 240 0 0 0 0 0 0)))

  (test-equal (bytevector-ieee-single-ref b 4 'little) 0.0)

  (test-equal (bytevector-ieee-double-ref b 0 'big) 1.0)

  (bytevector-ieee-single-native-set! b 4 3.0)

  (test-equal (bytevector-ieee-single-native-ref b 4) 3.0)

  (bytevector-ieee-double-native-set! b 0 5.0)

  (test-equal (bytevector-ieee-double-native-ref b 0) 5.0)

  (bytevector-ieee-double-set! b 0 1.75 'big)

  (test-equal (bytevector->u8-list b) '(63 252 0 0 0 0 0 0))) ; depends on ieee routines

(let ((b (make-bytevector 7 12)))
  (bytevector-fill! b 127)
  (test-equal (bytevector->u8-list b) '(127 127 127 127 127 127 127)))

(let ((b (u8-list->bytevector '(1 2 3 4 5 6 7 8))))
  (bytevector-copy! b 0 b 3 4)
  (test-equal (bytevector->u8-list b) '(1 2 3 1 2 3 4 8))
  (test-equal (bytevector=? b (bytevector-copy b)) #t))

(let ((b (u8-list->bytevector '(1 2 3 255 1 2 1 2))))
  (test-equal (bytevector->sint-list b (endianness little) 2)
              '(513 -253 513 513))
  (test-equal (bytevector->uint-list b (endianness little) 2)
              '(513 65283 513 513)))

(test-end "1.1 Basic Bytevector Tests")

(test-begin "1.2 IEEE Bytevector Tests")

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

(test-equal (roundtrip
              pinfty
              bytevector-ieee-single-ref bytevector-ieee-single-set! 0 'big)
            pinfty)

(test-equal (roundtrip
              minfty
              bytevector-ieee-single-ref bytevector-ieee-single-set! 0 'big)
            minfty)

(test-equal (let ((x (roundtrip
                       nanval
                       bytevector-ieee-single-ref bytevector-ieee-single-set!
                       0 'big)))
              (= x x))
            #f)

(test-equal (roundtrip
              1e10
              bytevector-ieee-single-ref bytevector-ieee-single-set! 0 'big)
            1e10)

(test-equal (roundtrip
              -0.2822580337524414
              bytevector-ieee-single-ref bytevector-ieee-single-set! 0 'big)
            -0.2822580337524414)

; Single precision, offset 0, little-endian

(test-equal (roundtrip
              pinfty
              bytevector-ieee-single-ref bytevector-ieee-single-set! 0 'little)
            pinfty)

(test-equal (roundtrip
              minfty
              bytevector-ieee-single-ref bytevector-ieee-single-set! 0 'little)
            minfty)

(test-equal (let ((x (roundtrip
                       nanval
                       bytevector-ieee-single-ref bytevector-ieee-single-set!
                       0 'little)))
              (= x x))
            #f)

(test-equal (roundtrip
              1e10
              bytevector-ieee-single-ref bytevector-ieee-single-set! 0 'little)
            1e10)

(test-equal (roundtrip
              -0.2822580337524414
              bytevector-ieee-single-ref bytevector-ieee-single-set! 0 'little)
            -0.2822580337524414)

; Single precision, offset 1, big-endian

(test-equal (roundtrip
              pinfty
              bytevector-ieee-single-ref bytevector-ieee-single-set! 1 'big)
            pinfty)

(test-equal (roundtrip
              minfty
              bytevector-ieee-single-ref bytevector-ieee-single-set! 1 'big)
            minfty)

(test-equal (let ((x (roundtrip
                       nanval
                       bytevector-ieee-single-ref bytevector-ieee-single-set!
                       1 'big)))
              (= x x))
            #f)

(test-equal (roundtrip
              1e10
              bytevector-ieee-single-ref bytevector-ieee-single-set! 1 'big)
            1e10)

(test-equal (roundtrip
              -0.2822580337524414
              bytevector-ieee-single-ref bytevector-ieee-single-set! 1 'big)
            -0.2822580337524414)

; Single precision, offset 1, little-endian

(test-equal (roundtrip
              pinfty
              bytevector-ieee-single-ref bytevector-ieee-single-set! 1 'little)
            pinfty)

(test-equal (roundtrip
              minfty
              bytevector-ieee-single-ref bytevector-ieee-single-set! 1 'little)
            minfty)

(test-equal (let ((x (roundtrip
                       nanval
                       bytevector-ieee-single-ref bytevector-ieee-single-set!
                       1 'little)))
              (= x x))
            #f)

(test-equal (roundtrip
              1e10
              bytevector-ieee-single-ref bytevector-ieee-single-set! 1 'little)
            1e10)

(test-equal (roundtrip
              -0.2822580337524414
              bytevector-ieee-single-ref bytevector-ieee-single-set! 1 'little)
            -0.2822580337524414)

; Single precision, offset 2, big-endian

(test-equal (roundtrip
              pinfty
              bytevector-ieee-single-ref bytevector-ieee-single-set! 2 'big)
            pinfty)

(test-equal (roundtrip
              minfty
              bytevector-ieee-single-ref bytevector-ieee-single-set! 2 'big)
            minfty)

(test-equal (let ((x (roundtrip
                       nanval
                       bytevector-ieee-single-ref bytevector-ieee-single-set!
                       2 'big)))
              (= x x))
            #f)

(test-equal (roundtrip
              1e10
              bytevector-ieee-single-ref bytevector-ieee-single-set! 2 'big)
            1e10)

(test-equal (roundtrip
              -0.2822580337524414
              bytevector-ieee-single-ref bytevector-ieee-single-set! 2 'big)
            -0.2822580337524414)

; Single precision, offset 2, little-endian

(test-equal (roundtrip
              pinfty
              bytevector-ieee-single-ref bytevector-ieee-single-set! 2 'little)
            pinfty)

(test-equal (roundtrip
              minfty
              bytevector-ieee-single-ref bytevector-ieee-single-set! 2 'little)
            minfty)

(test-equal (let ((x (roundtrip
                       nanval
                       bytevector-ieee-single-ref bytevector-ieee-single-set!
                       2 'little)))
              (= x x))
            #f)

(test-equal (roundtrip
              1e10
              bytevector-ieee-single-ref bytevector-ieee-single-set! 2 'little)
            1e10)

(test-equal (roundtrip
              -0.2822580337524414
              bytevector-ieee-single-ref bytevector-ieee-single-set! 2 'little)
            -0.2822580337524414)

; Single precision, offset 3, big-endian

(test-equal (roundtrip
              pinfty
              bytevector-ieee-single-ref bytevector-ieee-single-set! 3 'big)
            pinfty)

(test-equal (roundtrip
              minfty
              bytevector-ieee-single-ref bytevector-ieee-single-set! 3 'big)
            minfty)

(test-equal (let ((x (roundtrip
                       nanval
                       bytevector-ieee-single-ref bytevector-ieee-single-set!
                       3 'big)))
              (= x x))
            #f)

(test-equal (roundtrip
              1e10
              bytevector-ieee-single-ref bytevector-ieee-single-set! 3 'big)
            1e10)

(test-equal (roundtrip
              -0.2822580337524414
              bytevector-ieee-single-ref bytevector-ieee-single-set! 3 'big)
            -0.2822580337524414)

; Single precision, offset 3, little-endian

(test-equal (roundtrip
              pinfty
              bytevector-ieee-single-ref bytevector-ieee-single-set! 3 'little)
            pinfty)

(test-equal (roundtrip
              minfty
              bytevector-ieee-single-ref bytevector-ieee-single-set! 3 'little)
            minfty)

(test-equal (let ((x (roundtrip
                       nanval
                       bytevector-ieee-single-ref bytevector-ieee-single-set!
                       3 'little)))
              (= x x))
            #f)

(test-equal (roundtrip
              1e10
              bytevector-ieee-single-ref bytevector-ieee-single-set! 3 'little)
            1e10)

(test-equal (roundtrip
              -0.2822580337524414
              bytevector-ieee-single-ref bytevector-ieee-single-set! 3 'little)
            -0.2822580337524414)

; Double precision, offset 0, big-endian

(test-equal (roundtrip
              pinfty
              bytevector-ieee-double-ref bytevector-ieee-double-set! 0 'big)
            pinfty)

(test-equal (roundtrip
              minfty
              bytevector-ieee-double-ref bytevector-ieee-double-set! 0 'big)
            minfty)

(test-equal (let ((x (roundtrip
                       nanval
                       bytevector-ieee-double-ref bytevector-ieee-double-set!
                       0 'big)))
              (= x x))
            #f)

(test-equal (roundtrip
              1e10
              bytevector-ieee-double-ref bytevector-ieee-double-set! 0 'big)
            1e10)

(test-equal (roundtrip
              -0.2822580337524414
              bytevector-ieee-double-ref bytevector-ieee-double-set! 0 'big)
            -0.2822580337524414)

; Double precision, offset 0, little-endian

(test-equal (roundtrip
              pinfty
              bytevector-ieee-double-ref bytevector-ieee-double-set! 0 'little)
            pinfty)

(test-equal (roundtrip
              minfty
              bytevector-ieee-double-ref bytevector-ieee-double-set! 0 'little)
            minfty)

(test-equal (let ((x (roundtrip
                       nanval
                       bytevector-ieee-double-ref bytevector-ieee-double-set!
                       0 'little)))
              (= x x))
            #f)

(test-equal (roundtrip
              1e10
              bytevector-ieee-double-ref bytevector-ieee-double-set! 0 'little)
            1e10)

(test-equal (roundtrip
              -0.2822580337524414
              bytevector-ieee-double-ref bytevector-ieee-double-set! 0 'little)
            -0.2822580337524414)

; Double precision, offset 1, big-endian

(test-equal (roundtrip
              pinfty
              bytevector-ieee-double-ref bytevector-ieee-double-set! 1 'big)
            pinfty)

(test-equal (roundtrip
              minfty
              bytevector-ieee-double-ref bytevector-ieee-double-set! 1 'big)
            minfty)

(test-equal (let ((x (roundtrip
                       nanval
                       bytevector-ieee-double-ref bytevector-ieee-double-set!
                       1 'big)))
              (= x x))
            #f)

(test-equal (roundtrip
              1e10
              bytevector-ieee-double-ref bytevector-ieee-double-set! 1 'big)
            1e10)

(test-equal (roundtrip
              -0.2822580337524414
              bytevector-ieee-double-ref bytevector-ieee-double-set! 1 'big)
            -0.2822580337524414)

; Double precision, offset 1, little-endian

(test-equal (roundtrip
              pinfty
              bytevector-ieee-double-ref bytevector-ieee-double-set! 1 'little)
            pinfty)

(test-equal (roundtrip
              minfty
              bytevector-ieee-double-ref bytevector-ieee-double-set! 1 'little)
            minfty)

(test-equal (let ((x (roundtrip
                       nanval
                       bytevector-ieee-double-ref bytevector-ieee-double-set!
                       1 'little)))
              (= x x))
            #f)

(test-equal (roundtrip
              1e10
              bytevector-ieee-double-ref bytevector-ieee-double-set! 1 'little)
            1e10)

(test-equal (roundtrip
              -0.2822580337524414
              bytevector-ieee-double-ref bytevector-ieee-double-set! 1 'little)
            -0.2822580337524414)

; Double precision, offset 2, big-endian

(test-equal (roundtrip
              pinfty
              bytevector-ieee-double-ref bytevector-ieee-double-set! 2 'big)
            pinfty)

(test-equal (roundtrip
              minfty
              bytevector-ieee-double-ref bytevector-ieee-double-set! 2 'big)
            minfty)

(test-equal (let ((x (roundtrip
                       nanval
                       bytevector-ieee-double-ref bytevector-ieee-double-set!
                       2 'big)))
              (= x x))
            #f)

(test-equal (roundtrip
              1e10
              bytevector-ieee-double-ref bytevector-ieee-double-set! 2 'big)
            1e10)

(test-equal (roundtrip
              -0.2822580337524414
              bytevector-ieee-double-ref bytevector-ieee-double-set! 2 'big)
            -0.2822580337524414)

; Double precision, offset 2, little-endian

(test-equal (roundtrip
              pinfty
              bytevector-ieee-double-ref bytevector-ieee-double-set! 2 'little)
            pinfty)

(test-equal (roundtrip
              minfty
              bytevector-ieee-double-ref bytevector-ieee-double-set! 2 'little)
            minfty)

(test-equal (let ((x (roundtrip
                       nanval
                       bytevector-ieee-double-ref bytevector-ieee-double-set!
                       2 'little)))
              (= x x))
            #f)

(test-equal (roundtrip
              1e10
              bytevector-ieee-double-ref bytevector-ieee-double-set! 2 'little)
            1e10)

(test-equal (roundtrip
              -0.2822580337524414
              bytevector-ieee-double-ref bytevector-ieee-double-set! 2 'little)
            -0.2822580337524414)

; Double precision, offset 3, big-endian

(test-equal (roundtrip
              pinfty
              bytevector-ieee-double-ref bytevector-ieee-double-set! 3 'big)
            pinfty)

(test-equal (roundtrip
              minfty
              bytevector-ieee-double-ref bytevector-ieee-double-set! 3 'big)
            minfty)

(test-equal (let ((x (roundtrip
                       nanval
                       bytevector-ieee-double-ref bytevector-ieee-double-set!
                       3 'big)))
              (= x x))
            #f)

(test-equal (roundtrip
              1e10
              bytevector-ieee-double-ref bytevector-ieee-double-set! 3 'big)
            1e10)

(test-equal (roundtrip
              -0.2822580337524414
              bytevector-ieee-double-ref bytevector-ieee-double-set! 3 'big)
            -0.2822580337524414)

; Double precision, offset 3, little-endian

(test-equal (roundtrip
              pinfty
              bytevector-ieee-double-ref bytevector-ieee-double-set! 3 'little)
            pinfty)

(test-equal (roundtrip
              minfty
              bytevector-ieee-double-ref bytevector-ieee-double-set! 3 'little)
            minfty)

(test-equal (let ((x (roundtrip
                       nanval
                       bytevector-ieee-double-ref bytevector-ieee-double-set!
                       3 'little)))
              (= x x))
            #f)

(test-equal (roundtrip
              1e10
              bytevector-ieee-double-ref bytevector-ieee-double-set! 3 'little)
            1e10)

(test-equal (roundtrip
              -0.2822580337524414
              bytevector-ieee-double-ref bytevector-ieee-double-set! 3 'little)
            -0.2822580337524414)

(test-end "1.2 ieee bytevector tests")


(test-begin "2.2 General Operations")

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
             (newline)))))

(test-assert "native-endianness"
             (not (not (memq (native-endianness) '(big little)))))

(test-assert "make-bytevector"
             (and (bytevector? (make-bytevector 20))
                  (bytevector? (make-bytevector 20 3))))

(test-assert "bytevector-length"
             (= (bytevector-length (make-bytevector 20)) 20))

(test-assert "bytevector=?"
             (and (bytevector=? (make-bytevector 20 7)
                                (make-bytevector 20 7))
                  (not (bytevector=? (make-bytevector 20 7)
                                     (make-bytevector 20 0)))))

;; This failed prior to Guile 2.0.12.
;; See <http://bugs.gnu.org/19027>.
(test-assert "bytevector-fill! with fill 255"
             (u8-list->bytevector '(255 255 255 255))
             (let ((bv (make-bytevector 4)))
               (bytevector-fill! bv 255)
               bv))

(test-assert "bytevector-copy! overlapping"
             ;; See <http://debbugs.gnu.org/10070>.
             (let ((b (u8-list->bytevector '(1 2 3 4 5 6 7 8))))
               (bytevector-copy! b 0 b 3 4)
               (bytevector->u8-list b)
               (bytevector=? b (u8-list->bytevector '(1 2 3 1 2 3 4 8)))))

(test-end "2.2 General Operations")

(test-begin "2.3 Operations on Bytes and Octets")

(test-assert "bytevector-{u8,s8}-ref"
             (equal? '(127 129 -1 255)
                     (let ((b1 (make-bytevector 16 127))
                           (b2 (make-bytevector 16 255)))
                       (list (bytevector-s8-ref b1 0)
                             (bytevector-u8-ref b1 0)
                             (bytevector-s8-ref b2 0)
                             (bytevector-u8-ref b2 0)))))

(pass-if 128 "bytevector-{u8,s8}-set!"
         (equal? '(-126 130 -10 246)
                 (let ((b (make-bytevector 16 127)))

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
           (equal? bv1 bv2)))

(test-end "2.3 Operations on Bytes and Octets")

(test-begin "2.4 Operations on Integers of Arbitrary Size")

(test-assert "bytevector->sint-list"
             (let ((b (u8-list->bytevector '(1 2 3 255 1 2 1 2))))
               (equal? (bytevector->sint-list b (endianness little) 2)
                       '(513 -253 513 513))))

(test-assert "bytevector->uint-list"
             (let ((b (u8-list->bytevector '(2 1 255 3 2 1 2 1))))
               (equal? (bytevector->uint-list b (endianness big) 2)
                       '(513 65283 513 513))))

(test-assert "bytevector->uint-list [empty]"
             (let ((b (make-bytevector 0)))
               (null? (bytevector->uint-list b (endianness big) 2))))

(test-assert "{sint,uint}-list->bytevector"
             (let ((b1 (sint-list->bytevector '(513 -253 513 513)
                                              (endianness little) 2))
                   (b2 (uint-list->bytevector '(513 65283 513 513)
                                              (endianness little) 2))
                   (b3 (u8-list->bytevector '(1 2 3 255 1 2 1 2))))
               (and (bytevector=? b1 b2)
                    (bytevector=? b2 b3))))

(test-assert "sint-list->bytevector [limits]"
             (bytevector=? (sint-list->bytevector '(-32768 32767)
                                                  (endianness big) 2)
                           (let ((bv (make-bytevector 4)))
                             (bytevector-u8-set! bv 0 #x80)
                             (bytevector-u8-set! bv 1 #x00)
                             (bytevector-u8-set! bv 2 #x7f)
                             (bytevector-u8-set! bv 3 #xff)
                             bv)))

(test-end "2.4 Operations on Integers of Arbitrary Size")

(test-begin "2.5 Operations on 16-Bit Integers")

(test-assert "bytevector-u16-ref"
             (let ((b (u8-list->bytevector
                        '(255 255 255 255 255 255 255 255
                          255 255 255 255 255 255 255 253))))
               (and (equal? (bytevector-u16-ref b 14 (endianness little))
                            #xfdff)
                    (equal? (bytevector-u16-ref b 14 (endianness big))
                            #xfffd))))

(test-assert "bytevector-s16-ref"
             (let ((b (u8-list->bytevector
                        '(255 255 255 255 255 255 255 255
                          255 255 255 255 255 255 255 253))))
               (and (equal? (bytevector-s16-ref b 14 (endianness little))
                            -513)
                    (equal? (bytevector-s16-ref b 14 (endianness big))
                            -3))))

(test-assert "bytevector-s16-ref [unaligned]"
             (let ((b (u8-list->bytevector '(#xff #xf0 #xff))))
               (equal? (bytevector-s16-ref b 1 (endianness little))
                       -16)))

(test-assert "bytevector-{u16,s16}-ref"
             (let ((b (make-bytevector 2)))
               (bytevector-u16-set! b 0 44444 (endianness little))
               (and (equal? (bytevector-u16-ref b 0 (endianness little))
                            44444)
                    (equal? (bytevector-s16-ref b 0 (endianness little))
                            (- 44444 65536)))))

(test-assert "bytevector-native-{u16,s16}-{ref,set!}"
             (let ((b (make-bytevector 2)))
               (bytevector-u16-native-set! b 0 44444)
               (and (equal? (bytevector-u16-native-ref b 0)
                            44444)
                    (equal? (bytevector-s16-native-ref b 0)
                            (- 44444 65536)))))

(test-assert "bytevector-s16-{ref,set!} [unaligned]"
             (let ((b (make-bytevector 3)))
               (bytevector-s16-set! b 1 -77 (endianness little))
               (equal? (bytevector-s16-ref b 1 (endianness little))
                       -77)))

(test-end "2.5 Operations on 16-Bit Integers")

(test-begin "2.6 Operations on 32-bit Integers")

(test-assert "bytevector-u32-ref"
             (let ((b (u8-list->bytevector
                        '(255 255 255 255 255 255 255 255
                          255 255 255 255 255 255 255 253))))
               (and (equal? (bytevector-u32-ref b 12 (endianness little))
                            #xfdffffff)
                    (equal? (bytevector-u32-ref b 12 (endianness big))
                            #xfffffffd))))

(test-assert "bytevector-s32-ref"
             (let ((b (u8-list->bytevector
                        '(255 255 255 255 255 255 255 255
                          255 255 255 255 255 255 255 253))))
               (and (equal? (bytevector-s32-ref b 12 (endianness little))
                            -33554433)
                    (equal? (bytevector-s32-ref b 12 (endianness big))
                            -3))))

(test-assert "bytevector-{u32,s32}-ref"
             (let ((b (make-bytevector 4)))
               (bytevector-u32-set! b 0 2222222222 (endianness little))
               (and (equal? (bytevector-u32-ref b 0 (endianness little))
                            2222222222)
                    (equal? (bytevector-s32-ref b 0 (endianness little))
                            (- 2222222222 (expt 2 32))))))

(test-assert "bytevector-{u32,s32}-native-{ref,set!}"
             (let ((b (make-bytevector 4)))
               (bytevector-u32-native-set! b 0 2222222222)
               (and (equal? (bytevector-u32-native-ref b 0)
                            2222222222)
                    (equal? (bytevector-s32-native-ref b 0)
                            (- 2222222222 (expt 2 32))))))

(test-end "2.6 Operations on 32-bit Integers")

(test-begin "2.7 Operations on 64-bit Integers")

(test-assert "bytevector-u64-ref"
             (let ((b (u8-list->bytevector
                        '(255 255 255 255 255 255 255 255
                          255 255 255 255 255 255 255 253))))
               (and (equal? (bytevector-u64-ref b 8 (endianness little))
                            #xfdffffffffffffff)
                    (equal? (bytevector-u64-ref b 8 (endianness big))
                            #xfffffffffffffffd))))

(test-assert "bytevector-s64-ref"
             (let ((b (u8-list->bytevector
                        '(255 255 255 255 255 255 255 255
                          255 255 255 255 255 255 255 253))))
               (and (equal? (bytevector-s64-ref b 8 (endianness little))
                            -144115188075855873)
                    (equal? (bytevector-s64-ref b 8 (endianness big))
                            -3))))

(test-assert "bytevector-{u64,s64}-ref"
             (let ((b (make-bytevector 8))
                   (big 9333333333333333333))
               (bytevector-u64-set! b 0 big (endianness little))
               (and (equal? (bytevector-u64-ref b 0 (endianness little))
                            big)
                    (equal? (bytevector-s64-ref b 0 (endianness little))
                            (- big (expt 2 64))))))

(test-assert "bytevector-{u64,s64}-native-{ref,set!}"
             (let ((b (make-bytevector 8))
                   (big 9333333333333333333))
               (bytevector-u64-native-set! b 0 big)
               (and (equal? (bytevector-u64-native-ref b 0)
                            big)
                    (equal? (bytevector-s64-native-ref b 0)
                            (- big (expt 2 64))))))

(test-assert "ref/set! with zero"
             (let ((b (make-bytevector 8)))
               (bytevector-s64-set! b 0 -1 (endianness big))
               (bytevector-u64-set! b 0  0 (endianness big))
               (= 0 (bytevector-u64-ref b 0 (endianness big)))))

(test-end "2.7 Operations on 64-bit Integers")

(test-begin "2.8 Operations on IEEE-754 Representations")

(test-assert "single, little endian"
             ;; http://bugs.gnu.org/11310
             (let ((b (make-bytevector 4)))
               (bytevector-ieee-single-set! b 0 1.0 (endianness little))
               (equal? (u8-list->bytevector '(0 0 128 63)) b)))

(test-assert "single, big endian"
             ;; http://bugs.gnu.org/11310
             (let ((b (make-bytevector 4)))
               (bytevector-ieee-single-set! b 0 1.0 (endianness big))
               (equal? (u8-list->bytevector '(63 128 0 0)) b)))

(test-assert "bytevector-ieee-single-native-{ref,set!}"
             (let ((b (make-bytevector 4))
                   (number 3.00))
               (bytevector-ieee-single-native-set! b 0 number)
               (equal? (bytevector-ieee-single-native-ref b 0)
                       number)))

(test-assert "bytevector-ieee-single-{ref,set!}"
             (let ((b (make-bytevector 8))
                   (number 3.14))
               (bytevector-ieee-single-set! b 0 number (endianness little))
               (bytevector-ieee-single-set! b 4 number (endianness big))
               (equal? (bytevector-ieee-single-ref b 0 (endianness little))
                       (bytevector-ieee-single-ref b 4 (endianness big)))))

(test-assert "bytevector-ieee-single-{ref,set!} [unaligned]"
             (let ((b (make-bytevector 9))
                   (number 3.14))
               (bytevector-ieee-single-set! b 1 number (endianness little))
               (bytevector-ieee-single-set! b 5 number (endianness big))
               (equal? (bytevector-ieee-single-ref b 1 (endianness little))
                       (bytevector-ieee-single-ref b 5 (endianness big)))))

(test-assert "double, little endian"
             ;; http://bugs.gnu.org/11310
             (let ((b (make-bytevector 8)))
               (bytevector-ieee-double-set! b 0 1.0 (endianness little))
               (equal? (u8-list->bytevector '(0 0 0 0 0 0 240 63)) b)))

(test-assert "double, big endian"
             ;; http://bugs.gnu.org/11310
             (let ((b (make-bytevector 8)))
               (bytevector-ieee-double-set! b 0 1.0 (endianness big))
               (equal? (u8-list->bytevector '(63 240 0 0 0 0 0 0)) b)))

(test-assert "bytevector-ieee-double-native-{ref,set!}"
             (let ((b (make-bytevector 8))
                   (number 3.14))
               (bytevector-ieee-double-native-set! b 0 number)
               (equal? (bytevector-ieee-double-native-ref b 0)
                       number)))

(test-assert "bytevector-ieee-double-{ref,set!}"
             (let ((b (make-bytevector 16))
                   (number 3.14))
               (bytevector-ieee-double-set! b 0 number (endianness little))
               (bytevector-ieee-double-set! b 8 number (endianness big))
               (equal? (bytevector-ieee-double-ref b 0 (endianness little))
                       (bytevector-ieee-double-ref b 8 (endianness big)))))

(test-end "2.8 Operations on IEEE-754 Representations")

(test-begin "2.9 Operations on Strings")

(test-assert "string->utf8"
             (let* ((str  "hello, world")
                    (utf8 (string->utf8 str)))
               (and (bytevector? utf8)
                    (= (bytevector-length utf8)
                       (string-length str))
                    (equal? (string->list str)
                            (map integer->char (bytevector->u8-list utf8))))))

(test-assert "string->utf8 [latin-1]"
             (let* ((str  "hé, ça va bien ?")
                    (utf8 (string->utf8 str)))
               (and (bytevector? utf8)
                    (= (bytevector-length utf8)
                       (+ 2 (string-length str))))))

(test-assert "string->utf16"
             (let* ((str   "hello, world")
                    (utf16 (string->utf16 str)))
               (and (bytevector? utf16)
                    (= (bytevector-length utf16)
                       (* 2 (string-length str)))
                    (equal? (string->list str)
                            (map integer->char
                                 (bytevector->uint-list utf16
                                                        (endianness big) 2))))))

(test-assert "string->utf16 [little]"
             (let* ((str   "hello, world")
                    (utf16 (string->utf16 str (endianness little))))
               (and (bytevector? utf16)
                    (= (bytevector-length utf16)
                       (* 2 (string-length str)))
                    (equal? (string->list str)
                            (map integer->char
                                 (bytevector->uint-list utf16
                                                        (endianness little) 2))))))


(test-assert "string->utf32"
             (let* ((str   "hello, world")
                    (utf32 (string->utf32 str)))
               (and (bytevector? utf32)
                    (= (bytevector-length utf32)
                       (* 4 (string-length str)))
                    (equal? (string->list str)
                            (map integer->char
                                 (bytevector->uint-list utf32
                                                        (endianness big) 4))))))

(test-assert "string->utf32 [Greek]"
             (let* ((str   "Ἄνεμοι")
                    (utf32 (string->utf32 str)))
               (and (bytevector? utf32)
                    (equal? (bytevector->uint-list utf32 (endianness big) 4)
                            '(#x1f0c #x3bd #x3b5 #x3bc #x3bf #x3b9)))))

(test-assert "string->utf32 [little]"
             (let* ((str   "hello, world")
                    (utf32 (string->utf32 str (endianness little))))
               (and (bytevector? utf32)
                    (= (bytevector-length utf32)
                       (* 4 (string-length str)))
                    (equal? (string->list str)
                            (map integer->char
                                 (bytevector->uint-list utf32
                                                        (endianness little) 4))))))

(test-assert "utf8->string"
             (let* ((utf8  (u8-list->bytevector (map char->integer
                                                     (string->list "hello, world"))))
                    (str   (utf8->string utf8)))
               (and (string? str)
                    (= (string-length str)
                       (bytevector-length utf8))
                    (equal? (string->list str)
                            (map integer->char (bytevector->u8-list utf8))))))

(test-assert "utf8->string [latin-1]"
             (let* ((utf8  (string->utf8 "hé, ça va bien ?"))
                    (str   (utf8->string utf8)))
               (and (string? str)
                    (= (string-length str)
                       (- (bytevector-length utf8) 2)))))

(test-assert "utf8->string [replacement character]"
             '(104 105 65533)
             (map char->integer
                  (string->list (utf8->string (u8-list->bytevector '(104 105 239 191 189))))))

(test-assert "utf16->string"
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

(test-assert "utf16->string [little]"
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
(test-assert "utf32->string"
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

(test-assert "utf32->string [little]"
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
                                                        4))))))

(test-end "2.9 Operations on Strings")


(test-end "srfi-4")
