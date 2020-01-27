;; Copyright 2019 John Cowan
;; SPDX-License-Identifier: MIT

(test-begin "srfi-160")

;;;; Shared tests
;;; Hvector = homogeneous vector

;; Test for sameness

(define relerr (expt 2 -24))
(define (inexact-real? x) (and (number? x) (inexact? x) (real? x)))
(define (inexact-complex? x) (and (number? x) (inexact? x) (not (real? x))))
(define (realify z) (* (real-part z) (imag-part z)))

(define (same? result expected)
  (cond
    ((and (inexact-real? result) (inexact-real? expected))
     (let ((abserr (abs (* expected relerr))))
       (<= (- expected abserr) result (+ expected abserr))))
    ((and (inexact-complex? result) (inexact-complex? expected))
     (let ((abserr (abs (* (realify expected) relerr))))
       (<= (- (realify expected) abserr)
           (realify result)
           (+ (realify expected) abserr))))
    ((and (number? result) (number? expected))
     (= result expected))
    ((and (pair? result) (pair? expected))
     (list-same? result expected))
    (else
      (equal? result expected))))

 (define (list-same? result expected)
  (cond
    ((and (null? result) (null? expected))
     #t)
    ((and (pair? result) (pair? expected))
     (and (same? (car result) (car expected))
          (list-same? (cdr result) (cdr expected))))
    (else
     #f)))

(define (create label value)
  value)

(define (test-element-type
         tag make-Hvector Hvector Hvector? Hvector-length
         Hvector-ref Hvector-set! Hvector->list list->Hvector)
  (display "STARTING ")
  (display tag)
  (display "vector TESTS:")
  (newline)
  (let* ((first 32.0)
         (second 32.0+47.0i)
         (third -47.0i)
         (vec0 (make-Hvector 3))
         (vec1 (make-Hvector 3 second))
         (vec2 (Hvector first second third))
         (vec3 (list->Hvector (list third second first))))
    (test-equal #t (Hvector? vec0))
    (test-equal #t (Hvector? vec1))
    (test-equal #t (Hvector? vec2))
    (test-equal #t (Hvector? vec3))
    (test-equal 3 (Hvector-length vec0))
    (test-equal 3 (Hvector-length vec1))
    (test-equal 3 (Hvector-length vec2))
    (test-equal 3 (Hvector-length vec3))
    (Hvector-set! vec0 0 second)
    (Hvector-set! vec0 1 third)
    (Hvector-set! vec0 2 first)
    (test-equal second (Hvector-ref vec0 0))
    (test-equal third  (Hvector-ref vec0 1))
    (test-equal first  (Hvector-ref vec0 2))
    (test-equal second (Hvector-ref vec1 0))
    (test-equal second (Hvector-ref vec1 1))
    (test-equal second (Hvector-ref vec1 2))
    (test-equal first  (Hvector-ref vec2 0))
    (test-equal second (Hvector-ref vec2 1))
    (test-equal third  (Hvector-ref vec2 2))
    (test-equal third  (Hvector-ref vec3 0))
    (test-equal second (Hvector-ref vec3 1))
    (test-equal first  (Hvector-ref vec3 2))
    (test-equal (list second third first)   (Hvector->list vec0))
    (test-equal (list second second second) (Hvector->list vec1))
    (test-equal (list first second third)   (Hvector->list vec2))
    (test-equal (list third second first)   (Hvector->list vec3))))

(test-element-type
 'c64 make-c64vector c64vector c64vector? c64vector-length
 c64vector-ref c64vector-set! c64vector->list list->c64vector)

(test-element-type
 'c128 make-c128vector c128vector c128vector? c128vector-length
 c128vector-ref c128vector-set! c128vector->list list->c128vector)

(define-syntax integral-tests
  (syntax-rules ()
    ((integral-tests pred lo hi)
     (begin
       (test-assert (not (pred 1/2)))
       (test-assert (not (pred 1.0)))
       (test-assert (not (pred 1+2i)))
       (test-assert (not (pred 1.0+2.0i)))
       (test-assert (pred 0))
       (test-assert (pred hi))
       (test-assert (pred lo))
       (test-assert (not (pred (+ hi 1))))
       (test-assert (not (pred (- lo 1))))))))

(display "STARTING @? TESTS")
(newline)

(integral-tests u8? 0 255)
(integral-tests s8? -128 127)
(integral-tests u16? 0 65535)
(integral-tests s16? -32768 32767)
(integral-tests u32? 0 4294967295)
(integral-tests s32? -2147483648 2147483647)
(integral-tests u64? 0 18446744073709551615)
(integral-tests s64? -9223372036854775808 9223372036854775807)

(test-assert (f32? 1.0))
(test-assert (not (f32? 1)))
(test-assert (not (f32? 1.0+2.0i)))

(test-assert (f64? 1.0))
(test-assert (not (f64? 1)))
(test-assert (not (f64? 1.0+2.0i)))

(test-assert (c64? 1.0))
(test-assert (not (c64? 1)))
(test-assert (c64? 1.0+2.0i))

(test-assert (c128? 1.0))
(test-assert (not (c128? 1)))
(test-assert (c128? 1.0+2.0i))

(test-end "srfi-160")
