;; Copyright 1999 Oleg Kiselyov
;; Copyright 2012 Alex Shinn
;; SPDX-License-Identifier: MIT

(test-begin "srfi-2")

(test-equal 1 (and-let* () 1))
(test-equal 2 (and-let* () 1 2))
(test-equal #t (and-let* () ))
(test-equal #f (let ((x #f)) (and-let* (x))))
(test-equal 1 (let ((x 1)) (and-let* (x))))
(test-equal #f (and-let* ((x #f)) ))
(test-equal 1 (and-let* ((x 1)) ))
;; (test-syntax-error (and-let* ( #f (x 1))))
(test-equal #f (and-let* ( (#f) (x 1)) ))
;; (test-syntax-error (and-let* (2 (x 1))))
(test-equal 1 (and-let* ( (2) (x 1)) ))
(test-equal 2 (and-let* ( (x 1) (2)) ))
(test-equal #f (let ((x #f)) (and-let* (x) x)))
(test-equal "" (let ((x "")) (and-let* (x) x)))
(test-equal "" (let ((x "")) (and-let* (x)  )))
(test-equal 2 (let ((x 1)) (and-let* (x) (+ x 1))))
(test-equal #f (let ((x #f)) (and-let* (x) (+ x 1))))
(test-equal 2 (let ((x 1)) (and-let* (((positive? x))) (+ x 1))))
(test-equal #t (let ((x 1)) (and-let* (((positive? x))) )))
(test-equal #f (let ((x 0)) (and-let* (((positive? x))) (+ x 1))))
(test-equal 3  (let ((x 1)) (and-let* (((positive? x)) (x (+ x 1))) (+ x 1))))
(test-equal 4
  (let ((x 1))
    (and-let* (((positive? x)) (x (+ x 1)) (x (+ x 1))) (+ x 1))))
(test-equal 2 (let ((x 1)) (and-let* (x ((positive? x))) (+ x 1))))
(test-equal 2 (let ((x 1)) (and-let* ( ((begin x)) ((positive? x))) (+ x 1))))
(test-equal #f (let ((x 0)) (and-let* (x ((positive? x))) (+ x 1))))
(test-equal #f (let ((x #f)) (and-let* (x ((positive? x))) (+ x 1))))
(test-equal #f
  (let ((x #f)) (and-let* ( ((begin x)) ((positive? x))) (+ x 1))))

(test-equal #f
  (let ((x 1)) (and-let* (x (y (- x 1)) ((positive? y))) (/ x y))))
(test-equal #f
  (let ((x 0)) (and-let* (x (y (- x 1)) ((positive? y))) (/ x y))))
(test-equal #f
  (let ((x #f)) (and-let* (x (y (- x 1)) ((positive? y))) (/ x y))))
(test-equal 3/2
  (let ((x 3)) (and-let* (x (y (- x 1)) ((positive? y))) (/ x y))))

(test-end "srfi-2")
