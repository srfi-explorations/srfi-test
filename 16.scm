;; Copyright 2015 Alex Shinn
;; SPDX-License-Identifier: MIT

(test-begin "srfi-16")

(define plus
  (case-lambda
    (() 0)
    ((x) x)
    ((x y) (+ x y))
    ((x y z) (+ (+ x y) z))
    (args (apply + args))))

(define print
  (case-lambda
    (()
     (display ""))
    ((arg)
     (display arg))
    ((arg . args)
     (display arg)
     (display " ")
     (apply print args))))

(define (print-to-string . args)
  (let ((out (open-output-string))
        (old-out (current-output-port)))
    (dynamic-wind
      (lambda () (current-output-port out))
      (lambda () (apply print args))
      (lambda () (current-output-port old-out)))
    (get-output-string out)))

(test-eqv 0 (plus))
(test-eqv 1 (plus 1))
(test-eqv 6 (plus 1 2 3))
(test-error ((case-lambda ((a) a) ((a b) (* a b))) 1 2 3))

(test-equal "" (print-to-string))
(test-equal "hi" (print-to-string 'hi))
(test-equal "hi there world" (print-to-string 'hi 'there 'world))

(test-end "srfi-16")
