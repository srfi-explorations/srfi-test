;; Copyright 2020 Lassi Kortela
;; SPDX-License-Identifier: MIT

(test-begin "srfi-39")

(let ((counter (make-parameter 123)))
  (test-eq counter counter)
  (test-equal 123 (counter))
  (parameterize ((counter (+ 1 (counter))))
    (test-equal 124 (counter))
    (test-equal 125 (+ 1 (counter)))
    (counter 567)
    (test-equal 567 (counter)))
  (test-equal 123 (counter))
  (parameterize ((counter (+ (counter) (counter))))
    (test-equal 246 (counter)))
  (test-equal 123 (counter))
  (counter (parameterize ((counter (+ (counter) (counter))))
             (test-equal 246 (counter))
             (counter)))
  (test-equal 246 (counter)))

(let ((fahrenheit->celsius (lambda (f) (* 5/9 (- f 32)))))
  (define celsius (make-parameter 0 fahrenheit->celsius))
  (test-equal (fahrenheit->celsius 0) (celsius))
  (celsius 40)
  (test-equal (fahrenheit->celsius 40) (celsius)))

(test-end "srfi-39")
