(test-begin "srfi-31")

(define fibonacci
  (rec (fibonacci N)
       ((rec (G K L)
             (if (zero? K) L
               (G (- K 1) (* K L)))) N 1)))

(test-equal "1" (fibonacci 0) 1)
(test-equal "2" (fibonacci 10) 3628800)

(test-end "srfi-31")
