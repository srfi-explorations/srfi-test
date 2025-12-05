(test-begin "srfi-31")

(define F (rec (F N)
               ((rec (G K L)
                     (if (zero? K) L
                       (G (- K 1) (* K L)))) N 1)))

(test-equal "1" (F 0) 1)
(test-equal "2" (F 10) 3628800)

(test-end "srfi-31")
