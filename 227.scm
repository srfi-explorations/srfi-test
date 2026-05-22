(test-begin "srfi-227")

(define f
  (opt-lambda (a b (c 1) (d 2) . r)
              (list a b c d r)))

(test-equal (f 1 2) '(1 2 1 2 ()))
(test-equal (f 1 2 3) '(1 2 3 2 ()))
(test-equal (f 1 2 3 4) '(1 2 3 4 ()))
(test-equal (f 1 2 3 4 5) '(1 2 3 4 (5)))

(define n 1)
(define g
  (opt-lambda (n (m (* n 2)))
              (list n m)))

(test-equal (g 2) '(2 2))
(test-equal (g 2 3) '(2 3))

(set! n 2)
(test-equal (g 1) '(1 4))
(test-equal (g 1 2) '(1 2))

(define g*
  (opt*-lambda (n (m (* n 2)))
               (list n m)))

(set! n 1)
(test-equal (g* 2) '(2 4))
(test-equal (g* 2 3) '(2 3))

(test-equal (let-optionals '(1 2)
                           (x . y)
                           (list x y))
            '(1 (2)))

(test-equal (let-optionals '(1)
                           (x (y 2) (z 3))
                           (list x y z))
            '(1 2 3))

(test-equal (let-optionals* '(1 3)
                    (x (y 2) (z (+ x y)))
                      (list x y z))
            '(1 3 4))

(test-end "srfi-227")
