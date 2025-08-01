(test-begin "srfi-87")

(define (get-symbol) 'true)

(case (get-symbol)
  ((true) #t)
  ((false) #f)
  (else => (lambda (x) x)))

(test-end "srfi-87")
