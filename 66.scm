(test-begin "srfi-66")

(define u8v (make-u8vector 3 42))

(test-assert (u8vector? u8v))

(test-assert (= (u8vector-length u8v) 3))

(test-end "srfi-66")
