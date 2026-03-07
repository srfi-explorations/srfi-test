
(test-begin "srfi-180")

(define (json->string json)
  (let ((port (open-output-string)))
    (json-write json port)
    (get-output-string port)))

(test-equal "{\"a\":1,\"b\":2,\"b\":3}" (json->string `((a . 1) (b . 2) (b . 3))))

(test-equal "[1,2,3,4,5,6,7,8,9]" (json->string (vector 1 2 3 4 5 6 7 8 9)))

(test-end "srfi-180")

