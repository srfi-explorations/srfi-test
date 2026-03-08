
(test-begin "srfi-180")

(define tmpfile ".srfi-180-tmp")

(define (slurp file)
  (letrec
    ((looper (lambda (c result)
               (if (eof-object? c)
                 (list->string (reverse result))
                 (looper (read-char) (cons c result))))))
    (with-input-from-file file (lambda () (looper (read-char) '())))))

(define (json->string json)
  (when (file-exists? tmpfile) (delete-file tmpfile))
  (with-output-to-file tmpfile (lambda () (json-write json)))
  (slurp tmpfile))

(test-equal "{\"a\":1,\"b\":2,\"b\":3}" (json->string `((a . 1) (b . 2) (b . 3))))

(test-equal "[1,2,3,4,5,6,7,8,9]" (json->string (vector 1 2 3 4 5 6 7 8 9)))

(test-end "srfi-180")

