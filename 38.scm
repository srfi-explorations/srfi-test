;; SPDX-License-Identifier: MIT

(define a (list 1 2 3))

(define (write/ss->string obj)
  (parameterize
    ((current-output-port (open-output-string)))
    (write-with-shared-structure obj)
    (get-output-string (current-output-port))))

(test-begin "srfi-38")

(test-equal (write/ss->string a) "(1 2 3)")

(test-end "srfi-38")
