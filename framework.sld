(define-library (srfi-test framework)
  (export check-equal? check-predicate check-error)
  (import (scheme base))
  (include "framework.scm"))
