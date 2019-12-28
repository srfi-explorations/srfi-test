(define-syntax check-equal?
  (syntax-rules ()
    ((_ expr expected-value)
     (let ((value expr))
       (or (equal? value expected-value)
           (error "Test failed: not equal" 'expr value expected-value))))))
