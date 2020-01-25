(define-syntax check-predicate
  (syntax-rules ()
    ((_ predicate expr expected-value)
     (let ((value expr))
       (or (and (predicate value expected-value) #t)
           (error "Test failed: not equal" 'expr value expected-value))))))

(define-syntax check-equal?
  (syntax-rules ()
    ((_ expr expected-value)
     (let ((value expr))
       (or (equal? value expected-value)
           (error "Test failed: not equal" 'expr value expected-value))))))

(define-syntax check-error
  (syntax-rules ()
    ((_ expr) (guard (_ (else #t)) (begin expr #f)))))
