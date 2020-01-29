(test-begin "srfi-26")

(let ((x 'orig))
  (let ((f (cute list x)))
    (set! x 'wrong)
    (test-equal '(orig) (f))))
(let ((x 'wrong))
  (let ((f (cut list x)))
    (set! x 'right)
    (test-equal '(right) (f))))

(test-end "srfi-26")
