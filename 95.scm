(test-begin "srfi-95")

(define l1 (list 2 3 1))
(define sorted-l1 (sort l1))

(test-equal sorted-l1 '(1 2 3))
(test-assert (sorted? sorted-l1))

(define l2 (list 4 6 5))
(define sorted-l2 (sort l2))

(test-equal sorted-l2 '(4 5 6))
(test-assert (sorted? sorted-l2))

(define merged-l (merge sorted-l1 sorted-l2))

(test-equal merged-l '(1 2 3 4 5 6))

(test-end "srfi-95")
