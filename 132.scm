;; Copyright 2016 William D Clinger
;; SPDX-License-Identifier: MIT

(test-begin "srfi-132")

(define r7rs-vector-copy vector-copy)

(define (random-vector size)
  (let* ((v (make-vector size))
         (range (* 10 size))
         (half (quotient range 2)))
    (let loop ((i (vector-length v)))
      (if (= i 0) v (let ((i (- i 1)))
                      (vector-set! v i (- (random-integer range) half))
                      (loop i))))))

(test-eqv "list-sorted?:empty-list"
  #t (list-sorted? > '()))

(test-eqv "list-sorted?:singleton"
  #t (list-sorted? > '(987)))

(test-eqv "list-sorted?:non-empty-list"
  #t (list-sorted? > '(9 8 7)))

(test-eqv "vector-sorted?:empty-vector"
  #t (vector-sorted? > '#()))

(test-eqv "vector-sorted?:singleton"
  #t (vector-sorted? > '#(987)))

(test-eqv "vector-sorted?:non-empty-vector"
  #t (vector-sorted? > '#(9 8 7 6 5)))

(test-eqv "vector-sorted?:empty-vector:0"
  #t (vector-sorted? > '#() 0))

(test-eqv "vector-sorted?:singleton:1"
  #t (vector-sorted? > '#(987) 1))

(test-eqv "vector-sorted?:non-empty-vector:1"
  #t (vector-sorted? > '#(9 8 7 6 5) 1))

(test-eqv "vector-sorted?:empty-vector:0:0"
  #t (vector-sorted? > '#() 0 0))

(test-eqv "vector-sorted?:singleton:1:1"
  #t (vector-sorted? > '#(987) 1 1))

(test-eqv "vector-sorted?:non-empty-vector:1:2"
  #t (vector-sorted? > '#(9 8 7 6 5) 1 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-equal "list-sort:empty-list" '() (list-sort > (list)))

(test-equal "list-sort:singleton" '(987) (list-sort > (list 987)))

(test-equal "list-sort:doubleton" '(987 654) (list-sort > (list 987 654)))

(test-equal "list-sort:iota10"
  '(9 8 7 6 5 4 3 2 1 0)
  (list-sort > (list 9 8 6 3 0 4 2 5 7 1)))

(test-equal "list-stable-sort:empty-list"
  '() (list-stable-sort > (list)))

(test-equal "list-stable-sort:singleton"
  '(987) (list-stable-sort > (list 987)))

(test-equal "list-stable-sort:doubleton"
  '(987 654)
  (list-stable-sort > (list 987 654)))

(test-equal "list-stable-sort:iota10"
  '(9 8 7 6 5 4 3 2 1 0)
  (list-stable-sort > (list 9 8 6 3 0 4 2 5 7 1)))

(test-equal "list-stable-sort:iota10-quotient2"
  '(9 8 6 7 4 5 3 2 0 1)
  (list-stable-sort
   (lambda (x y) (> (quotient x 2) (quotient y 2)))
   (list 9 8 6 3 0 4 2 5 7 1)))

(test-equal "vector-sort:empty-vector"
  '#() (let ((v (vector))) (vector-sort > v)))

(test-equal "vector-sort:singleton"
  '#(987)
  (let ((v (vector 987))) (vector-sort > (vector 987))))

(test-equal "vector-sort:doubleton"
  '#(987 654)
  (let ((v (vector 987 654))) (vector-sort > v)))

(test-equal "vector-sort:iota10"
  '#(9 8 7 6 5 4 3 2 1 0)
  (let ((v (vector 9 8 6 3 0 4 2 5 7 1))) (vector-sort > v)))

(test-equal "vector-stable-sort:empty-vector"
  '#()
  (let ((v (vector))) (vector-stable-sort > v)))

(test-equal "vector-stable-sort:singleton"
  '#(987)
  (let ((v (vector 987))) (vector-stable-sort > (vector 987))))

(test-equal "vector-stable-sort:doubleton"
  '#(987 654)
  (let ((v (vector 987 654))) (vector-stable-sort > v)))

(test-equal "vector-stable-sort:iota10"
  '#(9 8 7 6 5 4 3 2 1 0)
  (let ((v (vector 9 8 6 3 0 4 2 5 7 1))) (vector-stable-sort > v)))

(test-equal "vector-stable-sort:iota10-quotient2"
  '#(9 8 6 7 4 5 3 2 0 1)
  (let ((v (vector 9 8 6 3 0 4 2 5 7 1)))
    (vector-stable-sort (lambda (x y) (> (quotient x 2) (quotient y 2))) v)))

(test-equal "vector-sort:empty-vector:0"
  '#()
  (let ((v (vector))) (vector-sort > v 0)))

(test-equal "vector-sort:singleton:1"
  '#()
  (let ((v (vector 987))) (vector-sort > (vector 987) 1)))

(test-equal "vector-sort:doubleton:1"
  '#(654)
  (let ((v (vector 987 654))) (vector-sort > v 1)))

(test-equal "vector-sort:iota10:3"
  '#(7 5 4 3 2 1 0)
  (let ((v (vector 9 8 6 3 0 4 2 5 7 1))) (vector-sort > v 3)))

(test-equal "vector-stable-sort:empty-vector:0"
  '#()
  (let ((v (vector))) (vector-stable-sort > v 0)))

(test-equal "vector-stable-sort:singleton:1"
  '#()
  (let ((v (vector 987))) (vector-stable-sort > (vector 987) 1)))

(test-equal "vector-stable-sort:doubleton:0:2"
  '#(654 987)
  (let ((v (vector 987 654))) (vector-stable-sort < v 0 2)))

(test-equal "vector-stable-sort:iota10:3"
  '#(7 5 4 3 2 1 0)
  (let ((v (vector 9 8 6 3 0 4 2 5 7 1))) (vector-stable-sort > v 3)))

(test-equal "vector-stable-sort:iota10-quotient2:3"
  '#(7 4 5 3 2 0 1)
  (let ((v (vector 9 8 6 3 0 4 2 5 7 1)))
    (vector-stable-sort (lambda (x y) (> (quotient x 2) (quotient y 2))) v 3)))

(test-equal "vector-sort:empty-vector:0:0"
  '#()
  (let ((v (vector))) (vector-sort > v 0 0)))

(test-equal "vector-sort:singleton:1:1"
  '#()
  (let ((v (vector 987))) (vector-sort > (vector 987) 1 1)))

(test-equal "vector-sort:doubleton:1:2"
  '#(654)
  (let ((v (vector 987 654))) (vector-sort > v 1 2)))

(test-equal "vector-sort:iota10:4:8"
  '#(5 4 2 0)
  (let ((v (vector 9 8 6 3 0 4 2 5 7 1))) (vector-sort > v 4 8)))

(test-equal "vector-stable-sort:empty-vector:0:0"
  '#()
  (let ((v (vector))) (vector-stable-sort > v 0 0)))

(test-equal "vector-stable-sort:singleton:1:1"
  '#()
  (let ((v (vector 987))) (vector-stable-sort > (vector 987) 1 1)))

(test-equal "vector-stable-sort:doubleton:1:2"
  '#(654)
  (let ((v (vector 987 654))) (vector-stable-sort > v 1 2)))

(test-equal "vector-stable-sort:iota10:2:6"
  '#(6 4 3 0)
  (let ((v (vector 9 8 6 3 0 4 2 5 7 1))) (vector-stable-sort > v 2 6)))

(test-equal "vector-stable-sort:iota10-quotient2:1:8"
  '#(8 6 4 5 3 2 0)
  (let ((v (vector 9 8 6 3 0 4 2 5 7 1)))
    (vector-stable-sort (lambda (x y) (> (quotient x 2) (quotient y 2))) v 1 8)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-equal "list-sort!:empty-list"
  '() (list-sort! > (list)))

(test-equal "list-sort!:singleton"
  '(987) (list-sort! > (list 987)))

(test-equal "list-sort!:doubleton"
  '(987 654) (list-sort! > (list 987 654)))

(test-equal "list-sort!:iota10"
  '(9 8 7 6 5 4 3 2 1 0)
  (list-sort! > (list 9 8 6 3 0 4 2 5 7 1)))

(test-equal "list-stable-sort!:empty-list"
  '() (list-stable-sort! > (list)))

(test-equal "list-stable-sort!:singleton"
  '(987) (list-stable-sort! > (list 987)))

(test-equal "list-stable-sort!:doubleton"
  '(987 654)
  (list-stable-sort! > (list 987 654)))

(test-equal "list-stable-sort!:iota10"
  '(9 8 7 6 5 4 3 2 1 0)
  (list-stable-sort! > (list 9 8 6 3 0 4 2 5 7 1)))

(test-equal "list-stable-sort!:iota10-quotient2"
  '(9 8 6 7 4 5 3 2 0 1)
  (list-stable-sort!
   (lambda (x y) (> (quotient x 2) (quotient y 2)))
   (list 9 8 6 3 0 4 2 5 7 1)))

(test-equal "vector-sort!:empty-vector"
  '#()
  (let ((v (vector))) (vector-sort! > v) v))

(test-equal "vector-sort!:singleton"
  '#(987)
  (let ((v (vector 987))) (vector-sort! > (vector 987)) v))

(test-equal "vector-sort!:doubleton"
  '#(987 654)
  (let ((v (vector 987 654))) (vector-sort! > v) v))

(test-equal "vector-sort!:iota10"
  '#(9 8 7 6 5 4 3 2 1 0)
  (let ((v (vector 9 8 6 3 0 4 2 5 7 1))) (vector-sort! > v) v))

(test-equal "vector-stable-sort!:empty-vector"
  '#()
  (let ((v (vector))) (vector-stable-sort! > v) v))

(test-equal "vector-stable-sort!:singleton"
  '#(987)
  (let ((v (vector 987))) (vector-stable-sort! > (vector 987)) v))

(test-equal "vector-stable-sort!:doubleton"
  '#(987 654)
  (let ((v (vector 987 654))) (vector-stable-sort! > v) v))

(test-equal "vector-stable-sort!:iota10"
  '#(9 8 7 6 5 4 3 2 1 0)
  (let ((v (vector 9 8 6 3 0 4 2 5 7 1))) (vector-stable-sort! > v) v))

(test-equal "vector-stable-sort!:iota10-quotient2"
  '#(9 8 6 7 4 5 3 2 0 1)
  (let ((v (vector 9 8 6 3 0 4 2 5 7 1)))
    (vector-stable-sort! (lambda (x y) (> (quotient x 2) (quotient y 2))) v)
    v))

(test-equal "vector-sort!:empty-vector:0"
  '#()
  (let ((v (vector))) (vector-sort! > v 0) v))

(test-equal "vector-sort!:singleton:1"
  '#(987)
  (let ((v (vector 987))) (vector-sort! > (vector 987) 1) v))

(test-equal "vector-sort!:doubleton:1"
  '#(987 654)
  (let ((v (vector 987 654))) (vector-sort! > v 1) v))

(test-equal "vector-sort!:iota10:3"
  '#(9 8 6 7 5 4 3 2 1 0)
  (let ((v (vector 9 8 6 3 0 4 2 5 7 1))) (vector-sort! > v 3) v))

(test-equal "vector-stable-sort!:empty-vector:0"
  '#()
  (let ((v (vector))) (vector-stable-sort! > v 0) v))

(test-equal "vector-stable-sort!:singleton:1"
  '#(987)
  (let ((v (vector 987))) (vector-stable-sort! > (vector 987) 1) v))

(test-equal "vector-stable-sort!:doubleton:0:2"
  '#(654 987)
  (let ((v (vector 987 654))) (vector-stable-sort! < v 0 2) v))

(test-equal "vector-stable-sort!:iota10:3"
  '#(9 8 6 7 5 4 3 2 1 0)
  (let ((v (vector 9 8 6 3 0 4 2 5 7 1))) (vector-stable-sort! > v 3) v))

(test-equal "vector-stable-sort!:iota10-quotient2:3"
  '#(9 8 6 7 4 5 3 2 0 1)
  (let ((v (vector 9 8 6 3 0 4 2 5 7 1)))
    (vector-stable-sort! (lambda (x y) (> (quotient x 2) (quotient y 2))) v 3)
    v))

(test-equal "vector-sort!:empty-vector:0:0"
  '#()
  (let ((v (vector))) (vector-sort! > v 0 0) v))

(test-equal "vector-sort!:singleton:1:1"
  '#(987)
  (let ((v (vector 987))) (vector-sort! > (vector 987) 1 1) v))

(test-equal "vector-sort!:doubleton:1:2"
  '#(987 654)
  (let ((v (vector 987 654))) (vector-sort! > v 1 2) v))

(test-equal "vector-sort!:iota10:4:8"
  '#(9 8 6 3 5 4 2 0 7 1)
  (let ((v (vector 9 8 6 3 0 4 2 5 7 1))) (vector-sort! > v 4 8) v))

(test-equal "vector-stable-sort!:empty-vector:0:0"
  '#()
  (let ((v (vector))) (vector-stable-sort! > v 0 0) v))

(test-equal "vector-stable-sort!:singleton:1:1"
  '#(987)
  (let ((v (vector 987))) (vector-stable-sort! > (vector 987) 1 1) v))

(test-equal "vector-stable-sort!:doubleton:1:2"
  '#(987 654)
  (let ((v (vector 987 654))) (vector-stable-sort! > v 1 2) v))

(test-equal "vector-stable-sort!:iota10:2:6"
  '#(9 8 6 4 3 0 2 5 7 1)
  (let ((v (vector 9 8 6 3 0 4 2 5 7 1))) (vector-stable-sort! > v 2 6) v))

(test-equal "vector-stable-sort!:iota10-quotient2:1:8"
  '#(9 8 6 4 5 3 2 0 7 1)
  (let ((v (vector 9 8 6 3 0 4 2 5 7 1)))
    (vector-stable-sort! (lambda (x y) (> (quotient x 2) (quotient y 2))) v 1 8)
    v))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-equal "list-merge:empty:empty"
  '() (list-merge > (list) (list)))

(test-equal "list-merge:empty:nonempty"
  '(9 6 3 0)
  (list-merge > (list) (list 9 6 3 0)))

(test-equal "list-merge:nonempty:empty"
  '(9 7 5 3 1)
  (list-merge > (list 9 7 5 3 1) (list)))

(test-equal "list-merge:nonempty:nonempty"
  '(9 9 7 6 5 3 3 1 0)
  (list-merge > (list 9 7 5 3 1) (list 9 6 3 0)))

(test-equal "list-merge!:empty:empty"
  '() (list-merge! > (list) (list)))

(test-equal "list-merge!:empty:nonempty"
  '(9 6 3 0)
  (list-merge! > (list) (list 9 6 3 0)))

(test-equal "list-merge!:nonempty:empty"
  '(9 7 5 3 1)
  (list-merge! > (list 9 7 5 3 1) (list)))

(test-equal "list-merge!:nonempty:nonempty"
  '(9 9 7 6 5 3 3 1 0)
  (list-merge! > (list 9 7 5 3 1) (list 9 6 3 0)))

(test-equal "vector-merge:empty:empty"
  '#() (vector-merge > (vector) (vector)))

(test-equal "vector-merge:empty:nonempty"
  '#(9 6 3 0)
  (vector-merge > (vector) (vector 9 6 3 0)))

(test-equal "vector-merge:nonempty:empty"
  '#(9 7 5 3 1)
  (vector-merge > (vector 9 7 5 3 1) (vector)))

(test-equal "vector-merge:nonempty:nonempty"
  '#(9 9 7 6 5 3 3 1 0)
  (vector-merge > (vector 9 7 5 3 1) (vector 9 6 3 0)))

(test-equal "vector-merge!:empty:empty"
  '#(#f #f #f #f #f #f #f #f #f #f #f #f)
  (let ((v (make-vector 12 #f))) (vector-merge! > v (vector) (vector)) v))

(test-equal "vector-merge!:empty:nonempty"
  '#(9 6 3 0 #f #f #f #f #f #f #f #f)
  (let ((v (make-vector 12 #f)))
    (vector-merge! > v (vector) (vector 9 6 3 0)) v))

(test-equal "vector-merge!:nonempty:empty"
  '#(9 7 5 3 1 #f #f #f #f #f #f #f)
  (let ((v (make-vector 12 #f)))
    (vector-merge! > v (vector 9 7 5 3 1) (vector)) v))

(test-equal "vector-merge!:nonempty:nonempty"
  '#(9 9 7 6 5 3 3 1 0 #f #f #f)
  (let ((v (make-vector 12 #f)))
    (vector-merge! > v (vector 9 7 5 3 1) (vector 9 6 3 0))
    v))

(test-equal "vector-merge!:empty:empty:0"
  '#(#f #f #f #f #f #f #f #f #f #f #f #f)
  (let ((v (make-vector 12 #f))) (vector-merge! > v (vector) (vector) 0) v))

(test-equal "vector-merge!:empty:nonempty:0"
  '#(9 6 3 0 #f #f #f #f #f #f #f #f)
  (let ((v (make-vector 12 #f)))
    (vector-merge! > v (vector) (vector 9 6 3 0) 0) v))

(test-equal "vector-merge!:nonempty:empty:0"
  '#(9 7 5 3 1 #f #f #f #f #f #f #f)
  (let ((v (make-vector 12 #f)))
    (vector-merge! > v (vector 9 7 5 3 1) (vector) 0)
    v))

(test-equal "vector-merge!:nonempty:nonempty:0"
  '#(9 9 7 6 5 3 3 1 0 #f #f #f)
  (let ((v (make-vector 12 #f)))
    (vector-merge! > v (vector 9 7 5 3 1) (vector 9 6 3 0) 0)
    v))

(test-equal "vector-merge!:empty:empty:2"
  '#(#f #f #f #f #f #f #f #f #f #f #f #f)
  (let ((v (make-vector 12 #f)))
    (vector-merge! > v (vector) (vector) 2) v))

(test-equal "vector-merge!:empty:nonempty:2"
  '#(#f #f 9 6 3 0 #f #f #f #f #f #f)
  (let ((v (make-vector 12 #f)))
    (vector-merge! > v (vector) (vector 9 6 3 0) 2) v))

(test-equal "vector-merge!:nonempty:empty:2"
  '#(#f #f 9 7 5 3 1 #f #f #f #f #f)
  (let ((v (make-vector 12 #f)))
    (vector-merge! > v (vector 9 7 5 3 1) (vector) 2)
    v))

(test-equal "vector-merge!:nonempty:nonempty:2"
  '#(#f #f 9 9 7 6 5 3 3 1 0 #f)
  (let ((v (make-vector 12 #f)))
    (vector-merge! > v (vector 9 7 5 3 1) (vector 9 6 3 0) 2)
    v))

(test-equal "vector-merge:empty:empty"
  '#() (vector-merge > (vector) (vector) 0))

(test-equal "vector-merge:empty:nonempty"
  '#(9 6 3 0)
  (vector-merge > (vector) (vector 9 6 3 0) 0))

(test-equal "vector-merge:nonempty:empty"
  '#(5 3 1)
  (vector-merge > (vector 9 7 5 3 1) (vector) 2))

(test-equal "vector-merge:nonempty:nonempty"
  '#(9 6 5 3 3 1 0)
  (vector-merge > (vector 9 7 5 3 1) (vector 9 6 3 0) 2))

(test-equal "vector-merge!:empty:empty:2"
  '#(#f #f #f #f #f #f #f #f #f #f #f #f)
  (let ((v (make-vector 12 #f))) (vector-merge! > v (vector) (vector) 2 0) v))

(test-equal "vector-merge!:empty:nonempty:2"
  '#(#f #f 9 6 3 0 #f #f #f #f #f #f)
  (let ((v (make-vector 12 #f)))
    (vector-merge! > v (vector) (vector 9 6 3 0) 2 0)
    v))

(test-equal "vector-merge!:nonempty:empty:2"
  '#(#f #f 5 3 1 #f #f #f #f #f #f #f)
  (let ((v (make-vector 12 #f)))
    (vector-merge! > v (vector 9 7 5 3 1) (vector) 2 2)
    v))

(test-equal "vector-merge!:nonempty:nonempty:2"
  '#(#f #f 9 6 5 3 3 1 0 #f #f #f)
  (let ((v (make-vector 12 #f)))
    (vector-merge! > v (vector 9 7 5 3 1) (vector 9 6 3 0) 2 2)
    v))

(test-equal "vector-merge:empty:empty"
  '#() (vector-merge > (vector) (vector) 0 0))

(test-equal "vector-merge:empty:nonempty"
  '#(9 6 3 0)
  (vector-merge > (vector) (vector 9 6 3 0) 0 0))

(test-equal "vector-merge:nonempty:empty"
  '#(5 3 1)
  (vector-merge > (vector 9 7 5 3 1) (vector) 2 5))

(test-equal "vector-merge:nonempty:nonempty"
  '#(9 6 5 3 3 1 0)
  (vector-merge > (vector 9 7 5 3 1) (vector 9 6 3 0) 2 5))

(test-equal "vector-merge!:empty:empty:2"
  '#(#f #f #f #f #f #f #f #f #f #f #f #f)
  (let ((v (make-vector 12 #f))) (vector-merge! > v (vector) (vector) 2 0 0) v))

(test-equal "vector-merge!:empty:nonempty:2"
  '#(#f #f 9 6 3 0 #f #f #f #f #f #f)
  (let ((v (make-vector 12 #f)))
    (vector-merge! > v (vector) (vector 9 6 3 0) 2 0 0)
    v))

(test-equal "vector-merge!:nonempty:empty:2"
  '#(#f #f 5 3 1 #f #f #f #f #f #f #f)
  (let ((v (make-vector 12 #f)))
    (vector-merge! > v (vector 9 7 5 3 1) (vector) 2 2 5)
    v))

(test-equal "vector-merge!:nonempty:nonempty:2"
  '#(#f #f 9 6 5 3 3 1 0 #f #f #f)
  (let ((v (make-vector 12 #f)))
    (vector-merge! > v (vector 9 7 5 3 1) (vector 9 6 3 0) 2 2 5)
    v))

;;; Some tests are duplicated to make the pattern easier to discern.

(test-equal "vector-merge:empty:empty"
  '#() (vector-merge > (vector) (vector) 0 0))

(test-equal "vector-merge:empty:nonempty"
  '#(9 6 3 0)
  (vector-merge > (vector) (vector 9 6 3 0) 0 0))

(test-equal "vector-merge:nonempty:empty"
  '#(5 3)
  (vector-merge > (vector 9 7 5 3 1) (vector) 2 4))

(test-equal "vector-merge:nonempty:nonempty"
  '#(9 6 5 3 3 0)
  (vector-merge > (vector 9 7 5 3 1) (vector 9 6 3 0) 2 4))

(test-equal "vector-merge!:empty:empty:2"
  '#(#f #f #f #f #f #f #f #f #f #f #f #f)
  (let ((v (make-vector 12 #f)))
    (vector-merge! > v (vector) (vector) 2 0 0) v))

(test-equal "vector-merge!:empty:nonempty:2"
  '#(#f #f 9 6 3 0 #f #f #f #f #f #f)
  (let ((v (make-vector 12 #f)))
    (vector-merge! > v (vector) (vector 9 6 3 0) 2 0 0)
    v))

(test-equal "vector-merge!:nonempty:empty:2"
  '#(#f #f 5 3 #f #f #f #f #f #f #f #f)
  (let ((v (make-vector 12 #f)))
    (vector-merge! > v (vector 9 7 5 3 1) (vector) 2 2 4)
    v))

(test-equal "vector-merge!:nonempty:nonempty:2"
  '#(#f #f 9 6 5 3 3 0 #f #f #f #f)
  (let ((v (make-vector 12 #f)))
    (vector-merge! > v (vector 9 7 5 3 1) (vector 9 6 3 0) 2 2 4)
    v))

(test-equal "vector-merge:empty:empty"
  '#()
  (vector-merge > (vector) (vector) 0 0 0))

(test-equal "vector-merge:empty:nonempty"
  '#(9 6 3 0)
  (vector-merge > (vector) (vector 9 6 3 0) 0 0 0))

(test-equal "vector-merge:nonempty:empty"
  '#(5 3)
  (vector-merge > (vector 9 7 5 3 1) (vector) 2 4 0))

(test-equal "vector-merge:nonempty:nonempty"
  '#(9 6 5 3 3 0)
  (vector-merge > (vector 9 7 5 3 1) (vector 9 6 3 0) 2 4 0))

(test-equal "vector-merge!:empty:empty:2"
  '#(#f #f #f #f #f #f #f #f #f #f #f #f)
  (let ((v (make-vector 12 #f)))
    (vector-merge! > v (vector) (vector) 2 0 0 0) v))

(test-equal "vector-merge!:empty:nonempty:2"
  '#(#f #f 9 6 3 0 #f #f #f #f #f #f)
  (let ((v (make-vector 12 #f)))
    (vector-merge! > v (vector) (vector 9 6 3 0) 2 0 0 0)
    v))

(test-equal "vector-merge!:nonempty:empty:2"
  '#(#f #f 5 3 #f #f #f #f #f #f #f #f)
  (let ((v (make-vector 12 #f)))
    (vector-merge! > v (vector 9 7 5 3 1) (vector) 2 2 4 0)
    v))

(test-equal "vector-merge!:nonempty:nonempty:2"
  '#(#f #f 9 6 5 3 3 0 #f #f #f #f)
  (let ((v (make-vector 12 #f)))
    (vector-merge! > v (vector 9 7 5 3 1) (vector 9 6 3 0) 2 2 4 0)
    v))

(test-equal "vector-merge:empty:empty"
  '#()
  (vector-merge > (vector) (vector) 0 0 0))

(test-equal "vector-merge:empty:nonempty"
  '#(6 3 0)
  (vector-merge > (vector) (vector 9 6 3 0) 0 0 1))

(test-equal "vector-merge:nonempty:empty"
  '#(5 3)
  (vector-merge > (vector 9 7 5 3 1) (vector) 2 4 0))

(test-equal "vector-merge:nonempty:nonempty"
  '#(6 5 3 3 0)
  (vector-merge > (vector 9 7 5 3 1) (vector 9 6 3 0) 2 4 1))

(test-equal "vector-merge!:empty:empty:2"
  '#(#f #f #f #f #f #f #f #f #f #f #f #f)
  (let ((v (make-vector 12 #f)))
    (vector-merge! > v (vector) (vector) 2 0 0 0) v))

(test-equal "vector-merge!:empty:nonempty:2"
  '#(#f #f 6 3 0 #f #f #f #f #f #f #f)
  (let ((v (make-vector 12 #f)))
    (vector-merge! > v (vector) (vector 9 6 3 0) 2 0 0 1)
    v))

(test-equal "vector-merge!:nonempty:empty:2"
  '#(#f #f 5 3 #f #f #f #f #f #f #f #f)
  (let ((v (make-vector 12 #f)))
    (vector-merge! > v (vector 9 7 5 3 1) (vector) 2 2 4 0)
    v))

(test-equal "vector-merge!:nonempty:nonempty:2"
  '#(#f #f 6 5 3 3 0 #f #f #f #f #f)
  (let ((v (make-vector 12 #f)))
    (vector-merge! > v (vector 9 7 5 3 1) (vector 9 6 3 0) 2 2 4 1)
    v))

(test-equal "vector-merge:empty:empty"
  '#()
  (vector-merge > (vector) (vector) 0 0 0 0))

(test-equal "vector-merge:empty:nonempty"
  '#(6 3 0)
  (vector-merge > (vector) (vector 9 6 3 0) 0 0 1 4))

(test-equal "vector-merge:nonempty:empty"
  '#(5 3)
  (vector-merge > (vector 9 7 5 3 1) (vector) 2 4 0 0))

(test-equal "vector-merge:nonempty:nonempty"
  '#(6 5 3 3 0)
  (vector-merge > (vector 9 7 5 3 1) (vector 9 6 3 0) 2 4 1 4))

(test-equal "vector-merge!:empty:empty:2"
  '#(#f #f #f #f #f #f #f #f #f #f #f #f)
  (let ((v (make-vector 12 #f)))
    (vector-merge! > v (vector) (vector) 2 0 0 0 0) v))

(test-equal "vector-merge!:empty:nonempty:2"
  '#(#f #f 6 3 0 #f #f #f #f #f #f #f)
  (let ((v (make-vector 12 #f)))
    (vector-merge! > v (vector) (vector 9 6 3 0) 2 0 0 1 4)
    v))

(test-equal "vector-merge!:nonempty:empty:2"
  '#(#f #f 5 3 #f #f #f #f #f #f #f #f)
  (let ((v (make-vector 12 #f)))
    (vector-merge! > v (vector 9 7 5 3 1) (vector) 2 2 4 0 0)
    v))

(test-equal "vector-merge!:nonempty:nonempty:2"
  '#(#f #f 6 5 3 3 0 #f #f #f #f #f)
  (let ((v (make-vector 12 #f)))
    (vector-merge! > v (vector 9 7 5 3 1) (vector 9 6 3 0) 2 2 4 1 4)
    v))

(test-equal "vector-merge:empty:empty"
  '#()
  (vector-merge > (vector) (vector) 0 0 0 0))

(test-equal "vector-merge:empty:nonempty"
  '#(6)
  (vector-merge > (vector) (vector 9 6 3 0) 0 0 1 2))

(test-equal "vector-merge:nonempty:empty"
  '#(5 3)
  (vector-merge > (vector 9 7 5 3 1) (vector) 2 4 0 0))

(test-equal "vector-merge:nonempty:nonempty"
  '#(6 5 3)
  (vector-merge > (vector 9 7 5 3 1) (vector 9 6 3 0) 2 4 1 2))

(test-equal "vector-merge!:empty:empty:2"
  '#(#f #f #f #f #f #f #f #f #f #f #f #f)
  (let ((v (make-vector 12 #f)))
    (vector-merge! > v (vector) (vector) 2 0 0 0 0) v))

(test-equal "vector-merge!:empty:nonempty:2"
  '#(#f #f 6 #f #f #f #f #f #f #f #f #f)
  (let ((v (make-vector 12 #f)))
    (vector-merge! > v (vector) (vector 9 6 3 0) 2 0 0 1 2)
    v))

(test-equal "vector-merge!:nonempty:empty:2"
  '#(#f #f 5 3 #f #f #f #f #f #f #f #f)
  (let ((v (make-vector 12 #f)))
    (vector-merge! > v (vector 9 7 5 3 1) (vector) 2 2 4 0 0)
    v))

(test-equal "vector-merge!:nonempty:nonempty:2"
  '#(#f #f 6 5 3 #f #f #f #f #f #f #f)
  (let ((v (make-vector 12 #f)))
    (vector-merge! > v (vector 9 7 5 3 1) (vector 9 6 3 0) 2 2 4 1 2)
    v))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-equal "list-delete-neighbor-dups:empty"
  '()
  (list-delete-neighbor-dups char=? (list)))

(test-equal "list-delete-neighbor-dups:singleton"
  '(#\a)
  (list-delete-neighbor-dups char=? (list #\a)))

(test-equal "list-delete-neighbor-dups:nonempty"
  '(#\a #\b #\a)
  (list-delete-neighbor-dups char=? (list #\a #\a #\a #\b #\b #\a)))

(test-equal "list-delete-neighbor-dups!:empty"
  '()
  (list-delete-neighbor-dups! char=? (list)))

(test-equal "list-delete-neighbor-dups!:singleton"
  '(#\a)
  (list-delete-neighbor-dups! char=? (list #\a)))

(test-equal "list-delete-neighbor-dups!:nonempty"
  '(#\a #\b #\a)
  (list-delete-neighbor-dups! char=? (list #\a #\a #\a #\b #\b #\a)))

(test-equal "vector-delete-neighbor-dups:empty"
  '#()
  (let ((v (vector))) (vector-delete-neighbor-dups char=? v)))

(test-equal "vector-delete-neighbor-dups:singleton"
  '#(#\a)
  (let ((v (vector #\a))) (vector-delete-neighbor-dups char=? v)))

(test-equal "vector-delete-neighbor-dups:nonempty"
  '#(#\a #\b #\a)
  (let ((v (vector #\a #\a #\a #\b #\b #\a)))
    (vector-delete-neighbor-dups char=? v)))

(test-equal "vector-delete-neighbor-dups!:empty"
  '(0 #())
  (let ((v (vector)))
    (list (vector-delete-neighbor-dups! char=? v) v)))

(test-equal "vector-delete-neighbor-dups!:singleton"
  '(1 #(#\a))
  (let ((v (vector #\a)))
    (list (vector-delete-neighbor-dups! char=? v) v)))

(test-equal "vector-delete-neighbor-dups!:nonempty"
  '(3 #(#\a #\b #\a #\b #\b #\a))
  (let ((v (vector #\a #\a #\a #\b #\b #\a)))
    (list (vector-delete-neighbor-dups! char=? v) v)))

(test-equal "vector-delete-neighbor-dups:empty:0"
  '#()
  (let ((v (vector)))
    (vector-delete-neighbor-dups char=? v 0)))

(test-equal "vector-delete-neighbor-dups:singleton:0"
  '#(#\a)
  (let ((v (vector #\a)))
    (vector-delete-neighbor-dups char=? v 0)))

(test-equal "vector-delete-neighbor-dups:nonempty:0"
  '#(#\a #\b #\a)
  (let ((v (vector #\a #\a #\a #\b #\b #\a)))
    (vector-delete-neighbor-dups char=? v 0)))

(test-equal "vector-delete-neighbor-dups!:empty:0"
  '(0 #())
  (let ((v (vector)))
    (list (vector-delete-neighbor-dups! char=? v 0) v)))

(test-equal "vector-delete-neighbor-dups!:singleton:0"
  '(1 #(#\a))
  (let ((v (vector #\a)))
    (list (vector-delete-neighbor-dups! char=? v 0) v)))

(test-equal "vector-delete-neighbor-dups!:nonempty:0"
  '(3 #(#\a #\b #\a #\b #\b #\a))
  (let ((v (vector #\a #\a #\a #\b #\b #\a)))
    (list (vector-delete-neighbor-dups! char=? v 0) v)))

(test-equal "vector-delete-neighbor-dups:empty:0"
  '#()
  (let ((v (vector)))
    (vector-delete-neighbor-dups char=? v 0)))

(test-equal "vector-delete-neighbor-dups:singleton:1"
  '#()
  (let ((v (vector #\a)))
    (vector-delete-neighbor-dups char=? v 1)))

(test-equal "vector-delete-neighbor-dups:nonempty:3"
  '#(#\b #\a)
  (let ((v (vector #\a #\a #\a #\b #\b #\a)))
    (vector-delete-neighbor-dups char=? v 3)))

(test-equal "vector-delete-neighbor-dups!:empty:0"
  '(0 #())
  (let ((v (vector)))
    (list (vector-delete-neighbor-dups! char=? v 0) v)))

(test-equal "vector-delete-neighbor-dups!:singleton:1"
  '(1 #(#\a))
  (let ((v (vector #\a)))
    (list (vector-delete-neighbor-dups! char=? v 1) v)))

(test-equal "vector-delete-neighbor-dups!:nonempty:3"
  '(5 #(#\a #\a #\a #\b #\a #\a))
  (let ((v (vector #\a #\a #\a #\b #\b #\a)))
    (list (vector-delete-neighbor-dups! char=? v 3) v)))

(test-equal "vector-delete-neighbor-dups:empty:0:0"
  '#()
  (let ((v (vector)))
    (vector-delete-neighbor-dups char=? v 0 0)))

(test-equal "vector-delete-neighbor-dups:singleton:1:1"
  '#()
  (let ((v (vector #\a)))
    (vector-delete-neighbor-dups char=? v 1 1)))

(test-equal "vector-delete-neighbor-dups:nonempty:3:5"
  '#(#\b)
  (let ((v (vector #\a #\a #\a #\b #\b #\a)))
    (vector-delete-neighbor-dups char=? v 3 5)))

(test-equal "vector-delete-neighbor-dups!:empty:0:0"
  '(0 #())
  (let ((v (vector)))
    (list (vector-delete-neighbor-dups! char=? v 0 0) v)))

(test-equal "vector-delete-neighbor-dups!:singleton:0:1"
  '(1 #(#\a))
  (let ((v (vector #\a)))
    (list (vector-delete-neighbor-dups! char=? v 0 1) v)))

(test-equal "vector-delete-neighbor-dups!:singleton:1:1"
  '(1 #(#\a))
  (let ((v (vector #\a)))
    (list (vector-delete-neighbor-dups! char=? v 1 1) v)))

(test-equal "vector-delete-neighbor-dups!:nonempty:3:5"
  '(4 #(#\a #\a #\a #\b #\b #\a))
  (let ((v (vector #\a #\a #\a #\b #\b #\a)))
    (list (vector-delete-neighbor-dups! char=? v 3 5) v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-equal "vector-find-median:empty"
  "knil"
  (vector-find-median < (vector) "knil"))

(test-equal "vector-find-median:singleton"
  17
  (vector-find-median < (vector 17) "knil"))

(test-equal "vector-find-median:8same"
  12
  (vector-find-median < (vector 18 1 12 14 12 5 18 2) "knil"))

(test-equal "vector-find-median:8diff"
  23/2
  (vector-find-median < (vector 18 1 11 14 12 5 18 2) "knil"))

(test-equal "vector-find-median:8samelist"
  (list 12 12)
  (vector-find-median < (vector 18 1 12 14 12 5 18 2) "knil" list))

(test-equal "vector-find-median:8difflist"
  (list 11 12)
  (vector-find-median < (vector 18 1 11 14 12 5 18 2) "knil" list))

(test-equal "vector-find-median:9"
  7
  (vector-find-median < (vector 7 6 9 3 1 18 15 7 8) "knil"))

(test-equal "vector-find-median:9list"
  7
  (vector-find-median < (vector 7 6 9 3 1 18 15 7 8) "knil" list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-equal "vector-select!:singleton:0"
  19
  (let ((v (vector 19)))
    (vector-select! < v 0)))

(test-equal "vector-select!:ten:0"
  3
  (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
    (vector-select! < v 0)))

(test-equal "vector-select!:ten:2"
  9
  (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
    (vector-select! < v 2)))

(test-equal "vector-select!:ten:8"
  22
  (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
    (vector-select! < v 8)))

(test-equal "vector-select!:ten:9"
  23
  (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
    (vector-select! < v 9)))

(test-equal "vector-select!:singleton:0:0"
  19
  (let ((v (vector 19)))
    (vector-select! < v 0 0)))

(test-equal "vector-select!:ten:0:0"
  3
  (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
    (vector-select! < v 0 0)))

(test-equal "vector-select!:ten:2:0"
  9
  (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
    (vector-select! < v 2 0)))

(test-equal "vector-select!:ten:8:0"
  22
  (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
    (vector-select! < v 8 0)))

(test-equal "vector-select!:ten:9:0"
  23
  (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
    (vector-select! < v 9 0)))

(test-equal "vector-select!:singleton:0:0:1"
  19
  (let ((v (vector 19)))
    (vector-select! < v 0 0 1)))

(test-equal "vector-select!:ten:0:0:10"
  3
  (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
    (vector-select! < v 0 0 10)))

(test-equal "vector-select!:ten:2:0:10"
  9
  (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
    (vector-select! < v 2 0 10)))

(test-equal "vector-select!:ten:8:0:10"
  22
  (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
    (vector-select! < v 8 0 10)))

(test-equal "vector-select!:ten:9:0:10"
  23
  (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
    (vector-select! < v 9 0 10)))

(test-equal "vector-select!:ten:0:4:10"
  3
  (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
    (vector-select! < v 0 4 10)))

(test-equal "vector-select!:ten:2:4:10"
  13
  (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
    (vector-select! < v 2 4 10)))

(test-equal "vector-select!:ten:4:4:10"
  21
  (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
    (vector-select! < v 4 4 10)))

(test-equal "vector-select!:ten:5:4:10"
  23
  (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
    (vector-select! < v 5 4 10)))

(test-equal "vector-select!:ten:0:4:10"
  3
  (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
    (vector-select! < v 0 4 10)))

(test-equal "vector-select!:ten:2:4:10"
  13
  (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
    (vector-select! < v 2 4 10)))

(test-equal "vector-select!:ten:3:4:10"
  13
  (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
    (vector-select! < v 3 4 10)))

(test-equal "vector-select!:ten:4:4:10"
  21
  (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
    (vector-select! < v 4 4 10)))

(test-equal "vector-select!:ten:9:4:10"
  23
  (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
    (vector-select! < v 5 4 10)))

(test-equal "vector-select!:ten:0:4:8"
  9
  (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
    (vector-select! < v 0 4 8)))

(test-equal "vector-select!:ten:1:4:8"
  13
  (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
    (vector-select! < v 1 4 8)))

(test-equal "vector-select!:ten:2:4:8"
  13
  (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
    (vector-select! < v 2 4 8)))

(test-equal "vector-select!:ten:3:4:8"
  21
  (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
    (vector-select! < v 3 4 8)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-equal "vector-separate!:empty:0"
  '#()
  (let ((v (vector)))
    (vector-separate! < v 0)
    (vector-sort < (r7rs-vector-copy v 0 0))))

(test-equal "vector-separate!:singleton:0"
  '#()
  (let ((v (vector 19)))
    (vector-separate! < v 0)
    (vector-sort < (r7rs-vector-copy v 0 0))))

(test-equal "vector-separate!:singleton:1"
  '#(19)
  (let ((v (vector 19)))
    (vector-separate! < v 1)
    (vector-sort < (r7rs-vector-copy v 0 1))))

(test-equal "vector-separate!:ten:0"
  '#()
  (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
    (vector-separate! < v 0)
    (vector-sort < (r7rs-vector-copy v 0 0))))

(test-equal "vector-separate!:ten:3"
  '#(3 8 9)
  (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
    (vector-separate! < v 3)
    (vector-sort < (r7rs-vector-copy v 0 3))))

(test-equal "vector-separate!:empty:0:0"
  '#()
  (let ((v (vector)))
    (vector-separate! < v 0 0)
    (vector-sort < (r7rs-vector-copy v 0 0))))

(test-equal "vector-separate!:singleton:0:0"
  '#()
  (let ((v (vector 19)))
    (vector-separate! < v 0 0)
    (vector-sort < (r7rs-vector-copy v 0 0))))

(test-equal "vector-separate!:singleton:1:0"
  '#(19)
  (let ((v (vector 19)))
    (vector-separate! < v 1 0)
    (vector-sort < (r7rs-vector-copy v 0 1))))

(test-equal "vector-separate!:ten:0:0"
  '#()
  (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
    (vector-separate! < v 0 0)
    (vector-sort < (r7rs-vector-copy v 0 0))))

(test-equal "vector-separate!:ten:3:0"
  '#(3 8 9)
  (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
    (vector-separate! < v 3 0)
    (vector-sort < (r7rs-vector-copy v 0 3))))

(test-equal "vector-separate!:singleton:0:1"
  '#()
  (let ((v (vector 19)))
    (vector-separate! < v 0 1)
    (vector-sort < (r7rs-vector-copy v 1 1))))

(test-equal "vector-separate!:ten:0:2"
  '#()
  (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
    (vector-separate! < v 0 2)
    (vector-sort < (r7rs-vector-copy v 2 2))))

(test-equal "vector-separate!:ten:3:2"
  '#(3 9 13)
  (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
    (vector-separate! < v 3 2)
    (vector-sort < (r7rs-vector-copy v 2 5))))

(test-equal "vector-separate!:empty:0:0:0"
  '#()
  (let ((v (vector)))
    (vector-separate! < v 0 0 0)
    (vector-sort < (r7rs-vector-copy v 0 0))))

(test-equal "vector-separate!:singleton:0:1:1"
  '#()
  (let ((v (vector 19)))
    (vector-separate! < v 0 1 1)
    (vector-sort < (r7rs-vector-copy v 1 1))))

(test-equal "vector-separate!:ten:0:2:8"
  '#()
  (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
    (vector-separate! < v 0 2 8)
    (vector-sort < (r7rs-vector-copy v 2 2))))

(test-equal "vector-separate!:ten:3:2:8"
  '#(9 13 13)
  (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
    (vector-separate! < v 3 2 8)
    (vector-sort < (r7rs-vector-copy v 2 5))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Sorting routines often have internal boundary cases or
;;; randomness, so it's prudent to run a lot of tests with
;;; different lengths.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (all-sorts-okay? m n)
  (if (> m 0)
      (let* ((v (random-vector n))
             (v2 (vector-copy v))
             (lst (vector->list v))
             (ans (vector-sort < v2))
             (med (cond ((= n 0) -97)
                        ((odd? n)
                         (vector-ref ans (quotient n 2)))
                        (else
                         (/ (+ (vector-ref ans (- (quotient n 2) 1))
                               (vector-ref ans (quotient n 2)))
                            2)))))
        (define (dsort vsort!)
          (let ((v2 (vector-copy v)))
            (vsort! < v2)
            v2))
        (and (equal? ans (list->vector (list-sort < lst)))
             (equal? ans (list->vector (list-stable-sort < lst)))
             (equal? ans (list->vector (list-sort! < (list-copy lst))))
             (equal? ans (list->vector (list-stable-sort! < (list-copy lst))))
             (equal? ans (vector-sort < v2))
             (equal? ans (vector-stable-sort < v2))
             (equal? ans (dsort vector-sort!))
             (equal? ans (dsort vector-stable-sort!))
             (equal? med (vector-find-median < v2 -97))
             (equal? v v2)
             (equal? lst (vector->list v))
             (equal? med (vector-find-median! < v2 -97))
             (equal? ans v2)
             (all-sorts-okay? (- m 1) n)))
      #t))

(test-eqv #t (all-sorts-okay? 3 0))
(test-eqv #t (all-sorts-okay? 5 1))
(test-eqv #t (all-sorts-okay? 10 2))
(test-eqv #t (all-sorts-okay? 10 3))
(test-eqv #t (all-sorts-okay? 10 4))
(test-eqv #t (all-sorts-okay? 20 5))
(test-eqv #t (all-sorts-okay? 20 10))
(test-eqv #t (all-sorts-okay? 10 20))
(test-eqv #t (all-sorts-okay? 10 30))
(test-eqv #t (all-sorts-okay? 10 40))
(test-eqv #t (all-sorts-okay? 10 50))
(test-eqv #t (all-sorts-okay? 10 99))
(test-eqv #t (all-sorts-okay? 10 100))
(test-eqv #t (all-sorts-okay? 10 101))
(test-eqv #t (all-sorts-okay? 10 499))
(test-eqv #t (all-sorts-okay? 10 500))
(test-eqv #t (all-sorts-okay? 10 501))

(test-end "srfi-132")
