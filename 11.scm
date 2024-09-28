;; Copyright 1999 Lars T Hansen
;; SPDX-License-Identifier: MIT

(test-begin "srfi-11")

(test-equal
 '(1 2 (3 4))
 (let-values (((a b . c) (values 1 2 3 4)))
   (list a b c)))

(test-equal
 '(x y a b)
 (let ((a 'a) (b 'b) (x 'x) (y 'y))
   (let-values (((a b) (values x y))
                ((x y) (values a b)))
     (list a b x y))))

(test-equal
 '(x y x y)
 (let ((a 'a) (b 'b) (x 'x) (y 'y))
   (let*-values (((a b) (values x y))
                 ((x y) (values a b)))
     (list a b x y))))

(test-end "srfi-11")
