;; Copyright 2002 Sebastian Egner
;; SPDX-License-Identifier: MIT

(test-begin "srfi-26")

;; The test cases are taken from the SRFI-26 test program by Sebastian Egner.
(test-equal "cut list" '() ((cut list)))
(test-equal "cut list <...>" '() ((cut list <...>)))
(test-equal "cut list 1" '(1) ((cut list 1)))
(test-equal "cut list <>" '(1) ((cut list <>) 1))
(test-equal "cut list <...>" '(1) ((cut list <...>) 1))
(test-equal "cut list 1 2" '(1 2) ((cut list 1 2)))
(test-equal "cut list 1 <>" '(1 2) ((cut list 1 <>) 2))
(test-equal "cut list 1 <...>" '(1 2) ((cut list 1 <...>) 2))
(test-equal "cut list 1 <...>" '(1 2 3 4) ((cut list 1 <...>) 2 3 4))
(test-equal "cut list 1 <> 3 <>" '(1 2 3 4) ((cut list 1 <> 3 <>) 2 4))
(test-equal "cut list 1 <> 3 <...>"
  '(1 2 3 4 5 6) ((cut list 1 <> 3 <...>) 2 4 5 6))
(test-equal "cut (eval order)" '(ok)
       (let* ([x 'wrong] [y (cut list x)]) (set! x 'ok) (y)))
(test-equal "cut (eval order)" 2
       (let ([a 0])
         (map (cut + (begin (set! a (+ a 1)) a) <>)
              '(1 2))
         a))

(test-equal "cute list" '() ((cute list)))
(test-equal "cute list <...>" '() ((cute list <...>)))
(test-equal "cute list 1" '(1) ((cute list 1)))
(test-equal "cute list <>" '(1) ((cute list <>) 1))
(test-equal "cute list <...>" '(1) ((cute list <...>) 1))
(test-equal "cute list 1 2" '(1 2) ((cute list 1 2)))
(test-equal "cute list 1 <>" '(1 2) ((cute list 1 <>) 2))
(test-equal "cute list 1 <...>" '(1 2) ((cute list 1 <...>) 2))
(test-equal "cute list 1 <...>" '(1 2 3 4) ((cute list 1 <...>) 2 3 4))
(test-equal "cute list 1 <> 3 <>" '(1 2 3 4) ((cute list 1 <> 3 <>) 2 4))
(test-equal "cute list 1 <> 3 <...>"
  '(1 2 3 4 5 6) ((cute list 1 <> 3 <...>) 2 4 5 6))
(test-equal "cute (eval order)" '(ok)
       (let* ([x 'ok] [y (cute list x)]) (set! x 'wrong) (y)))
(test-equal "cute (eval order)" 1
       (let ([a 0])
         (map (cute + (begin (set! a (+ a 1)) a) <>)
              '(1 2))
         a))

(test-end "srfi-26")
