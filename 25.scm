;;; array test
;;; 2001 Jussi Piitulainen
;;; 2026 Retropikzel

(test-begin "srfi-25")

;;; Simple tests

(test-begin "shape")

(test-assert (shape))
(test-assert (shape -1 -1))
(test-assert (shape -1 0))
(test-assert (shape -1 1))
(test-assert (shape 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8))

(test-assert (make-array (shape)))
(test-assert (make-array (shape) *))
(test-assert (make-array (shape -1 -1)))
(test-assert (make-array (shape -1 -1) *))
(test-assert (make-array (shape -1 1)))
(test-assert (make-array (shape 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8 1 2 3 4) *))

(test-end "shape")


(test-begin "make-array")

(test-assert (array (shape) *))
(test-assert (array (shape -1 -1)))
(test-assert (array (shape -1 1) * *))
(test-assert (array (shape 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8) *))

(test-end "make-array")


(test-begin "array")

(test-assert (= (array-rank (shape)) 2))
(test-assert (= (array-rank (shape -1 -1)) 2))
(test-assert (= (array-rank (shape -1 1)) 2))
(test-assert (= (array-rank (shape 1 2 3 4 5 6 7 8)) 2))

(test-end "array")


(test-begin "array-rank of shape")

(test-assert (= (array-rank (make-array (shape))) 0))
(test-assert (= (array-rank (make-array (shape -1 -1))) 1))
(test-assert (= (array-rank (make-array (shape -1 1))) 1))
(test-assert (= (array-rank (make-array (shape 1 2 3 4 5 6 7 8))) 4))

(test-end "array-rank of shape")


(test-begin "array-rank of make-array")

(test-assert (= (array-rank (array (shape) *)) 0))
(test-assert (= (array-rank (array (shape -1 -1))) 1))
(test-assert (= (array-rank (array (shape -1 1) * *)) 1))
(test-assert (= (array-rank (array (shape 1 2 3 4 5 6 7 8) *)) 4))

(test-end "array-rank of make-array")


(test-begin "array-rank of array")

(test-assert (= (array-start (shape -1 -1) 0) 0))
(test-assert (= (array-start (shape -1 -1) 1) 0))
(test-assert (= (array-start (shape -1 1) 0) 0))
(test-assert (= (array-start (shape -1 1) 1) 0))
(test-assert (= (array-start (shape 1 2 3 4 5 6 7 8) 0) 0))
(test-assert (= (array-start (shape 1 2 3 4 5 6 7 8) 1) 0))

(test-end "array-rank of array")


(test-begin "array-start of shape")

(test-assert (= (array-end (shape -1 -1) 0) 1))
(test-assert (= (array-end (shape -1 -1) 1) 2))
(test-assert (= (array-end (shape -1 1) 0) 1))
(test-assert (= (array-end (shape -1 1) 1) 2))
(test-assert (= (array-end (shape 1 2 3 4 5 6 7 8) 0) 4))
(test-assert (= (array-end (shape 1 2 3 4 5 6 7 8) 1) 2))

(test-end "array-start of shape")


(test-begin "array-end of shape")

(test-assert (= (array-start (make-array (shape -1 -1)) 0) -1))
(test-assert (= (array-start (make-array (shape -1 1)) 0) -1))
(test-assert (= (array-start (make-array (shape 1 2 3 4 5 6 7 8)) 0) 1))
(test-assert (= (array-start (make-array (shape 1 2 3 4 5 6 7 8)) 1) 3))
(test-assert (= (array-start (make-array (shape 1 2 3 4 5 6 7 8)) 2) 5))
(test-assert (= (array-start (make-array (shape 1 2 3 4 5 6 7 8)) 3) 7))

(test-end "array-end of shape")


(test-begin "array-start of make-array")

(test-assert (= (array-end (make-array (shape -1 -1)) 0) -1))
(test-assert (= (array-end (make-array (shape -1 1)) 0) 1))
(test-assert (= (array-end (make-array (shape 1 2 3 4 5 6 7 8)) 0) 2))
(test-assert (= (array-end (make-array (shape 1 2 3 4 5 6 7 8)) 1) 4))
(test-assert (= (array-end (make-array (shape 1 2 3 4 5 6 7 8)) 2) 6))
(test-assert (= (array-end (make-array (shape 1 2 3 4 5 6 7 8)) 3) 8))

(test-end "array-start of make-array")


(test-begin "array-end of make-array")

(test-assert (= (array-start (array (shape -1 -1)) 0) -1))
(test-assert (= (array-start (array (shape -1 1) * *) 0) -1))
(test-assert (= (array-start (array (shape 1 2 3 4 5 6 7 8) *) 0) 1))
(test-assert (= (array-start (array (shape 1 2 3 4 5 6 7 8) *) 1) 3))
(test-assert (= (array-start (array (shape 1 2 3 4 5 6 7 8) *) 2) 5))
(test-assert (= (array-start (array (shape 1 2 3 4 5 6 7 8) *) 3) 7))

(test-end "array-end of make-array")


(test-begin "array-start of array")

(test-assert (= (array-end (array (shape -1 -1)) 0) -1))
(test-assert (= (array-end (array (shape -1 1) * *) 0) 1))
(test-assert (= (array-end (array (shape 1 2 3 4 5 6 7 8) *) 0) 2))
(test-assert (= (array-end (array (shape 1 2 3 4 5 6 7 8) *) 1) 4))
(test-assert (= (array-end (array (shape 1 2 3 4 5 6 7 8) *) 2) 6))
(test-assert (= (array-end (array (shape 1 2 3 4 5 6 7 8) *) 3) 8))

(test-end "array-start of array")


(test-begin "array-end of array")

(test-assert (eq? (array-ref (make-array (shape) 'a)) 'a))
(test-assert (eq? (array-ref (make-array (shape -1 1) 'b) -1) 'b))
(test-assert (eq? (array-ref (make-array (shape -1 1) 'c) 0) 'c))
(test-assert (eq? (array-ref (make-array (shape 1 2 3 4 5 6 7 8) 'd) 1 3 5 7) 'd))

(test-end "array-end of array")


(test-begin "array-ref of make-array with arguments")

(test-assert (eq? (array-ref (make-array (shape) 'a) '#()) 'a))
(test-assert (eq? (array-ref (make-array (shape -1 1) 'b) '#(-1)) 'b))
(test-assert (eq? (array-ref (make-array (shape -1 1) 'c) '#(0)) 'c))
(test-assert (eq? (array-ref (make-array (shape 1 2 3 4 5 6 7 8) 'd)
                         '#(1 3 5 7))
              'd))

(test-end "array-ref of make-array with arguments")


(test-begin "array-ref of make-array with vector")

(test-assert (eq? (array-ref (make-array (shape) 'a)
                             (array (shape 0 0)))
                  'a))
(test-assert (eq? (array-ref (make-array (shape -1 1) 'b)
                             (array (shape 0 1) -1))
                  'b))
(test-assert (eq? (array-ref (make-array (shape -1 1) 'c)
                             (array (shape 0 1) 0))
                  'c))
(test-assert (eq? (array-ref (make-array (shape 1 2 3 4 5 6 7 8) 'd)
                             (array (shape 0 4) 1 3 5 7))
                  'd))

(test-end "array-ref of make-array with vector")


(test-begin "array-ref of make-array with array")

(test-assert (let ((arr (make-array (shape) 'o)))
               (array-set! arr 'a)
               (eq? (array-ref arr) 'a)))
(test-assert (let ((arr (make-array (shape -1 1) 'o)))
               (array-set! arr -1 'b)
               (array-set! arr 0 'c)
               (and (eq? (array-ref arr -1) 'b)
                    (eq? (array-ref arr 0) 'c))))
(test-assert (let ((arr (make-array (shape 1 2 3 4 5 6 7 8) 'o)))
               (array-set! arr 1 3 5 7 'd)
               (eq? (array-ref arr 1 3 5 7) 'd)))

(test-end "array-ref of make-array with array")


(test-begin "array-set! of make-array with arguments")

(test-assert (let ((arr (make-array (shape) 'o)))
               (array-set! arr '#() 'a)
               (eq? (array-ref arr) 'a)))
(test-assert (let ((arr (make-array (shape -1 1) 'o)))
               (array-set! arr '#(-1) 'b)
               (array-set! arr '#(0) 'c)
               (and (eq? (array-ref arr -1) 'b)
                    (eq? (array-ref arr 0) 'c))))
(test-assert (let ((arr (make-array (shape 1 2 3 4 5 6 7 8) 'o)))
               (array-set! arr '#(1 3 5 7) 'd)
               (eq? (array-ref arr 1 3 5 7) 'd)))

(test-end "array-set! of make-array with arguments")


(test-begin "array-set! of make-array with vector")

(test-assert (let ((arr (make-array (shape) 'o)))
               (array-set! arr 'a)
               (eq? (array-ref arr) 'a)))
(test-assert (let ((arr (make-array (shape -1 1) 'o)))
               (array-set! arr (array (shape 0 1) -1) 'b)
               (array-set! arr (array (shape 0 1) 0) 'c)
               (and (eq? (array-ref arr -1) 'b)
                    (eq? (array-ref arr 0) 'c))))
(test-assert (let ((arr (make-array (shape 1 2 3 4 5 6 7 8) 'o)))
               (array-set! arr (array (shape 0 4) 1 3 5 7) 'd)
               (eq? (array-ref arr 1 3 5 7) 'd)))

(test-end "array-set! of make-array with vector")


(test-begin "array-set! of make-array with array")

;;; Share and change:
;;;
;;;  org     brk     swp            box
;;;
;;;   0 1     1 2     5 6
;;; 6 a b   2 a b   3 d c   0 2 4 6 8: e
;;; 7 c d   3 e f   4 f e
;;; 8 e f

(let* ((org (array (shape 6 9 0 2) 'a 'b 'c 'd 'e 'f))
       (brk (share-array
              org
              (shape 2 4 1 3)
              (lambda (r k)
                (values
                  (+ 6 (* 2 (- r 2)))
                  (- k 1)))))
       (swp (share-array
              org
              (shape 3 5 5 7)
              (lambda (r k)
                (values
                  (+ 7 (- r 3))
                  (- 1 (- k 5))))))
       (box (share-array
              swp
              (shape 0 1 2 3 4 5 6 7 8 9)
              (lambda _ (values 4 6))))
       (org-contents (lambda ()
                       (list (array-ref org 6 0) (array-ref org 6 1)
                             (array-ref org 7 0) (array-ref org 7 1)
                             (array-ref org 8 0) (array-ref org 8 1))))
       (brk-contents (lambda ()
                       (list (array-ref brk 2 1) (array-ref brk 2 2)
                             (array-ref brk 3 1) (array-ref brk 3 2))))
       (swp-contents (lambda ()
                       (list (array-ref swp 3 5) (array-ref swp 3 6)
                             (array-ref swp 4 5) (array-ref swp 4 6))))
       (box-contents (lambda ()
                       (list (array-ref box 0 2 4 6 8)))))
  (test-assert (equal? (org-contents) '(a b c d e f)))
  (test-assert (equal? (brk-contents) '(a b e f)))
  (test-assert (equal? (swp-contents) '(d c f e)))
  (test-assert (equal? (box-contents) '(e)))
  (begin (array-set! org 6 0 'x) #t)
  (test-assert (equal? (org-contents) '(x b c d e f)))
  (test-assert (equal? (brk-contents) '(x b e f)))
  (test-assert (equal? (swp-contents) '(d c f e)))
  (test-assert (equal? (box-contents) '(e)))
  (begin (array-set! brk 3 1 'y) #t)
  (test-assert (equal? (org-contents) '(x b c d y f)))
  (test-assert (equal? (brk-contents) '(x b y f)))
  (test-assert (equal? (swp-contents) '(d c f y)))
  (test-assert (equal? (box-contents) '(y)))
  (begin (array-set! swp 4 5 'z) #t)
  (test-assert (equal? (org-contents) '(x b c d y z)))
  (test-assert (equal? (brk-contents) '(x b y z)))
  (test-assert (equal? (swp-contents) '(d c z y)))
  (test-assert (equal? (box-contents) '(y)))
  (begin (array-set! box 0 2 4 6 8 'e) #t)
  (test-assert (equal? (org-contents) '(x b c d e z)))
  (test-assert (equal? (brk-contents) '(x b e z)))
  (test-assert (equal? (swp-contents) '(d c z e)))
  (test-assert (equal? (box-contents) '(e))))

(test-end "array-set! of make-array with array")


(test-begin "shared change")

;;; Check that arrays copy the shape specification

(let ((shp (shape 10 12)))
  (let ((arr (make-array shp))
        (ars (array shp * *))
        (art (share-array (make-array shp) shp (lambda (k) k))))
    (array-set! shp 0 0 '?)
    (array-set! shp 0 1 '!)
    (test-assert (= (array-rank shp) 2))
    (test-assert (= (array-start shp 0) 0))
    (test-assert (= (array-end shp 0) 1))
    (test-assert (= (array-start shp 1) 0))
    (test-assert (= (array-end shp 1) 2))
    (test-assert (eq? (array-ref shp 0 0) '?))
    (test-assert (eq? (array-ref shp 0 1) '!))
    (test-assert (= (array-rank arr) 1))
    (test-assert (= (array-start arr 0) 10))
    (test-assert (= (array-end arr 0) 12))
    (test-assert (= (array-rank ars) 1))
    (test-assert (= (array-start ars 0) 10))
    (test-assert (= (array-end ars 0) 12))
    (test-assert (= (array-rank art) 1))
    (test-assert (= (array-start art 0) 10))
    (test-assert (= (array-end art 0) 12))))

(test-end "shared change")


(test-begin "array-set! of shape")

;;; Check that index arrays work even when they share
;;;
;;; arr       ixn
;;;   5  6      0 1
;;; 4 nw ne   0 4 6
;;; 5 sw se   1 5 4

(let ((arr (array (shape 4 6 5 7) 'nw 'ne 'sw 'se))
      (ixn (array (shape 0 2 0 2) 4 6 5 4)))
  (let ((col0 (share-array
                ixn
                (shape 0 2)
                (lambda (k)
                  (values k 0))))
        (row0 (share-array
                ixn
                (shape 0 2)
                (lambda (k)
                  (values 0 k))))
        (wor1 (share-array
                ixn
                (shape 0 2)
                (lambda (k)
                  (values 1 (- 1 k)))))
        (cod (share-array
               ixn
               (shape 0 2)
               (lambda (k)
                 (case k
                   ((0) (values 1 0))
                   ((1) (values 0 1))))))
        (box (share-array
               ixn
               (shape 0 2)
               (lambda (k)
                 (values 1 0)))))
    (test-assert (eq? (array-ref arr col0) 'nw))
    (test-assert (eq? (array-ref arr row0) 'ne))
    (test-assert (eq? (array-ref arr wor1) 'nw))
    (test-assert (eq? (array-ref arr cod) 'se))
    (test-assert (eq? (array-ref arr box) 'sw))
    (begin
      (array-set! arr col0 'ul)
      (array-set! arr row0 'ur)
      (array-set! arr cod 'lr)
      (array-set! arr box 'll)
      #t)
    (test-assert (eq? (array-ref arr 4 5) 'ul))
    (test-assert (eq? (array-ref arr 4 6) 'ur))
    (test-assert (eq? (array-ref arr 5 5) 'll))
    (test-assert (eq? (array-ref arr 5 6) 'lr))
    (begin
      (array-set! arr wor1 'xx)
      (test-assert (eq? (array-ref arr 4 5) 'xx)))))

(test-end "array-set! of shape")


(test-begin "array access with sharing index array")

;;; Check that shape arrays work even when they share
;;;
;;; arr             shp       shq       shr       shs
;;;    1  2  3  4      0  1      0  1      0  1      0  1 
;;; 1 10 12 16 20   0 10 12   0 12 20   0 10 10   0 12 12
;;; 2 10 11 12 13   1 10 11   1 11 13   1 11 12   1 12 12
;;;                                     2 12 16
;;;                                     3 13 20

(let ((arr (array (shape 1 3 1 5) 10 12 16 20 10 11 12 13)))
  (let ((shp (share-array
               arr
               (shape 0 2 0 2)
               (lambda (r k)
                 (values (+ r 1) (+ k 1)))))
        (shq (share-array
               arr
               (shape 0 2 0 2)
               (lambda (r k)
                 (values (+ r 1) (* 2 (+ 1 k))))))
        (shr (share-array
               arr
               (shape 0 4 0 2)
               (lambda (r k)
                 (values (- 2 k) (+ r 1)))))
        (shs (share-array
               arr
               (shape 0 2 0 2)
               (lambda (r k)
                 (values 2 3)))))
    (let ((arr-p (make-array shp)))
      (test-assert (= (array-rank arr-p) 2))
      (test-assert (= (array-start arr-p 0) 10))
      (test-assert (= (array-end arr-p 0) 12))
      (test-assert (= (array-start arr-p 1) 10))
      (test-assert (= (array-end arr-p 1) 11)))
    (let ((arr-q (array shq * * * *  * * * *  * * * *  * * * *)))
      (test-assert (= (array-rank arr-q) 2))
      (test-assert (= (array-start arr-q 0) 12))
      (test-assert (= (array-end arr-q 0) 20))
      (test-assert (= (array-start arr-q 1) 11))
      (test-assert (= (array-end arr-q 1) 13)))
    (let ((arr-r (share-array
                   (array (shape) *)
                   shr
                   (lambda _ (values)))))
      (test-assert (= (array-rank arr-r) 4))
      (test-assert (= (array-start arr-r 0) 10))
      (test-assert (= (array-end arr-r 0) 10))
      (test-assert (= (array-start arr-r 1) 11))
      (test-assert (= (array-end arr-r 1) 12))
      (test-assert (= (array-start arr-r 2) 12))
      (test-assert (= (array-end arr-r 2) 16))
      (test-assert (= (array-start arr-r 3) 13))
      (test-assert (= (array-end arr-r 3) 20)))
    (let ((arr-s (make-array shs)))
      (test-assert (= (array-rank arr-s) 2))
      (test-assert (= (array-start arr-s 0) 12))
      (test-assert (= (array-end arr-s 0) 12))
      (test-assert (= (array-start arr-s 1) 12))
      (test-assert (= (array-end arr-s 1) 12)))))

(test-end "array access with sharing index array")


(test-begin "sharing shape array")

(let ((super (array (shape 4 7 4 7)
                    1 * *
                    * 2 *
                    * * 3))
      (subshape (share-array
                  (array (shape 0 2 0 3)
                         * 4 *
                         * 7 *)
                  (shape 0 1 0 2)
                  (lambda (r k)
                    (values k 1)))))
  (let ((sub (share-array super subshape (lambda (k) (values k k)))))
    ;(array-equal? subshape (shape 4 7))
    (test-assert (= (array-rank subshape) 2))
    (test-assert (= (array-start subshape 0) 0))
    (test-assert (= (array-end subshape 0) 1))
    (test-assert (= (array-start subshape 1) 0))
    (test-assert (= (array-end subshape 1) 2))
    (test-assert (= (array-ref subshape 0 0) 4))
    (test-assert (= (array-ref subshape 0 1) 7))
    ;(array-equal? sub (array (shape 4 7) 1 2 3))
    (test-assert (= (array-rank sub) 1))
    (test-assert (= (array-start sub 0) 4))
    (test-assert (= (array-end sub 0) 7))
    (test-assert (= (array-ref sub 4) 1))
    (test-assert (= (array-ref sub 5) 2))
    (test-assert (= (array-ref sub 6) 3))))

(test-end "sharing shape array")

(test-end "srfi-25")
