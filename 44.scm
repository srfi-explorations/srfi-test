;; Tests for SRFI 44: Collections.
;; To run: chibi-scheme/gosh -A. tests/test-srfi-44.scm

;; SPDX-License-Identifier: MIT
;; SPDX-FileCopyrightText: 2024 Antero Mejr <mail@antr.me>

(import (except (scheme base)
                vector? make-vector vector map vector-copy vector-ref
                vector-set! vector->list
                list? make-list list list-ref list-set! list-copy
                string? make-string string string-copy string-ref string->list
                string-set!)
        (prefix (only (scheme base) list-ref) r7rs:)
        (srfi 8)
        (srfi 44)
        (srfi 64))

(test-begin "SRFI-44")

(test-group "collection"
  (let ((col (collection 1 2 3)))
    (test-assert "predicate" (collection? col))
    (test-assert "name" (eq? 'collection (collection-name col)))
    (test-assert "make-*" (collection? (make-collection)))
    (test-assert "size" (zero? (collection-size (make-collection))))
    (test-assert "size" (= 3 (collection-size col)))
    (test-assert "get-any" (number? (collection-get-any col)))
    (test-assert "empty" (collection-empty? (make-collection)))
    (test-assert "->list" (null? (collection->list (make-collection))))
    (test-assert "clear" (collection-empty? (collection-clear col)))
    (collection-clear! col)
    (test-assert "clear!" (collection-empty? col))
    (test-assert "=" (collection= = (collection 1 2 3) (collection 3 2 1)))
    (test-assert "constructor" (collection? (collection 1 2 3)))
    (let ((col2 (collection 1 2 3)))
      (test-assert "copy" (collection= = (collection-copy col2) col2)))
    (test-assert "fold-left" (equal? '(3 2 1)
                                     (collection-fold-left
                                      (collection 1 2 3)
                                      (lambda (x acc)
                                        (values #t (cons x acc)))
                                      '())))
    (test-assert "fold-right" (= -6 (collection-fold-right
                                     (collection 3 2 1)
                                     (lambda (x acc)
                                       (values #t (- acc x)))
                                     0)))))

(test-group "inheritance"
  (test-assert "collection= list"
               (collection= = (collection-clear (list 1 2 3)) (list)))
  (test-assert "clear sequence"
               (collection-empty? (collection-clear (sequence 1 2 3)))))

(test-group "limited-collection"
  (test-assert "vector limited" (limited-collection? (vector 1 2 3)))
  (test-assert "string limited" (limited-collection? (string #\a #\b #\c))))

(test-group "purely-mutable-collection"
  (test-assert "vector purely mutable"
               (purely-mutable-collection? (vector 1 2 3)))
  (test-assert "string purely mutable"
               (purely-mutable-collection? (string #\a #\b #\c))))

(test-group "ordered-collection"
  (let ((ocol (make-ordered-collection r7rs:list-ref)))
    (sequence-insert-right! ocol 1)
    (sequence-insert-right! ocol 2)
    (sequence-insert-right! ocol 3)
    (test-assert "predicate" (ordered-collection? ocol))
    (test-assert "ofunc accessor"
                 (procedure? (ordered-collection-ordering-function ocol)))
    (test-assert "get-left" (= 1 (ordered-collection-get-left ocol)))
    (test-assert "get-right" (= 3 (ordered-collection-get-right ocol)))
    (receive (rest l) (ordered-collection-delete-left ocol)
      (test-assert "delete-left" (= 1 l)))
    (let ((ocol2 (make-ordered-collection r7rs:list-ref)))
      (sequence-insert-right! ocol2 1)
      (sequence-insert-right! ocol2 2)
      (sequence-insert-right! ocol2 3)
      (test-assert "equality" (collection= = ocol ocol2)))
    (let ((my-list (make-ordered-collection r7rs:list-ref)))
      (sequence-insert-right! my-list 1)
      (sequence-insert-right! my-list 2)
      (receive (c v) (ordered-collection-delete-left! my-list)
        (test-assert "delete-left! (value)" (= v 1))
        (test-assert "delete-left!"
                     (collection=
                      = (sequence-insert-right
                         (make-ordered-collection r7rs:list-ref) 2)
                      my-list))))
    (let ((my-list (make-ordered-collection r7rs:list-ref)))
      (sequence-insert-right! my-list 1)
      (receive (rest r) (ordered-collection-delete-right my-list)
        (test-assert "delete-right (value)" (= 1 r))
        (test-assert "delete-right (collection)" (collection-empty? rest))))
    (let ((ocol2 (make-ordered-collection r7rs:list-ref)))
      (sequence-insert-right! ocol2 1)
      (receive (_ r) (ordered-collection-delete-right! ocol2)
        (test-assert "delete-right!" (collection-empty? ocol2))))
    (test-assert "make-*"
                 (ordered-collection?
                  (make-ordered-collection r7rs:list-ref)))))

(test-group "directional-collection"
  ;; (test-assert "predicate" (directional-collection? (sequence 1 2 3)))
  (test-assert "get-left"
               (= 1 (directional-collection-get-left (sequence 1 2 3))))
  (test-assert "get-right"
               (= 3 (directional-collection-get-right (sequence 1 2 3))))
  (test-assert "insert-left"
               (= 1 (directional-collection-get-left
                     (directional-collection-insert-left (sequence 2 3) 1))))
  (test-assert "insert-right"
               (= 1 (directional-collection-get-right
                     (directional-collection-insert-right (sequence 2 3) 1))))
  (let ((dc (sequence 1 2 3)))
    (directional-collection-insert-left! dc 0)
    (test-assert "insert-left!" (collection= = (sequence 0 1 2 3) dc)))
  (let ((dc (sequence 1 2 3)))
    (directional-collection-insert-right! dc 4)
    (test-assert "insert-left!" (collection= = (sequence 1 2 3 4) dc)))
  (test-assert "delete-left"
               (collection= = (sequence 2 3)
                            (receive (x v) (directional-collection-delete-left
                                            (sequence 1 2 3))
                              x)))
  (test-assert "delete-right"
               (collection= = (sequence 1 2)
                            (receive (x v) (directional-collection-delete-right
                                            (sequence 1 2 3))
                              x)))
  (let ((dc (sequence 1 2 3)))
    (directional-collection-delete-left! dc)
    (test-assert "delete-left!" (collection= = dc (sequence 2 3))))
  (let ((dc (sequence 1 2 3)))
    (directional-collection-delete-right! dc)
    (test-assert "delete-right!" (collection= = dc (sequence 1 2)))))

(test-group "bag"
  (test-assert "predicate" (bag? (make-bag)))
  (test-assert "constructor" (bag? (bag 1 2 3)))
  (test-assert "equiv-func accessor"
               (procedure? (bag-equivalence-function (bag 1))))
  (test-assert "count" (= 1 (bag-count (bag 3 2 1) 1)))
  (test-assert "contains?" (bag-contains? (bag 1 2 3) 3))
  (test-assert "add" (bag-contains? (bag-add (bag 2 3) 1) 1))
  (let ((b (bag 1 2 3)))
    (bag-add! b 4)
    (test-assert "add!" (bag-contains? b 4)))
  (test-assert "delete"
               (collection= = (bag 2 3) (bag-delete (bag 1 2 3) 1)))
  (let ((b (bag 1 2 3)))
    (bag-delete! b 1)
    (test-assert "delete!" (collection= equal? (bag 2 3) b)))
  (test-assert "delete-all"
               (collection= (bag 2 2) (bag-delete-all (bag 1 2 1 2) 1)))
  (let ((b (bag 1 2 1 2)))
    (bag-delete-all! b 1)
    (test-assert "delete-all!" (collection= equal? (bag 2 2) b)))
  (test-assert "add-from"
               (collection= (bag 1 2 3) (bag-add-from (bag 1) (bag 2 3))))
  (let ((b (bag 1 2 3)))
    (bag-add-from! b (bag 4))
    (test-assert "add-from!" (collection= equal? b (bag 1 2 3 4))))
  (test-assert "delete-from"
               (collection= (bag 1 2) (bag-delete-from (bag 1 2 3) (bag 3))))
  (let ((b (bag 1 2 3)))
    (bag-delete-from! b (bag 2 3))
    (test-assert "delete-from!" (collection= equal? b (bag 1))))
  (test-assert "delete-all-from" (collection= = (bag 1) (bag-delete-all-from
                                                         (bag 1 2 3)
                                                         (bag 3 2))))
  (let ((b (bag 1 2 3)))
    (bag-delete-all-from! (bag 3 2) b)
    (test-assert "delete-all-from!" (collection= (bag 1) b))))

(test-group "set"
  (test-assert "predicate" (set? (make-set)))
  (test-assert "constructor" (set? (set 1 2 2 3)))
  (test-assert "equiv-func accessor"
               (procedure? (set-equivalence-function (set))))
  (test-assert "contains?" (set-contains? (set 1 2 3) 2))
  (test-assert "subset?" (set-subset? (set 1 2) (set 1 2) (set 1 2 3)))
  (test-assert "add" (collection= = (set-add (set 2 3) 1) (set 3 2 1)))
  (let ((s (set 2 3)))
    (set-add! s 1)
    (test-assert "add!" (collection= = (set-add (set 2 3) 1) (set 3 2 1))))
  (test-assert "delete" (collection= = (set-delete (set 1 2 3) 2) (set 1 3)))
  (test-assert "count" (= 1 (set-count (set 2 2 2) 2)))
  (let ((s (set 1 2 3)))
    (set-delete! s 2)
    (test-assert "delete!" (collection= =  s (set 1 3))))
  (test-assert "union" (collection= = (set-union (set 2) (set 1 3))
                                    (set 1 2 3)))
  (let ((s (set 1)))
    (set-union! s (set 3 2))
    (test-assert "union!" (collection= = s (set 1 2 3))))
  (test-assert "intersection" (collection= (set 2) (set-intersection
                                                    (set 1 2) (set 2 3))))
  (let ((s (set 1 2)))
    (set-intersection! (set 3 2))
    (test-assert "intersection!" (collection= (set 2) s)))
  (test-assert "difference" (collection= = (set 1) (set-difference (set 1 2)
                                                                   (set 2 3))))
  (let ((s (set 1 2 3)))
    (set-difference! s (set 2 3))
    (test-assert "difference!" (collection= = s (set 1))))
  (test-assert "symmetric-difference"
               (collection= = (set 3 1) (set-symmetric-difference (set 1 2)
                                                                  (set 2 3))))
  (let ((s (set 1 2)))
    (set-symmetric-difference! s (set 2 3))
    (test-assert "symmetric-difference!" (collection= (set 1 3) s)))
  (test-assert "add-from" (collection= = (set-add-from (set 1) (bag 2))
                                       (set 1 2)))
  (let ((s (set 1)))
    (set-add-from! s (bag 2))
    (test-assert "add-from" (collection= = s (set 2 1))))
  (test-assert "delete-from" (collection= = (set-delete-from (set 1 2) (bag 1))
                                          (set 2)))
  (let ((s (set 1 2)))
    (set-delete-from! s (bag 1))
    (test-assert "delete-from" (collection= = s (set 2)))))

(test-group "map"
  (test-assert "predicate" (map? (make-map)))
  (test-assert "constructor" (map? (map '(1 . 2) '(2 . 3))))
  (test-assert "equiv-func accessor"
               (procedure? (map-equivalence-function (make-map))))
  (test-assert "key-equiv-func accessor"
               (procedure? (map-key-equivalence-function (make-map))))
  (test-assert "contains-key"
               (map-contains-key? (map (cons 'b 2) (cons 'a 1)) 'a))
  (test-assert "contains-key (not)"
               (not (map-contains-key? (map (cons 'b 2) (cons 'a 1)) 'c)))
  (test-assert "keys->list" (equal? (map-keys->list (map (cons 'a 1))) '(a)))
  (test-assert "get" (= 1 (map-get (map (cons 'a 1)) 'a)))
  (test-assert "get (thunk)" (= 2 (map-get (map (cons 'a 1)) 'b (lambda _ 2))))
  (test-assert "put" (collection=
                      equal? (receive (m _) (map-put (map (cons 'a 0)) 'a 1) m)
                      (map (cons 'a 1))))
  (test-assert "count" (= 2 (map-count (map (cons 1 2) (cons 1 2)) (cons 1 2))))
  (let ((m (map (cons 'a 1))))
    (map-put! m 'a 2)
    (test-assert "put!" (collection= equal? (map (cons 'a 2)) m)))
  #;(test-assert "update" (collection=
  = (map-update (map (cons 'a 0))
  'a (lambda (x) (+ x 1)))
  (map (cons 'a 1))))
  (let ((m (map (cons 'a 1))))
    (map-update! m 'a (lambda (x) (+ x 1)))
    (test-assert "update!" (collection= equal? (map (cons 'a 2)) m)))
  (test-assert "delete" (collection= equal? (map-delete (map (cons 'a 1)) 'a)
                                     (map)))
  (let ((m (map (cons 'a 1))))
    (map-delete! m 'a)
    (test-assert "delete!" (collection= equal? (map) m)))
  (test-assert "delete-from" (collection= equal? (map (cons 'a 1))
                                          (map-delete-from
                                           (map (cons 'a 1) (cons 'b 2))
                                           (bag 'b))))
  (let ((m (map (cons 'a 1) (cons 'b 2))))
    (map-delete-from! m (bag 'b))
    (test-assert "delete-from!" (collection= equal? (map (cons 'a 1)) m)))
  (test-assert "add-from" (collection= equal? (map (cons 'a 1) (cons 'b 2))
                                       (map-add-from (map (cons 'b 1))
                                                     (map (cons 'a 1)
                                                          (cons 'b 2)))))
  (let ((m (map (cons 'b 1))))
    (map-add-from! m (map (cons 'a 1) (cons 'b 2)))
    (test-assert "add-from!" (collection= equal?
                                          (map (cons 'a 1) (cons 'b 2)) m)))
  (test-assert "fold-keys-left"
               (equal? '(b a) (map-fold-keys-left (map (cons 'a 1) (cons 'b 2))
                                                  (lambda (key val acc)
                                                    (values #t (cons key acc)))
                                                  '())))
  (test-assert "fold-keys-right"
               (equal? '(a b) (map-fold-keys-right (map (cons 'a 1) (cons 'b 2))
                                                   (lambda (key val acc)
                                                     (values #t (cons key acc)))
                                                   '()))))

(test-group "sequence"
  (test-assert "predicate" (sequence? (make-sequence)))
  (test-assert "constructor"  (sequence? (sequence 1 2 3)))
  (test-assert "ref" (= 1 (sequence-ref (sequence 1 2 3) 0)))
  (test-assert "get-left" (= 1 (sequence-get-left (sequence 1 2 3))))
  (test-assert "get-right"  (= 3 (sequence-get-right (sequence 1 2 3))))
  (test-assert "insert-right"
               (collection= = (sequence-insert-right (sequence 1 2) 3)
                            (sequence 1 2 3)))
  (let ((seq (sequence 1 2)))
    (sequence-insert-right! seq 3)
    (test-assert "insert-right!" (collection= = (sequence 1 2 3) seq)))
  (test-assert "set" (collection= = (sequence 1 2 3)
                                  (sequence-set (sequence 1 1 3) 1 2)))
  (let ((seq (sequence 1 1 3)))
    (sequence-set! seq 1 2)
    (test-assert "set!" (collection= = (sequence 1 2 3) seq)))
  (test-assert "replace-from"
               (collection= = (sequence 1 2 3)
                            (sequence-replace-from (sequence 1 0 0) 1
                                                   (sequence 2 3))))
  (let ((seq (sequence 1 0 0)))
    (sequence-replace-from! seq 1 (sequence 2 3))
    (test-assert "replace-from!" (collection= = (sequence 1 2 3) seq)))
  (test-assert "fold-keys-left"
               (equal? '(2 1 0) (sequence-fold-keys-left
                                 (sequence 1 2 3)
                                 (lambda (key val acc)
                                   (values #t (cons key acc)))
                                 '())))
  (test-assert "fold-keys-right"
               (equal? '(0 1 2) (sequence-fold-keys-right
                                 (sequence 1 2 3)
                                 (lambda (key val acc)
                                   (values #t (cons key acc)))
                                 '()))))

(test-group "flexible-sequence"
  (test-assert "predicate" (flexible-sequence? (make-flexible-sequence)))
  (test-assert "constructor" (flexible-sequence? (flexible-sequence 1 2 3)))
  (test-assert "insert"
               (collection=
                = (flexible-sequence-insert (flexible-sequence 2 3) 0 1)
                (flexible-sequence 1 2 3)))
  (let ((fs (flexible-sequence 2 3)))
    (flexible-sequence-insert! fs 0 1)
    (test-assert "insert!" (collection= = fs (flexible-sequence 1 2 3))))
  (test-assert "delete-at"
               (collection= = (flexible-sequence 1 2 3)
                            (flexible-sequence-delete-at
                             (flexible-sequence 1 2 2 3) 1)))
  (let ((fs (flexible-sequence 1 2 2 3)))
    (flexible-sequence-delete-at! fs 1)
    (test-assert "delete-at!" (collection= = (flexible-sequence 1 2 3) fs))))

(test-group "vector"
  (test-assert "predicate" (vector? (make-vector 10)))
  (test-assert "constructor" (vector? (vector 1 2 3))))

(test-group "list"
  (test-assert "predicate" (list? (make-list)))
  (test-assert "constructor" (list? (list 1 2 3))))

(test-group "string"
  (test-assert "predicate" (string? (make-string 10)))
  (test-assert "constructor" (string? (string #\a #\b #\c))))

(test-group "alist-map"
  (test-assert "predicate" (alist-map? (make-alist-map =)))
  (test-assert "constructor" (alist-map? (alist-map eq? '(1 . 2) '(3 . 4))))
  (test-assert "delete-all" (collection=
                             equal?
                             (alist-map-delete-all (alist-map eq? (cons 'a 1)
                                                              (cons 'a 2))
                                                   'a)
                             (alist-map eq?)))
  (let ((am (alist-map eq? (cons 'a 1) (cons 'a 2))))
    (alist-map-delete-all! am 'a)
    (test-assert "delete-all!" (collection= equal? am (alist-map eq?))))
  (test-assert "delete-all-from"
               (collection= equal?
                            (alist-map-delete-all-from
                             (alist-map eq? (cons 'a 1) (cons 'b 2))
                             (bag 'a 'b))
                            (alist-map eq?)))
  (let ((am (alist-map eq? (cons 'a 1) (cons 'b 2))))
    (alist-map-delete-all-from! am (bag 'a 'b))
    (test-assert "delete-all-from!" (collection= equal? am (alist-map eq?))))
  (test-assert "get-all" (equal? '(1 2) (alist-map-get-all
                                         (alist-map eq? (cons 'a 1) (cons 'a 2))
                                         'a)))

  (test-assert "key count" (= 2 (alist-map-key-count
                                 (alist-map eq? (cons 'a 1) (cons 'a 2)) 'a)))
  (test-assert "replace-all" (collection=
                              equal?
                              (alist-map eq? (cons 'a 3) (cons 'a 3))
                              (alist-map-replace-all
                               (alist-map eq? (cons 'a 1) (cons 'a 2)) 'a 3)))
  (let ((am (alist-map eq? (cons 'a 1) (cons 'a 2))))
    (alist-map-replace-all! am 'a 3)
    (test-assert "replace-all!"
                 (collection= equal? am (alist-map eq? (cons 'a 3)
                                                   (cons 'a 3)))))
  (test-assert "update-all"
               (collection= equal? (alist-map eq? (cons 'a 1) (cons 'a 2))
                            (alist-map-update-all
                             (alist-map eq? (cons 'a 0) (cons 'a 1))
                             'a (lambda (x) (+ x 1)))))
  (test-assert "update-all (thunk)"
               (collection= equal? (alist-map eq? (cons 'a 0) (cons 'a 0)
                                              (cons 'b 2))
                            (alist-map-update-all
                             (alist-map eq? (cons 'a 0) (cons 'a 1))
                             'b (lambda (x) (+ x 1)) (lambda _ 2))))
  (let ((am (alist-map eq? (cons 'a 1) (cons 'a 2))))
    (alist-map-update-all! am 'a (lambda (x) (+ x 1)))
    (test-assert "update-all!"
                 (collection= equal? am (alist-map eq? (cons 'a 2)
                                                   (cons 'a 3))))))

(test-end)
