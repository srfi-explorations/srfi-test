;; TODO: License. From Chibi-Scheme. Written by Alex Shinn?

(test-begin "srfi-69")

(define (car-string<? a b) (string<? (car a) (car b)))
(define (car-symbol<? a b) (symbol<? (car a) (car b)))

(let ((ht (make-hash-table eq?)))
  ;; 3 initial elements
  (test-eqv 0 (hash-table-size ht))
  (hash-table-set! ht 'cat 'black)
  (hash-table-set! ht 'dog 'white)
  (hash-table-set! ht 'elephant 'pink)
  (test-eqv 3 (hash-table-size ht))
  (test-assert (hash-table-exists? ht 'dog))
  (test-assert (hash-table-exists? ht 'cat))
  (test-assert (hash-table-exists? ht 'elephant))
  (test-eqv #f (hash-table-exists? ht 'goose))
  (test-eqv 'white (hash-table-ref ht 'dog))
  (test-eqv 'black (hash-table-ref ht 'cat))
  (test-eqv 'pink (hash-table-ref ht 'elephant))
  (test-error (hash-table-ref ht 'goose))
  (test-eqv 'grey (hash-table-ref ht 'goose (lambda () 'grey)))
  (test-eqv 'grey (hash-table-ref/default ht 'goose 'grey))
  (test-equal '(cat dog elephant) (list-stable-sort symbol<? (hash-table-keys ht)))
  (test-equal '(black pink white) (list-stable-sort symbol<? (hash-table-values ht)))
  (test-equal '((cat . black) (dog . white) (elephant . pink))
    (list-stable-sort car-symbol<? (hash-table->alist ht)))

  ;; remove an element
  (hash-table-delete! ht 'dog)
  (test-eqv 2 (hash-table-size ht))
  (test-eqv #f (hash-table-exists? ht 'dog))
  (test-assert (hash-table-exists? ht 'cat))
  (test-assert (hash-table-exists? ht 'elephant))
  (test-error (hash-table-ref ht 'dog))
  (test-eqv 'black (hash-table-ref ht 'cat))
  (test-eqv 'pink (hash-table-ref ht 'elephant))
  (test-equal '(cat elephant) (list-stable-sort symbol<? (hash-table-keys ht)))
  (test-equal '(black pink) (list-stable-sort symbol<? (hash-table-values ht)))
  (test-equal '((cat . black) (elephant . pink))
    (list-stable-sort car-symbol<? (hash-table->alist ht)))

  ;; remove a non-existing element
  (hash-table-delete! ht 'dog)
  (test-eqv 2 (hash-table-size ht))
  (test-eqv #f (hash-table-exists? ht 'dog))

  ;; overwrite an existing element
  (hash-table-set! ht 'cat 'calico)
  (test-eqv 2 (hash-table-size ht))
  (test-eqv #f (hash-table-exists? ht 'dog))
  (test-assert (hash-table-exists? ht 'cat))
  (test-assert (hash-table-exists? ht 'elephant))
  (test-error (hash-table-ref ht 'dog))
  (test-eqv 'calico (hash-table-ref ht 'cat))
  (test-eqv 'pink (hash-table-ref ht 'elephant))
  (test-equal '(cat elephant) (list-stable-sort symbol<? (hash-table-keys ht)))
  (test-equal '(calico pink) (list-stable-sort symbol<? (hash-table-values ht)))
  (test-equal '((cat . calico) (elephant . pink))
    (list-stable-sort car-symbol<? (hash-table->alist ht)))

  ;; walk and fold
  (test-equal '((cat . calico) (elephant . pink))
    (let ((a '()))
      (hash-table-walk ht (lambda (k v) (set! a (cons (cons k v) a))))
      (list-stable-sort car-symbol<? a)))
  (test-equal '((cat . calico) (elephant . pink))
    (list-stable-sort
     car-symbol<?
     (hash-table-fold ht (lambda (k v a) (cons (cons k v) a)) '())))

  ;; copy
  (let ((ht2 (hash-table-copy ht)))
    (test-eqv 2 (hash-table-size ht2))
    (test-eqv #f (hash-table-exists? ht2 'dog))
    (test-assert (hash-table-exists? ht2 'cat))
    (test-assert (hash-table-exists? ht2 'elephant))
    (test-error (hash-table-ref ht2 'dog))
    (test-eqv 'calico (hash-table-ref ht2 'cat))
    (test-eqv 'pink (hash-table-ref ht2 'elephant))
    (test-equal '(cat elephant)
      (list-stable-sort symbol<? (hash-table-keys ht2)))
    (test-equal '(calico pink)
      (list-stable-sort symbol<? (hash-table-values ht2)))
    (test-equal '((cat . calico) (elephant . pink))
      (list-stable-sort car-symbol<? (hash-table->alist ht2))))

  ;; merge
  (let ((ht2 (make-hash-table eq?)))
    (hash-table-set! ht2 'bear 'brown)
    (test-eqv 1 (hash-table-size ht2))
    (test-eqv #f (hash-table-exists? ht2 'dog))
    (test-assert (hash-table-exists? ht2 'bear))
    (hash-table-merge! ht2 ht)
    (test-eqv 3 (hash-table-size ht2))
    (test-assert (hash-table-exists? ht2 'bear))
    (test-assert (hash-table-exists? ht2 'cat))
    (test-assert (hash-table-exists? ht2 'elephant))
    (test-eqv #f (hash-table-exists? ht2 'goose))
    (test-eqv 'brown (hash-table-ref ht2 'bear))
    (test-eqv 'calico (hash-table-ref ht2 'cat))
    (test-eqv 'pink (hash-table-ref ht2 'elephant))
    (test-error (hash-table-ref ht2 'goose))
    (test-eqv 'grey (hash-table-ref/default ht2 'goose 'grey))
    (test-equal '(bear cat elephant)
      (list-stable-sort symbol<? (hash-table-keys ht2)))
    (test-equal '(brown calico pink)
      (list-stable-sort symbol<? (hash-table-values ht2)))
    (test-equal '((bear . brown) (cat . calico) (elephant . pink))
      (list-stable-sort car-symbol<? (hash-table->alist ht2))))

  ;; alist->hash-table
  (test-equal (list-stable-sort car-symbol<? (hash-table->alist ht))
    (list-stable-sort car-symbol<?
                      (hash-table->alist
                       (alist->hash-table
                        '((cat . calico) (elephant . pink)))))))

;; update
(let ((ht (make-hash-table eq?))
      (add1 (lambda (x) (+ x 1))))
  (hash-table-set! ht 'sheep 0)
  (hash-table-update! ht 'sheep add1)
  (hash-table-update! ht 'sheep add1)
  (test-eqv 2 (hash-table-ref ht 'sheep))
  (hash-table-update!/default ht 'crows add1 0)
  (hash-table-update!/default ht 'crows add1 0)
  (hash-table-update!/default ht 'crows add1 0)
  (test-eqv 3 (hash-table-ref ht 'crows)))

;; string keys
(let ((ht (make-hash-table equal?)))
  (hash-table-set! ht "cat" 'black)
  (hash-table-set! ht "dog" 'white)
  (hash-table-set! ht "elephant" 'pink)
  (hash-table-ref/default ht "dog" #f)
  (test-eqv 'white (hash-table-ref ht "dog"))
  (test-eqv 'black (hash-table-ref ht "cat"))
  (test-eqv 'pink (hash-table-ref ht "elephant"))
  (test-error (hash-table-ref ht "goose"))
  (test-eqv 'grey (hash-table-ref/default ht "goose" 'grey))
  (test-equal '("cat" "dog" "elephant")
    (list-stable-sort string<? (hash-table-keys ht)))
  (test-equal '(black pink white)
    (list-stable-sort symbol<? (hash-table-values ht)))
  (test-equal '(("cat" . black) ("dog" . white) ("elephant" . pink))
    (list-stable-sort car-string<? (hash-table->alist ht))))

;; string-ci keys
(let ((ht (make-hash-table string-ci=? string-ci-hash)))
  (hash-table-set! ht "cat" 'black)
  (hash-table-set! ht "dog" 'white)
  (hash-table-set! ht "elephant" 'pink)
  (hash-table-ref/default ht "DOG" #f)
  (test-eqv 'white (hash-table-ref ht "DOG"))
  (test-eqv 'black (hash-table-ref ht "Cat"))
  (test-eqv 'pink (hash-table-ref ht "eLePhAnT"))
  (test-error (hash-table-ref ht "goose"))
  (test-equal '("cat" "dog" "elephant")
    (list-stable-sort string<? (hash-table-keys ht)))
  (test-equal '(black pink white)
    (list-stable-sort symbol<? (hash-table-values ht)))
  (test-equal '(("cat" . black) ("dog" . white) ("elephant" . pink))
    (list-stable-sort car-string<? (hash-table->alist ht))))

;; stress test
(test-eqv 625
  (let ((ht (make-hash-table)))
    (do ((i 0 (+ i 1))) ((= i 1000))
      (hash-table-set! ht i (* i i)))
    (hash-table-ref/default ht 25 #f)))

(test-end "srfi-69")
