(test-begin "srfi-43")

(test-assert "make-vector" (vector? (make-vector 0)))

(test-assert "vector" (vector? (vector)))

(test-equal "vector-unfold"
            #(0 -1 -2 -3 -4 -5 -6 -7 -8 -9)
            (vector-unfold (lambda (i x) (values x (- x 1))) 10 0))

(test-equal "vector-unfold-right"
            #(9 8 7 6 5 4 3 2 1 0)
            (vector-unfold-right (lambda (i x) (values x (+ x 1))) 10 0))

(test-equal "vector-copy - no args"
            #(a b c d e f g h i)
            (vector-copy '#(a b c d e f g h i)))

(test-equal "vector-copy - one arg"
            #(g h i)
            (vector-copy '#(a b c d e f g h i) 6))

(test-equal "vector-copy - two args"
            #(d e f)
            (vector-copy '#(a b c d e f g h i) 3 6))

(test-equal "vector-copy - three args"
            #(g h i x x x)
            (vector-copy '#(a b c d e f g h i) 6 12 'x))

(test-equal "vector-reverse-copy - no args"
            #(i h g f e d c b a)
            (vector-reverse-copy '#(a b c d e f g h i)))

(test-equal "vector-reverse-copy - one arg"
            #(i h g)
            (vector-reverse-copy '#(a b c d e f g h i) 6))

(test-equal "vector-reverse-copy - two args"
            #(f e d)
            (vector-reverse-copy '#(a b c d e f g h i) 3 6))

(test-equal "vector-append"
            #(x y)
            (vector-append '#(x) '#(y)))

(test-equal "vector-concatenate"
            #(a b c d)
            (vector-concatenate '(#(a b) #(c d))))

(test-assert "vector? - length 0" (vector? #()))
(test-assert "vector? - length 1" (vector? #(1)))

(test-assert "vector-empty? - contains symbol"
             (not (vector-empty? '#(a))))

(test-assert "vector-empty? - contains list"
             (not (vector-empty? '#(()))))

(test-assert "vector-empty? - contains vector"
             (not (vector-empty? '#(#()))))

(test-assert "vector-empty? - empty"
             (vector-empty? '#()))

(test-assert "vector= - eq? #t" (vector= eq? '#(a b c d) '#(a b c d)))
(test-assert "vector= - eq? #f" (not (vector= eq? '#(a b c d) '#(a b d c))))
(test-assert "vector= - = #f" (not (vector= = '#(1 2 3 4 5) '#(1 2 3 4))))
(test-assert "vector= - = #t" (vector= = '#(1 2 3 4) '#(1 2 3 4)))

(test-assert "vector= - eq? no args" (vector= eq?))
(test-assert "vector= - eq? one arg" (vector= eq? '#(a)))
(test-assert "vector=" (not (vector= eq? (vector (vector 'a)) (vector (vector 'a)))))

(test-equal "vector-ref" 'c (vector-ref '#(a b c d) 2))

(test-equal "vector-length" 3 (vector-length '#(a b c)))

(test-equal "vector-fold - string-length"
            6
            (vector-fold (lambda (index len str)
                       (max (string-length str) len))
                    0
                    #("short" "longer")))

(test-equal "vector-fold - into reverse list"
            '(3 2 1)
            (vector-fold (lambda (index tail elt)
                           (cons elt tail))
                         '()
                         #(1 2 3)))

(test-equal "vector-fold - count even"
            2
            (vector-fold (lambda (index counter n)
                           (if (even? n) (+ counter 1) counter))
                         0
                         #(1 2 3 4 5)))

(test-equal "vector-fold-right - vector to list"
            '(a b c d)
            (vector-fold-right (lambda (index tail elt)
                                 (cons elt tail))
                               '() '#(a b c d)))

(test-equal "vector-map"
            #(1 3 5 7 9 11)
            (vector-map (lambda (index item)
                          (+ item index))
                        #(1 2 3 4 5 6)))

(let ((vec (vector 1 2 3 4 5 6)))
  (vector-map! (lambda (index item)
                 (+ item index))
               vec)
  (test-equal "vector-map!" #(1 3 5 7 9 11) vec))

(let ((result '()))
  (vector-for-each (lambda (index item)
                     (set! result (append result (list index) (list item))))
                   '#("foo" "bar" "baz" "quux" "zot"))
  (test-equal '(0 "foo" 1 "bar" 2 "baz" 3 "quux" 4 "zot")
              result))

(test-equal "vector-count"
            3
            (vector-count (lambda (i elt)
                            (even? elt))
                          '#(3 1 4 1 5 9 2 5 6)))

(test-equal "vector-index - even?"
            2
            (vector-index even? '#(3 1 4 1 5 9)))

(test-equal "vector-index - <"
            1
            (vector-index < '#(3 1 4 1 5 9 2 5 6) '#(2 7 1 8 2)))

(test-assert "vector-index - ="
            (not (vector-index = '#(3 1 4 1 5 9 2 5 6) '#(2 7 1 8 2))))

(test-equal "vector-index-right - even?"
            2
            (vector-index-right even? '#(3 1 4 1 5 9)))

(test-equal "vector-skip"
            2
            (vector-skip number? '#(1 2 a b 3 4 c d)))

(test-equal "vector-skip-right"
            7
            (vector-skip-right number? '#(1 2 a b 3 4 c d)))

(test-equal "vector-binary-search - contains"
            1
            (vector-binary-search '#(#\a #\b #\c)
                                  #\b
                                  (lambda (char1 char2)
                                    (cond ((char<? char1 char2) -1)
                                          ((char=? char1 char2) 0)
                                          (else 1)))))

(test-assert "vector-binary-search - does not contain"
             (not (vector-binary-search '#(#\a #\b #\c)
                                        #\d
                                        (lambda (char1 char2)
                                          (cond ((char<? char1 char2) -1)
                                                ((char=? char1 char2) 0)
                                                (else 1))))))

(define (same-number a b)
  (if (and (number? a) (number? b) (= a b)) a #f))

(test-equal "vector-any - contains"
            3
            (vector-any same-number #(1 a 4 b 3 c) #(5 a 2 b 3 c)))

(test-equal "vector-any - does not contain"
            #f
            (vector-any same-number #(1 a 4 b 7 c) #(5 a 2 b 3 c)))

(let* ((collection '())
      (collect-same-number (lambda (a b)
                              (cond
                                ((and (number? a)
                                      (number? b) (= a b))
                                 (set! collection (append collection (list a)))
                                 collection)
                                (else collection)))))
  (test-equal "vector-every"
              '(1 3)
              (vector-every collect-same-number #(1 a 4 b 3 c) #(1 a 2 b 3 c))))

(let ((vec (vector)))
  (set! vec (vector 'a 'b 'c))
  (vector-set! vec 2 'd)
  (test-equal "vector-set!" #(a b d) vec)

  (set! vec (vector 'a 'b 'c))
  (vector-swap! vec 0 1)
  (test-equal "vector-swap!" #(b a c) vec)

  (set! vec (vector 'a 'b 'c))
  (vector-fill! vec 'a)
  (test-equal "vector-fill! - no args" #(a a a) vec)

  (set! vec (vector 'a 'b 'c))
  (vector-fill! vec 'b 1)
  (test-equal "vector-fill! - one arg" #(a b b) vec)

  (set! vec (vector 'a 'b 'c))
  (vector-fill! vec 'c 0 2)
  (test-equal "vector-fill! - two args" #(c c c) vec)

  (set! vec (vector 'a 'b 'c))
  (vector-reverse! vec)
  (test-equal "vector-reverse! - no args" #(c b a) vec)

  (set! vec (vector 'a 'b 'c))
  (vector-reverse! vec 1)
  (test-equal "vector-reverse! - one arg" #(a c b) vec)

  (set! vec (vector 'a 'b 'c))
  (vector-reverse! vec 0 2)
  (test-equal "vector-reverse! - two args" #(b a c) vec))


(let ((vec1 (vector 'a 'b 'c))
      (vec2 (vector 'd 'e 'f)))
  (vector-copy! vec2 0 vec1)
  (test-equal "vector-copy! - no args" #(a b c) vec2))

(let ((vec1 (vector 'a 'b 'c))
      (vec2 (vector 'd 'e 'f)))
  (vector-copy! vec2 1 vec1 1)
  (test-equal "vector-copy! - one arg" #(d b c) vec2))

(let ((vec1 (vector 'a 'b 'c))
      (vec2 (vector 'd 'e 'f)))
  (vector-copy! vec2 0 vec1 0 1)
  (test-equal "vector-copy! - two args" #(a e f) vec2))


(let ((vec1 (vector 'a 'b 'c))
      (vec2 (vector 'd 'e 'f)))
  (vector-reverse-copy! vec2 0 vec1)
  (test-equal "vector-reverse-copy! - no args" #(c b a) vec2))

(let ((vec1 (vector 'a 'b 'c))
      (vec2 (vector 'd 'e 'f)))
  (vector-reverse-copy! vec2 1 vec1 1)
  (test-equal "vector-reverse-copy! - one arg" #(d c b) vec2))

(let ((vec1 (vector 'a 'b 'c))
      (vec2 (vector 'd 'e 'f)))
  (vector-reverse-copy! vec2 0 vec1 0 1)
  (test-equal "vector-reverse-copy! - two args" #(a e f) vec2))

(test-equal "vector->list - no args" '(1 2 3) (vector->list #(1 2 3)))
(test-equal "vector->list - one arg" '(2 3) (vector->list #(1 2 3) 1))
(test-equal "vector->list - two args" '(1 2) (vector->list #(1 2 3) 0 2))

(test-equal "reverse-vector->list - no args" '(3 2 1) (reverse-vector->list #(1 2 3)))
(test-equal "reverse-vector->list - one arg" '(3 2) (reverse-vector->list #(1 2 3) 1))
(test-equal "reverse-vector->list - two args" '(2 1) (reverse-vector->list #(1 2 3) 0 2))

(test-equal "list->vector" #(1 2 3) (list->vector '(1 2 3)))
(test-equal "reverse-list->vector" #(3 2 1) (reverse-list->vector '(1 2 3)))

(test-end "srfi-43")
