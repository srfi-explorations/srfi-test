;; TODO: License. From Chibi-Scheme. Written by Alex Shinn?

(define (sc str c)
  (string-index->cursor str c))
(define (string-index->index str pred . o)
  (string-cursor->index str (apply string-index str pred o)))
(define (string-index-right->index str pred . o)
  (string-cursor->index str (apply string-index-right str pred o)))
(define char-set:not-letter (char-set-complement char-set:letter))

(test-begin "srfi-130")

;; tests adapted from Gauche's SRFI 13 tests, via Chicken

(test-equal "string-null?" #f (string-null? "abc"))
(test-equal "string-null?" #t (string-null? ""))
(test-equal "string-every" #t (string-every #\a ""))
(test-equal "string-every" #t (string-every #\a "aaaa"))
(test-equal "string-every" #f (string-every #\a "aaba"))
(test-equal "string-every" #t (string-every char-set:lower-case "aaba"))
(test-equal "string-every" #f (string-every char-set:lower-case "aAba"))
(test-equal "string-every" #t (string-every char-set:lower-case ""))
(test-equal "string-every" #t (string-every (lambda (x) (char->integer x)) "aAbA"))
(test-equal "string-every" #t (string-every (lambda (x) (error "hoge")) ""))
(test-equal "string-any" #t (string-any #\a "aaaa"))
(test-equal "string-any" #f (string-any #\a "Abcd"))
(test-equal "string-any" #f (string-any #\a ""))
(test-equal "string-any" #t (string-any char-set:lower-case "ABcD"))
(test-equal "string-any" #f (string-any char-set:lower-case "ABCD"))
(test-equal "string-any" #f (string-any char-set:lower-case ""))
(test-equal "string-any" (char->integer #\a)
            (string-any (lambda (x) (char->integer x)) "aAbA"))

(test-equal "string-tabulate" "0123456789"
            (string-tabulate (lambda (code)
                               (integer->char (+ code (char->integer #\0))))
                             10))
(test-equal "string-tabulate" ""
            (string-tabulate (lambda (code)
                               (integer->char (+ code (char->integer #\0))))
                             0))

(test-equal "reverse-list->string" "cBa"
            (reverse-list->string '(#\a #\B #\c)))
(test-equal "reverse-list->string" ""
            (reverse-list->string '()))

(test-equal "substring/cursors" "cde" (substring/cursors "abcde" 2 5))
(test-equal "substring/cursors" "cd"  (substring/cursors "abcde" 2 4))

(test-equal "string-copy!" "abCDEfg"
            (let ((x (string-copy "abcdefg")))
              (string-copy! x 2 "CDE")
              x))
(test-equal "string-copy!" "abCDEfg"
            (let ((x (string-copy "abcdefg")))
              (string-copy! x 2 "ZABCDE" 3)
              x))
(test-equal "string-copy!" "abCDEfg"
            (let ((x (string-copy "abcdefg")))
              (string-copy! x 2 "ZABCDEFG" 3 6)
              x))

(test-equal "string-take" "Pete S"  (string-take "Pete Szilagyi" 6))
(test-equal "string-take" ""        (string-take "Pete Szilagyi" 0))
(test-equal "string-take" "Pete Szilagyi" (string-take "Pete Szilagyi" 13))
(test-equal "string-drop" "zilagyi" (string-drop "Pete Szilagyi" 6))
(test-equal "string-drop" "Pete Szilagyi" (string-drop "Pete Szilagyi" 0))
(test-equal "string-drop" ""        (string-drop "Pete Szilagyi" 13))

(test-equal "string-take-right" "rules" (string-take-right "Beta rules" 5))
(test-equal "string-take-right" ""      (string-take-right "Beta rules" 0))
(test-equal "string-take-right" "Beta rules" (string-take-right "Beta rules" 10))
(test-equal "string-drop-right" "Beta " (string-drop-right "Beta rules" 5))
(test-equal "string-drop-right" "Beta rules" (string-drop-right "Beta rules" 0))
(test-equal "string-drop-right" ""      (string-drop-right "Beta rules" 10))

(test-equal "string-pad" "  325" (string-pad "325" 5))
(test-equal "string-pad" "71325" (string-pad "71325" 5))
(test-equal "string-pad" "71325" (string-pad "8871325" 5))
(test-equal "string-pad" "~~325" (string-pad "325" 5 #\~))
(test-equal "string-pad" "~~~25" (string-pad "325" 5 #\~ 1))
(test-equal "string-pad" "~~~~2" (string-pad "325" 5 #\~ 1 2))
(test-equal "string-pad-right" "325  " (string-pad-right "325" 5))
(test-equal "string-pad-right" "71325" (string-pad-right "71325" 5))
(test-equal "string-pad-right" "88713" (string-pad-right "8871325" 5))
(test-equal "string-pad-right" "325~~" (string-pad-right "325" 5 #\~))
(test-equal "string-pad-right" "25~~~" (string-pad-right "325" 5 #\~ 1))
(test-equal "string-pad-right" "2~~~~" (string-pad-right "325" 5 #\~ 1 2))

(test-equal "string-trim"  "a b c d  \n"
            (string-trim "  \t  a b c d  \n"))
(test-equal "string-trim"  "\t  a b c d  \n"
            (string-trim "  \t  a b c d  \n" #\space))
(test-equal "string-trim"  "a b c d  \n"
            (string-trim "4358948a b c d  \n" char-numeric?))

(test-equal "string-trim-right"  "  \t  a b c d"
            (string-trim-right "  \t  a b c d  \n"))
(test-equal "string-trim-right"  "  \t  a b c d  "
            (string-trim-right "  \t  a b c d  \n" (lambda (ch) (eqv? ch #\newline))))
(test-equal "string-trim-right"  "349853a b c d"
            (string-trim-right "349853a b c d03490" char-numeric?))

(test-equal "string-trim-both"  "a b c d"
            (string-trim-both "  \t  a b c d  \n"))
(test-equal "string-trim-both"  "  \t  a b c d  "
            (string-trim-both "  \t  a b c d  \n" (lambda (ch) (eqv? ch #\newline))))
(test-equal "string-trim-both"  "a b c d"
            (string-trim-both "349853a b c d03490" char-numeric?))

;; TODO: bunch of string= families

(test-equal "string-prefix-length" 5
            (string-prefix-length "cancaNCAM" "cancancan"))
(test-equal "string-suffix-length" 2
            (string-suffix-length "CanCan" "cankancan"))

(test-equal "string-prefix?" #t (string-prefix? "abcd" "abcdefg"))
(test-equal "string-prefix?" #f (string-prefix? "abcf" "abcdefg"))
(test-equal "string-suffix?" #t (string-suffix? "defg" "abcdefg"))
(test-equal "string-suffix?" #f (string-suffix? "aefg" "abcdefg"))

(test-equal "string-index #1" 4
            (string-index->index "abcd:efgh:ijkl" (lambda (ch) (eqv? ch #\:))))
(test-equal "string-index #2" 4
            (string-index->index "abcd:efgh;ijkl" (lambda (ch) (char-set-contains? char-set:not-letter ch))))
(test-equal "string-index #3" 14
            (string-index->index "abcd:efgh;ijkl" (lambda (ch) (char-set-contains? char-set:digit ch))))
(test-equal "string-index #4" 9
            (string-index->index "abcd:efgh:ijkl" (lambda (ch) (eqv? ch #\:)) 5))
(test-equal "string-index-right #1" 5
            (string-index-right->index "abcd:efgh;ijkl" (lambda (ch) (eqv? ch #\:))))
(test-equal "string-index-right #2" 10
            (string-index-right->index "abcd:efgh;ijkl" (lambda (ch) (char-set-contains? char-set:not-letter ch))))
(test-equal "string-index-right #3" 14
            (string-index-right->index "abcd:efgh;ijkl" char-alphabetic?))
(test-equal "string-index-right #4" 10
            (string-index-right->index "abcd:efgh;ijkl" (lambda (ch) (char-set-contains? char-set:not-letter ch)) 7))

(test-equal "string-count #1" 2
            (string-count "abc def\tghi jkl" (lambda (ch) (eqv? ch #\space))))
(test-equal "string-count #2" 3
            (string-count "abc def\tghi jkl" char-whitespace?))
(test-equal "string-count #3" 2
            (string-count "abc def\tghi jkl" char-whitespace? 4))
(test-equal "string-count #4" 1
            (string-count "abc def\tghi jkl" char-whitespace? 4 9))
(let ((s "Ma mere l'oye"))
  (test-assert "string-contains"
    (string-contains s "mer"))
  (test-equal "string-contains" #f
              (string-contains s "Mer"))
  (test-assert "string-contains"
    (string-contains s "mer" 1))
  (test-assert "string-contains"
    (string-contains s "mer" 1 8))
  (test-equal "string-contains"
    #f (string-contains s "mer" 4 8))
  (test-equal "string-contains"
    #f (string-contains s "mer" 1 5)))
(let ((s "eek -- it's a geek."))
  (test-equal 15 (string-cursor->index s (string-contains-right s "ee")))
  (test-equal 15 (string-cursor->index s (string-contains-right s "ee" 12 18)))
  (test-equal 19 (string-cursor->index s (string-contains-right s "")))
  (test-equal 0 (string-cursor->index "" (string-contains-right "" "")))
  (test-equal #f (string-contains-right s "kee" 12 18)))

(test-equal "string-reverse" "nomel on nolem on"
            (string-reverse "no melon no lemon"))
(test-equal "string-reverse" "nomel on"
            (string-reverse "no melon no lemon" 9))
(test-equal "string-reverse" "on"
            (string-reverse "no melon no lemon" 9 11))

(test-equal "string-append" #f
            (let ((s "test")) (eq? s (string-append s))))
(test-equal "string-concatenate" #f
            (let ((s "test")) (eq? s (string-concatenate (list s)))))
(test-equal "string-concatenate" "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
            (string-concatenate
             '("A" "B" "C" "D" "E" "F" "G" "H"
               "I" "J" "K" "L" "M" "N" "O" "P"
               "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"
               "a" "b" "c" "d" "e" "f" "g" "h"
               "i" "j" "k" "l" "m" "n" "o" "p"
               "q" "r" "s" "t" "u" "v" "w" "x" "y" "z")))
(test-equal "string-concatenate-reverse" "zyxwvutsrqponmlkjihgfedcbaZYXWVUTSRQPONMLKJIHGFEDCBA"
            (string-concatenate-reverse
             '("A" "B" "C" "D" "E" "F" "G" "H"
               "I" "J" "K" "L" "M" "N" "O" "P"
               "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"
               "a" "b" "c" "d" "e" "f" "g" "h"
               "i" "j" "k" "l" "m" "n" "o" "p"
               "q" "r" "s" "t" "u" "v" "w" "x" "y" "z")))
(test-equal "string-concatenate-reverse" #f
            (let ((s "test"))
              (eq? s (string-concatenate-reverse (list s)))))

(test-equal "string-map" "svool"
            (string-map (lambda (c)
                          (integer->char (- 219 (char->integer c))))
                        "hello"))
;; (test-equal "string-map" "vool"
;;   (string-map (lambda (c)
;;                 (integer->char (- 219 (char->integer c))))
;;               "hello" 1))
;; (test-equal "string-map" "vo"
;;   (string-map (lambda (c)
;;                 (integer->char (- 219 (char->integer c))))
;;               "hello" 1 3))

(test-equal "string-fold" '(#\o #\l #\l #\e #\h . #t)
            (string-fold cons #t "hello"))
(test-equal "string-fold" '(#\l #\e . #t)
            (string-fold cons #t "hello" 1 3))
(test-equal "string-fold-right" '(#\h #\e #\l #\l #\o . #t)
            (string-fold-right cons #t "hello"))
(test-equal "string-fold-right" '(#\e #\l . #t)
            (string-fold-right cons #t "hello" 1 3))

(test-equal "string-unfold" "hello"
            (string-unfold null? car cdr '(#\h #\e #\l #\l #\o)))
(test-equal "string-unfold" "hi hello"
            (string-unfold null? car cdr '(#\h #\e #\l #\l #\o) "hi "))
(test-equal "string-unfold" "hi hello ho"
            (string-unfold null? car cdr
                           '(#\h #\e #\l #\l #\o) "hi "
                           (lambda (x) " ho")))

(test-equal "string-unfold-right" "olleh"
            (string-unfold-right null? car cdr '(#\h #\e #\l #\l #\o)))
(test-equal "string-unfold-right" "olleh hi"
            (string-unfold-right null? car cdr '(#\h #\e #\l #\l #\o) " hi"))
(test-equal "string-unfold-right" "ho olleh hi"
            (string-unfold-right null? car cdr
                                 '(#\h #\e #\l #\l #\o) " hi"
                                 (lambda (x) "ho ")))

(test-equal "string-for-each" "CLtL"
            (let ((out (open-output-string))
                  (prev #f))
              (string-for-each (lambda (c)
                                 (if (or (not prev)
                                         (char-whitespace? prev))
                                     (write-char c out))
                                 (set! prev c))
                               "Common Lisp, the Language")

              (get-output-string out)))
;; (test-equal "string-for-each" "oLtL"
;;   (let ((out (open-output-string))
;;         (prev #f))
;;     (string-for-each (lambda (c)
;;                        (if (or (not prev)
;;                                (char-whitespace? prev))
;;                            (write-char c out))
;;                        (set! prev c))
;;                      "Common Lisp, the Language" 1)
;;     (get-output-string out)))
;; (test-equal "string-for-each" "oL"
;;   (let ((out (open-output-string))
;;         (prev #f))
;;     (string-for-each (lambda (c)
;;                        (if (or (not prev)
;;                                (char-whitespace? prev))
;;                            (write-char c out))
;;                        (set! prev c))
;;                      "Common Lisp, the Language" 1 10)
;;     (get-output-string out)))

(test-equal "string-for-each-cursor" '(4 3 2 1 0)
            (let ((r '()))
              (string-for-each-cursor (lambda (i) (set! r (cons i r))) "hello")
              (map (lambda (sc) (string-cursor->index "hello" sc)) r)))
(test-equal "string-for-each-cursor" '(4 3 2 1)
            (let ((r '()))
              (string-for-each-cursor (lambda (i) (set! r (cons i r))) "hello" 1)
              (map (lambda (sc) (string-cursor->index "hello" sc)) r)))
(test-equal "string-for-each-cursor" '(2 1)
            (let ((r '()))
              (string-for-each-cursor (lambda (i) (set! r (cons i r))) "hello" 1 3)
              (map (lambda (sc) (string-cursor->index "hello" sc)) r)))

(test-equal "string-replicate" "cdefab"
            (string-replicate "abcdef" 2 8))
(test-equal "string-replicate" "efabcd"
            (string-replicate "abcdef" -2 4))
(test-equal "string-replicate" "abcabca"
            (string-replicate "abc" 0 7))
(test-equal "string-replicate" "defdefd"
            (string-replicate "abcdefg" 0 7 3 6))
(test-equal "string-replicate" ""
            (string-replicate "abcdefg" 9 9 3 6))

(test-equal "string-replace" "abcdXYZghi"
            (string-replace "abcdefghi" "XYZ" 4 6))
(test-equal "string-replace" "abcdZghi"
            (string-replace "abcdefghi" "XYZ" 4 6 2))
(test-equal "string-replace" "abcdZefghi"
            (string-replace "abcdefghi" "XYZ" 4 4 2))
(test-equal "string-replace" "abcdefghi"
            (string-replace "abcdefghi" "XYZ" 4 4 1 1))
(test-equal "string-replace" "abcdhi"
            (string-replace "abcdefghi" "" 4 7))

(test-equal "string-filter" "rrrr"
            (string-filter (lambda (ch) (eqv? ch #\r))
                           "Help make programs run, run, RUN!"))
(test-equal "string-filter" "HelpmakeprogramsrunrunRUN"
            (string-filter char-alphabetic? "Help make programs run, run, RUN!"))

(test-equal "string-filter" "programsrunrun"
            (string-filter (lambda (c) (char-lower-case? c))
                           "Help make programs run, run, RUN!"
                           10))
(test-equal "string-filter" ""
            (string-filter (lambda (c) (char-lower-case? c)) ""))
(test-equal "string-remove" "Help make pogams un, un, RUN!"
            (string-remove (lambda (ch) (eqv? ch #\r))
                           "Help make programs run, run, RUN!"))
(test-equal "string-remove" "   , , !"
            (string-remove char-alphabetic? "Help make programs run, run, RUN!"))
(test-equal "string-remove" " , , RUN!"
            (string-remove (lambda (c) (char-lower-case? c))
                           "Help make programs run, run, RUN!"
                           10))
(test-equal "string-remove" ""
            (string-remove (lambda (c) (char-lower-case? c)) ""))

(test-equal "foo:bar:baz"
  (string-join '("foo" "bar" "baz") ":"))
(test-equal "foo:bar:baz:"
  (string-join '("foo" "bar" "baz") ":" 'suffix))
(test-equal "" (string-join '() ":"))
(test-equal "" (string-join '("") ":"))
(test-equal "" (string-join '() ":" 'suffix))
(test-equal ":" (string-join '("") ":" 'suffix))

(test-equal '("foo" "bar" "baz")
  (string-split "foo:bar:baz" ":"))
(test-equal '("foo" "bar" "baz")
  (string-split "foo:bar:baz:" ":" 'suffix))
(test-equal '("foo" "bar:baz:")
  (string-split "foo:bar:baz:" ":" 'suffix 1))
(test-equal '("foo" "bar" "baz:")
  (string-split "foo:bar:baz:" ":" 'suffix 2))
(test-equal '() (string-split "" ":"))
(test-equal '() (string-split "" ":" 'suffix))
(test-equal '("") (string-split ":" ":" 'suffix))

      ;;; Regression tests: check that reported bugs have been fixed

;; From: Matthias Radestock <matthias@sorted.org>
;; Date: Wed, 10 Dec 2003 21:05:22 +0100
                                        ;
;; Chris Double has found the following bug in the reference implementation:
                                        ;
;;  (string-contains "xabc" "ab") => 1    ;good
;;  (string-contains "aabc" "ab") => #f   ;bad
                                        ;
;; Matthias.

(test-equal "string-contains" 1
            (string-cursor->index "aabc" (string-contains "aabc" "ab")))

(test-equal "string-contains" 5
            (string-cursor->index "ababdabdabxxas" (string-contains "ababdabdabxxas" "abdabx")))

;; (message continues)
;;
;; PS: There is also an off-by-one error in the bounds check of the
;; unoptimized version of string-contains that is included as commented out
;; code in the reference implementation. This breaks things like
;; (string-contains "xab" "ab") and (string-contains "ab" "ab").

;; This off-by-one bug has been fixed in the comments of the version
;; of SRFI-13 shipped with Larceny.  In a version of the code without
;; the fix the following test will catch the bug:

(test-equal "string-contains" 0
            (string-cursor->index "ab" (string-contains "ab" "ab")))

(test-end "srfi-130")
