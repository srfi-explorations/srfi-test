;; Copyright 2001, 2002, 2011, 2012 Shiro Kawai
;; Copyright 2016 William D Clinger
;; Copyright 2016 Alex Shinn
;; SPDX-License-Identifier: MIT

(define (sc str c)
  (string-index->cursor str c))
(define (string-index->index str pred . o)
  (string-cursor->index str (apply string-index str pred o)))
(define (string-index-right->index str pred . o)
  (string-cursor->index str (apply string-index-right str pred o)))
(define char-set:not-letter (char-set-complement char-set:letter))

(test-begin "srfi-130")

(test-begin "sample-implementation")

;;; FIXME: These tests are incomplete because there's  a combinatorial
;;; explosion of possibilities for optional arguments that could be
;;; either indexes or cursors.  Consider string-prefix-length, for
;;; example.  For each test that calls that procedure with all four
;;; optional arguments, there are 16 possible combinations of indexes
;;; and cursors.  Beginning with string-take, the optional arguments
;;; tested are indexes rather than cursors.

;;; Unicode is the main motivation for string cursors, so we ought
;;; to use at least some non-ASCII strings for testing.
;;; Some systems would blow up if this file were to contain non-ASCII
;;; characters, however, so we have to be careful here.

(cond-expand (full-unicode
              (define ABC
                (list->string (map integer->char
                                   '(#x3b1 #x3b2 #x3b3))))
              (define ABCDEF
                (list->string (map integer->char
                                   '(#x0c0 #x062 #x0c7 #x064 #x0c9 #x066)))))
             (else
              (define ABC "abc")
              (define ABCDEF "abcdef")))

;;; Cursor operations

(test-assert "string-cursor?" (string-cursor? (string-index->cursor "" 0)))
(test-assert "string-cursor?" (not (string-cursor? #f)))
(test-assert "string-cursor?"
  (string-cursor? (string-index->cursor (make-string 10000) 9999)))
(test-assert "string-cursor-start"
  (= 0 (string-cursor->index "" (string-cursor-start ""))))
(test-assert "string-cursor-start"
  (= 0 (string-cursor->index ABC (string-cursor-start ABC))))
(test-assert "string-cursor-end"
  (= 0 (string-cursor->index "" (string-cursor-end ""))))
(test-assert "string-cursor-end"
  (= 3 (string-cursor->index ABC (string-cursor-end ABC))))
(test-assert "string-cursor-next"
  (= 1 (string-cursor->index ABC (string-cursor-next ABC 0))))
(test-assert "string-cursor-next"
  (= 2 (string-cursor->index ABC (string-cursor-next ABC 1))))
(test-assert "string-cursor-next"
  (= 3 (string-cursor->index ABC (string-cursor-next ABC 2))))
(test-assert "string-cursor-prev"
  (= 0 (string-cursor->index ABC (string-cursor-prev ABC 1))))
(test-assert "string-cursor-prev"
  (= 1 (string-cursor->index ABC (string-cursor-prev ABC 2))))
(test-assert "string-cursor-prev"
  (= 2 (string-cursor->index ABC (string-cursor-prev ABC 3))))
(test-assert "string-cursor-forward"
  (= 0 (string-cursor->index ABC (string-cursor-forward ABC 0 0))))
(test-assert "string-cursor-forward"
  (= 2 (string-cursor->index ABC (string-cursor-forward ABC 0 2))))
(test-assert "string-cursor-forward"
  (= 3 (string-cursor->index ABC (string-cursor-forward ABC 1 2))))
(test-assert "string-cursor-forward"
  (= 3 (string-cursor->index ABC (string-cursor-forward ABC 3 0))))
(test-assert "string-cursor-back"
  (= 0 (string-cursor->index ABC (string-cursor-back ABC 0 0))))
(test-assert "string-cursor-back"
  (= 0 (string-cursor->index ABC (string-cursor-back ABC 2 2))))
(test-assert "string-cursor-back"
  (= 1 (string-cursor->index ABC (string-cursor-back ABC 3 2))))
(test-assert "string-cursor-back"
  (= 3 (string-cursor->index ABC (string-cursor-back ABC 3 0))))

;;; These are supposed to work on both indexes and cursors.

(test-assert "string-cursor=?" (string-cursor=? 0 0))
(test-assert "string-cursor=?" (not (string-cursor=? 0 1)))
(test-assert "string-cursor=?" (not (string-cursor=? 0 2)))
(test-assert "string-cursor=?" (not (string-cursor=? 0 3)))
(test-assert "string-cursor=?" (not (string-cursor=? 1 0)))
(test-assert "string-cursor=?" (string-cursor=? 1 1))
(test-assert "string-cursor=?" (not (string-cursor=? 1 2)))
(test-assert "string-cursor=?" (not (string-cursor=? 1 3)))
(test-assert "string-cursor=?" (not (string-cursor=? 2 0)))
(test-assert "string-cursor=?" (not (string-cursor=? 2 1)))
(test-assert "string-cursor=?" (string-cursor=? 2 2))
(test-assert "string-cursor=?" (not (string-cursor=? 2 3)))
(test-assert "string-cursor=?" (not (string-cursor=? 3 0)))
(test-assert "string-cursor=?" (not (string-cursor=? 3 1)))
(test-assert "string-cursor=?" (not (string-cursor=? 3 2)))
(test-assert "string-cursor=?" (string-cursor=? 3 3))
(test-assert "string-cursor<?" (not (string-cursor<? 0 0)))
(test-assert "string-cursor<?" (string-cursor<? 0 1))
(test-assert "string-cursor<?" (string-cursor<? 0 2))
(test-assert "string-cursor<?" (string-cursor<? 0 3))
(test-assert "string-cursor<?" (not (string-cursor<? 1 0)))
(test-assert "string-cursor<?" (not (string-cursor<? 1 1)))
(test-assert "string-cursor<?" (string-cursor<? 1 2))
(test-assert "string-cursor<?" (string-cursor<? 1 3))
(test-assert "string-cursor<?" (not (string-cursor<? 2 0)))
(test-assert "string-cursor<?" (not (string-cursor<? 2 1)))
(test-assert "string-cursor<?" (not (string-cursor<? 2 2)))
(test-assert "string-cursor<?" (string-cursor<? 2 3))
(test-assert "string-cursor<?" (not (string-cursor<? 3 0)))
(test-assert "string-cursor<?" (not (string-cursor<? 3 1)))
(test-assert "string-cursor<?" (not (string-cursor<? 3 2)))
(test-assert "string-cursor<?" (not (string-cursor<? 3 3)))
(test-assert "string-cursor>?" (not (string-cursor>? 0 0)))
(test-assert "string-cursor>?" (not (string-cursor>? 0 1)))
(test-assert "string-cursor>?" (not (string-cursor>? 0 2)))
(test-assert "string-cursor>?" (not (string-cursor>? 0 3)))
(test-assert "string-cursor>?" (string-cursor>? 1 0))
(test-assert "string-cursor>?" (not (string-cursor>? 1 1)))
(test-assert "string-cursor>?" (not (string-cursor>? 1 2)))
(test-assert "string-cursor>?" (not (string-cursor>? 1 3)))
(test-assert "string-cursor>?" (string-cursor>? 2 0))
(test-assert "string-cursor>?" (string-cursor>? 2 1))
(test-assert "string-cursor>?" (not (string-cursor>? 2 2)))
(test-assert "string-cursor>?" (not (string-cursor>? 2 3)))
(test-assert "string-cursor>?" (string-cursor>? 3 0))
(test-assert "string-cursor>?" (string-cursor>? 3 1))
(test-assert "string-cursor>?" (string-cursor>? 3 2))
(test-assert "string-cursor>?" (not (string-cursor>? 3 3)))
(test-assert "string-cursor<=?" (string-cursor<=? 0 0))
(test-assert "string-cursor<=?" (string-cursor<=? 0 1))
(test-assert "string-cursor<=?" (string-cursor<=? 0 2))
(test-assert "string-cursor<=?" (string-cursor<=? 0 3))
(test-assert "string-cursor<=?" (not (string-cursor<=? 1 0)))
(test-assert "string-cursor<=?" (string-cursor<=? 1 1))
(test-assert "string-cursor<=?" (string-cursor<=? 1 2))
(test-assert "string-cursor<=?" (string-cursor<=? 1 3))
(test-assert "string-cursor<=?" (not (string-cursor<=? 2 0)))
(test-assert "string-cursor<=?" (not (string-cursor<=? 2 1)))
(test-assert "string-cursor<=?" (string-cursor<=? 2 2))
(test-assert "string-cursor<=?" (string-cursor<=? 2 3))
(test-assert "string-cursor<=?" (not (string-cursor<=? 3 0)))
(test-assert "string-cursor<=?" (not (string-cursor<=? 3 1)))
(test-assert "string-cursor<=?" (not (string-cursor<=? 3 2)))
(test-assert "string-cursor<=?" (string-cursor<=? 3 3))
(test-assert "string-cursor>=?" (string-cursor>=? 0 0))
(test-assert "string-cursor>=?" (not (string-cursor>=? 0 1)))
(test-assert "string-cursor>=?" (not (string-cursor>=? 0 2)))
(test-assert "string-cursor>=?" (not (string-cursor>=? 0 3)))
(test-assert "string-cursor>=?" (string-cursor>=? 1 0))
(test-assert "string-cursor>=?" (string-cursor>=? 1 1))
(test-assert "string-cursor>=?" (not (string-cursor>=? 1 2)))
(test-assert "string-cursor>=?" (not (string-cursor>=? 1 3)))
(test-assert "string-cursor>=?" (string-cursor>=? 2 0))
(test-assert "string-cursor>=?" (string-cursor>=? 2 1))
(test-assert "string-cursor>=?" (string-cursor>=? 2 2))
(test-assert "string-cursor>=?" (not (string-cursor>=? 2 3)))
(test-assert "string-cursor>=?" (string-cursor>=? 3 0))
(test-assert "string-cursor>=?" (string-cursor>=? 3 1))
(test-assert "string-cursor>=?" (string-cursor>=? 3 2))
(test-assert "string-cursor>=?" (string-cursor>=? 3 3))
(test-assert "string-cursor=?"
  (string-cursor=? (string-index->cursor ABC 0) (string-index->cursor ABC 0)))
(test-assert "string-cursor=?"
  (not (string-cursor=? (string-index->cursor ABC 0)
                        (string-index->cursor ABC 1))))
(test-assert "string-cursor=?"
  (not (string-cursor=? (string-index->cursor ABC 0)
                        (string-index->cursor ABC 2))))
(test-assert "string-cursor=?"
  (not (string-cursor=? (string-index->cursor ABC 0)
                        (string-index->cursor ABC 3))))
(test-assert "string-cursor=?"
  (not (string-cursor=? (string-index->cursor ABC 1)
                        (string-index->cursor ABC 0))))
(test-assert "string-cursor=?"
  (string-cursor=? (string-index->cursor ABC 1) (string-index->cursor ABC 1)))
(test-assert "string-cursor=?"
  (not (string-cursor=? (string-index->cursor ABC 1)
                        (string-index->cursor ABC 2))))
(test-assert "string-cursor=?"
  (not (string-cursor=? (string-index->cursor ABC 1)
                        (string-index->cursor ABC 3))))
(test-assert "string-cursor=?"
  (not (string-cursor=? (string-index->cursor ABC 2)
                        (string-index->cursor ABC 0))))
(test-assert "string-cursor=?"
  (not (string-cursor=? (string-index->cursor ABC 2)
                        (string-index->cursor ABC 1))))
(test-assert "string-cursor=?"
  (string-cursor=? (string-index->cursor ABC 2)
                   (string-index->cursor ABC 2)))
(test-assert "string-cursor=?"
  (not (string-cursor=? (string-index->cursor ABC 2)
                        (string-index->cursor ABC 3))))
(test-assert "string-cursor=?"
  (not (string-cursor=? (string-index->cursor ABC 3)
                        (string-index->cursor ABC 0))))
(test-assert "string-cursor=?"
  (not (string-cursor=? (string-index->cursor ABC 3)
                        (string-index->cursor ABC 1))))
(test-assert "string-cursor=?"
  (not (string-cursor=? (string-index->cursor ABC 3)
                        (string-index->cursor ABC 2))))
(test-assert "string-cursor=?"
  (string-cursor=? (string-index->cursor ABC 3)
                   (string-index->cursor ABC 3)))
(test-assert "string-cursor<?"
  (not (string-cursor<? (string-index->cursor ABC 0)
                        (string-index->cursor ABC 0))))
(test-assert "string-cursor<?"
  (string-cursor<? (string-index->cursor ABC 0) (string-index->cursor ABC 1)))
(test-assert "string-cursor<?"
  (string-cursor<? (string-index->cursor ABC 0) (string-index->cursor ABC 2)))
(test-assert "string-cursor<?"
  (string-cursor<? (string-index->cursor ABC 0) (string-index->cursor ABC 3)))
(test-assert "string-cursor<?"
  (not (string-cursor<? (string-index->cursor ABC 1)
                        (string-index->cursor ABC 0))))
(test-assert "string-cursor<?"
  (not (string-cursor<? (string-index->cursor ABC 1)
                        (string-index->cursor ABC 1))))
(test-assert "string-cursor<?"
  (string-cursor<? (string-index->cursor ABC 1) (string-index->cursor ABC 2)))
(test-assert "string-cursor<?"
  (string-cursor<? (string-index->cursor ABC 1) (string-index->cursor ABC 3)))
(test-assert "string-cursor<?"
  (not (string-cursor<? (string-index->cursor ABC 2)
                        (string-index->cursor ABC 0))))
(test-assert "string-cursor<?"
  (not (string-cursor<? (string-index->cursor ABC 2)
                        (string-index->cursor ABC 1))))
(test-assert "string-cursor<?"
  (not (string-cursor<? (string-index->cursor ABC 2)
                        (string-index->cursor ABC 2))))
(test-assert "string-cursor<?"
  (string-cursor<? (string-index->cursor ABC 2) (string-index->cursor ABC 3)))
(test-assert "string-cursor<?"
  (not (string-cursor<? (string-index->cursor ABC 3)
                        (string-index->cursor ABC 0))))
(test-assert "string-cursor<?"
  (not (string-cursor<? (string-index->cursor ABC 3)
                        (string-index->cursor ABC 1))))
(test-assert "string-cursor<?"
  (not (string-cursor<? (string-index->cursor ABC 3)
                        (string-index->cursor ABC 2))))
(test-assert "string-cursor<?"
  (not (string-cursor<? (string-index->cursor ABC 3)
                        (string-index->cursor ABC 3))))
(test-assert "string-cursor>?"
  (not (string-cursor>? (string-index->cursor ABC 0)
                        (string-index->cursor ABC 0))))
(test-assert "string-cursor>?"
  (not (string-cursor>? (string-index->cursor ABC 0)
                        (string-index->cursor ABC 1))))
(test-assert "string-cursor>?"
  (not (string-cursor>? (string-index->cursor ABC 0)
                        (string-index->cursor ABC 2))))
(test-assert "string-cursor>?"
  (not (string-cursor>? (string-index->cursor ABC 0)
                        (string-index->cursor ABC 3))))
(test-assert "string-cursor>?"
  (string-cursor>? (string-index->cursor ABC 1) (string-index->cursor ABC 0)))
(test-assert "string-cursor>?"
  (not (string-cursor>? (string-index->cursor ABC 1)
                        (string-index->cursor ABC 1))))
(test-assert "string-cursor>?"
  (not (string-cursor>? (string-index->cursor ABC 1)
                        (string-index->cursor ABC 2))))
(test-assert "string-cursor>?"
  (not (string-cursor>? (string-index->cursor ABC 1)
                        (string-index->cursor ABC 3))))
(test-assert "string-cursor>?"
  (string-cursor>? (string-index->cursor ABC 2) (string-index->cursor ABC 0)))
(test-assert "string-cursor>?"
  (string-cursor>? (string-index->cursor ABC 2) (string-index->cursor ABC 1)))
(test-assert "string-cursor>?"
  (not (string-cursor>? (string-index->cursor ABC 2)
                        (string-index->cursor ABC 2))))
(test-assert "string-cursor>?"
  (not (string-cursor>? (string-index->cursor ABC 2)
                        (string-index->cursor ABC 3))))
(test-assert "string-cursor>?"
  (string-cursor>? (string-index->cursor ABC 3) (string-index->cursor ABC 0)))
(test-assert "string-cursor>?"
  (string-cursor>? (string-index->cursor ABC 3) (string-index->cursor ABC 1)))
(test-assert "string-cursor>?"
  (string-cursor>? (string-index->cursor ABC 3) (string-index->cursor ABC 2)))
(test-assert     "string-cursor>?"
  (not (string-cursor>? (string-index->cursor ABC 3)
                        (string-index->cursor ABC 3))))
(test-assert "string-cursor<=?"
  (string-cursor<=? (string-index->cursor ABC 0)
                    (string-index->cursor ABC 0)))
(test-assert "string-cursor<=?"
  (string-cursor<=? (string-index->cursor ABC 0)
                    (string-index->cursor ABC 1)))
(test-assert "string-cursor<=?"
  (string-cursor<=? (string-index->cursor ABC 0)
                    (string-index->cursor ABC 2)))
(test-assert "string-cursor<=?"
  (string-cursor<=? (string-index->cursor ABC 0)
                    (string-index->cursor ABC 3)))
(test-assert "string-cursor<=?"
  (not (string-cursor<=? (string-index->cursor ABC 1)
                         (string-index->cursor ABC 0))))
(test-assert "string-cursor<=?"
  (string-cursor<=? (string-index->cursor ABC 1)
                    (string-index->cursor ABC 1)))
(test-assert "string-cursor<=?"
  (string-cursor<=? (string-index->cursor ABC 1)
                    (string-index->cursor ABC 2)))
(test-assert "string-cursor<=?"
  (string-cursor<=? (string-index->cursor ABC 1)
                    (string-index->cursor ABC 3)))
(test-assert "string-cursor<=?"
  (not (string-cursor<=? (string-index->cursor ABC 2)
                         (string-index->cursor ABC 0))))
(test-assert "string-cursor<=?"
  (not (string-cursor<=? (string-index->cursor ABC 2)
                         (string-index->cursor ABC 1))))
(test-assert "string-cursor<=?"
  (string-cursor<=? (string-index->cursor ABC 2)
                    (string-index->cursor ABC 2)))
(test-assert "string-cursor<=?"
  (string-cursor<=? (string-index->cursor ABC 2)
                    (string-index->cursor ABC 3)))
(test-assert "string-cursor<=?"
  (not (string-cursor<=? (string-index->cursor ABC 3)
                         (string-index->cursor ABC 0))))
(test-assert "string-cursor<=?"
  (not (string-cursor<=? (string-index->cursor ABC 3)
                         (string-index->cursor ABC 1))))
(test-assert "string-cursor<=?"
  (not (string-cursor<=? (string-index->cursor ABC 3)
                         (string-index->cursor ABC 2))))
(test-assert "string-cursor<=?"
  (string-cursor<=? (string-index->cursor ABC 3)
                    (string-index->cursor ABC 3)))
(test-assert "string-cursor>=?"
  (string-cursor>=? (string-index->cursor ABC 0)
                    (string-index->cursor ABC 0)))
(test-assert "string-cursor>=?"
  (not (string-cursor>=? (string-index->cursor ABC 0)
                         (string-index->cursor ABC 1))))
(test-assert "string-cursor>=?"
  (not (string-cursor>=? (string-index->cursor ABC 0)
                         (string-index->cursor ABC 2))))
(test-assert "string-cursor>=?"
  (not (string-cursor>=? (string-index->cursor ABC 0)
                         (string-index->cursor ABC 3))))
(test-assert "string-cursor>=?"
  (string-cursor>=? (string-index->cursor ABC 1)
                    (string-index->cursor ABC 0)))
(test-assert "string-cursor>=?"
  (string-cursor>=? (string-index->cursor ABC 1)
                    (string-index->cursor ABC 1)))
(test-assert "string-cursor>=?"
  (not (string-cursor>=? (string-index->cursor ABC 1)
                         (string-index->cursor ABC 2))))
(test-assert "string-cursor>=?"
  (not (string-cursor>=? (string-index->cursor ABC 1)
                         (string-index->cursor ABC 3))))
(test-assert "string-cursor>=?"
  (string-cursor>=? (string-index->cursor ABC 2)
                    (string-index->cursor ABC 0)))
(test-assert "string-cursor>=?"
  (string-cursor>=? (string-index->cursor ABC 2)
                    (string-index->cursor ABC 1)))
(test-assert "string-cursor>=?"
  (string-cursor>=? (string-index->cursor ABC 2)
                    (string-index->cursor ABC 2)))
(test-assert "string-cursor>=?"
  (not (string-cursor>=? (string-index->cursor ABC 2)
                         (string-index->cursor ABC 3))))
(test-assert "string-cursor>=?"
  (string-cursor>=? (string-index->cursor ABC 3)
                    (string-index->cursor ABC 0)))
(test-assert "string-cursor>=?"
  (string-cursor>=? (string-index->cursor ABC 3)
                    (string-index->cursor ABC 1)))
(test-assert "string-cursor>=?"
  (string-cursor>=? (string-index->cursor ABC 3)
                    (string-index->cursor ABC 2)))
(test-assert "string-cursor>=?"
  (string-cursor>=? (string-index->cursor ABC 3)
                    (string-index->cursor ABC 3)))
(test-assert "string-cursor-diff"
  (= 0 (string-cursor-diff ""
                           (string-index->cursor ABC 0)
                           (string-index->cursor ABC 0))))
(test-assert "string-cursor-diff"
  (= 3 (string-cursor-diff ABC
                           (string-index->cursor ABC 0)
                           (string-index->cursor ABC 3))))
(test-assert "string-cursor->index"
  (= 0 (string-cursor->index "" (string-index->cursor "" 0))))
(test-assert "string-cursor->index"
  (= 3 (string-cursor->index ABC (string-index->cursor ABC 3))))
(test-assert "string-index->cursor"
  (= 0 (string-index->cursor "" (string-index->cursor "" 0))))
(test-assert "string-index->cursor"
  (= 3 (string-index->cursor ABC (string-index->cursor ABC 3))))

;;; Predicates

(test-assert "string-null" (string-null? ""))
(test-assert "string-null" (not (string-null? "abc")))
(test-assert "string-every"
  (eqv? #t (string-every (lambda (c) (if (char? c) c #f)) "")))
(test-assert "string-every"
  (eqv? #\c (string-every (lambda (c) (if (char? c) c #f)) "abc")))
(test-assert "string-every"
  (eqv? #f (string-every (lambda (c) (if (char>? c #\b) c #f)) "abc")))
(test-assert "string-every"
  (eqv? #\c (string-every (lambda (c) (if (char>? c #\b) c #f)) "abc" 2)))
(test-assert "string-every"
  (eqv? #t (string-every (lambda (c) (if (char>? c #\b) c #f)) "abc" 1 1)))
(test-assert "string-any"
  (eqv? #f (string-any (lambda (c) (if (char? c) c #f)) "")))
(test-assert "string-any"
  (eqv? #\a (string-any (lambda (c) (if (char? c) c #f)) "abc")))
(test-assert "string-any"
  (eqv? #\c (string-any (lambda (c) (if (char>? c #\b) c #f)) "abc")))
(test-assert "string-any"
  (eqv? #\c (string-any (lambda (c) (if (char>? c #\b) c #f)) "abc" 2)))
(test-assert "string-any"
  (eqv? #f (string-any (lambda (c) (if (char>? c #\b) c #f)) "abc" 0 2)))

;;; Constructors

(test-assert "string-tabulate"
  (equal? "" (string-tabulate
              (lambda (i) (integer->char (+ i (char->integer #\a))))
              0)))
(test-assert "string-tabulate"
  (equal? "abc" (string-tabulate
                 (lambda (i) (integer->char (+ i (char->integer #\a))))
                 3)))
(test-assert "string-unfold"
  (equal? "abc"
          (let ((p (open-input-string "abc")))
            (string-unfold eof-object? values
                           (lambda (x) (read-char p)) (read-char p)))))
(test-assert "string-unfold" (equal? "" (string-unfold null? car cdr '())))
(test-assert "string-unfold"
  (equal? "abc" (string-unfold null? car cdr (string->list "abc"))))
(test-assert "string-unfold"
  (equal? "def" (string-unfold null? car cdr '() "def")))
(test-assert "string-unfold"
  (equal? "defabcG" (string-unfold null? car cdr (string->list "abc") "def"
                                   (lambda (x) (and (null? x) "G")))))
(test-assert "string-unfold-right"
  (equal? "" (string-unfold-right null? car cdr '())))
(test-assert "string-unfold-right"
  (equal? "cba" (string-unfold-right null? car cdr (string->list "abc"))))
(test-assert "string-unfold-right"
  (equal? "def" (string-unfold-right null? car cdr '() "def")))
(test-assert "string-unfold-right"
  (equal? "Gcbadef" (string-unfold-right
                     null? car cdr (string->list "abc")
                     "def" (lambda (x) (and (null? x) "G")))))

;;; Conversion

(test-assert "string->list/cursors" (equal? '() (string->list/cursors "")))
(test-assert "string->list/cursors" (equal? '() (string->list/cursors "" 0)))
(test-assert "string->list/cursors"
  (equal? '() (string->list/cursors "" 0 0)))
(test-assert "string->list/cursors"
  (equal? '(#\a #\b #\c) (string->list/cursors "abc")))
(test-assert "string->list/cursors"
  (equal? '() (string->list/cursors "abc" 3)))
(test-assert "string->list/cursors"
  (equal? '(#\b #\c) (string->list/cursors "abc" 1 3)))
(test-assert "string->list/cursors"
  (equal? '(#\b #\c)
          (string->list/cursors "abc" (string-index->cursor "abc" 1)
                                (string-index->cursor "abc" 3))))
(test-assert "string->vector/cursors"
  (equal? '#() (string->vector/cursors "")))
(test-assert "string->vector/cursors"
  (equal? '#() (string->vector/cursors "" 0)))
(test-assert "string->vector/cursors"
  (equal? '#() (string->vector/cursors "" 0 0)))
(test-assert "string->vector/cursors"
  (equal? '#(#\a #\b #\c) (string->vector/cursors "abc")))
(test-assert "string->vector/cursors"
  (equal? '#() (string->vector/cursors "abc" 3)))
(test-assert "string->vector/cursors"
  (equal? '#(#\b #\c) (string->vector/cursors "abc" 1 3)))
(test-assert
    "string->vector/cursors"
  (equal?
   '#(#\b #\c)
   (string->vector/cursors "abc" (string-index->cursor "abc" 1)
                           (string-index->cursor "abc" 3))))
(test-assert "reverse-list->string" (equal? "" (reverse-list->string '())))
(test-assert "reverse-list->string"
  (equal? "cba" (reverse-list->string '(#\a #\b #\c))))
(test-assert "string-join" (equal? "" (string-join '())))
(test-assert "string-join"
  (equal? " ab cd  e f " (string-join '("" "ab" "cd" "" "e" "f" ""))))
(test-assert "string-join" (equal? "" (string-join '() "")))
(test-assert "string-join"
  (equal? "abcdef" (string-join '("" "ab" "cd" "" "e" "f" "") "")))
(test-assert "string-join" (equal? "" (string-join '() "xyz")))
(test-assert
    "string-join"
  (equal? "xyzabxyzcdxyzxyzexyzfxyz"
          (string-join '("" "ab" "cd" "" "e" "f" "") "xyz")))
(test-assert "string-join" (equal? "" (string-join '() "" 'infix)))
(test-assert "string-join"
  (equal? "abcdef" (string-join '("" "ab" "cd" "" "e" "f" "") "" 'infix)))
(test-assert "string-join" (equal? "" (string-join '() "xyz" 'infix)))
(test-assert
    "string-join"
  (equal? "xyzabxyzcdxyzxyzexyzfxyz"
          (string-join '("" "ab" "cd" "" "e" "f" "") "xyz" 'infix)))
(test-assert "string-join"
  (equal? 'horror
          (guard (exn (#t 'horror)) (string-join '() "" 'strict-infix))))
(test-assert "string-join"
  (equal? "abcdef"
          (string-join '("" "ab" "cd" "" "e" "f" "") "" 'strict-infix)))
(test-assert "string-join"
  (equal? 'wham
          (guard (exn (else 'wham)) (string-join '() "xyz" 'strict-infix))))
(test-assert "string-join"
  (equal? "xyzabxyzcdxyzxyzexyzfxyz"
          (string-join '("" "ab" "cd" "" "e" "f" "") "xyz" 'strict-infix)))
(test-assert "string-join" (equal? "" (string-join '() "" 'suffix)))
(test-assert "string-join"
  (equal? "abcdef" (string-join '("" "ab" "cd" "" "e" "f" "") "" 'suffix)))
(test-assert "string-join" (equal? "" (string-join '() "xyz" 'suffix)))
(test-assert
    "string-join"
  (equal? "xyzabxyzcdxyzxyzexyzfxyzxyz"
          (string-join '("" "ab" "cd" "" "e" "f" "") "xyz" 'suffix)))
(test-assert "string-join" (equal? "" (string-join '() "" 'prefix)))
(test-assert "string-join"
  (equal? "abcdef" (string-join '("" "ab" "cd" "" "e" "f" "") "" 'prefix)))
(test-assert "string-join" (equal? "" (string-join '() "xyz" 'prefix)))
(test-assert
    "string-join"
  (equal? "xyzxyzabxyzcdxyzxyzexyzfxyz"
          (string-join '("" "ab" "cd" "" "e" "f" "") "xyz" 'prefix)))

;;; Selection

(test-assert "string-ref/cursor" (char=? #\a (string-ref/cursor "abc" 0)))
(test-assert "string-ref/cursor" (char=? #\c (string-ref/cursor "abc" 2)))
(test-assert "string-ref/cursor"
  (char=? #\a (string-ref/cursor "abc" (string-index->cursor "abc" 0))))
(test-assert "string-ref/cursor"
  (char=? #\c (string-ref/cursor "abc" (string-index->cursor "abc" 2))))
(test-assert "substring/cursors" (string=? "" (substring/cursors "" 0 0)))
(test-assert "substring/cursors" (string=? "" (substring/cursors "abc" 0 0)))
(test-assert "substring/cursors" (string=? "" (substring/cursors "abc" 3 3)))
(test-assert "substring/cursors" (string=? ABC (substring/cursors ABC 0 3)))
(test-assert
    "substring/cursors"
  (string=?
   ABC
   (substring/cursors ABC (string-index->cursor "abc" 0)
                      (string-index->cursor "abc" 3))))
(test-assert "substring/cursors" (string=? "b" (substring/cursors "abc" 1 2)))
(test-assert "string-copy/cursors" (string=? "" (string-copy/cursors "")))
(test-assert "string-copy/cursors"
  (string=? "abc" (string-copy/cursors "abc")))
(test-assert "string-copy/cursors"
  (string=? "" (string-copy/cursors "abc" 3)))
(test-assert "string-copy/cursors"
  (string=? "c" (string-copy/cursors "abc" 2)))
(test-assert "string-copy/cursors"
  (string=? "abc" (string-copy/cursors "abc" 0)))
(test-assert "string-copy/cursors"
  (string=? "b" (string-copy/cursors "abc" 1 2)))
(test-assert "string-copy/cursors" (string=? "" (string-copy/cursors "" 0 0)))
(test-assert "string-copy/cursors"
  (string=? "" (string-copy/cursors "abc" 0 0)))
(test-assert "string-copy/cursors"
  (string=? "" (string-copy/cursors "abc" 3 3)))
(test-assert "string-copy/cursors"
  (string=? "abc" (string-copy/cursors "abc" 0 3)))
(test-assert "string-copy/cursors"
  (string=? "b" (string-copy/cursors "abc" 1 2)))
(test-assert
    "string-copy/cursors"
  (string=?
   (substring ABC 1 2)
   (string-copy/cursors ABC (string-index->cursor "abc" 1)
                        (string-index->cursor "abc" 2))))
(test-assert "string-take" (string=? "" (string-take "" 0)))
(test-assert "string-take" (string=? "" (string-take "abcdef" 0)))
(test-assert "string-take" (string=? "ab" (string-take "abcdef" 2)))
(test-assert "string-drop" (string=? "" (string-drop "" 0)))
(test-assert "string-drop" (string=? "abcdef" (string-drop "abcdef" 0)))
(test-assert "string-drop" (string=? "cdef" (string-drop "abcdef" 2)))
(test-assert "string-take-right" (string=? "" (string-take-right "" 0)))
(test-assert "string-take-right" (string=? "" (string-take-right "abcdef" 0)))
(test-assert "string-take-right"
  (string=? "ef" (string-take-right "abcdef" 2)))
(test-assert "string-drop-right" (string=? "" (string-drop-right "" 0)))
(test-assert "string-drop-right"
  (string=? "abcdef" (string-drop-right "abcdef" 0)))
(test-assert "string-drop-right"
  (string=? "abcd" (string-drop-right "abcdef" 2)))
(test-assert "string-pad" (string=? "" (string-pad "" 0)))
(test-assert "string-pad" (string=? "     " (string-pad "" 5)))
(test-assert "string-pad" (string=? "  325" (string-pad "325" 5)))
(test-assert "string-pad" (string=? "71325" (string-pad "71325" 5)))
(test-assert "string-pad" (string=? "71325" (string-pad "8871325" 5)))
(test-assert "string-pad" (string=? "" (string-pad "" 0 #\*)))
(test-assert "string-pad" (string=? "*****" (string-pad "" 5 #\*)))
(test-assert "string-pad" (string=? "**325" (string-pad "325" 5 #\*)))
(test-assert "string-pad" (string=? "71325" (string-pad "71325" 5 #\*)))
(test-assert "string-pad" (string=? "71325" (string-pad "8871325" 5 #\*)))
(test-assert "string-pad" (string=? "" (string-pad "" 0 #\* 0)))
(test-assert "string-pad" (string=? "*****" (string-pad "" 5 #\* 0)))
(test-assert "string-pad" (string=? "**325" (string-pad "325" 5 #\* 0)))
(test-assert "string-pad" (string=? "71325" (string-pad "71325" 5 #\* 0)))
(test-assert "string-pad" (string=? "71325" (string-pad "8871325" 5 #\* 0)))
(test-assert "string-pad" (string=? "***25" (string-pad "325" 5 #\* 1)))
(test-assert "string-pad" (string=? "*1325" (string-pad "71325" 5 #\* 1)))
(test-assert "string-pad" (string=? "71325" (string-pad "8871325" 5 #\* 1)))
(test-assert "string-pad" (string=? "" (string-pad "" 0 #\* 0 0)))
(test-assert "string-pad" (string=? "*****" (string-pad "" 5 #\* 0 0)))
(test-assert "string-pad" (string=? "**325" (string-pad "325" 5 #\* 0 3)))
(test-assert "string-pad" (string=? "**713" (string-pad "71325" 5 #\* 0 3)))
(test-assert "string-pad" (string=? "**887" (string-pad "8871325" 5 #\* 0 3)))
(test-assert "string-pad" (string=? "***25" (string-pad "325" 5 #\* 1 3)))
(test-assert "string-pad" (string=? "**132" (string-pad "71325" 5 #\* 1 4)))
(test-assert "string-pad" (string=? "*8713" (string-pad "8871325" 5 #\* 1 5)))
(test-assert "string-pad-right" (string=? "" (string-pad-right "" 0)))
(test-assert "string-pad-right" (string=? "     " (string-pad-right "" 5)))
(test-assert "string-pad-right" (string=? "325  " (string-pad-right "325" 5)))
(test-assert "string-pad-right"
  (string=? "71325" (string-pad-right "71325" 5)))
(test-assert "string-pad-right"
  (string=? "88713" (string-pad-right "8871325" 5)))
(test-assert "string-pad-right" (string=? "" (string-pad-right "" 0 #\*)))
(test-assert "string-pad-right"
  (string=? "*****" (string-pad-right "" 5 #\*)))
(test-assert "string-pad-right"
  (string=? "325**" (string-pad-right "325" 5 #\*)))
(test-assert "string-pad-right"
  (string=? "71325" (string-pad-right "71325" 5 #\*)))
(test-assert "string-pad-right"
  (string=? "88713" (string-pad-right "8871325" 5 #\*)))
(test-assert "string-pad-right" (string=? "" (string-pad-right "" 0 #\* 0)))
(test-assert "string-pad-right"
  (string=? "*****" (string-pad-right "" 5 #\* 0)))
(test-assert "string-pad-right"
  (string=? "325**" (string-pad-right "325" 5 #\* 0)))
(test-assert "string-pad-right"
  (string=? "71325" (string-pad-right "71325" 5 #\* 0)))
(test-assert "string-pad-right"
  (string=? "88713" (string-pad-right "8871325" 5 #\* 0)))
(test-assert "string-pad-right"
  (string=? "25***" (string-pad-right "325" 5 #\* 1)))
(test-assert "string-pad-right"
  (string=? "1325*" (string-pad-right "71325" 5 #\* 1)))
(test-assert "string-pad-right"
  (string=? "87132" (string-pad-right "8871325" 5 #\* 1)))
(test-assert "string-pad-right" (string=? "" (string-pad-right "" 0 #\* 0 0)))
(test-assert "string-pad-right"
  (string=? "*****" (string-pad-right "" 5 #\* 0 0)))
(test-assert "string-pad-right"
  (string=? "325**" (string-pad-right "325" 5 #\* 0 3)))
(test-assert "string-pad-right"
  (string=? "713**" (string-pad-right "71325" 5 #\* 0 3)))
(test-assert "string-pad-right"
  (string=? "887**" (string-pad-right "8871325" 5 #\* 0 3)))
(test-assert "string-pad-right"
  (string=? "25***" (string-pad-right "325" 5 #\* 1 3)))
(test-assert "string-pad-right"
  (string=? "132**" (string-pad-right "71325" 5 #\* 1 4)))
(test-assert "string-pad-right"
  (string=? "8713*" (string-pad-right "8871325" 5 #\* 1 5)))
(test-assert "string-trim" (string=? "" (string-trim "")))
(test-assert "string-trim" (string=? "a  b  c  " (string-trim "  a  b  c  ")))
(test-assert "string-trim" (string=? "" (string-trim "" char-whitespace?)))
(test-assert "string-trim"
  (string=? "a  b  c  " (string-trim "  a  b  c  " char-whitespace?)))
(test-assert "string-trim" (string=? "" (string-trim "  a  b  c  " char?)))
(test-assert "string-trim" (string=? "" (string-trim "" char-whitespace? 0)))
(test-assert "string-trim"
  (string=? "a  b  c  " (string-trim "  a  b  c  " char-whitespace? 0)))
(test-assert "string-trim" (string=? "" (string-trim "  a  b  c  " char? 0)))
(test-assert "string-trim"
  (string=? "b  c  " (string-trim "  a  b  c  " char-whitespace? 3)))
(test-assert "string-trim" (string=? "" (string-trim "  a  b  c  " char? 3)))
(test-assert "string-trim"
  (string=? "" (string-trim "  a  b  c  " char? 0 11)))
(test-assert "string-trim"
  (string=? "b  c  " (string-trim "  a  b  c  " char-whitespace? 3 11)))
(test-assert "string-trim"
  (string=? "" (string-trim "  a  b  c  " char? 3 11)))
(test-assert "string-trim"
  (string=? "" (string-trim "  a  b  c  " char? 0 8)))
(test-assert "string-trim"
  (string=? "b  " (string-trim "  a  b  c  " char-whitespace? 3 8)))
(test-assert "string-trim"
  (string=? "" (string-trim "  a  b  c  " char? 3 8)))
(test-assert "string-trim-right" (string=? "" (string-trim-right "")))
(test-assert "string-trim-right"
  (string=? "  a  b  c" (string-trim-right "  a  b  c  ")))
(test-assert "string-trim-right"
  (string=? "" (string-trim-right "" char-whitespace?)))
(test-assert "string-trim-right"
  (string=? "  a  b  c" (string-trim-right "  a  b  c  " char-whitespace?)))
(test-assert "string-trim-right"
  (string=? "" (string-trim-right "  a  b  c  " char?)))
(test-assert "string-trim-right"
  (string=? "" (string-trim-right "" char-whitespace? 0)))
(test-assert "string-trim-right"
  (string=? "  a  b  c" (string-trim-right "  a  b  c  " char-whitespace? 0)))
(test-assert "string-trim-right"
  (string=? "" (string-trim-right "  a  b  c  " char? 0)))
(test-assert "string-trim-right"
  (string=? "  b  c" (string-trim-right "  a  b  c  " char-whitespace? 3)))
(test-assert "string-trim-right"
  (string=? "" (string-trim-right "  a  b  c  " char? 3)))
(test-assert "string-trim-right"
  (string=? "" (string-trim-right "  a  b  c  " char? 0 11)))
(test-assert "string-trim-right"
  (string=? "  b  c" (string-trim-right "  a  b  c  " char-whitespace? 3 11)))
(test-assert "string-trim-right"
  (string=? "" (string-trim-right "  a  b  c  " char? 3 11)))
(test-assert "string-trim-right"
  (string=? "" (string-trim-right "  a  b  c  " char? 0 8)))
(test-assert "string-trim-right"
  (string=? "  b" (string-trim-right "  a  b  c  " char-whitespace? 3 8)))
(test-assert "string-trim-right"
  (string=? "" (string-trim-right "  a  b  c  " char? 3 8)))
(test-assert "string-trim-both" (string=? "" (string-trim-both "")))
(test-assert "string-trim-both"
  (string=? "a  b  c" (string-trim-both "  a  b  c  ")))
(test-assert "string-trim-both"
  (string=? "" (string-trim-both "" char-whitespace?)))
(test-assert "string-trim-both"
  (string=? "a  b  c" (string-trim-both "  a  b  c  " char-whitespace?)))
(test-assert "string-trim-both"
  (string=? "" (string-trim-both "  a  b  c  " char?)))
(test-assert "string-trim-both"
  (string=? "" (string-trim-both "" char-whitespace? 0)))
(test-assert "string-trim-both"
  (string=? "a  b  c" (string-trim-both "  a  b  c  " char-whitespace? 0)))
(test-assert "string-trim-both"
  (string=? "" (string-trim-both "  a  b  c  " char? 0)))
(test-assert "string-trim-both"
  (string=? "b  c" (string-trim-both "  a  b  c  " char-whitespace? 3)))
(test-assert "string-trim-both"
  (string=? "" (string-trim-both "  a  b  c  " char? 3)))
(test-assert "string-trim-both"
  (string=? "" (string-trim-both "  a  b  c  " char? 0 11)))
(test-assert "string-trim-both"
  (string=? "b  c" (string-trim-both "  a  b  c  " char-whitespace? 3 11)))
(test-assert "string-trim-both"
  (string=? "" (string-trim-both "  a  b  c  " char? 3 11)))
(test-assert "string-trim-both"
  (string=? "" (string-trim-both "  a  b  c  " char? 0 8)))
(test-assert "string-trim-both"
  (string=? "b" (string-trim-both "  a  b  c  " char-whitespace? 3 8)))
(test-assert "string-trim-both"
  (string=? "" (string-trim-both "  a  b  c  " char? 3 8)))
(test-assert "string-prefix-length" (= 0 (string-prefix-length "" "")))
(test-assert "string-prefix-length"
  (= 0 (string-prefix-length "" "aabbccddee")))
(test-assert "string-prefix-length" (= 0 (string-prefix-length "aisle" "")))
(test-assert "string-prefix-length"
  (= 0 (string-prefix-length "" "aabbccddee")))
(test-assert "string-prefix-length"
  (= 1 (string-prefix-length "aisle" "aabbccddee")))
(test-assert "string-prefix-length"
  (= 0 (string-prefix-length "bail" "aabbccddee")))
(test-assert "string-prefix-length"
  (= 4 (string-prefix-length "prefix" "preface")))
(test-assert "string-prefix-length" (= 0 (string-prefix-length "" "" 0)))
(test-assert "string-prefix-length"
  (= 0 (string-prefix-length "" "aabbccddee" 0)))
(test-assert "string-prefix-length" (= 0 (string-prefix-length "aisle" "" 0)))
(test-assert "string-prefix-length"
  (= 1 (string-prefix-length "aisle" "aabbccddee" 0)))
(test-assert "string-prefix-length"
  (= 0 (string-prefix-length "bail" "aabbccddee" 0)))
(test-assert "string-prefix-length"
  (= 4 (string-prefix-length "prefix" "preface" 0)))
(test-assert "string-prefix-length" (= 0 (string-prefix-length "aisle" "" 1)))
(test-assert "string-prefix-length"
  (= 0 (string-prefix-length "aisle" "aabbccddee" 1)))
(test-assert "string-prefix-length"
  (= 1 (string-prefix-length "bail" "aabbccddee" 1)))
(test-assert "string-prefix-length"
  (= 0 (string-prefix-length "prefix" "preface" 1)))
(test-assert "string-prefix-length" (= 0 (string-prefix-length "" "" 0 0)))
(test-assert "string-prefix-length"
  (= 0 (string-prefix-length "" "aabbccddee" 0 0)))
(test-assert "string-prefix-length"
  (= 0 (string-prefix-length "aisle" "" 0 4)))
(test-assert "string-prefix-length"
  (= 1 (string-prefix-length "aisle" "aabbccddee" 0 4)))
(test-assert "string-prefix-length"
  (= 0 (string-prefix-length "bail" "aabbccddee" 0 1)))
(test-assert "string-prefix-length"
  (= 0 (string-prefix-length "aisle" "" 1 4)))
(test-assert "string-prefix-length"
  (= 0 (string-prefix-length "aisle" "aabbccddee" 1 4)))
(test-assert "string-prefix-length"
  (= 1 (string-prefix-length "bail" "aabbccddee" 1 4)))
(test-assert "string-prefix-length"
  (= 0 (string-prefix-length "prefix" "preface" 1 5)))
(test-assert "string-prefix-length" (= 0 (string-prefix-length "" "" 0 0 0)))
(test-assert "string-prefix-length"
  (= 0 (string-prefix-length "" "aabbccddee" 0 0 0)))
(test-assert "string-prefix-length"
  (= 0 (string-prefix-length "aisle" "" 0 4 0)))
(test-assert "string-prefix-length"
  (= 0 (string-prefix-length "aisle" "aabbccddee" 0 4 2)))
(test-assert "string-prefix-length"
  (= 1 (string-prefix-length "bail" "aabbccddee" 0 1 2)))
(test-assert "string-prefix-length"
  (= 0 (string-prefix-length "prefix" "preface" 0 5 1)))
(test-assert "string-prefix-length"
  (= 0 (string-prefix-length "aisle" "" 1 4 0)))
(test-assert "string-prefix-length"
  (= 0 (string-prefix-length "aisle" "aabbccddee" 1 4 3)))
(test-assert "string-prefix-length"
  (= 0 (string-prefix-length "bail" "aabbccddee" 1 4 3)))
(test-assert "string-prefix-length"
  (= 3 (string-prefix-length "prefix" "preface" 1 5 1)))
(test-assert "string-prefix-length"
  (= 0 (string-prefix-length "" "" 0 0 0 0)))
(test-assert "string-prefix-length"
  (= 0 (string-prefix-length "" "aabbccddee" 0 0 0 0)))
(test-assert "string-prefix-length"
  (= 0 (string-prefix-length "aisle" "" 0 4 0 0)))
(test-assert "string-prefix-length"
  (= 0 (string-prefix-length "aisle" "aabbccddee" 0 4 2 10)))
(test-assert "string-prefix-length"
  (= 1 (string-prefix-length "bail" "aabbccddee" 0 1 2 10)))
(test-assert "string-prefix-length"
  (= 0 (string-prefix-length "prefix" "preface" 0 5 1 6)))
(test-assert "string-prefix-length"
  (= 0 (string-prefix-length "aisle" "" 1 4 0 0)))
(test-assert "string-prefix-length"
  (= 0 (string-prefix-length "aisle" "aabbccddee" 1 4 3 3)))
(test-assert "string-prefix-length"
  (= 0 (string-prefix-length "bail" "aabbccddee" 1 4 3 6)))
(test-assert "string-prefix-length"
  (= 3 (string-prefix-length "prefix" "preface" 1 5 1 7)))
(test-assert "string-suffix-length" (= 0 (string-suffix-length "" "")))
(test-assert "string-suffix-length"
  (= 0 (string-suffix-length "" "aabbccddee")))
(test-assert "string-suffix-length" (= 0 (string-suffix-length "aisle" "")))
(test-assert "string-suffix-length"
  (= 0 (string-suffix-length "" "aabbccddee")))
(test-assert "string-suffix-length"
  (= 1 (string-suffix-length "aisle" "aabbccddee")))
(test-assert "string-suffix-length"
  (= 0 (string-suffix-length "bail" "aabbccddee")))
(test-assert "string-suffix-length"
  (= 3 (string-suffix-length "place" "preface")))
(test-assert "string-suffix-length" (= 0 (string-suffix-length "" "" 0)))
(test-assert "string-suffix-length"
  (= 0 (string-suffix-length "" "aabbccddee" 0)))
(test-assert "string-suffix-length" (= 0 (string-suffix-length "aisle" "" 0)))
(test-assert "string-suffix-length"
  (= 1 (string-suffix-length "aisle" "aabbccddee" 0)))
(test-assert "string-suffix-length"
  (= 0 (string-suffix-length "bail" "aabbccddee" 0)))
(test-assert "string-suffix-length"
  (= 3 (string-suffix-length "place" "preface" 0)))
(test-assert "string-suffix-length" (= 0 (string-suffix-length "aisle" "" 1)))
(test-assert "string-suffix-length"
  (= 1 (string-suffix-length "aisle" "aabbccddee" 1)))
(test-assert "string-suffix-length"
  (= 0 (string-suffix-length "bail" "aabbccddee" 1)))
(test-assert "string-suffix-length"
  (= 3 (string-suffix-length "place" "preface" 1)))
(test-assert "string-suffix-length" (= 0 (string-suffix-length "" "" 0 0)))
(test-assert "string-suffix-length"
  (= 0 (string-suffix-length "" "aabbccddee" 0 0)))
(test-assert "string-suffix-length"
  (= 0 (string-suffix-length "aisle" "" 0 4)))
(test-assert "string-suffix-length"
  (= 0 (string-suffix-length "aisle" "aabbccddee" 0 4)))
(test-assert "string-suffix-length"
  (= 0 (string-suffix-length "bail" "aabbccddee" 0 1)))
(test-assert "string-suffix-length"
  (= 0 (string-suffix-length "aisle" "" 1 4)))
(test-assert "string-suffix-length"
  (= 0 (string-suffix-length "aisle" "aabbccddee" 1 4)))
(test-assert "string-suffix-length"
  (= 1 (string-suffix-length "aisle" "aabbccddee" 1 5)))
(test-assert "string-suffix-length"
  (= 0 (string-suffix-length "bail" "aabbccddee" 1 4)))
(test-assert "string-suffix-length"
  (= 3 (string-suffix-length "place" "preface" 1 5)))
(test-assert "string-suffix-length" (= 0 (string-suffix-length "" "" 0 0 0)))
(test-assert "string-suffix-length"
  (= 0 (string-suffix-length "" "aabbccddee" 0 0 0)))
(test-assert "string-suffix-length"
  (= 0 (string-suffix-length "aisle" "" 0 4 0)))
(test-assert "string-suffix-length"
  (= 0 (string-suffix-length "aisle" "aabbccddee" 0 4 2)))
(test-assert "string-suffix-length"
  (= 0 (string-suffix-length "bail" "aabbccddee" 0 1 2)))
(test-assert "string-suffix-length"
  (= 3 (string-suffix-length "place" "preface" 0 5 1)))
(test-assert "string-suffix-length"
  (= 0 (string-suffix-length "aisle" "" 1 4 0)))
(test-assert "string-suffix-length"
  (= 0 (string-suffix-length "aisle" "aabbccddee" 1 4 3)))
(test-assert "string-suffix-length"
  (= 0 (string-suffix-length "bail" "aabbccddee" 1 4 3)))
(test-assert "string-suffix-length"
  (= 3 (string-suffix-length "place" "preface" 1 5 1)))
(test-assert "string-suffix-length"
  (= 0 (string-suffix-length "" "" 0 0 0 0)))
(test-assert "string-suffix-length"
  (= 0 (string-suffix-length "" "aabbccddee" 0 0 0 0)))
(test-assert "string-suffix-length"
  (= 0 (string-suffix-length "aisle" "" 0 4 0 0)))
(test-assert "string-suffix-length"
  (= 1 (string-suffix-length "aisle" "aabbccddee" 0 5 2 10)))
(test-assert "string-suffix-length"
  (= 1 (string-suffix-length "bail" "aabbccddee" 0 1 2 4)))
(test-assert "string-suffix-length"
  (= 0 (string-suffix-length "place" "preface" 0 5 1 6)))
(test-assert "string-suffix-length"
  (= 2 (string-suffix-length "place" "preface" 0 4 1 6)))
(test-assert "string-suffix-length"
  (= 0 (string-suffix-length "aisle" "" 1 4 0 0)))
(test-assert "string-suffix-length"
  (= 0 (string-suffix-length "aisle" "aabbccddee" 1 4 3 3)))
(test-assert "string-suffix-length"
  (= 0 (string-suffix-length "bail" "aabbccddee" 1 4 3 6)))
(test-assert "string-suffix-length"
  (= 3 (string-suffix-length "place" "preface" 1 5 1 7)))
(test-assert "string-prefix?" (eq? #t (string-prefix? "" "")))
(test-assert "string-prefix?" (eq? #t (string-prefix? "" "abc")))
(test-assert "string-prefix?" (eq? #t (string-prefix? "a" "abc")))
(test-assert "string-prefix?" (eq? #f (string-prefix? "c" "abc")))
(test-assert "string-prefix?" (eq? #t (string-prefix? "ab" "abc")))
(test-assert "string-prefix?" (eq? #f (string-prefix? "ac" "abc")))
(test-assert "string-prefix?" (eq? #t (string-prefix? "abc" "abc")))
(test-assert "string-suffix?" (eq? #t (string-suffix? "" "")))
(test-assert "string-suffix?" (eq? #t (string-suffix? "" "abc")))
(test-assert "string-suffix?" (eq? #f (string-suffix? "a" "abc")))
(test-assert "string-suffix?" (eq? #t (string-suffix? "c" "abc")))
(test-assert "string-suffix?" (eq? #f (string-suffix? "ac" "abc")))
(test-assert "string-suffix?" (eq? #t (string-suffix? "bc" "abc")))
(test-assert "string-suffix?" (eq? #t (string-suffix? "abc" "abc")))
(test-assert "string-prefix?" (eq? #t (string-prefix? "" "" 0)))
(test-assert "string-prefix?" (eq? #t (string-prefix? "" "abc" 0)))
(test-assert "string-prefix?" (eq? #t (string-prefix? "a" "abc" 0)))
(test-assert "string-prefix?" (eq? #f (string-prefix? "c" "abc" 0)))
(test-assert "string-prefix?" (eq? #t (string-prefix? "ab" "abc" 0)))
(test-assert "string-prefix?" (eq? #f (string-prefix? "ac" "abc" 0)))
(test-assert "string-prefix?" (eq? #t (string-prefix? "abc" "abc" 0)))
(test-assert "string-suffix?" (eq? #t (string-suffix? "" "" 0)))
(test-assert "string-suffix?" (eq? #t (string-suffix? "" "abc" 0)))
(test-assert "string-suffix?" (eq? #f (string-suffix? "a" "abc" 0)))
(test-assert "string-suffix?" (eq? #t (string-suffix? "c" "abc" 0)))
(test-assert "string-suffix?" (eq? #f (string-suffix? "ac" "abc" 0)))
(test-assert "string-suffix?" (eq? #t (string-suffix? "bc" "abc" 0)))
(test-assert "string-suffix?" (eq? #t (string-suffix? "abc" "abc" 0)))
(test-assert "string-prefix?" (eq? #t (string-prefix? "ab" "abc" 2)))
(test-assert "string-prefix?" (eq? #t (string-prefix? "ac" "abc" 2)))
(test-assert "string-prefix?" (eq? #f (string-prefix? "abc" "abc" 2)))
(test-assert "string-suffix?" (eq? #t (string-suffix? "ac" "abc" 2)))
(test-assert "string-suffix?" (eq? #t (string-suffix? "bc" "abc" 2)))
(test-assert "string-suffix?" (eq? #t (string-suffix? "abc" "abc" 2)))
(test-assert "string-prefix?" (eq? #t (string-prefix? "" "" 0 0)))
(test-assert "string-prefix?" (eq? #t (string-prefix? "" "abc" 0 0)))
(test-assert "string-prefix?" (eq? #t (string-prefix? "a" "abc" 0 0)))
(test-assert "string-prefix?" (eq? #f (string-prefix? "c" "abc" 0 1)))
(test-assert "string-prefix?" (eq? #t (string-prefix? "ab" "abc" 0 1)))
(test-assert "string-prefix?" (eq? #t (string-prefix? "ab" "abc" 0 2)))
(test-assert "string-prefix?" (eq? #f (string-prefix? "ac" "abc" 0 2)))
(test-assert "string-prefix?" (eq? #t (string-prefix? "abc" "abc" 0 3)))
(test-assert "string-suffix?" (eq? #t (string-suffix? "" "" 0 0)))
(test-assert "string-suffix?" (eq? #t (string-suffix? "" "abc" 0 0)))
(test-assert "string-suffix?" (eq? #f (string-suffix? "a" "abc" 0 1)))
(test-assert "string-suffix?" (eq? #t (string-suffix? "c" "abc" 0 1)))
(test-assert "string-suffix?" (eq? #t (string-suffix? "ac" "abc" 1 2)))
(test-assert "string-suffix?" (eq? #f (string-suffix? "ac" "abc" 0 2)))
(test-assert "string-suffix?" (eq? #t (string-suffix? "bc" "abc" 0 2)))
(test-assert "string-suffix?" (eq? #t (string-suffix? "abc" "abc" 0 3)))
(test-assert "string-prefix?" (eq? #t (string-prefix? "ab" "abc" 2 2)))
(test-assert "string-prefix?" (eq? #t (string-prefix? "ac" "abc" 2 2)))
(test-assert "string-prefix?" (eq? #f (string-prefix? "abc" "abc" 2 3)))
(test-assert "string-suffix?" (eq? #t (string-suffix? "ac" "abc" 2 2)))
(test-assert "string-suffix?" (eq? #t (string-suffix? "bc" "abc" 2 2)))
(test-assert "string-suffix?" (eq? #t (string-suffix? "abc" "abc" 2 3)))
(test-assert "string-prefix?" (eq? #t (string-prefix? "" "" 0 0 0)))
(test-assert "string-prefix?" (eq? #t (string-prefix? "" "abc" 0 0 0)))
(test-assert "string-prefix?" (eq? #t (string-prefix? "a" "abc" 0 0 0)))
(test-assert "string-prefix?" (eq? #f (string-prefix? "c" "abc" 0 1 0)))
(test-assert "string-prefix?" (eq? #t (string-prefix? "ab" "abc" 0 1 0)))
(test-assert "string-prefix?" (eq? #t (string-prefix? "ab" "abc" 0 2 0)))
(test-assert "string-prefix?" (eq? #f (string-prefix? "ac" "abc" 0 2 0)))
(test-assert "string-prefix?" (eq? #t (string-prefix? "abc" "abc" 0 3 0)))
(test-assert "string-suffix?" (eq? #t (string-suffix? "" "" 0 0 0)))
(test-assert "string-suffix?" (eq? #t (string-suffix? "" "abc" 0 0 0)))
(test-assert "string-suffix?" (eq? #f (string-suffix? "a" "abc" 0 1 0)))
(test-assert "string-suffix?" (eq? #t (string-suffix? "c" "abc" 0 1 0)))
(test-assert "string-suffix?" (eq? #t (string-suffix? "ac" "abc" 1 2 0)))
(test-assert "string-suffix?" (eq? #f (string-suffix? "ac" "abc" 0 2 0)))
(test-assert "string-suffix?" (eq? #t (string-suffix? "bc" "abc" 0 2 0)))
(test-assert "string-suffix?" (eq? #t (string-suffix? "abc" "abc" 0 3 0)))
(test-assert "string-prefix?" (eq? #t (string-prefix? "ab" "abc" 2 2 0)))
(test-assert "string-prefix?" (eq? #t (string-prefix? "ac" "abc" 2 2 0)))
(test-assert "string-prefix?" (eq? #f (string-prefix? "abc" "abc" 2 3 0)))
(test-assert "string-suffix?" (eq? #t (string-suffix? "ac" "abc" 2 2 0)))
(test-assert "string-suffix?" (eq? #t (string-suffix? "bc" "abc" 2 2 0)))
(test-assert "string-suffix?" (eq? #t (string-suffix? "abc" "abc" 2 3 0)))
(test-assert "string-prefix?" (eq? #t (string-prefix? "" "abc" 0 0 1)))
(test-assert "string-prefix?" (eq? #t (string-prefix? "a" "abc" 0 0 1)))
(test-assert "string-prefix?" (eq? #t (string-prefix? "c" "abc" 0 1 2)))
(test-assert "string-prefix?" (eq? #f (string-prefix? "ab" "abc" 0 1 2)))
(test-assert "string-prefix?" (eq? #f (string-prefix? "ab" "abc" 0 2 1)))
(test-assert "string-prefix?" (eq? #f (string-prefix? "ac" "abc" 0 2 1)))
(test-assert "string-prefix?" (eq? #f (string-prefix? "abc" "abc" 0 3 1)))
(test-assert "string-suffix?" (eq? #f (string-suffix? "a" "abc" 0 1 2)))
(test-assert "string-suffix?" (eq? #t (string-suffix? "c" "abc" 0 1 1)))
(test-assert "string-suffix?" (eq? #t (string-suffix? "ac" "abc" 1 2 2)))
(test-assert "string-suffix?" (eq? #t (string-suffix? "bc" "abc" 0 2 1)))
(test-assert "string-suffix?" (eq? #f (string-suffix? "bc" "abc" 0 2 2)))
(test-assert "string-prefix?" (eq? #t (string-prefix? "" "" 0 0 0 0)))
(test-assert "string-prefix?" (eq? #t (string-prefix? "" "abc" 0 0 0 3)))
(test-assert "string-prefix?" (eq? #t (string-prefix? "a" "abc" 0 0 0 3)))
(test-assert "string-prefix?" (eq? #f (string-prefix? "c" "abc" 0 1 0 3)))
(test-assert "string-prefix?" (eq? #t (string-prefix? "ab" "abc" 0 1 0 3)))
(test-assert "string-prefix?" (eq? #t (string-prefix? "ab" "abc" 0 2 0 3)))
(test-assert "string-prefix?" (eq? #f (string-prefix? "ac" "abc" 0 2 0 3)))
(test-assert "string-prefix?" (eq? #t (string-prefix? "abc" "abc" 0 3 0 3)))
(test-assert "string-suffix?" (eq? #t (string-suffix? "" "abc" 0 0 0 3)))
(test-assert "string-suffix?" (eq? #f (string-suffix? "a" "abc" 0 1 0 3)))
(test-assert "string-suffix?" (eq? #t (string-suffix? "c" "abc" 0 1 0 3)))
(test-assert "string-suffix?" (eq? #t (string-suffix? "ac" "abc" 1 2 0 3)))
(test-assert "string-suffix?" (eq? #f (string-suffix? "ac" "abc" 0 2 0 3)))
(test-assert "string-suffix?" (eq? #t (string-suffix? "bc" "abc" 0 2 0 3)))
(test-assert "string-suffix?" (eq? #t (string-suffix? "abc" "abc" 0 3 0 3)))
(test-assert "string-prefix?" (eq? #t (string-prefix? "ab" "abc" 2 2 0 3)))
(test-assert "string-prefix?" (eq? #t (string-prefix? "ac" "abc" 2 2 0 3)))
(test-assert "string-prefix?" (eq? #f (string-prefix? "abc" "abc" 2 3 0 3)))
(test-assert "string-suffix?" (eq? #t (string-suffix? "ac" "abc" 2 2 0 3)))
(test-assert "string-suffix?" (eq? #t (string-suffix? "bc" "abc" 2 2 0 3)))
(test-assert "string-suffix?" (eq? #t (string-suffix? "abc" "abc" 2 3 0 3)))
(test-assert "string-prefix?" (eq? #t (string-prefix? "" "abc" 0 0 1 3)))
(test-assert "string-prefix?" (eq? #t (string-prefix? "a" "abc" 0 0 1 3)))
(test-assert "string-prefix?" (eq? #t (string-prefix? "c" "abc" 0 1 2 3)))
(test-assert "string-prefix?" (eq? #f (string-prefix? "ab" "abc" 0 1 2 3)))
(test-assert "string-prefix?" (eq? #f (string-prefix? "ab" "abc" 0 2 1 3)))
(test-assert "string-prefix?" (eq? #f (string-prefix? "ac" "abc" 0 2 1 3)))
(test-assert "string-prefix?" (eq? #f (string-prefix? "abc" "abc" 0 3 1 3)))
(test-assert "string-suffix?" (eq? #f (string-suffix? "a" "abc" 0 1 2 3)))
(test-assert "string-suffix?" (eq? #t (string-suffix? "c" "abc" 0 1 1 3)))
(test-assert "string-suffix?" (eq? #t (string-suffix? "ac" "abc" 1 2 2 3)))
(test-assert "string-suffix?" (eq? #t (string-suffix? "bc" "abc" 0 2 1 3)))
(test-assert "string-suffix?" (eq? #f (string-suffix? "bc" "abc" 0 2 2 3)))
(test-assert "string-prefix?" (eq? #t (string-prefix? "" "abc" 0 0 0 2)))
(test-assert "string-prefix?" (eq? #t (string-prefix? "a" "abc" 0 0 0 2)))
(test-assert "string-prefix?" (eq? #f (string-prefix? "c" "abc" 0 1 0 2)))
(test-assert "string-prefix?" (eq? #t (string-prefix? "ab" "abc" 0 1 0 2)))
(test-assert "string-prefix?" (eq? #f (string-prefix? "abc" "abc" 0 3 0 2)))
(test-assert "string-suffix?" (eq? #t (string-suffix? "" "abc" 0 0 0 2)))
(test-assert "string-suffix?" (eq? #f (string-suffix? "c" "abc" 0 1 0 2)))
(test-assert "string-suffix?" (eq? #f (string-suffix? "ac" "abc" 1 2 0 2)))

;;; Searching

(test-assert "string-index"
  (= 0 (string-cursor->index "" (string-index "" char?))))
(test-assert "string-index"
  (= 0 (string-cursor->index "abcdef" (string-index "abcdef" char?))))
(test-assert "string-index"
  (= 4 (string-cursor->index
        "abcdef" (string-index "abcdef" (lambda (c) (char>? c #\d))))))
(test-assert "string-index"
  (= 6 (string-cursor->index
        "abcdef" (string-index "abcdef" char-whitespace?))))
(test-assert "string-index-right"
  (= 0 (string-cursor->index "abcdef" (string-index-right "" char?))))
(test-assert "string-index-right"
  (= 6 (string-cursor->index "abcdef" (string-index-right "abcdef" char?))))
(test-assert "string-index-right"
  (= 6 (string-cursor->index
        "abcdef" (string-index-right "abcdef" (lambda (c) (char>? c #\d))))))
(test-assert "string-index-right"
  (= 0 (string-cursor->index
        "abcdef" (string-index-right "abcdef" char-whitespace?))))
(test-assert "string-skip"
  (= 0 (string-cursor->index "" (string-skip "" string?))))
(test-assert "string-skip"
  (= 0 (string-cursor->index "abcdef" (string-skip "abcdef" string?))))
(test-assert "string-skip"
  (= 4 (string-cursor->index
        "abcdef" (string-skip "abcdef" (lambda (c) (char<=? c #\d))))))
(test-assert "string-skip"
  (= 6 (string-cursor->index "abcdef" (string-skip "abcdef" char?))))
(test-assert "string-skip-right"
  (= 0 (string-cursor->index "" (string-skip-right "" string?))))
(test-assert "string-skip-right"
  (= 6 (string-cursor->index "abcdef" (string-skip-right "abcdef" string?))))
(test-assert "string-skip-right"
  (= 6 (string-cursor->index
        "abcdef" (string-skip-right "abcdef" (lambda (c) (char<=? c #\d))))))
(test-assert "string-skip-right"
  (= 0 (string-cursor->index "abcdef" (string-skip-right "abcdef" char?))))
(test-assert "string-index"
  (= 2 (string-cursor->index "abcdef" (string-index "abcdef" char? 2))))
(test-assert "string-index"
  (= 4 (string-cursor->index
        "abcdef" (string-index "abcdef" (lambda (c) (char>? c #\d)) 2))))
(test-assert "string-index"
  (= 6 (string-cursor->index
        "abcdef" (string-index "abcdef" char-whitespace? 2))))
(test-assert "string-index-right"
  (= 6 (string-cursor->index "abcdef" (string-index-right "abcdef" char? 2))))
(test-assert "string-index-right"
  (= 6 (string-cursor->index
        "abcdef"
        (string-index-right "abcdef" (lambda (c) (char>? c #\d)) 2))))
(test-assert "string-index-right"
  (= 2 (string-cursor->index
        "abcdef" (string-index-right "abcdef" char-whitespace? 2))))
(test-assert "string-skip"
  (= 2 (string-cursor->index "abcdef" (string-skip "abcdef" string? 2))))
(test-assert "string-skip"
  (= 4 (string-cursor->index
        "abcdef" (string-skip "abcdef" (lambda (c) (char<=? c #\d)) 2))))
(test-assert "string-skip"
  (= 6 (string-cursor->index "abcdef" (string-skip "abcdef" char? 2))))
(test-assert "string-skip-right"
  (= 6 (string-cursor->index
        "abcdef" (string-skip-right "abcdef" string? 2))))
(test-assert "string-skip-right"
  (= 6 (string-cursor->index
        "abcdef"
        (string-skip-right "abcdef" (lambda (c) (char<=? c #\d)) 2))))
(test-assert "string-skip-right"
  (= 2 (string-cursor->index "abcdef" (string-skip-right "abcdef" char? 2))))
(test-assert "string-index"
  (= 2 (string-cursor->index "abcdef" (string-index "abcdef" char? 2 5))))
(test-assert "string-index"
  (= 4 (string-cursor->index
        "abcdef" (string-index "abcdef" (lambda (c) (char>? c #\d)) 2 5))))
(test-assert "string-index"
  (= 5 (string-cursor->index
        "abcdef" (string-index "abcdef" char-whitespace? 2 5))))
(test-assert "string-index-right"
  (= 5 (string-cursor->index
        "abcdef" (string-index-right "abcdef" char? 2 5))))
(test-assert "string-index-right"
  (= 5 (string-cursor->index
        "abcdef"
        (string-index-right "abcdef" (lambda (c) (char>? c #\d)) 2 5))))
(test-assert"string-index-right"
  (= 2 (string-cursor->index
        "abcdef" (string-index-right "abcdef" char-whitespace? 2 5))))
(test-assert "string-skip"
  (= 2 (string-cursor->index "abcdef" (string-skip "abcdef" string? 2 5))))
(test-assert "string-skip"
  (= 4 (string-cursor->index
        "abcdef" (string-skip "abcdef" (lambda (c) (char<=? c #\d)) 2 5))))
(test-assert "string-skip"
  (= 5 (string-cursor->index "abcdef" (string-skip "abcdef" char? 2 5))))
(test-assert "string-skip-right"
  (= 5 (string-cursor->index
        "abcdef" (string-skip-right "abcdef" string? 2 5))))
(test-assert "string-skip-right"
  (= 5 (string-cursor->index
        "abcdef"
        (string-skip-right "abcdef" (lambda (c) (char<=? c #\d)) 2 5))))
(test-assert "string-skip-right"
  (= 2 (string-cursor->index
        "abcdef" (string-skip-right "abcdef" char? 2 5))))
(test-assert "string-contains"
  (eqv? 0 (string-cursor->index "" (string-contains "" ""))))
(test-assert "string-contains"
  (eqv? 0 (string-cursor->index
           "abcdeffffoo" (string-contains "abcdeffffoo" ""))))
(test-assert "string-contains"
  (eqv? 0 (string-cursor->index
           "abcdeffffoo" (string-contains "abcdeffffoo" "a"))))
(test-assert "string-contains"
  (eqv? 5 (string-cursor->index
           "abcdeffffoo" (string-contains "abcdeffffoo" "ff"))))
(test-assert "string-contains"
  (eqv? 4 (string-cursor->index
           "abcdeffffoo" (string-contains "abcdeffffoo" "eff"))))
(test-assert "string-contains"
  (eqv? 8 (string-cursor->index
           "abcdeffffoo" (string-contains "abcdeffffoo" "foo"))))
(test-assert "string-contains"
  (eqv? #f (string-cursor->index
            "abcdeffffoo" (string-contains "abcdeffffoo" "efffoo"))))
(test-assert "string-contains-right"
  (eqv? 0 (string-cursor->index "" (string-contains-right "" ""))))
(test-assert "string-contains-right"
  (eqv? 11 (string-cursor->index
            "abcdeffffoo" (string-contains-right "abcdeffffoo" ""))))
(test-assert "string-contains-right"
  (eqv? 0 (string-cursor->index
           "abcdeffffoo" (string-contains-right "abcdeffffoo" "a"))))
(test-assert "string-contains-right"
  (eqv? 7 (string-cursor->index
           "abcdeffffoo" (string-contains-right "abcdeffffoo" "ff"))))
(test-assert "string-contains-right"
  (eqv? 4 (string-cursor->index
           "abcdeffffoo" (string-contains-right "abcdeffffoo" "eff"))))
(test-assert "string-contains-right"
  (eqv? 8 (string-cursor->index
           "abcdeffffoo" (string-contains-right "abcdeffffoo" "foo"))))
(test-assert "string-contains-right"
  (eqv? #f (string-cursor->index
            "abcdeffffoo" (string-contains-right "abcdeffffoo" "efffoo"))))
(test-assert "string-contains"
  (eqv? 0 (string-cursor->index "" (string-contains "" "" 0))))
(test-assert "string-contains"
  (eqv? 2 (string-cursor->index
           "abcdeffffoo" (string-contains "abcdeffffoo" "" 2))))
(test-assert "string-contains"
  (eqv? #f (string-cursor->index
            "abcdeffffoo" (string-contains "abcdeffffoo" "a" 2))))
(test-assert "string-contains"
  (eqv? 5 (string-cursor->index
           "abcdeffffoo" (string-contains "abcdeffffoo" "ff" 2))))
(test-assert "string-contains"
  (eqv? 4 (string-cursor->index
           "abcdeffffoo" (string-contains "abcdeffffoo" "eff" 2))))
(test-assert "string-contains"
  (eqv? 8 (string-cursor->index
           "abcdeffffoo" (string-contains "abcdeffffoo" "foo" 2))))
(test-assert "string-contains"
  (eqv? #f (string-cursor->index
            "abcdeffffoo" (string-contains "abcdeffffoo" "efffoo" 2))))
(test-assert "string-contains-right"
  (eqv? 0 (string-cursor->index "" (string-contains-right "" "" 0))))
(test-assert "string-contains-right"
  (eqv? 11 (string-cursor->index
            "abcdeffffoo" (string-contains-right "abcdeffffoo" "" 2))))
(test-assert "string-contains-right"
  (eqv? #f (string-cursor->index
            "abcdeffffoo" (string-contains-right "abcdeffffoo" "a" 2))))
(test-assert "string-contains-right"
  (eqv? 7 (string-cursor->index
           "abcdeffffoo" (string-contains-right "abcdeffffoo" "ff" 2))))
(test-assert "string-contains-right"
  (eqv? 4 (string-cursor->index
           "abcdeffffoo" (string-contains-right "abcdeffffoo" "eff" 2))))
(test-assert "string-contains-right"
  (eqv? 8 (string-cursor->index
           "abcdeffffoo" (string-contains-right "abcdeffffoo" "foo" 2))))
(test-assert "string-contains-right"
  (eqv? #f (string-cursor->index
            "abcdeffffoo" (string-contains-right "abcdeffffoo" "efffoo" 2))))
(test-assert "string-contains"
  (eqv? 0 (string-cursor->index "" (string-contains "" "" 0 0))))
(test-assert "string-contains"
  (eqv? 2 (string-cursor->index
           "abcdeffffoo" (string-contains "abcdeffffoo" "" 2 10))))
(test-assert "string-contains"
  (eqv? #f (string-cursor->index
            "abcdeffffoo" (string-contains "abcdeffffoo" "a" 2 10))))
(test-assert "string-contains"
  (eqv? 5 (string-cursor->index
           "abcdeffffoo" (string-contains "abcdeffffoo" "ff" 2 10))))
(test-assert "string-contains"
  (eqv? 4 (string-cursor->index
           "abcdeffffoo" (string-contains "abcdeffffoo" "eff" 2 10))))
(test-assert "string-contains"
  (eqv? #f (string-cursor->index
            "abcdeffffoo" (string-contains "abcdeffffoo" "foo" 2 10))))
(test-assert "string-contains"
  (eqv? #f (string-cursor->index
            "abcdeffffoo" (string-contains "abcdeffffoo" "efffoo" 2 10))))
(test-assert "string-contains-right"
  (eqv? 0 (string-cursor->index "" (string-contains-right "" "" 0 0))))
(test-assert "string-contains-right"
  (eqv? 10 (string-cursor->index
            "abcdeffffoo" (string-contains-right "abcdeffffoo" "" 2 10))))
(test-assert "string-contains-right"
  (eqv? #f (string-cursor->index
            "abcdeffffoo" (string-contains-right "abcdeffffoo" "a" 2 10))))
(test-assert "string-contains-right"
  (eqv? 7 (string-cursor->index
           "abcdeffffoo" (string-contains-right "abcdeffffoo" "ff" 2 10))))
(test-assert "string-contains-right"
  (eqv? 4 (string-cursor->index
           "abcdeffffoo" (string-contains-right "abcdeffffoo" "eff" 2 10))))
(test-assert "string-contains-right"
  (eqv? #f (string-cursor->index
            "abcdeffffoo" (string-contains-right "abcdeffffoo" "foo" 2 10))))
(test-assert "string-contains-right"
  (eqv? #f (string-cursor->index
            "abcdeffffoo"
            (string-contains-right "abcdeffffoo" "efffoo" 2 10))))
(test-assert "string-contains"
  (eqv? 0 (string-cursor->index "" (string-contains "" "" 0 0 0))))
(test-assert "string-contains"
  (eqv? 2 (string-cursor->index
           "abcdeffffoo" (string-contains "abcdeffffoo" "" 2 10 0))))
(test-assert "string-contains"
  (eqv? 2 (string-cursor->index
           "abcdeffffoo" (string-contains "abcdeffffoo" "a" 2 10 1))))
(test-assert "string-contains"
  (eqv? 5 (string-cursor->index
           "abcdeffffoo" (string-contains "abcdeffffoo" "ff" 2 10 1))))
(test-assert "string-contains"
  (eqv? 5 (string-cursor->index
           "abcdeffffoo" (string-contains "abcdeffffoo" "eff" 2 10 1))))
(test-assert "string-contains"
  (eqv? #f (string-cursor->index
            "abcdeffffoo" (string-contains "abcdeffffoo" "foo" 2 10 1))))
(test-assert "string-contains"
  (eqv? #f (string-cursor->index
            "abcdeffffoo" (string-contains "abcdeffffoo" "efffoo" 2 10 1))))
(test-assert "string-contains-right"
  (eqv? 0 (string-cursor->index "" (string-contains-right "" "" 0 0 0))))
(test-assert     "string-contains-right"
  (eqv? 10 (string-cursor->index
            "abcdeffffoo" (string-contains-right "abcdeffffoo" "" 2 10 0))))
(test-assert "string-contains-right"
  (eqv? 10 (string-cursor->index
            "abcdeffffoo" (string-contains-right "abcdeffffoo" "a" 2 10 1))))
(test-assert "string-contains-right"
  (eqv? 8 (string-cursor->index
           "abcdeffffoo" (string-contains-right "abcdeffffoo" "ff" 2 10 1))))
(test-assert "string-contains-right"
  (eqv? 7 (string-cursor->index
           "abcdeffffoo" (string-contains-right "abcdeffffoo" "eff" 2 10 1))))
(test-assert "string-contains-right"
  (eqv? #f (string-cursor->index
            "abcdeffffoo"
            (string-contains-right "abcdeffffoo" "foo" 2 10 1))))
(test-assert "string-contains-right"
  (eqv? #f (string-cursor->index
            "abcdeffffoo"
            (string-contains-right "abcdeffffoo" "efffoo" 2 10 1))))
(test-assert "string-contains"
  (eqv? 0 (string-cursor->index "" (string-contains "" "" 0 0 0 0))))
(test-assert "string-contains"
  (eqv? 2 (string-cursor->index
           "abcdeffffoo" (string-contains "abcdeffffoo" "" 2 10 0 0))))
(test-assert "string-contains"
  (eqv? 2 (string-cursor->index
           "abcdeffffoo" (string-contains "abcdeffffoo" "a" 2 10 1 1))))
(test-assert "string-contains"
  (eqv? 5 (string-cursor->index
           "abcdeffffoo" (string-contains "abcdeffffoo" "ff" 2 10 1 2))))
(test-assert "string-contains"
  (eqv? 5 (string-cursor->index
           "abcdeffffoo" (string-contains "abcdeffffoo" "eff" 2 10 1 2))))
(test-assert "string-contains"
  (eqv? 9 (string-cursor->index
           "abcdeffffoo" (string-contains "abcdeffffoo" "foo" 2 10 1 2))))
(test-assert "string-contains"
  (eqv? 4 (string-cursor->index
           "abcdeffffoo" (string-contains "abcdeffffoo" "efffoo" 2 10 0 2))))
(test-assert "string-contains-right"
  (eqv? 0 (string-cursor->index "" (string-contains-right "" "" 0 0 0 0))))
(test-assert "string-contains-right"
  (eqv? 10 (string-cursor->index
            "abcdeffffoo" (string-contains-right "abcdeffffoo" "" 2 10 0 0))))
(test-assert "string-contains-right"
  (eqv? 10 (string-cursor->index
            "abcdeffffoo"
            (string-contains-right "abcdeffffoo" "a" 2 10 1 1))))
(test-assert "string-contains-right"
  (eqv? 8 (string-cursor->index
           "abcdeffffoo"
           (string-contains-right "abcdeffffoo" "ff" 2 10 1 2))))
(test-assert "string-contains-right"
  (eqv? 8 (string-cursor->index
           "abcdeffffoo"
           (string-contains-right "abcdeffffoo" "eff" 2 10 1 2))))
(test-assert "string-contains-right"
  (eqv? 9 (string-cursor->index
           "abcdeffffoo"
           (string-contains-right "abcdeffffoo" "foo" 2 10 1 2))))
(test-assert "string-contains-right"
  (eqv? 7 (string-cursor->index
           "abcdeffffoo"
           (string-contains-right "abcdeffffoo" "efffoo" 2 10 1 3))))

;;; The whole string

(test-assert "string-reverse" (string=? "" (string-reverse "")))
(test-assert "string-reverse" (string=? "fedcba" (string-reverse "abcdef")))
(test-assert "string-reverse" (string=? "" (string-reverse "" 0)))
(test-assert "string-reverse" (string=? "fedcba" (string-reverse "abcdef" 0)))
(test-assert "string-reverse" (string=? "fedc" (string-reverse "abcdef" 2)))
(test-assert "string-reverse" (string=? "" (string-reverse "" 0 0)))
(test-assert "string-reverse"
  (string=? "fedcba" (string-reverse "abcdef" 0 6)))
(test-assert "string-reverse" (string=? "edc" (string-reverse "abcdef" 2 5)))
(test-assert "string-concatenate" (string=? "" (string-concatenate '())))
(test-assert "string-concatenate"
  (string=? "abcdef" (string-concatenate '("" "a" "bcd" "" "ef" "" ""))))
(test-assert "string-concatenate-reverse"
  (string=? "" (string-concatenate-reverse '())))
(test-assert "string-concatenate-reverse"
  (string=? "efbcda"
            (string-concatenate-reverse '("" "a" "bcd" "" "ef" "" ""))))
(test-assert "string-concatenate-reverse"
  (string=? "huh?" (string-concatenate-reverse '() "huh?")))
(test-assert
    "string-concatenate-reverse"
  (string=? "efbcdaxy"
            (string-concatenate-reverse '("" "a" "bcd" "" "ef" "" "") "xy")))
(test-assert "string-concatenate-reverse"
  (string=? "huh" (string-concatenate-reverse '() "huh?" 3)))
(test-assert
    "string-concatenate-reverse"
  (string=? "efbcdax"
            (string-concatenate-reverse '("" "a" "bcd" "" "ef" "" "") "x" 1)))
(test-assert
    "string-fold"
  (=
   8
   (string-fold (lambda (c count) (if (char-whitespace? c) (+ count 1) count))
                0 " ...a couple of spaces in this one... ")))
(test-assert
    "string-fold"
  (=
   7
   (string-fold (lambda (c count) (if (char-whitespace? c) (+ count 1) count))
                0 " ...a couple of spaces in this one... " 1)))
(test-assert
    "string-fold"
  (=
   6
   (string-fold (lambda (c count) (if (char-whitespace? c) (+ count 1) count))
                0 " ...a couple of spaces in this one... " 1 32)))
(test-assert "string-fold-right"
  (equal? (string->list "abcdef") (string-fold-right cons '() "abcdef")))
(test-assert "string-fold-right"
  (equal? (string->list "def") (string-fold-right cons '() "abcdef" 3)))
(test-assert "string-fold-right"
  (equal? (string->list "cde") (string-fold-right cons '() "abcdef" 2 5)))
(test-assert "string-fold"
  (string=?
   "aabraacaadaabraa"
   (let* ((s "abracadabra")
          (ans-len
           (string-fold (lambda (c sum) (+ sum (if (char=? c #\a) 2 1)))
                        0 s))
          (ans (make-string ans-len)))
     (string-fold
      (lambda (c i)
        (let ((i (if (char=? c #\a)
                     (begin (string-set! ans i #\a) (+ i 1))
                     i)))
          (string-set! ans i c) (+ i 1)))
      0 s)
     ans)))
(test-assert "string-for-each-cursor"
  (equal?
   '(101 100 99 98 97)
   (let ((s "abcde") (v '()))
     (string-for-each-cursor
      (lambda (cur)
        (set! v (cons (char->integer (string-ref/cursor s cur)) v)))
      s)
     v)))
(test-assert "string-replicate"
  (string=? "cdefabcdefabcd" (string-replicate "abcdef" -4 10)))
(test-assert "string-replicate"
  (string=? "bcdefbcdefbcd" (string-replicate "abcdef" 90 103 1)))
(test-assert "string-replicate"
  (string=? "ecdecdecde" (string-replicate "abcdef" -13 -3 2 5)))
(test-assert "string-count" (= 6 (string-count "abcdef" char?)))
(test-assert "string-count"
  (= 4 (string-count "counting  whitespace, again " char-whitespace? 5)))
(test-assert "string-count"
  (= 3 (string-count "abcdefwxyz" (lambda (c) (odd? (char->integer c))) 2 8)))
(test-assert "string-replace"
  (string=? "It's lots of fun to code it up in Scheme."
            (string-replace "It's easy to code it up in Scheme."
                            "lots of fun"
                            5 9)))
(test-assert "string-replace"
  (string=? "The miserable perl programmer endured daily ridicule."
            (string-replace "The TCL programmer endured daily ridicule."
                            "another miserable perl drone" 4 7 8 22)))
(test-assert "string-replace"
  (string=? "It's really easy to code it up in Scheme."
            (string-replace "It's easy to code it up in Scheme."
                            "really "
                            5 5)))
(test-assert "string-split" (equal? '() (string-split "" "")))
(test-assert "string-split" (equal? '("a" "b" "c") (string-split "abc" "")))
(test-assert "string-split"
  (equal? '("too" "" "much" "" "data") (string-split "too  much  data" " ")))
(test-assert
    "string-split"
  (equal? '("" "there" "ya" "go" "")
          (string-split "***there***ya***go***" "***")))
(test-assert "string-split" (equal? '() (string-split "" "" 'infix)))
(test-assert "string-split"
  (equal? '("a" "b" "c") (string-split "abc" "" 'infix)))
(test-assert
    "string-split"
  (equal? '("too" "" "much" "" "data")
          (string-split "too  much  data" " " 'infix)))
(test-assert
    "string-split"
  (equal? '("" "there" "ya" "go" "")
          (string-split "***there***ya***go***" "***" 'infix)))
(test-assert "string-split"
  (equal? 'error
          (guard (exn (else 'error))
            (string-split "" "" 'strict-infix))))
(test-assert "string-split"
  (equal? '("a" "b" "c") (string-split "abc" "" 'strict-infix)))
(test-assert
    "string-split"
  (equal? '("too" "" "much" "" "data")
          (string-split "too  much  data" " " 'strict-infix)))
(test-assert
    "string-split"
  (equal? '("" "there" "ya" "go" "")
          (string-split "***there***ya***go***" "***" 'strict-infix)))
(test-assert "string-split" (equal? '() (string-split "" "" 'prefix)))
(test-assert "string-split"
  (equal? '("a" "b" "c") (string-split "abc" "" 'prefix)))
(test-assert
    "string-split"
  (equal? '("too" "" "much" "" "data")
          (string-split "too  much  data" " " 'prefix)))
(test-assert
    "string-split"
  (equal? '("there" "ya" "go" "")
          (string-split "***there***ya***go***" "***" 'prefix)))
(test-assert "string-split" (equal? '() (string-split "" "" 'suffix)))
(test-assert "string-split"
  (equal? '("a" "b" "c") (string-split "abc" "" 'suffix)))
(test-assert
    "string-split"
  (equal? '("too" "" "much" "" "data")
          (string-split "too  much  data" " " 'suffix)))
(test-assert
    "string-split"
  (equal? '("" "there" "ya" "go")
          (string-split "***there***ya***go***" "***" 'suffix)))
(test-assert "string-split" (equal? '() (string-split "" "" 'infix #f)))
(test-assert "string-split"
  (equal? '("a" "b" "c") (string-split "abc" "" 'infix #f)))
(test-assert
    "string-split"
  (equal? '("too" "" "much" "" "data")
          (string-split "too  much  data" " " 'infix #f)))
(test-assert
    "string-split"
  (equal? '("" "there" "ya" "go" "")
          (string-split "***there***ya***go***" "***" 'infix #f)))
(test-assert
    "string-split"
  (equal? 'error
          (guard (exn (else 'error)) (string-split "" "" 'strict-infix #f))))
(test-assert "string-split"
  (equal? '("a" "b" "c") (string-split "abc" "" 'strict-infix #f)))
(test-assert
    "string-split"
  (equal? '("too" "" "much" "" "data")
          (string-split "too  much  data" " " 'strict-infix #f)))
(test-assert
    "string-split"
  (equal? '("" "there" "ya" "go" "")
          (string-split "***there***ya***go***" "***" 'strict-infix #f)))
(test-assert "string-split" (equal? '() (string-split "" "" 'prefix #f)))
(test-assert "string-split"
  (equal? '("a" "b" "c") (string-split "abc" "" 'prefix #f)))
(test-assert
    "string-split"
  (equal? '("too" "" "much" "" "data")
          (string-split "too  much  data" " " 'prefix #f)))
(test-assert
    "string-split"
  (equal? '("there" "ya" "go" "")
          (string-split "***there***ya***go***" "***" 'prefix #f)))
(test-assert "string-split" (equal? '() (string-split "" "" 'suffix #f)))
(test-assert "string-split"
  (equal? '("a" "b" "c") (string-split "abc" "" 'suffix #f)))
(test-assert
    "string-split"
  (equal? '("too" "" "much" "" "data")
          (string-split "too  much  data" " " 'suffix #f)))
(test-assert
    "string-split"
  (equal? '("" "there" "ya" "go")
          (string-split "***there***ya***go***" "***" 'suffix #f)))
(test-assert
    "string-split"
  (equal? 'error
          (guard (exn (else 'error)) (string-split "" "" 'strict-infix 3))))
(test-assert "string-split"
  (equal? '("a" "b" "c") (string-split "abc" "" 'strict-infix 3)))
(test-assert
    "string-split"
  (equal? '("too" "" "much" " data")
          (string-split "too  much  data" " " 'strict-infix 3)))
(test-assert
    "string-split"
  (equal? '("" "there" "ya" "go***")
          (string-split "***there***ya***go***" "***" 'strict-infix 3)))
(test-assert "string-split" (equal? '() (string-split "" "" 'prefix 3)))
(test-assert "string-split"
  (equal? '("a" "b" "c") (string-split "abc" "" 'prefix 3)))
(test-assert
    "string-split"
  (equal? '("too" "" "much" " data")
          (string-split "too  much  data" " " 'prefix 3)))
(test-assert
    "string-split"
  (equal? '("there" "ya" "go***")
          (string-split "***there***ya***go***" "***" 'prefix 3)))
(test-assert "string-split" (equal? '() (string-split "" "" 'suffix 3)))
(test-assert "string-split"
  (equal? '("a" "b" "c") (string-split "abc" "" 'suffix 3)))
(test-assert
    "string-split"
  (equal? '("too" "" "much" " data")
          (string-split "too  much  data" " " 'suffix 3)))
(test-assert
    "string-split"
  (equal? '("" "there" "ya" "go***")
          (string-split "***there***ya***go***" "***" 'suffix 3)))
(test-assert
    "string-split"
  (equal? 'error
          (guard (exn (else 'error)) (string-split "" "" 'strict-infix 3 0))))
(test-assert "string-split"
  (equal? '("b" "c") (string-split "abc" "" 'strict-infix 3 1)))
(test-assert
    "string-split"
  (equal? '("oo" "" "much" " data")
          (string-split "too  much  data" " " 'strict-infix 3 1)))
(test-assert
    "string-split"
  (equal? '("**there" "ya" "go" "")
          (string-split "***there***ya***go***" "***" 'strict-infix 3 1)))
(test-assert "string-split" (equal? '() (string-split "" "" 'prefix 3 0)))
(test-assert "string-split"
  (equal? '("b" "c") (string-split "abc" "" 'prefix 3 1)))
(test-assert "string-split"
  (equal? '("oo" "" "much" " data")
          (string-split "too  much  data" " " 'prefix 3 1)))
(test-assert "string-split"
  (equal? '("**there" "ya" "go" "")
          (string-split "***there***ya***go***" "***" 'prefix 3 1)))
(test-assert "string-split" (equal? '() (string-split "" "" 'suffix 3 0)))
(test-assert "string-split"
  (equal? '("b" "c") (string-split "abc" "" 'suffix 3 1)))
(test-assert "string-split"
  (equal? '("oo" "" "much" " data")
          (string-split "too  much  data" " " 'suffix 3 1)))
(test-assert "string-split"
  (equal? '("**there" "ya" "go")
          (string-split "***there***ya***go***" "***" 'suffix 3 1)))
(test-assert "string-split"
  (equal? 'error
          (guard (exn (else 'error))
            (string-split "" "" 'strict-infix 3 0 0))))
(test-assert "string-split"
  (equal? '("b") (string-split "abc" "" 'strict-infix 3 1 2)))
(test-assert "string-split"
  (equal? '("oo" "" "much" " ")
          (string-split "too  much  data" " " 'strict-infix 3 1 11)))
(test-assert "string-split" (equal? '() (string-split "" "" 'prefix 3 0 0)))
(test-assert "string-split"
  (equal? '("b") (string-split "abc" "" 'prefix 3 1 2)))
(test-assert "string-split"
  (equal? '("oo" "" "much" " ")
          (string-split "too  much  data" " " 'prefix 3 1 11)))
(test-assert "string-split" (equal? '() (string-split "" "" 'suffix 3 0 0)))
(test-assert "string-split"
  (equal? '("b") (string-split "abc" "" 'suffix 3 1 2)))
(test-assert "string-split"
  (equal? '("oo" "" "much" " ")
          (string-split "too  much  data" " " 'suffix 3 1 11)))
(test-assert "string-filter"
  (string=? "aiueaaaoi"
            (string-filter (lambda (c) (memv c (string->list "aeiou")))
                           "What is number, that man may know it?")))
(test-assert "string-remove"
  (string=? "And wmn, tht sh my knw nmbr?"
            (string-remove (lambda (c) (memv c (string->list "aeiou")))
                           "And woman, that she may know number?")))
(test-assert "string-filter"
  (string=? "iueaaaoi"
            (string-filter (lambda (c) (memv c (string->list "aeiou")))
                           "What is number, that man may know it?" 4)))
(test-assert "string-remove"
  (string=? "mn, tht sh my knw nmbr?"
            (string-remove (lambda (c) (memv c (string->list "aeiou")))
                           "And woman, that she may know number?" 6)))
(test-assert "string-filter"
  (string=? "aaao"
            (string-filter (lambda (c) (memv c (string->list "aeiou")))
                           "What is number, that man may know it?" 16 32)))
(test-assert "string-remove"
  (string=? "And woman, that sh may know"
            (string-remove (lambda (c) (memv c (string->list "eiu")))
                           "And woman, that she may know number?" 0 28)))

(test-end "sample-implementation")

(test-equal "string-null?" #f (string-null? "abc"))
(test-equal "string-null?" #t (string-null? ""))
(test-equal "string-every" #t (string-every #\a ""))
(test-equal "string-every" #t (string-every #\a "aaaa"))
(test-equal "string-every" #f (string-every #\a "aaba"))
(test-equal "string-every" #t (string-every char-set:lower-case "aaba"))
(test-equal "string-every" #f (string-every char-set:lower-case "aAba"))
(test-equal "string-every" #t (string-every char-set:lower-case ""))
(test-equal "string-every"
  #t (string-every (lambda (x) (char->integer x)) "aAbA"))
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
(test-equal "string-take-right"
  "Beta rules" (string-take-right "Beta rules" 10))
(test-equal "string-drop-right" "Beta " (string-drop-right "Beta rules" 5))
(test-equal "string-drop-right"
  "Beta rules" (string-drop-right "Beta rules" 0))
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
(test-equal "string-trim-right"
  "  \t  a b c d  "
  (string-trim-right "  \t  a b c d  \n" (lambda (ch) (eqv? ch #\newline))))
(test-equal "string-trim-right"  "349853a b c d"
            (string-trim-right "349853a b c d03490" char-numeric?))

(test-equal "string-trim-both"  "a b c d"
            (string-trim-both "  \t  a b c d  \n"))
(test-equal "string-trim-both"
  "  \t  a b c d  "
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

(test-equal "string-index #1"
  4 (string-index->index "abcd:efgh:ijkl" (lambda (ch) (eqv? ch #\:))))
(test-equal "string-index #2"
  4 (string-index->index
     "abcd:efgh;ijkl"
     (lambda (ch) (char-set-contains? char-set:not-letter ch))))
(test-equal "string-index #3"
  14 (string-index->index
      "abcd:efgh;ijkl"
      (lambda (ch) (char-set-contains? char-set:digit ch))))
(test-equal "string-index #4"
  9 (string-index->index
     "abcd:efgh:ijkl" (lambda (ch) (eqv? ch #\:)) 5))
(test-equal "string-index-right #1"
  5 (string-index-right->index "abcd:efgh;ijkl" (lambda (ch) (eqv? ch #\:))))
(test-equal "string-index-right #2"
  10 (string-index-right->index
      "abcd:efgh;ijkl"
      (lambda (ch) (char-set-contains? char-set:not-letter ch))))
(test-equal "string-index-right #3" 14
            (string-index-right->index "abcd:efgh;ijkl" char-alphabetic?))
(test-equal "string-index-right #4"
  10 (string-index-right->index
      "abcd:efgh;ijkl"
      (lambda (ch) (char-set-contains? char-set:not-letter ch)) 7))

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
  (test-equal 15
    (string-cursor->index s (string-contains-right s "ee" 12 18)))
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
(test-equal "string-concatenate"
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
  (string-concatenate
   '("A" "B" "C" "D" "E" "F" "G" "H"
     "I" "J" "K" "L" "M" "N" "O" "P"
     "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"
     "a" "b" "c" "d" "e" "f" "g" "h"
     "i" "j" "k" "l" "m" "n" "o" "p"
     "q" "r" "s" "t" "u" "v" "w" "x" "y" "z")))
(test-equal "string-concatenate-reverse"
  "zyxwvutsrqponmlkjihgfedcbaZYXWVUTSRQPONMLKJIHGFEDCBA"
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

(test-equal "string-for-each-cursor"
  '(4 3 2 1 0)
  (let ((r '()))
    (string-for-each-cursor (lambda (i) (set! r (cons i r))) "hello")
    (map (lambda (sc) (string-cursor->index "hello" sc)) r)))
(test-equal "string-for-each-cursor"
  '(4 3 2 1)
  (let ((r '()))
    (string-for-each-cursor (lambda (i) (set! r (cons i r))) "hello" 1)
    (map (lambda (sc) (string-cursor->index "hello" sc)) r)))
(test-equal "string-for-each-cursor"
  '(2 1)
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
            (string-filter char-alphabetic?
                           "Help make programs run, run, RUN!"))

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
            (string-remove char-alphabetic?
                           "Help make programs run, run, RUN!"))
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

(test-equal "string-contains"
  1 (string-cursor->index "aabc" (string-contains "aabc" "ab")))

(test-equal "string-contains"
  5 (string-cursor->index "ababdabdabxxas"
                          (string-contains "ababdabdabxxas" "abdabx")))

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
