;; Copyright 2019 Lassi Kortela
;; SPDX-License-Identifier: MIT

(test-begin "srfi-175")

(test-eqv #f (ascii-codepoint? -1))
(test-eqv #t (ascii-codepoint? 0))
(test-eqv #t (ascii-codepoint? #x7f))
(test-eqv #f (ascii-codepoint? #x80))

(test-eqv #t (ascii-char? (integer->char 0)))
(test-eqv #t (ascii-char? (integer->char #x7f)))
(test-eqv #f (ascii-char? (integer->char #x80)))

(test-eqv #t (ascii-string? ""))
(test-eqv #t (ascii-string? "a"))
(test-eqv #t (ascii-string? "a b c"))
(test-eqv #f (ascii-string? "å b o"))
(test-eqv #t (ascii-string? (make-string 1 (integer->char #x7f))))
(test-eqv #f (ascii-string? (make-string 1 (integer->char #x80))))

(test-eqv #t (ascii-bytevector? (string->utf8 "")))
(test-eqv #t (ascii-bytevector? (string->utf8 "a")))
(test-eqv #t (ascii-bytevector? (string->utf8 "a b c")))
(test-eqv #f (ascii-bytevector? (string->utf8 "å b o")))
(test-eqv #t (ascii-bytevector?
              (string->utf8 (make-string 1 (integer->char #x7f)))))
(test-eqv #f (ascii-bytevector?
              (string->utf8 (make-string 1 (integer->char #x80)))))

(test-eqv #t (ascii-non-control? #\space))
(test-eqv #f (ascii-non-control? #\tab))
(test-eqv #f (ascii-non-control? #\newline))
(test-eqv #f (ascii-non-control? (integer->char #x0d)))

(test-eqv #t (ascii-space-or-tab? #\space))
(test-eqv #t (ascii-space-or-tab? #\tab))
(test-eqv #f (ascii-space-or-tab? #\newline))
(test-eqv #f (ascii-non-control? (integer->char #x0d)))

(let ((lowers "abcdefghijklmnopqrstuvwxyz")
      (uppers "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
  (let loop ((i 0))
    (when (< i 26)
      (let ((lower (string-ref lowers i))
            (upper (string-ref uppers i)))
        (test-eqv upper (ascii-upcase upper))
        (test-eqv upper (ascii-upcase lower))
        (test-eqv lower (ascii-downcase upper))
        (test-eqv lower (ascii-downcase lower))
        (loop (+ i 1))))))

(let loop ((cc 0))
  (when (< cc #x80)
    (unless (ascii-alphabetic? cc)
      (test-eqv cc (ascii-upcase cc))
      (test-eqv cc (ascii-downcase cc)))
    (loop (+ cc 1))))

(let loop ((cc 0))
  (when (< cc #x80)
    (test-eqv #f (ascii-char? cc))
    (test-eqv #t (ascii-char? (integer->char cc)))
    (cond ((ascii-alphabetic? cc)
           (test-eqv #t (ascii-upper-case? (ascii-upcase cc)))
           (test-eqv #t (ascii-lower-case? (ascii-downcase cc)))
           (test-eqv #f (ascii-lower-case? (ascii-upcase cc)))
           (test-eqv #f (ascii-upper-case? (ascii-downcase cc)))
           (test-eqv #t (ascii-alphanumeric? cc))
           (test-eqv #t (ascii-non-control? cc))
           (test-eqv #f (ascii-other-graphic? cc))
           (test-eqv #f (ascii-control? cc))
           (test-eqv #f (ascii-numeric? cc 10))
           (test-eqv #f (ascii-whitespace? cc))
           (test-eqv #f (ascii-space-or-tab? cc)))
          ((ascii-control? cc)
           (test-eqv #f (ascii-non-control? cc))
           (test-eqv #f (ascii-other-graphic? cc))
           (test-eqv cc
             (ascii-graphic->control
              (ascii-control->graphic cc)))
           (test-eqv (integer->char cc)
             (ascii-graphic->control
              (ascii-control->graphic (integer->char cc)))))
          ((member cc '(#\( #\) #\[ #\] #\{ #\} #\< #\>))
           (test-eqv cc (ascii-mirror-bracket (ascii-mirror-bracket cc)))))
    (loop (+ cc 1))))

(let outer ((a 0))
  (when (< a 26)
    (let inner ((b 0))
      (if (= b 26)
          (outer (+ a 1))
          (begin (test-eqv (= a b)  (ascii-ci=?
                                     (ascii-nth-lower-case a)
                                     (ascii-nth-upper-case b)))
                 (test-eqv (< a b)  (ascii-ci<?
                                     (ascii-nth-lower-case a)
                                     (ascii-nth-upper-case b)))
                 (test-eqv (<= a b) (ascii-ci<=?
                                     (ascii-nth-lower-case a)
                                     (ascii-nth-upper-case b)))
                 (test-eqv (> a b)  (ascii-ci>?
                                     (ascii-nth-lower-case a)
                                     (ascii-nth-upper-case b)))
                 (test-eqv (>= a b) (ascii-ci>=?
                                     (ascii-nth-lower-case a)
                                     (ascii-nth-upper-case b)))
                 (inner (+ b 1)))))))

(ascii-ci>? #\A #\_)
(ascii-ci>? #\Z #\_)

(test-eqv #f (ascii-char? -1))
(test-eqv #f (ascii-char? #x80))
(test-eqv #f (ascii-char? (integer->char #x80)))

(test-eqv #f (ascii-control? -1))
(test-eqv #t (ascii-control? #x00))
(test-eqv #t (ascii-control? #x1f))
(test-eqv #f (ascii-control? #x20))
(test-eqv #f (ascii-control? #x7e))
(test-eqv #t (ascii-control? #x7f))
(test-eqv #f (ascii-control? #x80))

(test-eqv 0 (ascii-digit-value #\0 10))
(test-eqv 0 (ascii-digit-value #\0 1))
(test-eqv #f (ascii-digit-value #\0 0))
(test-eqv #f (ascii-digit-value #\0 -1))
(test-eqv 7 (ascii-digit-value #\7 8))
(test-eqv #f (ascii-digit-value #\7 7))
(test-eqv #f (ascii-digit-value #\: 10))

(test-eqv 0 (ascii-upper-case-value #\A 0 26))
(test-eqv 25 (ascii-upper-case-value #\Z 0 26))
(test-eqv #f (ascii-upper-case-value #\Z 0 25))

(test-eqv 0 (ascii-lower-case-value #\a 0 26))
(test-eqv 25 (ascii-lower-case-value #\z 0 26))
(test-eqv #f (ascii-lower-case-value #\z 0 25))

(test-eqv 0 (ascii-lower-case-value #\a 0 1))
(test-eqv #f (ascii-lower-case-value #\a 0 0))
(test-eqv #f (ascii-lower-case-value #\a 0 -1))
(test-eqv 9001 (ascii-lower-case-value #\b 9000 2))

(test-eqv #f (ascii-nth-digit -1))
(test-eqv #\0 (ascii-nth-digit 0))
(test-eqv #\9 (ascii-nth-digit 9))
(test-eqv #f (ascii-nth-digit 10))

(test-eqv #\Z (ascii-nth-upper-case -1))
(test-eqv #\A (ascii-nth-upper-case 0))
(test-eqv #\Z (ascii-nth-upper-case 25))
(test-eqv #\A (ascii-nth-upper-case 26))

(test-eqv #\z (ascii-nth-lower-case -1))
(test-eqv #\a (ascii-nth-lower-case 0))
(test-eqv #\z (ascii-nth-lower-case 25))
(test-eqv #\a (ascii-nth-lower-case 26))

(define (count-matching predicates value)
  (let loop ((ps predicates) (n 0))
    (if (null? ps) n (loop (cdr ps) (if ((car ps) value) (+ n 1) n)))))

(define (union? whole . parts)
  (let check ((cc 0))
    (or (= cc #x80)
        (if (and (whole cc) (not (= 1 (count-matching parts cc))))
            #f (check (+ cc 1))))))

(define (subset? small-set . bigger-sets)
  (let check ((cc 0))
    (or (= cc #x80)
        (if (and (small-set cc) (= 0 (count-matching bigger-sets cc)))
            #f (check (+ cc 1))))))

(define (disjoint? . predicates)
  (let check ((cc 0))
    (or (= cc #x80) (and (<= (count-matching predicates cc) 1)
                         (check (+ cc 1))))))

(define (decimal-numeric? x) (ascii-numeric? x 10))

(test-eqv #t (union? ascii-alphanumeric? ascii-alphabetic? decimal-numeric?))
(test-eqv #t (union? ascii-alphabetic? ascii-upper-case? ascii-lower-case?))

(test-eqv #t (subset? ascii-space-or-tab? ascii-whitespace?))
(test-eqv #t (subset? ascii-other-graphic? ascii-non-control?))
(test-eqv #t (subset? ascii-upper-case?
                      ascii-alphabetic? ascii-non-control?))
(test-eqv #t (subset? ascii-lower-case?
                      ascii-alphabetic? ascii-non-control?))
(test-eqv #t (subset? ascii-alphabetic?
                      ascii-alphanumeric? ascii-non-control?))
(test-eqv #t (subset? decimal-numeric?
                      ascii-alphanumeric? ascii-non-control?))
(test-eqv #t (subset? ascii-alphanumeric? ascii-non-control?))

(test-eqv #t (disjoint? ascii-control? ascii-non-control?))
(test-eqv #t (disjoint? ascii-whitespace?
                        ascii-other-graphic?
                        ascii-upper-case?
                        ascii-lower-case?
                        decimal-numeric?))
(test-eqv #t (disjoint? ascii-control?
                        ascii-other-graphic?
                        ascii-upper-case?
                        ascii-lower-case?
                        decimal-numeric?))

(define (check-string-ci a b cmp)
  (test-eqv (= cmp 0) (ascii-string-ci=? a b))
  (test-eqv (< cmp 0) (ascii-string-ci<? a b))
  (test-eqv (> cmp 0) (ascii-string-ci>? a b))
  (test-eqv (<= cmp 0) (ascii-string-ci<=? a b))
  (test-eqv (>= cmp 0) (ascii-string-ci>=? a b)))

(check-string-ci "" "" 0)
(check-string-ci "a" "a" 0)
(check-string-ci "A" "a" 0)
(check-string-ci "a" "A" 0)

(check-string-ci "a" "b" -1)
(check-string-ci "b" "a" 1)

(check-string-ci "a" "B" -1)
(check-string-ci "B" "a" 1)

(check-string-ci "aa" "aa" 0)
(check-string-ci "aa" "ab" -1)
(check-string-ci "ab" "aa" 1)
(check-string-ci "aa" "aaa" -1)
(check-string-ci "aaa" "aa" 1)

(test-end "srfi-175")
