;; Copyright 2015 John Cowan
;; SPDX-License-Identifier: MIT

;;;; Chicken-specific tests for titlecase library

;;; Note:  The strings embed Unicode characters using the Chicken-specific lexical
;;; syntax "\u1234" rather than the R7RS syntax "\x1234;"

;; (use utf8)  ; Chicken

(test-begin "titlecase")

(test-begin "predicate")
    (test-assert (char-title-case? #\u01C5))
    (test-assert (char-title-case? #\u1FFC))
    (test-assert (not (char-title-case? #\Z)))
    (test-assert (not (char-title-case? #\z)))
(test-end "predicate")

(test-begin "char")
    (test-equal #\u01C5 (char-titlecase #\u01C4))
    (test-equal #\u01C5 (char-titlecase #\u01C6))
    (test-equal #\Z (char-titlecase #\Z))
    (test-equal #\Z (char-titlecase #\z))
(test-end "char")

(test-begin "string")
    (test-equal "\u01C5" (string-titlecase "\u01C5"))
    (test-equal "\u01C5" (string-titlecase "\u01C4"))
    (test-equal "Ss" (string-titlecase "\u00DF"))
    (test-equal "Xi\u0307" (string-titlecase "x\u0130"))
    (test-equal "\u1F88" (string-titlecase "\u1F80"))
    (test-equal "\u1F88" (string-titlecase "\u1F88"))
    (define Floo "\uFB02oo")
    (define Floo-bar "\uFB02oo bar")
    (define Baffle "Ba\uFB04e")
    (define LJUBLJANA "\u01C7ub\u01C7ana")
    (define Ljubljana "\u01C8ub\u01C9ana")
    (define ljubljana "\u01C9ub\u01C9ana")
    (test-equal "Bar Baz" (string-titlecase "bAr baZ"))
    (test-equal "Floo" (string-titlecase "floo"))
    (test-equal "Floo" (string-titlecase "FLOO"))
    (test-equal "Floo" (string-titlecase Floo))
    (test-equal "Floo Bar" (string-titlecase "floo bar"))
    (test-equal "Floo Bar" (string-titlecase "FLOO BAR"))
    (test-equal "Floo Bar" (string-titlecase Floo-bar))
    (test-equal Baffle (string-titlecase Baffle))
    (test-equal Ljubljana (string-titlecase LJUBLJANA))
    (test-equal Ljubljana (string-titlecase Ljubljana))
    (test-equal Ljubljana (string-titlecase ljubljana))
(test-end "string")

(test-end "titlecase")
