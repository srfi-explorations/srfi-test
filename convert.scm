;; Copyright 2019 Lassi Kortela
;; SPDX-License-Identifier: MIT

;;; Convert the generic tests to work with the quirks of particular
;;; Scheme implementations.

(cond-expand
  (chicken)
  (gambit (import (gambit)))
  (r7rs (import (scheme base) (scheme file) (scheme write))))

(cond-expand
  (chicken
   (import (rename (only (chicken file) create-directory)
                   (create-directory ensure-directory-exists))))
  (gambit
   (define (ensure-directory-exists path)
     (or (file-exists? path) (create-directory path)))))

(cond-expand
  (gambit
   (define (pretty-print x)
     (pp x (current-output-port))))
  (else
   (define (pretty-print x)
     (write x)
     (newline))))

;;

(define (read-source-file filename)
  (with-input-from-file filename
    (lambda ()
      (let loop ((forms '()))
        (let ((form (read)))
          (if (eof-object? form) (reverse forms)
              (loop (cons form forms))))))))

(define (write-source-file dirname basename top-level-forms)
  (ensure-directory-exists dirname)
  (with-output-to-file (string-append dirname "/" basename)
    (lambda ()
      (let loop ((first? #t) (forms top-level-forms))
        (unless (null? forms)
          (unless first? (newline))
          (pretty-print (car forms))
          (newline)
          (loop #f (cdr forms)))))))

;;

(define prelude
  '(

    ;; Insertion sort with SRFI 132 procedure name and args.
    (define (list-stable-sort less? xs)
      (if (null? xs) '()
          (let insert ((x (car xs)) (xs (list-stable-sort less? (cdr xs))))
            (if (null? xs) (list x)
                (let ((y (car xs)) (ys (cdr xs)))
                  (if (less? x y) (cons x xs) (cons y (insert x ys))))))))

    (define (symbol<? a b) (string<? (symbol->string a) (symbol->string b)))

    ))

;;

(define (write-gauche-test srfi-number)
  (let ((basename (string-append (number->string srfi-number) ".scm")))
    (write-source-file "gauche" basename
                       `((import (scheme base)
                                 (scheme char)
                                 (scheme write)
                                 (srfi 64)
                                 (srfi ,srfi-number))
                         ,@prelude
                         ,@(read-source-file basename)))))

;;

(write-gauche-test 69)
(write-gauche-test 175)
