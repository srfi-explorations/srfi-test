;; Copyright 2020 Lassi Kortela
;; SPDX-License-Identifier: MIT

;;; Convert the generic tests to work with the quirks of particular
;;; Scheme implementations.

(import (scheme base)
        (scheme file)
        (scheme read)
        (scheme write))

(cond-expand
  (gambit (import (gambit)))
  (else))

;; Chibi currently cannot refer to an identifier in the same
;; cond-expand where it was imported.
(cond-expand (chibi (import (only (chibi filesystem) create-directory*)))
             (else))

(cond-expand
  (chibi
   (define ensure-directory-exists create-directory*))
  (chicken
   (import (rename (only (chicken file) create-directory)
                   (create-directory ensure-directory-exists))))
  (gambit
   (define (ensure-directory-exists path)
     (or (file-exists? path) (create-directory path))))
  (gauche
   (import (rename (only (file util) create-directory*)
                   (create-directory* ensure-directory-exists))))
  (guile
   (define (ensure-directory-exists path)
     (or (file-exists? path) (mkdir path))))
  (kawa
   (import (rename (only (kawa base) create-directory)
                   (create-directory ensure-directory-exists)))))

(cond-expand
  (chicken
   (import (only (chicken pretty-print) pretty-print)))
  (gambit)  ;; A suitable pretty-print comes standard.
  (gauche
   (import (only (gauche base) pprint))
   (define (pretty-print x)
     (pprint x)))
  (guile
   (use-modules (ice-9 pretty-print)))
  (kawa
   (import (only (kawa pprint) pprint))
   (define (pretty-print x)
     (pprint x)
     (newline)))
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
  (when (file-exists? (string-append dirname "/" basename))
    (delete-file (string-append dirname "/" basename)))
  (ensure-directory-exists dirname)
  (with-output-to-file
    (string-append dirname "/" basename)
    (lambda ()
      (let loop ((first? #t) (forms top-level-forms))
        (unless (null? forms)
          (unless first? (newline))
          (pretty-print (car forms))
          (loop #f (cdr forms)))))))
;;

(define (srfi-dependencies srfi-number)
  (case srfi-number
    ((13 130) '(14))
    (else '())))

;;

(define r6rs-prelude
  '((define (symbol<? a b) (string<? (symbol->string a) (symbol->string b)))
    (define (list-sort less? xs)
      (if (null? xs) '()
        (let insert ((x (car xs)) (xs (list-sort less? (cdr xs))))
          (if (null? xs) (list x)
            (let ((y (car xs)) (ys (cdr xs)))
              (if (less? x y) (cons x xs) (cons y (insert x ys))))))))))

(define r7rs-prelude
  '(

    ;; Sort with SRFI 132 procedure name and args.
    (define (list-sort less? xs)
      (if (null? xs) '()
          (let insert ((x (car xs)) (xs (list-sort less? (cdr xs))))
            (if (null? xs) (list x)
                (let ((y (car xs)) (ys (cdr xs)))
                  (if (less? x y) (cons x xs) (cons y (insert x ys))))))))

    (define (written x)
      (cond-expand
        (r7rs
         (call-with-port (open-output-string)
                         (lambda (out) (write x out)
                                 (get-output-string out))))
        (else
         (call-with-output-string (lambda (out) (write x out))))))

    (define (symbol<? a b) (string<? (symbol->string a) (symbol->string b)))

    (define (call-with-false-on-error proc)
                           (guard (_ (else #f)) (proc)))

    ))

(define (r6rs-imports srfi-number)
  (cond
    ((= srfi-number 4)
     '((rnrs base)
       (rnrs programs)
       (rnrs control)
       (only (rnrs r5rs)
             remainder
             quotient)
       (only (rnrs bytevectors)
             make-bytevector
             bytevector-copy
             bytevector?
             bytevector-length
             bytevector-u8-ref
             bytevector-u8-set!
             string->utf8)
       (only (rnrs mutable-strings)
             string-set!)
       (only (rnrs io simple)
             write
             newline)
       (only (rnrs lists)
             memq)
       (srfi :0)))
    ((= srfi-number 11)
     '((except (rnrs base)
               let-values
               let*-values)
       (rnrs programs)
       (rnrs control)))
    (else '((rnrs base)
            (rnrs programs)
            (rnrs control)))))

(define (r7rs-imports srfi-number)
  (cond
    ((= srfi-number 4)
     '((scheme base)
       (scheme char)
       (scheme write)
       (scheme file)
       (scheme inexact)
       (scheme process-context)))
    ((= srfi-number 5)
     '((except (scheme base)
               let)
       (scheme write)
       (scheme process-context)))
    ((= srfi-number 11)
     '((except (scheme base)
               let-values
               let*-values)
       (scheme write)
       (scheme process-context)))
    ((= srfi-number 13)
     '((except (scheme base)
               string-copy
               string-map
               string-for-each
               string-fill!
               string-copy!
               string->list)
       (except (scheme char)
               char-ci=?
               char-ci<?
               char-downcase
               char-upcase
               char-alphabetic?
               string-downcase
               string-upcase)
       (scheme inexact)
       (except (scheme char) string-upcase string-downcase)
       (scheme write)
       (scheme process-context)
       (scheme file)
       (scheme cxr)))
    ((= srfi-number 39)
     '((except (scheme base) make-parameter parameterize)
       (scheme char)
       (scheme inexact)
       (scheme write)
       (scheme process-context)
       (scheme file)
       (scheme cxr)))
    ((= srfi-number 44)
     '((except (scheme base)
               vector? make-vector vector map vector-copy vector-ref
               vector-set! vector->list
               list? make-list list list-ref list-set! list-copy
               string? make-string string string-copy string-ref string->list
               string-set!)
       (prefix (only (scheme base) list-ref) r7rs:)
       (srfi 8)
       (srfi 44)
       (srfi 64)))
    ((= srfi-number 44)
     '((except (scheme base)
               vector? make-vector vector map vector-copy vector-ref
               vector-set! vector->list
               list? make-list list list-ref list-set! list-copy
               string? make-string string string-copy string-ref string->list
               string-set!)
       (prefix (only (scheme base) list-ref) r7rs:)
       (srfi 8)
       (srfi 44)
       (srfi 64)))
    ((= srfi-number 87)
     '((except (scheme base) case)
       (scheme char)
       (scheme inexact)
       (scheme read)
       (scheme write)
       (scheme process-context)
       (scheme file)
       (scheme cxr)))
    (else '((scheme base)
            (scheme char)
            (scheme inexact)
            (scheme read)
            (scheme write)
            (scheme process-context)
            (scheme file)
            (scheme cxr)))))

(define (srfi-import-numbers srfi-number . extra-srfi-numbers)
  (append (list srfi-number)
          (srfi-dependencies srfi-number)
          extra-srfi-numbers))

(define (r6rs-srfi-imports srfi-number . extra-srfi-numbers)
  (map (lambda (n) `(srfi
                      ,(string->symbol
                         (string-append ":"
                                        (number->string n)))))
       (apply srfi-import-numbers srfi-number extra-srfi-numbers)))

(define (r7rs-srfi-imports srfi-number . extra-srfi-numbers)
  (map (lambda (n) `(srfi ,n))
       (apply srfi-import-numbers srfi-number extra-srfi-numbers)))

;;

(define (write-r6rs-test-library srfi-number)
  (let ((scm-basename (string-append (number->string srfi-number) ".sps"))
        (sld-basename (string-append (number->string srfi-number) ".sls")))
    (write-source-file "r6rs-libraries/srfi-test" sld-basename
                       `((define-library (srfi-test ,srfi-number)
                           (export)
                           (import ,@(r6rs-imports srfi-number)
                                   ,@(r6rs-srfi-imports srfi-number 64))
                           (begin ,@r6rs-prelude)
                           (include ,(string-append "../../" scm-basename)))))))

(define (write-r6rs-test-program srfi-number)
  (let ((input-file (string-append (number->string srfi-number) ".scm"))
        (output-file (string-append (number->string srfi-number) ".sps")))
    (write-source-file "r6rs-programs" output-file
                       ;; Do not double import SRFI-64, Foment throws error
                       `((import ,@(r6rs-imports srfi-number)
                                 ,@(if (= srfi-number 64)
                                    (r6rs-srfi-imports srfi-number)
                                    (r6rs-srfi-imports srfi-number 64)))
                         ,@r6rs-prelude
                         ,@(read-source-file input-file)
                         (exit 0)))))

(define (write-r7rs-test-library srfi-number)
  (let ((scm-basename (string-append (number->string srfi-number) ".scm"))
        (sld-basename (string-append (number->string srfi-number) ".sld")))
    (write-source-file "r7rs-libraries/srfi-test" sld-basename
                       `((define-library (srfi-test ,srfi-number)
                           (export)
                           (import ,@(r7rs-imports srfi-number)
                                   ,@(r7rs-srfi-imports srfi-number 64))
                           (begin ,@r7rs-prelude)
                           (include ,(string-append "../../" scm-basename)))))))

(define (write-r7rs-test-program srfi-number)
  (let ((basename (string-append (number->string srfi-number) ".scm")))
    (write-source-file "r7rs-programs" basename
                       ;; Do not double import SRFI-64, Foment throws error
                       `((import ,@(r7rs-imports srfi-number)
                                 ,@(if (= srfi-number 64)
                                    (r7rs-srfi-imports srfi-number)
                                    (r7rs-srfi-imports srfi-number 64)))
                         ,@r7rs-prelude
                         ,@(read-source-file basename)
                         (exit 0)))))

(define (write-chibi-test srfi-number)
  (let ((basename (string-append (number->string srfi-number) ".scm")))
    (write-source-file "chibi" basename
                       `((import ,@(r7rs-imports srfi-number)
                                 (chibi)
                                 ;; snow-chibi install '(srfi 64)'
                                 ,@(r7rs-srfi-imports srfi-number 64))
                         (define (arity-error? e)
                           (and (error-object? e)
                                (let ((m (error-object-message e)))
                                  (or (string=? m "not enough args")
                                      (string=? m "too many args")))))
                         (define-syntax test-arity-error
                           (syntax-rules ()
                             ((_ test-expr)
                              (test-assert (guard (_ (arity-error? #t)
                                                     (else #f))
                                             test-expr
                                             #f)))))
                         (define (call-with-false-on-error proc)
                           (guard (_ (else #f)) (proc)))
                         ,@r7rs-prelude
                         ,@(read-source-file basename)))))

(define (write-chicken-test srfi-number)
  (let ((basename (string-append (number->string srfi-number) ".scm")))
    (write-source-file "chicken" basename
                       `((import (chicken base)
                                 (chicken port)
                                 ,@(r7rs-srfi-imports srfi-number 64))
                         (import (rename
                                  (only (chicken random)
                                        pseudo-random-integer)
                                  (pseudo-random-integer random-integer)))
                         (define (call-with-false-on-error proc)
                           (call-with-current-continuation
                            (lambda (return)
                              (handle-exceptions _ (return #f) (proc)))))
                         ,@r7rs-prelude
                         ,@(read-source-file basename)))))

(define (write-gauche-test srfi-number)
  (let ((basename (string-append (number->string srfi-number) ".scm")))
    (write-source-file "gauche" basename
                       `((import ,@(r7rs-imports srfi-number)
                                 ,@(r7rs-srfi-imports srfi-number 64))
                         (define (call-with-false-on-error proc)
                           (guard (_ (else #f)) (proc)))
                         ,@r7rs-prelude
                         ,@(read-source-file basename)))))

(define (write-guile-test srfi-number)
  (let ((basename (string-append (number->string srfi-number) ".scm")))
    (write-source-file "guile" basename
                       `((import
                          (guile)
                          ,@(map (lambda (n)
                                   `(srfi ,(string->symbol
                                            (string-append
                                             ":" (number->string n)))))
                                 (srfi-import-numbers srfi-number 64)))
                         (define (call-with-false-on-error proc)
                           (catch #t proc (lambda (return) (return #f))))
                         ,@r7rs-prelude
                         ,@(read-source-file basename)))))

(define (write-kawa-test srfi-number)
  (let ((basename (string-append (number->string srfi-number) ".scm")))
    (write-source-file "kawa" basename
                       `((import
                          (kawa base) ; base includes SRFI 64
                          ,@(r7rs-srfi-imports srfi-number 64))
                         (define (random-integer limit)
                           (let ((source (java.util.Random)))
                             (source:nextInt limit)))
                         (define (call-with-false-on-error proc)
                           (guard (_ (else #f)) (proc)))
                         ,@r7rs-prelude
                         ,@(read-source-file basename)))))

;;

(define all-srfis
  '(1 2 4 5 8 11 13 14 16 19 25 26 27 28 29 31 37 38 39 41 42 43 44 48 51 54 64
    60 63 66 69 87 95 115 129 130 132 133 145 151 160 175 180))

(for-each write-r6rs-test-library all-srfis)
(for-each write-r6rs-test-program all-srfis)
(for-each write-r7rs-test-library all-srfis)
(for-each write-r7rs-test-program all-srfis)
(for-each write-chibi-test all-srfis)
(for-each write-chicken-test all-srfis)
(for-each write-gauche-test all-srfis)
(for-each write-guile-test all-srfis)
(for-each write-kawa-test all-srfis)
