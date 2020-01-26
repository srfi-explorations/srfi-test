(define-library (framework)
  (export

   ;; Our own test framework - macros:

   check-predicate
   check-equal?
   check-error
   test-lset-eq?
   test-lset-equal?
   test
   test-not
   test-assert
   test-error
   want

   ;; Our own test framework - procedures:

   lset=

   ;; Re-exported from standard Scheme:

   *
   +
   -
   /
   <
   <=
   =
   >
   >=
   and
   begin
   car
   cdr
   cond
   cons
   define
   define-syntax
   do
   else
   eq?
   equal?
   eqv?
   error
   guard
   if
   integer->char
   lambda
   let
   let*
   let-values
   list?
   make-string
   member
   memq
   memv
   not
   null?
   or
   pair?
   quote
   set!
   string->utf8
   string-ci=?
   string-ref
   syntax-rules
   unless
   when

   )
  (import (scheme base) (scheme char))
  (cond-expand (gambit (import (gambit)))
               (else))
  (include "framework-impl.scm"))
