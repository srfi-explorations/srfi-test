(test-begin "srfi-51")

(define caller values)
(define rest-list '(x 1))

(receive (a b)
         (rest-values rest-list)
         (test-equal a 'x)
         (test-equal b 1))
#|
(test-equal '((values) x 1) (rest-values rest-list 2))
(test-equal '((values) x 1) (rest-values caller rest-list))
(rest-values caller rest-list -3)        => x 1
(rest-values rest-list -2 'y 3 1)
 => error too many defaults (y 3 1) default-list (<= (length default-list) 2)
(rest-values 'caller rest-list 1 '(x y z))
 => error too many arguments (x 1) rest-list (<= (length rest-list) 1) caller
(rest-values caller rest-list 2 (list 'x 'y 'z) (cons "str" string?))
 => error incorrect argument 1 arg (<procedure string?> arg) <procedure caller>
(rest-values rest-list 2 '(y z) `(100 . ,number?))
 => error unmatched argument x arg (member arg (y z))
(rest-values "caller: bad argument" rest-list 2 '(y z) `(100 . ,number?))
 => error caller: bad argument x arg (member arg (y z))
(rest-values 'caller rest-list (list 'x 'y) (cons 1 number?))
 => error bad optional argument (x y) option
    (or (boolean? option) (integer? option) (memq option (list + -))) caller
(rest-values rest-list - 'y 100 "str")
 => x 1 "str"
(rest-values rest-list + `(x y z) `(100 . ,number?) `("str" . ,string?))
 => x 1 "str"
(rest-values rest-list #t `(x y z) `(100 . ,number?) `("str" . ,string?))
 => x 1 "str"
(rest-values rest-list #t `(100 . ,number?) `("str" . ,string?) `(x y z))
 => 1 "str" x
(rest-values rest-list #t `(100 . ,number?) `("str" . ,string?) `(y z))
 => error bad argument (x) rest-list (null? rest-list)
(rest-values rest-list #f `(100 . ,number?) `("str" . ,string?) `(y z))
 => 1 "str" y x
 |#

(test-end "srfi-51")
