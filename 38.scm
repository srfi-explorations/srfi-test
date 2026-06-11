(test-begin "srfi-38")

(define (obj->string obj)
  (let ((port (open-output-string)))
    (write-with-shared-structure obj port)
    (let ((str (get-output-string port)))
      (close-port port)
      str)))

(define (string->obj str)
  (read-with-shared-structure (open-input-string str)))

(let ((a '(val1 val2)))
  (set! a (cons a (list a)))
  (let ((teststr (obj->string a)))
    (test-assert "circular"
                 (or (string=? teststr "(#0=(val1 val2 #1#)")
                     (string=? teststr "(#1=(val1 val2) #1#)")
                     (string=? teststr "(#2=(val1 val2) #2#)")))))

(test-equal "list->string" (obj->string '(val1 val2)) "(val1 val2)")
(test-equal "string->list" '(val1 val2) (string->obj "(val1 val2)"))

(test-equal "number->string" (obj->string 5) "5")
(test-equal "string->number" 5 (string->obj "5"))

(test-equal "string->string" (obj->string "Just a string") "\"Just a string\"")
(test-equal "string->string" "Just a string" (string->obj "\"Just a string\""))

(test-end "srfi-38")
