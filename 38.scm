(test-begin "srfi-38")

(define (obj->string obj)
  (parameterize
    ((current-output-port (open-output-string)))
    (write-with-shared-structure obj)
    (get-output-string (current-output-port))))

(define (string->obj str)
  (read-shared-structure (open-input-string str)))

(let ((a '(val1 val2)))
  (set! a (cons a (list a)))
  (let ((teststr (obj->string a)))
    (test-assert "circular"
                 (or (string=? teststr "(#0=(val1 val2 #1#)")
                     (string=? teststr "(#1=(val1 val2) #1#)")
                     (string=? teststr "(#2=(val1 val2) #2#)")))))

(test-equal "list" (obj->string '(val1 val2)) "(val1 val2)")

(test-equal "number" (obj->string 5) "5")

(test-equal "string" (obj->string "Just a string") "\"Just a string\"")

(test-end "srfi-38")
