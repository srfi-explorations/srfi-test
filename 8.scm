
(test-begin "srfi-8")

(receive (a b) (values 1 2)
         (begin
           (test-equal "1" a 1)
           (test-equal "2" b 2)))


(test-end "srfi-8")
