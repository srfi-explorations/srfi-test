(test-begin "srfi-5")

(test-equal (let (fibonacci (n 10) (i 0) (f0 0) (f1 1))
              (if (= i n)
                f0
                (fibonacci n (+ i 1) f1 (+ f0 f1))))
            55)

(test-end "srfi-5")
