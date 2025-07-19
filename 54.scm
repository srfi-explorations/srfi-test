(test-begin "srfi-54")

(test-equal (cat 129.995 -10 2.) "130.00    ")

(test-equal (cat 129.995 10 2.) "    130.00")

(test-equal (cat 129.985 10 2.) "    129.98")

(test-equal (cat 129.985001 10 2.) "    129.99")

(test-equal (cat 129.995 2. 'exact) "#e130.00")

(test-equal (cat 129 -2.) "129.00")

(test-equal (cat 129 2.) "#e129.00")

(test-equal (cat 129 10 2. #\0 'sign) "#e+0129.00")

(test-equal (cat 129 10 2. #\* 'sign) "*#e+129.00")

(test-equal (cat 1/3) "1/3")

(test-equal (cat 1/3 10 2.) "    #e0.33")

(test-equal (cat 1/3 10 -2.) "      0.33")

(test-equal (cat 129.995 10 '(#\, 2)) " 1,29.99,5")

(test-equal (cat 129995 10 '(#\,) 'sign) "  +129,995")

(test-equal (cat (cat 129.995 0.) '(0 -1)) "130")

(test-equal (cat 99.5 10 'sign 'octal) "#i#o+307/2")

(test-equal (cat 99.5 10 'sign 'octal 'exact) "  #o+307/2")

(test-equal (cat #x123 'octal 'sign) "#o+443")

(test-equal (cat #x123 -10 2. 'sign #\*) "#e+291.00*")

(test-equal (cat -1.2345e+15+1.2355e-15i 3.) "-1.234e15+1.236e-15i")

(test-equal (cat 1.2345e+15 10 3. 'sign) " +1.234e15")

(test-equal (cat "string" -10) "string    ")

(test-equal (cat "string" 10 (list string-upcase)) "    STRING")

(test-equal (cat "string" 10 (list string-upcase) '(-2)) "      RING")

(test-equal (cat "string" 10 `(,string-titlecase) '(2 3)) "     Sting")

(test-equal (cat "string" `(,string-reverse ,string-upcase) => "GNIRTS"

(test-equal (cat #\a 10) "         a")

(test-equal (cat 'symbol 10) "    symbol")

(test-equal (cat '#(#\a "str" s)) "#(#\\a \"str\" s)")

(test-equal (cat '(#\a "str" s)) "(#\\a \"str\" s)")

(test-equal (cat '(#\a "str" s) #t) (#\a "str" s)"(#\\a \"str\" s)")

(test-equal (cat '(#\a "str" s) (current-output-port)) (#\a "str" s)"(#\\a \"str\" s)")

(test-equal (cat 3 (cat 's) " " (cat "str" write)) "3s \"str\"")

(test-equal (cat 3 #t (cat 's) " " (cat "str" write)) 3s "str""3s \"str\"")

(test-equal (cat 3 #t (cat 's #t) " " (cat "str" write)) s3s "str""3s \"str\"")
(test-end "srfi-54")
