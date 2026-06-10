;; From https://practical-scheme.net/gauche/man/gauche-refe/A-program-argument-processor.html

(test-begin "srfi-37")

(define options
  (list (option '(#\d "debug") #f #t
                (lambda (option name arg debug batch paths files)
                  (values (or arg "2") batch paths files)))
        (option '(#\b "batch") #f #f
                (lambda (option name arg debug batch paths files)
                  (values debug #t paths files)))
        (option '(#\I "include") #t #f
                (lambda (option name arg debug batch paths files)
                  (values debug batch (cons arg paths) files)))))

(write options)
(newline)

(test-begin "defaults")
(let-values
  (((debug-level mode paths files)
    (args-fold '()
               options
               (lambda (option name arg . seeds)         ; unrecognized
                 (error "Unrecognized option:" name))
               (lambda (operand debug batch paths files) ; operand
                 (values debug batch paths (cons operand files)))
               0      ; default value of debug level
               #f     ; default value of batch mode
               '()    ; initial value of include paths
               '()    ; initial value of files
               )))
  (test-equal "default debug-level" debug-level 0)
  (test-equal "default mode" mode #f)
  (test-equal "default paths" paths '())
  (test-equal "default files" files '()))
(test-end "defaults")

(test-begin "args")
(define cmd-line '("--debug=1" "-I" "." "-b" "main.scm"))
(let-values
  (((debug-level mode paths files)
    (args-fold cmd-line
               options
               (lambda (option name arg . seeds)         ; unrecognized
                 (error "Unrecognized option:" name))
               (lambda (operand debug batch paths files) ; operand
                 (values debug batch paths (cons operand files)))
               0      ; default value of debug level
               #f     ; default value of batch mode
               '()    ; initial value of include paths
               '()    ; initial value of files
               )))
  (test-equal "debug-level" debug-level "1")
  (test-equal "mode" mode #t)
  (test-equal "paths" paths '("."))
  (test-equal "files" files '("main.scm")))
(test-end "args")


(test-end "srfi-37")
