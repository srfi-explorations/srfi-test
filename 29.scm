(import (srfi 28))

(let ((translations
        '(((en) . ((time . "Its ~a, ~a.")
                   (goodbye . "Goodbye, ~a.")))
          ((fr) . ((time . "~1@*~a, c'est ~a.")
                   (goodbye . "Au revoir, ~a."))))))
  (for-each (lambda (translation)
              (let ((bundle-name (cons 'hello-program (car translation))))
                (if (not (load-bundle! bundle-name))
                  (begin
                    (declare-bundle! bundle-name (cdr translation))
                    (store-bundle! bundle-name)))))
            translations))

(define localized-message
  (lambda (message-name . args)
    (apply format (cons (localized-template 'hello-program
                                            message-name)
                        args))))

(let ((myname "Fred"))
  (display (localized-message 'time "12:00" myname))
  (display #\newline)

  (display (localized-message 'goodbye myname))
  (display #\newline))

;; Displays (English):
;; Its 12:00, Fred.
;; Goodbye, Fred.
;;
;; French:
;; Fred, c'est 12:00.
;; Au revoir, Fred.
