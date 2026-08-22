(test-begin "29")

(define translations
  '(((en) . ((time . "Its ~a, ~a.")
             (goodbye . "Goodbye, ~a.")))
    ((fr) . ((time . "~a, c'est ~a.")
             (goodbye . "Au revoir, ~a.")))))

(for-each (lambda (translation)
            (let ((bundle-name (cons 'hello-program (car translation))))
              (if (not (load-bundle! bundle-name))
                (begin
                  (declare-bundle! bundle-name (cdr translation))
                  (store-bundle! bundle-name)))))
          translations)

(define localized-message
  (lambda (message-name . args)
    (apply format (cons (localized-template 'hello-program message-name) args))))

(define myname "Fred")

;; Displays (English):
;; Its 12:00, Fred.
;; Goodbye, Fred.
(test-equal "Its 12:00, Fred." (localized-message 'time "12:00" myname))
(test-equal "Goodbye, Fred." (localized-message 'goodbye myname))

;; French:
;; Fred, c'est 12:00.
;; Au revoir, Fred.

(test-end "29")

