(test-begin "srfi-8")

(test-assert "Hello, World!" (string=? (format "Hello, ~a" "World!") "Hello, World!"))

(test-end "srfi-8")
