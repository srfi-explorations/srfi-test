(test-begin "srfi-28")

(test-assert "Hello, World!" (string=? (format "Hello, ~a" "World!") "Hello, World!"))

(test-end "srfi-28")
