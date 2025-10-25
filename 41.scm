; Copyright (C) 2007 by Philip L. Bewig of Saint Louis, Missouri, USA.  All rights
; reserved.  Permission is hereby granted, free of charge, to any person obtaining a copy of
; this software and associated documentation files (the "Software"), to deal in the Software
; without restriction, including without limitation the rights to use, copy, modify, merge,
; publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to
; whom the Software is furnished to do so, subject to the following conditions: The above
; copyright notice and this permission notice shall be included in all copies or substantial
; portions of the Software.  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
; FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
; CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR
; THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; unit tests

(test-begin "srfi-41")

(define strm123 (stream 1 2 3))

; uncommenr next line only for testing
;(define (error s x) (string-append (symbol->string s) ": " x))

; executing (unit-test) should produce no output
; stream-null
(test-equal (stream? stream-null) #t)
(test-equal (stream-null? stream-null) #t)
(test-equal (stream-pair? stream-null) #f)

; stream-cons
(test-equal (stream? (stream-cons 1 stream-null)) #t)
(test-equal (stream-null? (stream-cons 1 stream-null)) #f)
(test-equal (stream-pair? (stream-cons 1 stream-null)) #t)

; stream?
(test-equal (stream? stream-null) #t)
(test-equal (stream? (stream-cons 1 stream-null)) #t)
(test-equal (stream? "four") #f)

; stream-null?
(test-equal (stream-null? stream-null) #t)
(test-equal (stream-null? (stream-cons 1 stream-null)) #f)
(test-equal (stream-null? "four") #f)

; stream-pair?
(test-equal (stream-pair? stream-null) #f)
(test-equal (stream-pair? (stream-cons 1 stream-null)) #t)
(test-equal (stream-pair? "four") #f)

; stream-car
(test-equal (stream-car "four") "stream-car: non-stream")
(test-equal (stream-car stream-null) "stream-car: null stream")
(test-equal (stream-car strm123) 1)

; stream-cdr
(test-equal (stream-cdr "four") "stream-cdr: non-stream")
(test-equal (stream-cdr stream-null) "stream-cdr: null stream")
(test-equal (stream-car (stream-cdr strm123)) 2)

; stream-lambda
(test-equal
  (stream->list
    (letrec ((double
               (stream-lambda (strm)
                              (if (stream-null? strm)
                                stream-null
                                (stream-cons
                                  (* 2 (stream-car strm))
                                  (double (stream-cdr strm)))))))
      (double strm123)))
  '(2 4 6))

; define-stream
(test-equal
  (stream->list
    (let ()
      (define-stream (double strm)
                     (if (stream-null? strm)
                       stream-null
                       (stream-cons
                         (* 2 (stream-car strm))
                         (double (stream-cdr strm)))))
      (double strm123)))
  '(2 4 6))

; list->stream
(test-equal (list->stream "four") "list->stream: non-list argument")
(test-equal (stream->list (list->stream '())) '())
(test-equal (stream->list (list->stream '(1 2 3))) '(1 2 3))

; port->stream
(let* ((p (open-input-file "streams.ss"))
       (s (port->stream p)))
  (test-equal (port->stream "four") "port->stream: non-input-port argument")
  (test-equal (string=? (list->string (stream->list 11 s)) "; Copyright") #t)
  (close-input-port p))

; stream
(test-equal (stream->list (stream)) '())
(test-equal (stream->list (stream 1)) '(1))
(test-equal (stream->list (stream 1 2 3)) '(1 2 3))

; stream->list
(test-equal (stream->list '()) "stream->list: non-stream argument")
(test-equal (stream->list "four" strm123) "stream->list: non-integer count")
(test-equal (stream->list -1 strm123) "stream->list: negative count")
(test-equal (stream->list (stream)) '())
(test-equal (stream->list strm123) '(1 2 3))
(test-equal (stream->list 5 strm123) '(1 2 3))
(test-equal (stream->list 3 (stream-from 1)) '(1 2 3))

; stream-append
(test-equal (stream-append "four") "stream-append: non-stream argument")
(test-equal (stream->list (stream-append strm123)) '(1 2 3))
(test-equal (stream->list (stream-append strm123 strm123)) '(1 2 3 1 2 3))
(test-equal (stream->list (stream-append strm123 strm123 strm123)) '(1 2 3 1 2 3 1 2 3))
(test-equal (stream->list (stream-append strm123 stream-null)) '(1 2 3))
(test-equal (stream->list (stream-append stream-null strm123)) '(1 2 3))

; stream-concat
(test-equal (stream-concat "four") "stream-concat: non-stream argument")
(test-equal (stream->list (stream-concat (stream strm123))) '(1 2 3))
(test-equal (stream->list (stream-concat (stream strm123 strm123))) '(1 2 3 1 2 3))

; stream-constant
(test-equal (stream-ref (stream-constant 1) 100) 1)
(test-equal (stream-ref (stream-constant 1 2) 100) 1)
(test-equal (stream-ref (stream-constant 1 2 3) 3) 1)

; stream-drop
(test-equal (stream-drop "four" strm123) "stream-drop: non-integer argument")
(test-equal (stream-drop -1 strm123) "stream-drop: negative argument")
(test-equal (stream-drop 2 "four") "stream-drop: non-stream argument")
(test-equal (stream->list (stream-drop 0 stream-null)) '())
(test-equal (stream->list (stream-drop 0 strm123)) '(1 2 3))
(test-equal (stream->list (stream-drop 1 strm123)) '(2 3))
(test-equal (stream->list (stream-drop 5 strm123)) '())

; stream-drop-while
(test-equal (stream-drop-while "four" strm123) "stream-drop-while: non-procedural argument")
(test-equal (stream-drop-while odd? "four") "stream-drop-while: non-stream argument")
(test-equal (stream->list (stream-drop-while odd? stream-null)) '())
(test-equal (stream->list (stream-drop-while odd? strm123)) '(2 3))
(test-equal (stream->list (stream-drop-while even? strm123)) '(1 2 3))
(test-equal (stream->list (stream-drop-while positive? strm123)) '())
(test-equal (stream->list (stream-drop-while negative? strm123)) '(1 2 3))

; stream-filter
(test-equal (stream-filter "four" strm123) "stream-filter: non-procedural argument")
(test-equal (stream-filter odd? '()) "stream-filter: non-stream argument")
(test-equal (stream-null? (stream-filter odd? (stream))) #t)
(test-equal (stream->list (stream-filter odd? strm123)) '(1 3))
(test-equal (stream->list (stream-filter even? strm123)) '(2))
(test-equal (stream->list (stream-filter positive? strm123)) '(1 2 3))
(test-equal (stream->list (stream-filter negative? strm123)) '())
(let loop ((n 10))
  (test-equal (odd? (stream-ref (stream-filter odd? (stream-from 0)) n)) #t)
  (if (positive? n) (loop (- n 1))))
(let loop ((n 10))
  (test-equal (even? (stream-ref (stream-filter odd? (stream-from 0)) n)) #f)
  (if (positive? n) (loop (- n 1))))

; stream-fold
(test-equal (stream-fold "four" 0 strm123) "stream-fold: non-procedural argument")
(test-equal (stream-fold + 0 '()) "stream-fold: non-stream argument")
(test-equal (stream-fold + 0 strm123) 6)

; stream-for-each
(test-equal (stream-for-each "four" strm123) "stream-for-each: non-procedural argument")
(test-equal (stream-for-each display) "stream-for-each: no stream arguments")
(test-equal (stream-for-each display "four") "stream-for-each: non-stream argument")
(test-equal (let ((sum 0)) (stream-for-each (lambda (x) (set! sum (+ sum x))) strm123) sum) 6)

; stream-from
(test-equal (stream-from "four") "stream-from: non-numeric starting number")
(test-equal (stream-from 1 "four") "stream-from: non-numeric step size")
(test-equal (stream-ref (stream-from 0) 100) 100)
(test-equal (stream-ref (stream-from 1 2) 100) 201)
(test-equal (stream-ref (stream-from 0 -1) 100) -100)

; stream-iterate
(test-equal (stream-iterate "four" 0) "stream-iterate: non-procedural argument")
;(test-equal (stream->list 3 (stream-iterate (lsec + 1) 1)) '(1 2 3))

; stream-length
(test-equal (stream-length "four") "stream-length: non-stream argument")
(test-equal (stream-length (stream)) 0)
(test-equal (stream-length strm123) 3)

; stream-let
(test-equal (stream->list
              (stream-let loop ((strm strm123))
                          (if (stream-null? strm)
                            stream-null
                            (stream-cons
                              (* 2 (stream-car strm))
                              (loop (stream-cdr strm))))))
            '(2 4 6))

; stream-map
(test-equal (stream-map "four" strm123) "stream-map: non-procedural argument")
(test-equal (stream-map odd?) "stream-map: no stream arguments")
(test-equal (stream-map odd? "four") "stream-map: non-stream argument")
(test-equal (stream->list (stream-map - strm123)) '(-1 -2 -3))
(test-equal (stream->list (stream-map + strm123 strm123)) '(2 4 6))
(test-equal (stream->list (stream-map + strm123 (stream-from 1))) '(2 4 6))
(test-equal (stream->list (stream-map + (stream-from 1) strm123)) '(2 4 6))
(test-equal (stream->list (stream-map + strm123 strm123 strm123)) '(3 6 9))

; stream-match
;(test-equal (stream-match '(1 2 3) (_ 'ok)) "stream-match: non-stream argument")
;(test-equal (stream-match strm123 (() 42)) "stream-match: pattern failure")
;(test-equal (stream-match stream-null (() 'ok)) 'ok)
;(test-equal (stream-match strm123 (() 'no) (else 'ok)) 'ok)
;(test-equal (stream-match (stream 1) (() 'no) ((a) a)) 1)
;(test-equal (stream-match (stream 1) (() 'no) ((_) 'ok)) 'ok)
;(test-equal (stream-match strm123 ((a b c) (list a b c))) '(1 2 3))
;(test-equal (stream-match strm123 ((a . _) a)) 1)
;(test-equal (stream-match strm123 ((a b . _) (list a b))) '(1 2))
;(test-equal (stream-match strm123 ((a b . c) (list a b (stream-car c)))) '(1 2 3))
;(test-equal (stream-match strm123 (s (stream->list s))) '(1 2 3))
;(test-equal (stream-match strm123 ((a . _) (= a 1) 'ok)) 'ok)
;(test-equal (stream-match strm123 ((a . _) (= a 2) 'yes) (_ 'no)) 'no)
;(test-equal (stream-match strm123 ((a b c) (= a b) 'yes) (_ 'no)) 'no)
;(test-equal (stream-match (stream 1 1 2) ((a b c) (= a b) 'yes) (_ 'no)) 'yes)

; stream-of
(test-equal (stream->list
              (stream-of (+ y 6)
                         (x in (stream-range 1 6))
                         (odd? x)
                         (y is (* x x)))) '(7 15 31))
(test-equal (stream->list
              (stream-of (* x y)
                         (x in (stream-range 1 4))
                         (y in (stream-range 1 5))))
            '(1 2 3 4 2 4 6 8 3 6 9 12))
(test-equal (stream-car (stream-of 1)) 1)

; stream-range
(test-equal (stream-range "four" 0) "stream-range: non-numeric starting number")
(test-equal (stream-range 0 "four") "stream-range: non-numeric ending number")
(test-equal (stream-range 1 2 "three") "stream-range: non-numeric step size")
(test-equal (stream->list (stream-range 0 5)) '(0 1 2 3 4))
(test-equal (stream->list (stream-range 5 0)) '(5 4 3 2 1))
(test-equal (stream->list (stream-range 0 5 2)) '(0 2 4))
(test-equal (stream->list (stream-range 5 0 -2)) '(5 3 1))
(test-equal (stream->list (stream-range 0 1 -1)) '())

; stream-ref
(test-equal (stream-ref '() 4) "stream-ref: non-stream argument")
;(test-equal (stream-ref nats 3.5) "stream-ref: non-integer argument")
;(test-equal (stream-ref nats -3) "stream-ref: negative argument")
(test-equal (stream-ref strm123 5) "stream-ref: beyond end of stream")
(test-equal (stream-ref strm123 0) 1)
(test-equal (stream-ref strm123 1) 2)
(test-equal (stream-ref strm123 2) 3)

; stream-reverse
(test-equal (stream-reverse '()) "stream-reverse: non-stream argument")
(test-equal (stream->list (stream-reverse (stream))) '())
(test-equal (stream->list (stream-reverse strm123)) '(3 2 1))

; stream-scan
(test-equal (stream-scan "four" 0 strm123) "stream-scan: non-procedural argument")
(test-equal (stream-scan + 0 '()) "stream-scan: non-stream argument")
(test-equal (stream->list (stream-scan + 0 strm123)) '(0 1 3 6))

; stream-take
(test-equal (stream-take 5 "four") "stream-take: non-stream argument")
(test-equal (stream-take "four" strm123) "stream-take: non-integer argument")
(test-equal (stream-take -4 strm123) "stream-take: negative argument")
(test-equal (stream->list (stream-take 5 stream-null)) '())
(test-equal (stream->list (stream-take 0 stream-null)) '())
(test-equal (stream->list (stream-take 0 strm123)) '())
(test-equal (stream->list (stream-take 2 strm123)) '(1 2))
(test-equal (stream->list (stream-take 3 strm123)) '(1 2 3))
(test-equal (stream->list (stream-take 5 strm123)) '(1 2 3))

; stream-take-while
(test-equal (stream-take-while odd? "four") "stream-take-while: non-stream argument")
(test-equal (stream-take-while "four" strm123) "stream-take-while: non-procedural argument")
(test-equal (stream->list (stream-take-while odd? strm123)) '(1))
(test-equal (stream->list (stream-take-while even? strm123)) '())
(test-equal (stream->list (stream-take-while positive? strm123)) '(1 2 3))
(test-equal (stream->list (stream-take-while negative? strm123)) '())

; stream-unfold
(test-equal (stream-unfold "four" odd? + 0) "stream-unfold: non-procedural mapper")
(test-equal (stream-unfold + "four" + 0) "stream-unfold: non-procedural pred?")
(test-equal (stream-unfold + odd? "four" 0) "stream-unfold: non-procedural generator")
#;(test-equal (stream->list (stream-unfold (rsec expt 2) (rsec < 10) (rsec + 1) 0))
'(0 1 4 9 16 25 36 49 64 81))

; stream-unfolds
(test-equal
  (stream->list
    (stream-unfolds
      (lambda (x)
        (let ((n (car x)) (s (cdr x)))
          (if (zero? n)
            (values 'dummy '())
            (values
              (cons (- n 1) (stream-cdr s))
              (list (stream-car s))))))
      (cons 5 (stream-from 0))))
  '(0 1 2 3 4))

; stream-zip
(test-equal (stream-zip) "stream-zip: no stream arguments")
(test-equal (stream-zip "four") "stream-zip: non-stream argument")
(test-equal (stream-zip strm123 "four") "stream-zip: non-stream argument")
(test-equal (stream->list (stream-zip strm123 stream-null)) '())
(test-equal (stream->list (stream-zip strm123)) '((1) (2) (3)))
(test-equal (stream->list (stream-zip strm123 strm123)) '((1 1) (2 2) (3 3)))
(test-equal (stream->list (stream-zip strm123 (stream-from 1))) '((1 1) (2 2) (3 3)))
(test-equal (stream->list (stream-zip strm123 strm123 strm123)) '((1 1 1) (2 2 2) (3 3 3)))

; other tests
#;(test-equal
(stream-car
  (stream-reverse
    (stream-take-while
      (rsec < 1000)
      primes)))
997)

#;(test-equal
(equal?
  (stream->list (qsort < (stream 3 1 5 2 4)))
  (stream->list (isort < (stream 2 5 1 4 3))))
#t)

#;(test-equal
(equal?
  (stream->list (msort < (stream 3 1 5 2 4)))
  (stream->list (isort < (stream 2 5 1 4 3))))
#t)

; http://www.research.att.com/~njas/sequences/A051037
;(test-equal (stream-ref hamming 999) 51200000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; leak tests

; These tests can't be automated with portable code, so they need to be run by hand.
; Thus, they have been commented out.  To run the tests, uncomment them one by one,
; load them into a running Scheme system, and monitor space consumption using some
; os-specific tool outside the Scheme system.  All should run in bounded space.

; traversing a stream should take bounded space ...
; (define-stream (traverse s) (traverse (stream-cdr s)))
; (stream-ref (traverse (stream-from 0)) 10000000)

; ... even if something holds the head of the stream
; (define s (traverse (stream-from 0)))
; (stream-ref s 10000000)

; the infamous times3 test from SRFI-40
; (define (times3 n)
;   (stream-ref
;     (stream-filter
;       (lambda (x)
;         (zero? (modulo x n)))
;       (stream-from 0))
;     3))
; (times3 10000000)

(test-end "srfi-41")