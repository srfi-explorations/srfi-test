
(test-begin "srfi-4")

;;; (r6rs bytevectors) starts
;;; Copyright 2015 William D Clinger.
;;;
;;; Permission to copy this software, in whole or in part, to use this
;;; software for any lawful purpose, and to redistribute this software
;;; is granted subject to the restriction that all copies made of this
;;; software must include this copyright and permission notice in full.
;;;
;;; I also request that you send me a copy of any improvements that you
;;; make to this software so that they may be incorporated within it to
;;; the benefit of the Scheme community.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This R7RS-portable implementation of (rnrs bytevectors) is
;;; mostly derived from Larceny's src/Lib/Common/bytevector.sch.
;;;
;;; The R6RS requires implementations to select a native endianness.
;;; That choice is arbitrary, intended to affect performance but not
;;; behavior.  In this implementation, the native endianness is
;;; obtained via cond-expand, which should coincide with the
;;; endianness obtained by calling the features procedure.  Of the
;;; R7RS systems I've tested, only one omits endianness from its
;;; (features), and it's a slow interpreter for which the native
;;; endianness probably won't affect performance.
;;;
;;; This implementation defines a 53-bit exact integer constant,
;;; and the procedures that work with byte fields of arbitrary
;;; width may create even larger exact integers.
;;;
;;; FIXME: It should be possible to delay creation of that 53-bit
;;; constant until it's needed, which might be better for systems
;;; that don't support exact 53-bit integers.  It looks as though
;;; most systems R7RS systems either support exact 53-bit integers
;;; or overflow into inexact 53-bit integers; if the constant turns
;;; out to be inexact, then the procedure that needs it will fail
;;; when it is called, which is what would happen if creation of
;;; that constant were delayed.

;;; Local stuff.

(define (complain who . irritants)
  (apply error
         (string-append "illegal arguments passed to "
                        (symbol->string who))
         irritants))

; Help syntax and procedures; not exported.

(define-syntax unspecified
  (syntax-rules ()
    ((_) (if #f #f))))

(define-syntax bytevector:div
  (syntax-rules ()
    ((_ x y) (quotient x y))))

(define-syntax bytevector:mod
  (syntax-rules ()
    ((_ x y) (remainder x y))))

(define-syntax u8->s8
  (syntax-rules ()
    ((_ octet0)
     (let ((octet octet0))
       (if (> octet 127)
         (- octet 256)
         octet)))))

(define-syntax s8->u8
  (syntax-rules ()
    ((_ val0)
     (let ((val val0))
       (if (negative? val)
         (+ val 256)
         val)))))

(define (make-uint-ref size)
  (lambda (bytevector k endianness)
    (bytevector-uint-ref bytevector k endianness size)))

(define (make-sint-ref size)
  (lambda (bytevector k endianness)
    (bytevector-sint-ref bytevector k endianness size)))

(define (make-uint-set! size)
  (lambda (bytevector k n endianness)
    (bytevector-uint-set! bytevector k n endianness size)))

(define (make-sint-set! size)
  (lambda (bytevector k n endianness)
    (bytevector-sint-set! bytevector k n endianness size)))

(define (make-ref/native base base-ref)
  (lambda (bytevector index)
    (ensure-aligned index base)
    (base-ref bytevector index (native-endianness))))

(define (make-set!/native base base-set!)
  (lambda (bytevector index val)
    (ensure-aligned index base)
    (base-set! bytevector index val (native-endianness))))

(define (ensure-aligned index base)
  (if (not (zero? (bytevector:mod index base)))
    (error "non-aligned bytevector access" index base)))

(define (make-bytevector->int-list bytevector-ref)
  (lambda (b endness size)
    (let ((ref (lambda (i) (bytevector-ref b i endness size)))
          (length (bytevector-length b)))
      (let loop ((i 0) (r '()))
        (if (>= i length)
          (reverse r)
          (loop (+ i size)
                (cons (ref i) r)))))))

(define (make-int-list->bytevector bytevector-set!)
  (lambda (l endness size)
    (let* ((bytevector (make-bytevector (* size (length l))))
           (setter! (lambda (i n)
                      (bytevector-set! bytevector i n endness size))))
      (let loop ((i 0) (l l))
        (if (null? l)
          bytevector
          (begin
            (setter! i (car l))
            (loop (+ i size) (cdr l))))))))

;;; Magic numbers for IEEE-754 single and double precision:
;;;
;;;     the largest biased exponent (255 or 2047)
;;;     the exponent bias (127 or 1023)
;;;     the integer value of the hidden bit (2^23 or 2^52)

(define bytevector:single-maxexponent 255)
(define bytevector:single-bias
  (bytevector:div bytevector:single-maxexponent 2))
(define bytevector:single-hidden-bit (expt 2 23))

(define bytevector:double-maxexponent 2047)
(define bytevector:double-bias
  (bytevector:div bytevector:double-maxexponent 2))
(define bytevector:double-hidden-bit (expt 2 52))    ; must be exact integer

(define two^48 (expt 2 48))
(define two^40 (expt 2 40))
(define two^32 (expt 2 32))
(define two^24 (expt 2 24))
(define two^16 (expt 2 16))
(define two^8  (expt 2 8))

;;; Given four exact integers, returns
;;;
;;;     (-1)^sign * (2^exponent) * p/q
;;;
;;; as an inexact real.
;;;
;;; FIXME: this procedure is not used, but it might eventually
;;; become relevant to a rewrite of this implementation so I'm
;;; just commenting it out.

#;
(define (bytevector:normalized sign exponent p q)
  (let* ((p/q (inexact (/ p q)))
         (x (* p/q (expt 2.0 exponent))))
    (cond ((= sign 0) x)
          ((= x 0.0) -0.0)
          (else (- x)))))

;;; Given exact positive integers p and q,
;;; returns three values:
;;; exact integers exponent, p2, and q2 such that
;;;     q2 <= p2 < q2+q2
;;;     p / q = (p2 * 2^exponent) / q2

(define (bytevector:normalized-ieee-parts p q)
  (cond ((< p q)
         (do ((p p (+ p p))
              (e 0 (- e 1)))
           ((>= p q)
            (values e p q))))
        ((<= (+ q q) p)
         (do ((q q (+ q q))
              (e 0 (+ e 1)))
           ((< p (+ q q))
            (values e p q))))
        (else
          (values 0 p q))))

;;; Given an inexact real x, an exponent bias, and an exact positive
;;; integer q that is a power of 2 representing the integer value of
;;; the hidden bit, returns three exact integers:
;;;
;;; sign
;;; biased-exponent
;;; p
;;;
;;; If x is normalized, then 0 < biased-exponent <= bias+bias,
;;; q <= p < 2*q, and
;;;
;;;     x = (-1)^sign * (2^(biased-exponent - bias)) * p/q
;;;
;;; If x is denormalized, then p < q and the equation holds.
;;; If x is zero, then biased-exponent and p are zero.
;;; If x is infinity, then biased-exponent = bias+bias+1 and p=0.
;;; If x is a NaN, then biased-exponent = bias+bias+1 and p>0.
;;;

(define (bytevector:ieee-parts x bias q)
  (cond ((nan? x)
         (values 0 (+ bias bias 1) (- q 1)))
        ((infinite? x)
         (values (if (positive? x) 0 1) (+ bias bias 1) 0))
        ((zero? x)
         (values (if (eqv? x -0.0) 1 0) 0 0))
        (else
          (let* ((sign (if (negative? x) 1 0))
                 (y (exact (abs x)))
                 (num (numerator y))
                 (den (denominator y)))
            (call-with-values
              (lambda () (bytevector:normalized-ieee-parts num den))
              (lambda (exponent num den)
                (let ((biased-exponent (+ exponent bias)))
                  (cond ((< 0 biased-exponent (+ bias bias 1))
                         ; within the range of normalized numbers
                         (if (<= den q)
                           (let* ((factor (/ q den))
                                  (num*factor (* num factor)))
                             (if (integer? factor)
                               (values sign biased-exponent num*factor)
                               (error 'bytevector:ieee-parts
                                      "this shouldn't happen: " x bias q)))
                           (let* ((factor (/ den q))
                                  (num*factor (/ num factor)))
                             (values sign
                                     biased-exponent
                                     (round num*factor)))))
                        ((>= biased-exponent (+ bias bias 1))
                         ; infinity
                         (values (if (positive? x) 0 1) (+ bias bias 1) 0))
                        (else
                          ; denormalized
                          ; FIXME: this has the double rounding bug
                          (do ((biased biased-exponent (+ biased 1))
                               (num (round (/ (* q num) den))
                                    (round (bytevector:div num 2))))
                            ((and (< num q) (= biased 1))
                             (values sign biased num))))))))))))

;;; This procedure should work even if
;;;     exact integers are limited to as little as 20 bits
;;;     inexact reals are limited to IEEE single precision
;;;
;;; If inexact reals are limited to single precision, then
;;; the result might overflow, but we can't help that.

(define (bytevector-ieee-double-big-endian-ref bytevector k)
  (let* ((byte0 (bytevector-u8-ref bytevector (+ k 0)))
         (byte1 (bytevector-u8-ref bytevector (+ k 1)))
         (byte2 (bytevector-u8-ref bytevector (+ k 2)))
         (byte3 (bytevector-u8-ref bytevector (+ k 3)))
         (byte4 (bytevector-u8-ref bytevector (+ k 4)))
         (byte5 (bytevector-u8-ref bytevector (+ k 5)))
         (byte6 (bytevector-u8-ref bytevector (+ k 6)))
         (byte7 (bytevector-u8-ref bytevector (+ k 7)))
         (sign (quotient byte0 128))
         (biased-exponent (+ (* 16 (remainder byte0 128))
                             (quotient byte1 16)))
         (hibits (+ (* 65536 (remainder byte1 16))
                    (* 256 byte2)
                    byte3))
         (midbits (+ (* 256 byte4) byte5))
         (lobits (+ (* 256 byte6) byte7)))
    (make-ieee-double sign biased-exponent hibits midbits lobits)))

(define (bytevector-ieee-double-little-endian-ref bytevector k)
  (let* ((byte0 (bytevector-u8-ref bytevector (+ k 7)))
         (byte1 (bytevector-u8-ref bytevector (+ k 6)))
         (byte2 (bytevector-u8-ref bytevector (+ k 5)))
         (byte3 (bytevector-u8-ref bytevector (+ k 4)))
         (byte4 (bytevector-u8-ref bytevector (+ k 3)))
         (byte5 (bytevector-u8-ref bytevector (+ k 2)))
         (byte6 (bytevector-u8-ref bytevector (+ k 1)))
         (byte7 (bytevector-u8-ref bytevector (+ k 0)))
         (sign (quotient byte0 128))
         (biased-exponent (+ (* 16 (remainder byte0 128))
                             (quotient byte1 16)))
         (hibits (+ (* 65536 (remainder byte1 16))
                    (* 256 byte2)
                    byte3))
         (midbits (+ (* 256 byte4) byte5))
         (lobits (+ (* 256 byte6) byte7)))
    (make-ieee-double sign biased-exponent hibits midbits lobits)))

;;; This procedure should work even if
;;;     exact integers are limited to as little as 23 bits
;;;     inexact reals are limited to IEEE single precision

(define (bytevector-ieee-single-big-endian-ref bytevector k)
  (let* ((byte0 (bytevector-u8-ref bytevector (+ k 0)))
         (byte1 (bytevector-u8-ref bytevector (+ k 1)))
         (byte2 (bytevector-u8-ref bytevector (+ k 2)))
         (byte3 (bytevector-u8-ref bytevector (+ k 3)))
         (sign (quotient byte0 128))
         (biased-exponent (+ (* 2 (remainder byte0 128))
                             (quotient byte1 128)))
         (bits (+ (* 65536 (remainder byte1 128))
                  (* 256 byte2)
                  byte3)))
    (make-ieee-single sign biased-exponent bits)))

(define (bytevector-ieee-single-little-endian-ref bytevector k)
  (let* ((byte0 (bytevector-u8-ref bytevector (+ k 3)))
         (byte1 (bytevector-u8-ref bytevector (+ k 2)))
         (byte2 (bytevector-u8-ref bytevector (+ k 1)))
         (byte3 (bytevector-u8-ref bytevector (+ k 0)))
         (sign (quotient byte0 128))
         (biased-exponent (+ (* 2 (remainder byte0 128))
                             (quotient byte1 128)))
         (bits (+ (* 65536 (remainder byte1 128))
                  (* 256 byte2)
                  byte3)))
    (make-ieee-single sign biased-exponent bits)))

;;; Given
;;;
;;;     the sign bit
;;;     biased exponent
;;;     integer value of the 20 high order bits without the hidden bit
;;;     integer value of the 16 mid-order bits
;;;     integer value of the 16 low-order bits
;;;
;;; returns an inexact real approximating the IEEE double precision
;;; number with the given representation.  If an implementation
;;; implements inexact reals using IEEE double precision, and
;;; implements IEEE-754 arithmetic correctly, and the arguments
;;; do not imply a NaN, then the inexact real that's returned
;;; should be exactly right.

(define (make-ieee-double sign biased-exponent hibits midbits lobits)
  (cond ((= biased-exponent bytevector:double-maxexponent)
         (if (zero? (+ hibits midbits lobits))
           (if (= 0 sign)
             +inf.0
             -inf.0)
           (if (= 0 sign)
             +nan.0
             -nan.0)))
        ((= 0 biased-exponent)
         (if (and (= 0 hibits)
                  (= 0 midbits)
                  (= 0 lobits))
           (if (= 0 sign)
             +0.0
             -0.0)
           (let* ((x (inexact hibits))
                  (x (+ (* 65536.0 x)
                        (inexact midbits)))
                  (x (+ (* 65536.0 x)
                        (inexact lobits)))
                  (two^51 2.251799813685248e15)
                  (x (/ x two^51))
                  (x (* x (expt 2.0 (- bytevector:double-bias)))))
             (if (= 0 sign)
               x
               (- x)))))
        (else
          (let* ((hibits (+ #x100000    ; hidden bit
                            hibits))
                 (x (inexact hibits))
                 (x (+ (* 65536.0 x)
                       (inexact midbits)))
                 (x (+ (* 65536.0 x)
                       (inexact lobits)))
                 (two^52 4.503599627370496e15)
                 (x (/ x two^52))
                 (x (* x (expt 2.0
                               (- biased-exponent bytevector:double-bias)))))
            (if (= 0 sign)
              x
              (- x))))))

;;; Given
;;;
;;;     the sign bit
;;;     biased exponent
;;;     integer value of the 23-bit mantissa without the hidden bit
;;;
;;; returns an inexact real approximating the IEEE single precision
;;; number with the given representation.  If an implementation
;;; implements inexact reals using IEEE single or double precision,
;;; and implements IEEE-754 arithmetic correctly, and the arguments
;;; do not imply a NaN, then the inexact real that's returned
;;; should be exactly right.

(define (make-ieee-single sign biased-exponent bits)
  (cond ((= biased-exponent bytevector:single-maxexponent)
         (if (zero? bits)
           (if (= 0 sign)
             +inf.0
             -inf.0)
           (if (= 0 sign)
             +nan.0
             -nan.0)))
        ((= 0 biased-exponent)
         (if (= 0 bits)
           (if (= 0 sign)
             +0.0
             -0.0)
           (let* ((x (inexact bits))
                  (two^22 4194304.0)
                  (x (/ x two^22))
                  (x (* x (expt 2.0 (- bytevector:single-bias)))))
             (if (= 0 sign)
               x
               (- x)))))
        (else
          (let* ((bits (+ #x800000   ; hidden bit
                          bits))
                 (x (inexact bits))
                 (two^23 8388608.0)
                 (x (/ x two^23))
                 (x (* x (expt 2.0
                               (- biased-exponent bytevector:single-bias)))))
            (if (= 0 sign)
              x
              (- x))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Exported stuff.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The R6RS allows implementations to support other symbols as well.

(define-syntax endianness
  (syntax-rules ()
    ((_ big)
     (quote big))
    ((_ little)
     (quote little))))

(cond-expand
  (little-endian
    (define (native-endianness)
      'little))
  (else
    (define (native-endianness)
      'big)))

;;; Already defined by (scheme base):
;;;
;;;     bytevector? make-bytevector bytevector-length

(define (bytevector=? bv1 bv2)
  (if (and (bytevector? bv1)
           (bytevector? bv2))
    (equal? bv1 bv2)
    (complain 'bytevector=? bv1 bv2)))

(define (bytevector-fill! b fill)
  (if (<= -128 fill -1)
    (bytevector-fill! b (+ fill 256))
    (let ((n (bytevector-length b)))
      (do ((i 0 (+ i 1)))
        ((= i n))
        (bytevector-u8-set! b i fill)))))

(define r6rs:bytevector-copy bytevector-copy)

(define (r6rs:bytevector-copy! source source-start target target-start count)
  (if (>= source-start target-start)
    (do ((i 0 (+ i 1)))
      ((>= i count))
      (bytevector-u8-set! target
                          (+ target-start i)
                          (bytevector-u8-ref source (+ source-start i))))
    (do ((i (- count 1) (- i 1)))
      ((< i 0))
      (bytevector-u8-set! target
                          (+ target-start i)
                          (bytevector-u8-ref source (+ source-start i))))))

;;; Already defined by (scheme base), perhaps in greater generality:
;;;
;;;     bytevector-copy
;;;     bytevector-u8-ref
;;;     bytevector-u8-set!

(define (bytevector-s8-ref b k)
  (u8->s8 (bytevector-u8-ref b k)))

(define (bytevector-s8-set! b k val)
  (bytevector-u8-set! b k (s8->u8 val)))

(define (bytevector->u8-list b)
  (let ((n (bytevector-length b)))
    (do ((i (- n 1) (- i 1))
         (result '() (cons (bytevector-u8-ref b i) result)))
      ((< i 0)
       result))))

(define (u8-list->bytevector vals)
  (let* ((n (length vals))
         (b (make-bytevector n)))
    (do ((vals vals (cdr vals))
         (i 0 (+ i 1)))
      ((null? vals))
      (bytevector-u8-set! b i (car vals)))
    b))

(define (bytevector-uint-ref bytevector index endness size)
  (case endness
    ((big)
     (do ((i 0 (+ i 1))
          (result 0 (+ (* 256 result)
                       (bytevector-u8-ref bytevector (+ index i)))))
       ((>= i size)
        result)))
    ((little)
     (do ((i (- size 1) (- i 1))
          (result 0 (+ (* 256 result)
                       (bytevector-u8-ref bytevector (+ index i)))))
       ((< i 0)
        result)))
    (else
      (bytevector-uint-ref bytevector index (native-endianness) size))))

(define (bytevector-sint-ref bytevector index endness size)
  (let* ((high-byte (bytevector-u8-ref bytevector
                                       (if (eq? endness 'big)
                                         index
                                         (+ index size -1))))
         (uresult (bytevector-uint-ref bytevector index endness size)))
    (if (> high-byte 127)
      (- uresult (expt 256 size))
      uresult)))

; FIXME: Some of these procedures may not do enough range checking.

(define (bytevector-uint-set! bytevector index val endness size)
  (case endness
    ((little)
     (do ((i 0 (+ i 1))
          (val val (bytevector:div val 256)))
       ((>= i size)
        (unspecified))
       (bytevector-u8-set! bytevector (+ index i) (bytevector:mod val 256))))
    ((big)
     (do ((i (- size 1) (- i 1))
          (val val (bytevector:div val 256)))
       ((< i 0)
        (unspecified))
       (bytevector-u8-set! bytevector (+ index i) (bytevector:mod val 256))))
    (else
      (bytevector-uint-set! bytevector index val (native-endianness) size))))

(define (bytevector-sint-set! bytevector index val endness size)
  (let ((uval (if (< val 0)
                (+ val (expt 256 size))
                val)))
    (bytevector-uint-set! bytevector index uval endness size)))

(define bytevector->uint-list (make-bytevector->int-list bytevector-uint-ref))
(define bytevector->sint-list (make-bytevector->int-list bytevector-sint-ref))

(define uint-list->bytevector (make-int-list->bytevector bytevector-uint-set!))
(define sint-list->bytevector (make-int-list->bytevector bytevector-sint-set!))

(define bytevector-u16-ref (make-uint-ref 2))
(define bytevector-s16-ref (make-sint-ref 2))
(define bytevector-u16-set! (make-uint-set! 2))
(define bytevector-s16-set! (make-sint-set! 2))
(define bytevector-u16-native-ref (make-ref/native 2 bytevector-u16-ref))
(define bytevector-s16-native-ref (make-ref/native 2 bytevector-s16-ref))
(define bytevector-u16-native-set! (make-set!/native 2 bytevector-u16-set!))
(define bytevector-s16-native-set! (make-set!/native 2 bytevector-s16-set!))

(define bytevector-u32-ref (make-uint-ref 4))
(define bytevector-s32-ref (make-sint-ref 4))
(define bytevector-u32-set! (make-uint-set! 4))
(define bytevector-s32-set! (make-sint-set! 4))
(define bytevector-u32-native-ref (make-ref/native 4 bytevector-u32-ref))
(define bytevector-s32-native-ref (make-ref/native 4 bytevector-s32-ref))
(define bytevector-u32-native-set! (make-set!/native 4 bytevector-u32-set!))
(define bytevector-s32-native-set! (make-set!/native 4 bytevector-s32-set!))

(define bytevector-u64-ref (make-uint-ref 8))
(define bytevector-s64-ref (make-sint-ref 8))
(define bytevector-u64-set! (make-uint-set! 8))
(define bytevector-s64-set! (make-sint-set! 8))
(define bytevector-u64-native-ref (make-ref/native 8 bytevector-u64-ref))
(define bytevector-s64-native-ref (make-ref/native 8 bytevector-s64-ref))
(define bytevector-u64-native-set! (make-set!/native 8 bytevector-u64-set!))
(define bytevector-s64-native-set! (make-set!/native 8 bytevector-s64-set!))

(cond-expand
  (little-endian
    (define (bytevector-ieee-single-native-ref bytevector k)
      (if (not (= 0 (remainder k 4)))
        (complain 'bytevector-ieee-single-native-ref bytevector k))
      (bytevector-ieee-single-little-endian-ref bytevector k))
    (define (bytevector-ieee-double-native-ref bytevector k)
      (if (not (= 0 (remainder k 8)))
        (complain 'bytevector-ieee-double-native-ref bytevector k))
      (bytevector-ieee-double-little-endian-ref bytevector k))
    (define (bytevector-ieee-single-native-set! bytevector k x)
      (if (not (= 0 (remainder k 4)))
        (complain 'bytevector-ieee-single-native-set! bytevector k x))
      (bytevector-ieee-single-set! bytevector k x 'little))
    (define (bytevector-ieee-double-native-set! bytevector k x)
      (if (not (= 0 (remainder k 8)))
        (complain 'bytevector-ieee-double-native-set! bytevector k x))
      (bytevector-ieee-double-set! bytevector k x 'little)))
  (else
    (define (bytevector-ieee-single-native-ref bytevector k)
      (if (not (= 0 (remainder k 4)))
        (complain 'bytevector-ieee-single-native-ref bytevector k))
      (bytevector-ieee-single-big-endian-ref bytevector k))
    (define (bytevector-ieee-double-native-ref bytevector k)
      (if (not (= 0 (remainder k 8)))
        (complain 'bytevector-ieee-double-native-ref bytevector k))
      (bytevector-ieee-double-big-endian-ref bytevector k))
    (define (bytevector-ieee-single-native-set! bytevector k x)
      (if (not (= 0 (remainder k 4)))
        (complain 'bytevector-ieee-single-native-set! bytevector k x))
      (bytevector-ieee-single-set! bytevector k x 'big))
    (define (bytevector-ieee-double-native-set! bytevector k x)
      (if (not (= 0 (remainder k 8)))
        (complain 'bytevector-ieee-double-native-set! bytevector k x))
      (bytevector-ieee-double-set! bytevector k x 'big))))

(define (bytevector-ieee-single-ref bytevector k endianness)
  (case endianness
    ((big)
     (bytevector-ieee-single-big-endian-ref bytevector k))
    ((little)
     (bytevector-ieee-single-little-endian-ref bytevector k))
    (else
      (complain 'bytevector-ieee-single-ref bytevector k endianness))))

(define (bytevector-ieee-double-ref bytevector k endianness)
  (case endianness
    ((big)
     (bytevector-ieee-double-big-endian-ref bytevector k))
    ((little)
     (bytevector-ieee-double-little-endian-ref bytevector k))
    (else
      (complain 'bytevector-ieee-double-ref bytevector k endianness))))

(define (bytevector-ieee-single-set! bytevector k x endianness)
  (call-with-values
    (lambda ()
      (bytevector:ieee-parts x
                             bytevector:single-bias
                             bytevector:single-hidden-bit))
    (lambda (sign biased-exponent frac)
      (define (store! sign biased-exponent frac)
        (if (eq? 'big endianness)
          (begin
            (bytevector-u8-set! bytevector k
                                (+ (* 128 sign)
                                   (bytevector:div biased-exponent 2)))
            (bytevector-u8-set! bytevector (+ k 1)
                                (+ (* 128 (bytevector:mod biased-exponent 2))
                                   (bytevector:div frac (* 256 256))))
            (bytevector-u8-set! bytevector (+ k 2)
                                (bytevector:div
                                  (bytevector:mod frac (* 256 256)) 256))
            (bytevector-u8-set! bytevector (+ k 3)
                                (bytevector:mod frac 256)))
          (begin
            (bytevector-u8-set! bytevector (+ k 3)
                                (+ (* 128 sign)
                                   (bytevector:div biased-exponent 2)))
            (bytevector-u8-set! bytevector (+ k 2)
                                (+ (* 128 (bytevector:mod biased-exponent 2))
                                   (bytevector:div frac (* 256 256))))
            (bytevector-u8-set! bytevector (+ k 1)
                                (bytevector:div
                                  (bytevector:mod frac (* 256 256)) 256))
            (bytevector-u8-set! bytevector k
                                (bytevector:mod frac 256))))
        (unspecified))
      (cond ((= biased-exponent bytevector:single-maxexponent)
             (store! sign biased-exponent frac))
            ((< frac bytevector:single-hidden-bit)
             (store! sign 0 frac))
            (else
              (store! sign biased-exponent
                      (- frac bytevector:single-hidden-bit)))))))

(define (bytevector-ieee-double-set! bytevector k x endianness)
  (call-with-values
    (lambda ()
      (bytevector:ieee-parts x
                             bytevector:double-bias
                             bytevector:double-hidden-bit))
    (lambda (sign biased-exponent frac)

      (define (store! sign biased-exponent frac)
        (bytevector-u8-set! bytevector (+ k 7)
                            (+ (* 128 sign)
                               (bytevector:div biased-exponent 16)))
        (bytevector-u8-set! bytevector (+ k 6)
                            (+ (* 16 (bytevector:mod biased-exponent 16))
                               (bytevector:div frac two^48)))
        (bytevector-u8-set! bytevector (+ k 5)
                            (bytevector:div (bytevector:mod frac two^48)
                                            two^40))
        (bytevector-u8-set! bytevector (+ k 4)
                            (bytevector:div (bytevector:mod frac two^40)
                                            two^32))
        (bytevector-u8-set! bytevector (+ k 3)
                            (bytevector:div (bytevector:mod frac two^32)
                                            two^24))
        (bytevector-u8-set! bytevector (+ k 2)
                            (bytevector:div (bytevector:mod frac two^24)
                                            two^16))
        (bytevector-u8-set! bytevector (+ k 1)
                            (bytevector:div (bytevector:mod frac two^16)
                                            256))
        (bytevector-u8-set! bytevector k
                            (bytevector:mod frac 256))
        (if (not (eq? endianness 'little))
          (begin (swap! (+ k 0) (+ k 7))
                 (swap! (+ k 1) (+ k 6))
                 (swap! (+ k 2) (+ k 5))
                 (swap! (+ k 3) (+ k 4))))
        (unspecified))

      (define (swap! i j)
        (let ((bi (bytevector-u8-ref bytevector i))
              (bj (bytevector-u8-ref bytevector j)))
          (bytevector-u8-set! bytevector i bj)
          (bytevector-u8-set! bytevector j bi)))

      (cond ((= biased-exponent bytevector:double-maxexponent)
             (store! sign biased-exponent frac))
            ((< frac bytevector:double-hidden-bit)
             (store! sign 0 frac))
            (else
              (store! sign biased-exponent
                      (- frac bytevector:double-hidden-bit)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Conversions between bytevectors and strings.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Already defined by (scheme base), with greater generality:
;;;
;;;     string->utf8
;;;     utf8->string

; (utf-16-codec) might write a byte order mark,
; so it's better not to use textual i/o for this.

(define (string->utf16 string . rest)
  (let* ((endianness (cond ((null? rest)
                            'big)
                           ((not (null? (cdr rest)))
                            (apply complain 'string->utf16 string rest))
                           ((eq? (car rest) 'big)
                            'big)
                           ((eq? (car rest) 'little)
                            'little)
                           (else
                             (apply complain 'string->utf16 string rest))))

         ; endianness-dependent adjustments to indexing

         (hi (if (eq? 'big endianness) 0 1))
         (lo (- 1 hi))

         (n (string-length string)))

    (define (result-length)
      (do ((i 0 (+ i 1))
           (k 0 (let ((sv (char->integer (string-ref string i))))
                  (if (< sv #x10000) (+ k 2) (+ k 4)))))
        ((= i n) k)))

    (let ((bv (make-bytevector (result-length))))

      (define (loop i k)
        (if (< i n)
          (let ((sv (char->integer (string-ref string i))))
            (if (< sv #x10000)
              (let ((hibits (quotient sv 256))
                    (lobits (remainder sv 256)))
                (bytevector-u8-set! bv (+ k hi) hibits)
                (bytevector-u8-set! bv (+ k lo) lobits)
                (loop (+ i 1) (+ k 2)))
              (let* ((x (- sv #x10000))
                     (hibits (quotient x 1024))
                     (lobits (remainder x 1024))
                     (hi16 (+ #xd800 hibits))
                     (lo16 (+ #xdc00 lobits))
                     (hi1 (quotient hi16 256))
                     (lo1 (remainder hi16 256))
                     (hi2 (quotient lo16 256))
                     (lo2 (remainder lo16 256)))
                (bytevector-u8-set! bv (+ k hi) hi1)
                (bytevector-u8-set! bv (+ k lo) lo1)
                (bytevector-u8-set! bv (+ k hi 2) hi2)
                (bytevector-u8-set! bv (+ k lo 2) lo2)
                (loop (+ i 1) (+ k 4)))))))

      (loop 0 0)
      bv)))

;;; The second argument to utf16->string should be optional,
;;; and was optional in the R5.94RS draft, but was made mandatory
;;; in the R5.95RS draft by someone who misinterpreted John Cowan's
;;; response of 27 May 2007 to an ambiguous question posed by
;;; Mike Sperber.  This error was not spotted by anyone, and
;;; made its way into the ratified R6RS.
;;;
;;; This implementation does not perpetuate that error.  In this
;;; implementation, the second argument is optional.
;;;
;;; The R6RS also contradicts itself by saying the bytevector
;;; will be decoded according to UTF-16BE or UTF-16LE, which
;;; implies any BOM must be ignored.  I believe the intended
;;; specification was along these lines:
;;;
;;;    Bytevector is decoded acccording to UTF-16, UTF-16BE,
;;;    UTF-16LE, or a fourth encoding scheme that differs from
;;;    all three of those, depending upon the optional arguments
;;;    endianness and endianness-mandatory.  If endianness
;;;    is the symbol big and endianness-mandatory is absent
;;;    or false, then bytevector is decoded according to
;;;    UTF-16.  If endianness is the symbol big and
;;;    endianness-mandatory is #t, then bytevector is decoded
;;;    according to UTF-16BE.  If endianness is the symbol
;;;    little and endianness-mandatory is #t, then bytevector
;;;    is decoded according to UTF-16LE.  If endianness is
;;;    the symbol little and endianness-mandatory is absent
;;;    or #f, then the bytevector is decoded according to
;;;    UTF-16 if it begins with a BOM but is decoded according
;;;    to UTF-16LE if it does not begin with a BOM; note that
;;;    this fourth decoding does not correspond to any of the
;;;    Unicode encoding schemes that are defined by the Unicode
;;;    standard.
;;;
;;; That is the specification implemented here.

(define (utf16->string bytevector . rest)
  (let* ((n (bytevector-length bytevector))

         (begins-with-bom?
           (and (<= 2 n)
                (let ((b0 (bytevector-u8-ref bytevector 0))
                      (b1 (bytevector-u8-ref bytevector 1)))
                  (or (and (= b0 #xfe) (= b1 #xff) 'big)
                      (and (= b0 #xff) (= b1 #xfe) 'little)))))

         (mandatory? (cond ((or (null? rest) (null? (cdr rest)))
                            #f)
                           ((and (null? (cddr rest))
                                 (boolean? (cadr rest)))
                            (cadr rest))
                           (else
                             (apply complain 'utf16->string bytevector rest))))

         (endianness (cond ((null? rest)
                            (or begins-with-bom? 'big))
                           ((eq? (car rest) 'big)
                            (if mandatory?
                              'big
                              (or begins-with-bom? 'big)))
                           ((eq? (car rest) 'little)
                            (if mandatory?
                              'little
                              (or begins-with-bom? 'little)))
                           (else (apply complain
                                        'utf16->string
                                        bytevector rest))))

         (begins-with-bom? (if mandatory? #f begins-with-bom?))

         (endianness (if mandatory? (car rest) endianness))

         ; endianness-dependent adjustments to indexing

         (hi (if (eq? 'big endianness) 0 1))
         (lo (- 1 hi))

         (replacement-character (integer->char #xfffd)))

    ; computes the length of the encoded string

    (define (result-length)
      (define (loop i k)
        (if (>= i n)
          k
          (let ((octet (bytevector-u8-ref bytevector i)))
            (cond ((< octet #xd8)
                   (loop (+ i 2) (+ k 1)))
                  ((< octet #xdc)
                   (let* ((i2 (+ i 2))
                          (octet2 (if (< i2 n)
                                    (bytevector-u8-ref bytevector i2)
                                    0)))
                     (if (<= #xdc octet2 #xdf)
                       (loop (+ i 4) (+ k 1))
                       ; bad surrogate pair, becomes replacement character
                       (loop i2 (+ k 1)))))
                  (else (loop (+ i 2) (+ k 1)))))))
      (if begins-with-bom?
        (loop (+ hi 2) 0)
        (loop hi 0)))

    (if (odd? n)
      (error "bytevector passed to utf16->string has odd length" bytevector))

    (let ((s (make-string (result-length))))
      (define (loop i k)
        (if (< i n)
          (let ((hibits (bytevector-u8-ref bytevector (+ i hi)))
                (lobits (bytevector-u8-ref bytevector (+ i lo))))
            (cond ((< hibits #xd8)
                   (let ((c (integer->char
                              (+ (* hibits 256)
                                 lobits))))
                     (string-set! s k c))
                   (loop (+ i 2) (+ k 1)))
                  ((< hibits #xdc)
                   (let* ((i2 (+ i hi 2))
                          (i3 (+ i lo 2))
                          (octet2 (if (< i2 n)
                                    (bytevector-u8-ref bytevector i2)
                                    0))
                          (octet3 (if (< i2 n)
                                    (bytevector-u8-ref bytevector i3)
                                    0)))
                     (if (<= #xdc octet2 #xdf)
                       (let* ((sv (+ #x10000
                                     (* #x0400
                                        (remainder
                                          (+ (* hibits 256)
                                             lobits)
                                          #x0400))
                                     (remainder
                                       (+ (* octet2 256)
                                          octet3)
                                       #x0400)))
                              (c (if (<= #x10000 sv #x10ffff)
                                   (integer->char sv)
                                   replacement-character)))
                         (string-set! s k c)
                         (loop (+ i 4) (+ k 1)))
                       ; bad surrogate pair
                       (begin (string-set! s k replacement-character)
                              (loop (+ i 2) (+ k 1))))))
                  ((< hibits #xe0)
                   ; second surrogate not preceded by a first surrogate
                   (string-set! s k replacement-character)
                   (loop (+ i 2) (+ k 1)))
                  (else
                    (let ((c (integer->char
                               (+ (* hibits 256)
                                  lobits))))
                      (string-set! s k c))
                    (loop (+ i 2) (+ k 1)))))))
      (if begins-with-bom?
        (loop 2 0)
        (loop 0 0))
      s)))

;;; There is no utf-32-codec, so we can't use textual i/o for this.

(define (string->utf32 string . rest)
  (let* ((endianness (cond ((null? rest) 'big)
                           ((eq? (car rest) 'big) 'big)
                           ((eq? (car rest) 'little) 'little)
                           (else (apply complain
                                        'string->utf32
                                        string
                                        rest))))
         (n (string-length string))
         (result (make-bytevector (* 4 n))))
    (do ((i 0 (+ i 1)))
      ((= i n) result)
      (bytevector-u32-set! result
                           (* 4 i)
                           (char->integer (string-ref string i))
                           endianness))))

;;; There is no utf-32-codec, so we can't use textual i/o for this.

(define (utf32->string bytevector . rest)
  (let* ((n (bytevector-length bytevector))

         (begins-with-bom?
           (and (<= 4 n)
                (let ((b0 (bytevector-u8-ref bytevector 0))
                      (b1 (bytevector-u8-ref bytevector 1))
                      (b2 (bytevector-u8-ref bytevector 2))
                      (b3 (bytevector-u8-ref bytevector 3)))
                  (or (and (= b0 0) (= b1 0) (= b2 #xfe) (= b3 #xff)
                           'big)
                      (and (= b0 #xff) (= b1 #xfe) (= b2 0) (= b3 0)
                           'little)))))

         (mandatory? (cond ((or (null? rest) (null? (cdr rest)))
                            #f)
                           ((and (null? (cddr rest))
                                 (boolean? (cadr rest)))
                            (cadr rest))
                           (else
                             (apply complain 'utf32->string bytevector rest))))

         (endianness (cond ((null? rest)
                            (or begins-with-bom? 'big))
                           ((eq? (car rest) 'big)
                            (if mandatory?
                              'big
                              (or begins-with-bom? 'big)))
                           ((eq? (car rest) 'little)
                            (if mandatory?
                              'little
                              (or begins-with-bom? 'little)))
                           (else (apply complain
                                        'utf32->string
                                        bytevector
                                        rest))))

         (begins-with-bom? (if mandatory? #f begins-with-bom?))

         (endianness (if mandatory? (car rest) endianness))

         (i0 (if begins-with-bom? 4 0))

         (result (if (zero? (remainder n 4))
                   (make-string (quotient (- n i0) 4))
                   (complain
                     "bytevector passed to utf32->string has bad length"
                     bytevector))))

    (do ((i i0 (+ i 4))
         (j 0 (+ j 1)))
      ((= i n) result)
      (let* ((sv (bytevector-u32-ref bytevector i endianness))
             (sv (cond ((< sv #xd800) sv)
                       ((< sv #xe000) #xfffd) ; replacement character
                       ((< sv #x110000) sv)
                       (else #xfffd)))        ; replacement character
             (c (integer->char sv)))
        (string-set! result j c)))))

;;;; (r6rs bytevectors) ends

(define pinfty +inf.0)
(define minfty -inf.0)
(define nanval +nan.0)

;(define pinfty 1.0e307)
;(define minfty -1.0e307)
;(define nanval 12345.0)

;;;; Basic tests of (rnrs bytevectors).

(define (iota count . o)
  (let ((start (if (pair? o) (car o) 0))
        (step (if (and (pair? o) (pair? (cdr o))) (cadr o) 1)))
    (let lp ((i count) (res '()))
      (if (<= i 0)
        res
        (lp (- i 1) (cons (+ start (* (- i 1) step)) res))))))

(test-begin "1.1 Basic Bytevector Tests")

(test-equal (or (eq? (native-endianness) 'big)
                (eq? (native-endianness) 'little)) #t)

(test-equal (bytevector? (vector)) #f)
(test-equal (bytevector? (make-bytevector 3)) #t)

(test-equal (bytevector-length (make-bytevector 44)) 44)

(test-equal (let ((b1 (make-bytevector 16 127))
                  (b2 (make-bytevector 16 255)))
              (list
                (bytevector-s8-ref b1 0)
                (bytevector-u8-ref b1 0)
                (bytevector-s8-ref b2 0)
                (bytevector-u8-ref b2 0)))
            '(127 129 -1 255))

(test-equal (let ((b (make-bytevector 16 127)))
              (bytevector-s8-set! b 0 -126)
              (bytevector-u8-set! b 1 246)
              (list
                (bytevector-s8-ref b 0)
                (bytevector-u8-ref b 0)
                (bytevector-s8-ref b 1)
                (bytevector-u8-ref b 1)))
            '(-126 130 -10 246))

(let ()
  (define b (make-bytevector 16 127))
  (bytevector-uint-set! b 0 (- (expt 2 128) 3) (endianness little) 16)

  (test-equal (bytevector-uint-ref b 0 (endianness little) 16)
              #xfffffffffffffffffffffffffffffffd)

  (test-equal (bytevector-sint-ref b 0 (endianness little) 16) -3)

  (test-equal (bytevector->u8-list b)
              '(253 255 255 255 255 255 255 255
                255 255 255 255 255 255 255 255))

  (bytevector-uint-set! b 0 (- (expt 2 128) 3) (endianness big) 16)

  (test-equal (bytevector-uint-ref b 0 (endianness big) 16)
              #xfffffffffffffffffffffffffffffffd)

  (test-equal (bytevector-sint-ref b 0 (endianness big) 16) -3)

  (test-equal (bytevector->u8-list b)
              '(255 255 255 255 255 255 255 255
                255 255 255 255 255 255 255 )))

(let ()
  (define b
    (u8-list->bytevector
      '(255 255 255 255 255 255 255 255
        255 255 255 255 255 255 255 253)))

  (test-equal (bytevector-u16-ref b 14 (endianness little)) 65023)

  (test-equal (bytevector-s16-ref b 14 (endianness little)) -513)

  (test-equal (bytevector-u16-ref b 14 (endianness big)) 65533)

  (test-equal (bytevector-s16-ref b 14 (endianness big)) -3)

  (bytevector-u16-set! b 0 12345 (endianness little))

  (test-equal (bytevector-u16-ref b 0 (endianness little)) 12345)

  (bytevector-u16-native-set! b 0 12345)

  (test-equal (bytevector-u16-native-ref b 0) 12345))

(let ()
  (define b
    (u8-list->bytevector
      '(255 255 255 255 255 255 255 255
        255 255 255 255 255 255 255 253)))

  (test-equal (bytevector-u32-ref b 12 (endianness little)) 4261412863)

  (test-equal (bytevector-s32-ref b 12 (endianness little)) -33554433)

  (test-equal (bytevector-u32-ref b 12 (endianness big)) 4294967293)

  (test-equal (bytevector-s32-ref b 12 (endianness big)) -3))

(let ()
  (define b
    (u8-list->bytevector
      '(255 255 255 255 255 255 255 255
        255 255 255 255 255 255 255 253)))

  (test-equal (bytevector-u64-ref b 8 (endianness little))
              18302628885633695743)

  (test-equal (bytevector-s64-ref b 8 (endianness little))
              -144115188075855873)

  (test-equal (bytevector-u64-ref b 8 (endianness big))
              18446744073709551613)

  (test-equal (bytevector-s64-ref b 8 (endianness big)) -3))

(let ()
  (define b1 (u8-list->bytevector '(255 2 254 3 255)))
  (define b2 (u8-list->bytevector '(255 3 254 2 255)))
  (define b3 (u8-list->bytevector '(255 3 254 2 255)))
  (define b4 (u8-list->bytevector '(255 3 255)))

  (test-equal (bytevector=? b1 b2) #f)
  (test-equal (bytevector=? b2 b3) #t)
  (test-equal (bytevector=? b3 b4) #f)
  (test-equal (bytevector=? b4 b3) #f))

(let ()
  (define b
    (u8-list->bytevector
      '(63 240 0 0 0 0 0 0)))

  (test-equal (bytevector-ieee-single-ref b 4 'little) 0.0)

  (test-equal (bytevector-ieee-double-ref b 0 'big) 1.0)

  (bytevector-ieee-single-native-set! b 4 3.0)

  (test-equal (bytevector-ieee-single-native-ref b 4) 3.0)

  (bytevector-ieee-double-native-set! b 0 5.0)

  (test-equal (bytevector-ieee-double-native-ref b 0) 5.0)

  (bytevector-ieee-double-set! b 0 1.75 'big)

  (test-equal (bytevector->u8-list b) '(63 252 0 0 0 0 0 0))) ; depends on ieee routines

(let ((b (make-bytevector 7 12)))
  (bytevector-fill! b 127)
  (test-equal (bytevector->u8-list b) '(127 127 127 127 127 127 127)))

(let ((b (u8-list->bytevector '(1 2 3 4 5 6 7 8))))
  (r6rs:bytevector-copy! b 0 b 3 4)
  (test-equal (bytevector->u8-list b) '(1 2 3 1 2 3 4 8))
  (test-equal (bytevector=? b (r6rs:bytevector-copy b)) #t))

(let ((b (u8-list->bytevector '(1 2 3 255 1 2 1 2))))
  (test-equal (bytevector->sint-list b (endianness little) 2)
              '(513 -253 513 513))
  (test-equal (bytevector->uint-list b (endianness little) 2)
              '(513 65283 513 513)))

(test-end "1.1 Basic Bytevector Tests")

(test-begin "1.2 IEEE Bytevector Tests")

(define (roundtrip x getter setter! k endness)
  (let ((b (make-bytevector 100)))
    (setter! b k x endness)
    (getter b k endness)))

(define (->single x)
  (roundtrip
    x bytevector-ieee-single-ref bytevector-ieee-single-set! 0 'big))

(define (->double x)
  (roundtrip
    x bytevector-ieee-double-ref bytevector-ieee-double-set! 0 'big))

; Single precision, offset 0, big-endian

(test-equal (roundtrip
              pinfty
              bytevector-ieee-single-ref bytevector-ieee-single-set! 0 'big)
            pinfty)

(test-equal (roundtrip
              minfty
              bytevector-ieee-single-ref bytevector-ieee-single-set! 0 'big)
            minfty)

(test-equal (let ((x (roundtrip
                       nanval
                       bytevector-ieee-single-ref bytevector-ieee-single-set!
                       0 'big)))
              (= x x))
            #f)

(test-equal (roundtrip
              1e10
              bytevector-ieee-single-ref bytevector-ieee-single-set! 0 'big)
            1e10)

(test-equal (roundtrip
              -0.2822580337524414
              bytevector-ieee-single-ref bytevector-ieee-single-set! 0 'big)
            -0.2822580337524414)

; Single precision, offset 0, little-endian

(test-equal (roundtrip
              pinfty
              bytevector-ieee-single-ref bytevector-ieee-single-set! 0 'little)
            pinfty)

(test-equal (roundtrip
              minfty
              bytevector-ieee-single-ref bytevector-ieee-single-set! 0 'little)
            minfty)

(test-equal (let ((x (roundtrip
                       nanval
                       bytevector-ieee-single-ref bytevector-ieee-single-set!
                       0 'little)))
              (= x x))
            #f)

(test-equal (roundtrip
              1e10
              bytevector-ieee-single-ref bytevector-ieee-single-set! 0 'little)
            1e10)

(test-equal (roundtrip
              -0.2822580337524414
              bytevector-ieee-single-ref bytevector-ieee-single-set! 0 'little)
            -0.2822580337524414)

; Single precision, offset 1, big-endian

(test-equal (roundtrip
              pinfty
              bytevector-ieee-single-ref bytevector-ieee-single-set! 1 'big)
            pinfty)

(test-equal (roundtrip
              minfty
              bytevector-ieee-single-ref bytevector-ieee-single-set! 1 'big)
            minfty)

(test-equal (let ((x (roundtrip
                       nanval
                       bytevector-ieee-single-ref bytevector-ieee-single-set!
                       1 'big)))
              (= x x))
            #f)

(test-equal (roundtrip
              1e10
              bytevector-ieee-single-ref bytevector-ieee-single-set! 1 'big)
            1e10)

(test-equal (roundtrip
              -0.2822580337524414
              bytevector-ieee-single-ref bytevector-ieee-single-set! 1 'big)
            -0.2822580337524414)

; Single precision, offset 1, little-endian

(test-equal (roundtrip
              pinfty
              bytevector-ieee-single-ref bytevector-ieee-single-set! 1 'little)
            pinfty)

(test-equal (roundtrip
              minfty
              bytevector-ieee-single-ref bytevector-ieee-single-set! 1 'little)
            minfty)

(test-equal (let ((x (roundtrip
                       nanval
                       bytevector-ieee-single-ref bytevector-ieee-single-set!
                       1 'little)))
              (= x x))
            #f)

(test-equal (roundtrip
              1e10
              bytevector-ieee-single-ref bytevector-ieee-single-set! 1 'little)
            1e10)

(test-equal (roundtrip
              -0.2822580337524414
              bytevector-ieee-single-ref bytevector-ieee-single-set! 1 'little)
            -0.2822580337524414)

; Single precision, offset 2, big-endian

(test-equal (roundtrip
              pinfty
              bytevector-ieee-single-ref bytevector-ieee-single-set! 2 'big)
            pinfty)

(test-equal (roundtrip
              minfty
              bytevector-ieee-single-ref bytevector-ieee-single-set! 2 'big)
            minfty)

(test-equal (let ((x (roundtrip
                       nanval
                       bytevector-ieee-single-ref bytevector-ieee-single-set!
                       2 'big)))
              (= x x))
            #f)

(test-equal (roundtrip
              1e10
              bytevector-ieee-single-ref bytevector-ieee-single-set! 2 'big)
            1e10)

(test-equal (roundtrip
              -0.2822580337524414
              bytevector-ieee-single-ref bytevector-ieee-single-set! 2 'big)
            -0.2822580337524414)

; Single precision, offset 2, little-endian

(test-equal (roundtrip
              pinfty
              bytevector-ieee-single-ref bytevector-ieee-single-set! 2 'little)
            pinfty)

(test-equal (roundtrip
              minfty
              bytevector-ieee-single-ref bytevector-ieee-single-set! 2 'little)
            minfty)

(test-equal (let ((x (roundtrip
                       nanval
                       bytevector-ieee-single-ref bytevector-ieee-single-set!
                       2 'little)))
              (= x x))
            #f)

(test-equal (roundtrip
              1e10
              bytevector-ieee-single-ref bytevector-ieee-single-set! 2 'little)
            1e10)

(test-equal (roundtrip
              -0.2822580337524414
              bytevector-ieee-single-ref bytevector-ieee-single-set! 2 'little)
            -0.2822580337524414)

; Single precision, offset 3, big-endian

(test-equal (roundtrip
              pinfty
              bytevector-ieee-single-ref bytevector-ieee-single-set! 3 'big)
            pinfty)

(test-equal (roundtrip
              minfty
              bytevector-ieee-single-ref bytevector-ieee-single-set! 3 'big)
            minfty)

(test-equal (let ((x (roundtrip
                       nanval
                       bytevector-ieee-single-ref bytevector-ieee-single-set!
                       3 'big)))
              (= x x))
            #f)

(test-equal (roundtrip
              1e10
              bytevector-ieee-single-ref bytevector-ieee-single-set! 3 'big)
            1e10)

(test-equal (roundtrip
              -0.2822580337524414
              bytevector-ieee-single-ref bytevector-ieee-single-set! 3 'big)
            -0.2822580337524414)

; Single precision, offset 3, little-endian

(test-equal (roundtrip
              pinfty
              bytevector-ieee-single-ref bytevector-ieee-single-set! 3 'little)
            pinfty)

(test-equal (roundtrip
              minfty
              bytevector-ieee-single-ref bytevector-ieee-single-set! 3 'little)
            minfty)

(test-equal (let ((x (roundtrip
                       nanval
                       bytevector-ieee-single-ref bytevector-ieee-single-set!
                       3 'little)))
              (= x x))
            #f)

(test-equal (roundtrip
              1e10
              bytevector-ieee-single-ref bytevector-ieee-single-set! 3 'little)
            1e10)

(test-equal (roundtrip
              -0.2822580337524414
              bytevector-ieee-single-ref bytevector-ieee-single-set! 3 'little)
            -0.2822580337524414)

; Double precision, offset 0, big-endian

(test-equal (roundtrip
              pinfty
              bytevector-ieee-double-ref bytevector-ieee-double-set! 0 'big)
            pinfty)

(test-equal (roundtrip
              minfty
              bytevector-ieee-double-ref bytevector-ieee-double-set! 0 'big)
            minfty)

(test-equal (let ((x (roundtrip
                       nanval
                       bytevector-ieee-double-ref bytevector-ieee-double-set!
                       0 'big)))
              (= x x))
            #f)

(test-equal (roundtrip
              1e10
              bytevector-ieee-double-ref bytevector-ieee-double-set! 0 'big)
            1e10)

(test-equal (roundtrip
              -0.2822580337524414
              bytevector-ieee-double-ref bytevector-ieee-double-set! 0 'big)
            -0.2822580337524414)

; Double precision, offset 0, little-endian

(test-equal (roundtrip
              pinfty
              bytevector-ieee-double-ref bytevector-ieee-double-set! 0 'little)
            pinfty)

(test-equal (roundtrip
              minfty
              bytevector-ieee-double-ref bytevector-ieee-double-set! 0 'little)
            minfty)

(test-equal (let ((x (roundtrip
                       nanval
                       bytevector-ieee-double-ref bytevector-ieee-double-set!
                       0 'little)))
              (= x x))
            #f)

(test-equal (roundtrip
              1e10
              bytevector-ieee-double-ref bytevector-ieee-double-set! 0 'little)
            1e10)

(test-equal (roundtrip
              -0.2822580337524414
              bytevector-ieee-double-ref bytevector-ieee-double-set! 0 'little)
            -0.2822580337524414)

; Double precision, offset 1, big-endian

(test-equal (roundtrip
              pinfty
              bytevector-ieee-double-ref bytevector-ieee-double-set! 1 'big)
            pinfty)

(test-equal (roundtrip
              minfty
              bytevector-ieee-double-ref bytevector-ieee-double-set! 1 'big)
            minfty)

(test-equal (let ((x (roundtrip
                       nanval
                       bytevector-ieee-double-ref bytevector-ieee-double-set!
                       1 'big)))
              (= x x))
            #f)

(test-equal (roundtrip
              1e10
              bytevector-ieee-double-ref bytevector-ieee-double-set! 1 'big)
            1e10)

(test-equal (roundtrip
              -0.2822580337524414
              bytevector-ieee-double-ref bytevector-ieee-double-set! 1 'big)
            -0.2822580337524414)

; Double precision, offset 1, little-endian

(test-equal (roundtrip
              pinfty
              bytevector-ieee-double-ref bytevector-ieee-double-set! 1 'little)
            pinfty)

(test-equal (roundtrip
              minfty
              bytevector-ieee-double-ref bytevector-ieee-double-set! 1 'little)
            minfty)

(test-equal (let ((x (roundtrip
                       nanval
                       bytevector-ieee-double-ref bytevector-ieee-double-set!
                       1 'little)))
              (= x x))
            #f)

(test-equal (roundtrip
              1e10
              bytevector-ieee-double-ref bytevector-ieee-double-set! 1 'little)
            1e10)

(test-equal (roundtrip
              -0.2822580337524414
              bytevector-ieee-double-ref bytevector-ieee-double-set! 1 'little)
            -0.2822580337524414)

; Double precision, offset 2, big-endian

(test-equal (roundtrip
              pinfty
              bytevector-ieee-double-ref bytevector-ieee-double-set! 2 'big)
            pinfty)

(test-equal (roundtrip
              minfty
              bytevector-ieee-double-ref bytevector-ieee-double-set! 2 'big)
            minfty)

(test-equal (let ((x (roundtrip
                       nanval
                       bytevector-ieee-double-ref bytevector-ieee-double-set!
                       2 'big)))
              (= x x))
            #f)

(test-equal (roundtrip
              1e10
              bytevector-ieee-double-ref bytevector-ieee-double-set! 2 'big)
            1e10)

(test-equal (roundtrip
              -0.2822580337524414
              bytevector-ieee-double-ref bytevector-ieee-double-set! 2 'big)
            -0.2822580337524414)

; Double precision, offset 2, little-endian

(test-equal (roundtrip
              pinfty
              bytevector-ieee-double-ref bytevector-ieee-double-set! 2 'little)
            pinfty)

(test-equal (roundtrip
              minfty
              bytevector-ieee-double-ref bytevector-ieee-double-set! 2 'little)
            minfty)

(test-equal (let ((x (roundtrip
                       nanval
                       bytevector-ieee-double-ref bytevector-ieee-double-set!
                       2 'little)))
              (= x x))
            #f)

(test-equal (roundtrip
              1e10
              bytevector-ieee-double-ref bytevector-ieee-double-set! 2 'little)
            1e10)

(test-equal (roundtrip
              -0.2822580337524414
              bytevector-ieee-double-ref bytevector-ieee-double-set! 2 'little)
            -0.2822580337524414)

; Double precision, offset 3, big-endian

(test-equal (roundtrip
              pinfty
              bytevector-ieee-double-ref bytevector-ieee-double-set! 3 'big)
            pinfty)

(test-equal (roundtrip
              minfty
              bytevector-ieee-double-ref bytevector-ieee-double-set! 3 'big)
            minfty)

(test-equal (let ((x (roundtrip
                       nanval
                       bytevector-ieee-double-ref bytevector-ieee-double-set!
                       3 'big)))
              (= x x))
            #f)

(test-equal (roundtrip
              1e10
              bytevector-ieee-double-ref bytevector-ieee-double-set! 3 'big)
            1e10)

(test-equal (roundtrip
              -0.2822580337524414
              bytevector-ieee-double-ref bytevector-ieee-double-set! 3 'big)
            -0.2822580337524414)

; Double precision, offset 3, little-endian

(test-equal (roundtrip
              pinfty
              bytevector-ieee-double-ref bytevector-ieee-double-set! 3 'little)
            pinfty)

(test-equal (roundtrip
              minfty
              bytevector-ieee-double-ref bytevector-ieee-double-set! 3 'little)
            minfty)

(test-equal (let ((x (roundtrip
                       nanval
                       bytevector-ieee-double-ref bytevector-ieee-double-set!
                       3 'little)))
              (= x x))
            #f)

(test-equal (roundtrip
              1e10
              bytevector-ieee-double-ref bytevector-ieee-double-set! 3 'little)
            1e10)

(test-equal (roundtrip
              -0.2822580337524414
              bytevector-ieee-double-ref bytevector-ieee-double-set! 3 'little)
            -0.2822580337524414)

(test-end "1.2 IEEE Bytevector Tests")


(test-begin "2.2 General Operations")

; Denormalized numbers.

(do ((x 1.0 (* .5 x)))
  ((= x 0.0))
  (let ((y (->single x)))
    (if (and (> y 0.0)
             (not (= x y)))
      (begin (write (list 'inaccurate-single-conversion: x '=> y))
             (newline)))))

(do ((x 1.0 (* .5 x)))
  ((= x 0.0))
  (let ((y (->double x)))
    (if (not (= x y))
      (begin (write (list 'inaccurate-double-conversion: x '=> y))
             (newline)))))

(test-assert "native-endianness"
             (not (not (memq (native-endianness) '(big little)))))

(test-assert "make-bytevector"
             (and (bytevector? (make-bytevector 20))
                  (bytevector? (make-bytevector 20 3))))

(test-assert "bytevector-length"
             (= (bytevector-length (make-bytevector 20)) 20))

(test-assert "bytevector=?"
             (and (bytevector=? (make-bytevector 20 7)
                                (make-bytevector 20 7))
                  (not (bytevector=? (make-bytevector 20 7)
                                     (make-bytevector 20 0)))))

(test-equal "bytevector-fill! with fill 255"
             (u8-list->bytevector '(255 255 255 255))
             (let ((bv (make-bytevector 4)))
               (bytevector-fill! bv 255)
               bv))

(test-assert "r6rs:bytevector-copy! overlapping"
             ;; See <http://debbugs.gnu.org/10070>.
             (let ((b (u8-list->bytevector '(1 2 3 4 5 6 7 8))))
               (r6rs:bytevector-copy! b 0 b 3 4)
               (bytevector->u8-list b)
               (bytevector=? b (u8-list->bytevector '(1 2 3 1 2 3 4 8)))))

(test-end "2.2 General Operations")

(test-begin "2.3 Operations on Bytes and Octets")

(test-assert "bytevector-{u8,s8}-ref"
             (equal? '(127 129 -1 255)
                     (let ((b1 (make-bytevector 16 127))
                           (b2 (make-bytevector 16 255)))
                       (list (bytevector-s8-ref b1 0)
                             (bytevector-u8-ref b1 0)
                             (bytevector-s8-ref b2 0)
                             (bytevector-u8-ref b2 0)))))

(test-assert "bytevector-{u8,s8}-set!"
         (equal? '(-126 130 -10 246)
                 (let ((b (make-bytevector 16 127)))

                   (bytevector-s8-set! b 0 -126)
                   (bytevector-u8-set! b 1 246)

                   (list (bytevector-s8-ref b 0)
                         (bytevector-u8-ref b 0)
                         (bytevector-s8-ref b 1)
                         (bytevector-u8-ref b 1)))))

(test-assert "bytevector->u8-list"
         (let ((lst '(1 2 3 128 150 255)))
           (equal? lst
                   (bytevector->u8-list
                     (let ((b (make-bytevector 6)))
                       (for-each (lambda (i v)
                                   (bytevector-u8-set! b i v))
                                 (iota 6)
                                 lst)
                       b)))))

(test-assert "u8-list->bytevector"
         (let ((lst '(1 2 3 128 150 255)))
           (equal? lst
                   (bytevector->u8-list (u8-list->bytevector lst)))))

(test-assert "bytevector-uint-{ref,set!} [small]"
         (let ((b (make-bytevector 15)))
           (bytevector-uint-set! b 0 #x1234
                                 (endianness little) 2)
           (equal? (bytevector-uint-ref b 0 (endianness big) 2)
                   #x3412)))

(test-assert "bytevector-uint-set! [large]"
         (let ((b (make-bytevector 16)))
           (bytevector-uint-set! b 0 (- (expt 2 128) 3)
                                 (endianness little) 16)
           (equal? (bytevector->u8-list b)
                   '(253 255 255 255 255 255 255 255
                     255 255 255 255 255 255 255 255))))

(test-assert "bytevector-uint-{ref,set!} [large]"
         (let ((b (make-bytevector 120)))
           (bytevector-uint-set! b 0 (- (expt 2 128) 3)
                                 (endianness little) 16)
           (equal? (bytevector-uint-ref b 0 (endianness little) 16)
                   #xfffffffffffffffffffffffffffffffd)))

(test-assert "bytevector-sint-ref [small]"
         (let ((b (u8-list->bytevector '(#xff #xf0 #xff))))
           (= (bytevector-sint-ref b 0 (endianness big) 2)
              (bytevector-sint-ref b 1 (endianness little) 2)
              -16)))

(test-assert "bytevector-sint-ref [large]"
         (let ((b (make-bytevector 50)))
           (bytevector-uint-set! b 0 (- (expt 2 128) 3)
                                 (endianness little) 16)
           (equal? (bytevector-sint-ref b 0 (endianness little) 16)
                   -3)))

(test-assert "bytevector-sint-set! [small]"
         (let ((b (make-bytevector 3)))
           (bytevector-sint-set! b 0 -16 (endianness big) 2)
           (bytevector-sint-set! b 1 -16 (endianness little) 2)
           (equal? (bytevector->u8-list b)
                   '(#xff #xf0 #xff))))

(test-assert "equal?"
         (let ((bv1 (u8-list->bytevector (iota 123)))
               (bv2 (u8-list->bytevector (iota 123))))
           (equal? bv1 bv2)))

(test-end "2.3 Operations on Bytes and Octets")

(test-begin "2.4 Operations on Integers of Arbitrary Size")

(test-assert "bytevector->sint-list"
             (let ((b (u8-list->bytevector '(1 2 3 255 1 2 1 2))))
               (equal? (bytevector->sint-list b (endianness little) 2)
                       '(513 -253 513 513))))

(test-assert "bytevector->uint-list"
             (let ((b (u8-list->bytevector '(2 1 255 3 2 1 2 1))))
               (equal? (bytevector->uint-list b (endianness big) 2)
                       '(513 65283 513 513))))

(test-assert "bytevector->uint-list [empty]"
             (let ((b (make-bytevector 0)))
               (null? (bytevector->uint-list b (endianness big) 2))))

(test-assert "{sint,uint}-list->bytevector"
             (let ((b1 (sint-list->bytevector '(513 -253 513 513)
                                              (endianness little) 2))
                   (b2 (uint-list->bytevector '(513 65283 513 513)
                                              (endianness little) 2))
                   (b3 (u8-list->bytevector '(1 2 3 255 1 2 1 2))))
               (and (bytevector=? b1 b2)
                    (bytevector=? b2 b3))))

(test-assert "sint-list->bytevector [limits]"
             (bytevector=? (sint-list->bytevector '(-32768 32767)
                                                  (endianness big) 2)
                           (let ((bv (make-bytevector 4)))
                             (bytevector-u8-set! bv 0 #x80)
                             (bytevector-u8-set! bv 1 #x00)
                             (bytevector-u8-set! bv 2 #x7f)
                             (bytevector-u8-set! bv 3 #xff)
                             bv)))

(test-end "2.4 Operations on Integers of Arbitrary Size")

(test-begin "2.5 Operations on 16-Bit Integers")

(test-assert "bytevector-u16-ref"
             (let ((b (u8-list->bytevector
                        '(255 255 255 255 255 255 255 255
                          255 255 255 255 255 255 255 253))))
               (and (equal? (bytevector-u16-ref b 14 (endianness little))
                            #xfdff)
                    (equal? (bytevector-u16-ref b 14 (endianness big))
                            #xfffd))))

(test-assert "bytevector-s16-ref"
             (let ((b (u8-list->bytevector
                        '(255 255 255 255 255 255 255 255
                          255 255 255 255 255 255 255 253))))
               (and (equal? (bytevector-s16-ref b 14 (endianness little))
                            -513)
                    (equal? (bytevector-s16-ref b 14 (endianness big))
                            -3))))

(test-assert "bytevector-s16-ref [unaligned]"
             (let ((b (u8-list->bytevector '(#xff #xf0 #xff))))
               (equal? (bytevector-s16-ref b 1 (endianness little))
                       -16)))

(test-assert "bytevector-{u16,s16}-ref"
             (let ((b (make-bytevector 2)))
               (bytevector-u16-set! b 0 44444 (endianness little))
               (and (equal? (bytevector-u16-ref b 0 (endianness little))
                            44444)
                    (equal? (bytevector-s16-ref b 0 (endianness little))
                            (- 44444 65536)))))

(test-assert "bytevector-native-{u16,s16}-{ref,set!}"
             (let ((b (make-bytevector 2)))
               (bytevector-u16-native-set! b 0 44444)
               (and (equal? (bytevector-u16-native-ref b 0)
                            44444)
                    (equal? (bytevector-s16-native-ref b 0)
                            (- 44444 65536)))))

(test-assert "bytevector-s16-{ref,set!} [unaligned]"
             (let ((b (make-bytevector 3)))
               (bytevector-s16-set! b 1 -77 (endianness little))
               (equal? (bytevector-s16-ref b 1 (endianness little))
                       -77)))

(test-end "2.5 Operations on 16-Bit Integers")

(test-begin "2.6 Operations on 32-bit Integers")

(test-assert "bytevector-u32-ref"
             (let ((b (u8-list->bytevector
                        '(255 255 255 255 255 255 255 255
                          255 255 255 255 255 255 255 253))))
               (and (equal? (bytevector-u32-ref b 12 (endianness little))
                            #xfdffffff)
                    (equal? (bytevector-u32-ref b 12 (endianness big))
                            #xfffffffd))))

(test-assert "bytevector-s32-ref"
             (let ((b (u8-list->bytevector
                        '(255 255 255 255 255 255 255 255
                          255 255 255 255 255 255 255 253))))
               (and (equal? (bytevector-s32-ref b 12 (endianness little))
                            -33554433)
                    (equal? (bytevector-s32-ref b 12 (endianness big))
                            -3))))

(test-assert "bytevector-{u32,s32}-ref"
             (let ((b (make-bytevector 4)))
               (bytevector-u32-set! b 0 2222222222 (endianness little))
               (and (equal? (bytevector-u32-ref b 0 (endianness little))
                            2222222222)
                    (equal? (bytevector-s32-ref b 0 (endianness little))
                            (- 2222222222 (expt 2 32))))))

(test-assert "bytevector-{u32,s32}-native-{ref,set!}"
             (let ((b (make-bytevector 4)))
               (bytevector-u32-native-set! b 0 2222222222)
               (and (equal? (bytevector-u32-native-ref b 0)
                            2222222222)
                    (equal? (bytevector-s32-native-ref b 0)
                            (- 2222222222 (expt 2 32))))))

(test-end "2.6 Operations on 32-bit Integers")

(test-begin "2.7 Operations on 64-bit Integers")

(test-assert "bytevector-u64-ref"
             (let ((b (u8-list->bytevector
                        '(255 255 255 255 255 255 255 255
                          255 255 255 255 255 255 255 253))))
               (and (equal? (bytevector-u64-ref b 8 (endianness little))
                            #xfdffffffffffffff)
                    (equal? (bytevector-u64-ref b 8 (endianness big))
                            #xfffffffffffffffd))))

(test-assert "bytevector-s64-ref"
             (let ((b (u8-list->bytevector
                        '(255 255 255 255 255 255 255 255
                          255 255 255 255 255 255 255 253))))
               (and (equal? (bytevector-s64-ref b 8 (endianness little))
                            -144115188075855873)
                    (equal? (bytevector-s64-ref b 8 (endianness big))
                            -3))))

(test-assert "bytevector-{u64,s64}-ref"
             (let ((b (make-bytevector 8))
                   (big 9333333333333333333))
               (bytevector-u64-set! b 0 big (endianness little))
               (and (equal? (bytevector-u64-ref b 0 (endianness little))
                            big)
                    (equal? (bytevector-s64-ref b 0 (endianness little))
                            (- big (expt 2 64))))))

(test-assert "bytevector-{u64,s64}-native-{ref,set!}"
             (let ((b (make-bytevector 8))
                   (big 9333333333333333333))
               (bytevector-u64-native-set! b 0 big)
               (and (equal? (bytevector-u64-native-ref b 0)
                            big)
                    (equal? (bytevector-s64-native-ref b 0)
                            (- big (expt 2 64))))))

(test-assert "ref/set! with zero"
             (let ((b (make-bytevector 8)))
               (bytevector-s64-set! b 0 -1 (endianness big))
               (bytevector-u64-set! b 0  0 (endianness big))
               (= 0 (bytevector-u64-ref b 0 (endianness big)))))

(test-end "2.7 Operations on 64-bit Integers")

(test-begin "2.8 Operations on IEEE-754 Representations")

(test-assert "single, little endian"
             ;; http://bugs.gnu.org/11310
             (let ((b (make-bytevector 4)))
               (bytevector-ieee-single-set! b 0 1.0 (endianness little))
               (equal? (u8-list->bytevector '(0 0 128 63)) b)))

(test-assert "single, big endian"
             ;; http://bugs.gnu.org/11310
             (let ((b (make-bytevector 4)))
               (bytevector-ieee-single-set! b 0 1.0 (endianness big))
               (equal? (u8-list->bytevector '(63 128 0 0)) b)))

(test-assert "bytevector-ieee-single-native-{ref,set!}"
             (let ((b (make-bytevector 4))
                   (number 3.00))
               (bytevector-ieee-single-native-set! b 0 number)
               (equal? (bytevector-ieee-single-native-ref b 0)
                       number)))

(test-assert "bytevector-ieee-single-{ref,set!}"
             (let ((b (make-bytevector 8))
                   (number 3.14))
               (bytevector-ieee-single-set! b 0 number (endianness little))
               (bytevector-ieee-single-set! b 4 number (endianness big))
               (equal? (bytevector-ieee-single-ref b 0 (endianness little))
                       (bytevector-ieee-single-ref b 4 (endianness big)))))

(test-assert "bytevector-ieee-single-{ref,set!} [unaligned]"
             (let ((b (make-bytevector 9))
                   (number 3.14))
               (bytevector-ieee-single-set! b 1 number (endianness little))
               (bytevector-ieee-single-set! b 5 number (endianness big))
               (equal? (bytevector-ieee-single-ref b 1 (endianness little))
                       (bytevector-ieee-single-ref b 5 (endianness big)))))

(test-assert "double, little endian"
             ;; http://bugs.gnu.org/11310
             (let ((b (make-bytevector 8)))
               (bytevector-ieee-double-set! b 0 1.0 (endianness little))
               (equal? (u8-list->bytevector '(0 0 0 0 0 0 240 63)) b)))

(test-assert "double, big endian"
             ;; http://bugs.gnu.org/11310
             (let ((b (make-bytevector 8)))
               (bytevector-ieee-double-set! b 0 1.0 (endianness big))
               (equal? (u8-list->bytevector '(63 240 0 0 0 0 0 0)) b)))

(test-assert "bytevector-ieee-double-native-{ref,set!}"
             (let ((b (make-bytevector 8))
                   (number 3.14))
               (bytevector-ieee-double-native-set! b 0 number)
               (equal? (bytevector-ieee-double-native-ref b 0)
                       number)))

(test-assert "bytevector-ieee-double-{ref,set!}"
             (let ((b (make-bytevector 16))
                   (number 3.14))
               (bytevector-ieee-double-set! b 0 number (endianness little))
               (bytevector-ieee-double-set! b 8 number (endianness big))
               (equal? (bytevector-ieee-double-ref b 0 (endianness little))
                       (bytevector-ieee-double-ref b 8 (endianness big)))))

(test-end "2.8 Operations on IEEE-754 Representations")

(test-begin "2.9 Operations on Strings")

(test-assert "string->utf8"
             (let* ((str  "hello, world")
                    (utf8 (string->utf8 str)))
               (and (bytevector? utf8)
                    (= (bytevector-length utf8)
                       (string-length str))
                    (equal? (string->list str)
                            (map integer->char (bytevector->u8-list utf8))))))

(test-assert "string->utf8 [latin-1]"
             (let* ((str  "hé, ça va bien ?")
                    (utf8 (string->utf8 str)))
               (and (bytevector? utf8)
                    (= (bytevector-length utf8)
                       (+ 2 (string-length str))))))

(test-assert "string->utf16"
             (let* ((str   "hello, world")
                    (utf16 (string->utf16 str)))
               (and (bytevector? utf16)
                    (= (bytevector-length utf16)
                       (* 2 (string-length str)))
                    (equal? (string->list str)
                            (map integer->char
                                 (bytevector->uint-list utf16
                                                        (endianness big) 2))))))

(test-assert "string->utf16 [little]"
             (let* ((str   "hello, world")
                    (utf16 (string->utf16 str (endianness little))))
               (and (bytevector? utf16)
                    (= (bytevector-length utf16)
                       (* 2 (string-length str)))
                    (equal? (string->list str)
                            (map integer->char
                                 (bytevector->uint-list utf16
                                                        (endianness little) 2))))))


(test-assert "string->utf32"
             (let* ((str   "hello, world")
                    (utf32 (string->utf32 str)))
               (and (bytevector? utf32)
                    (= (bytevector-length utf32)
                       (* 4 (string-length str)))
                    (equal? (string->list str)
                            (map integer->char
                                 (bytevector->uint-list utf32
                                                        (endianness big) 4))))))

(test-assert "string->utf32 [Greek]"
             (let* ((str   "Ἄνεμοι")
                    (utf32 (string->utf32 str)))
               (and (bytevector? utf32)
                    (equal? (bytevector->uint-list utf32 (endianness big) 4)
                            '(#x1f0c #x3bd #x3b5 #x3bc #x3bf #x3b9)))))

(test-assert "string->utf32 [little]"
             (let* ((str   "hello, world")
                    (utf32 (string->utf32 str (endianness little))))
               (and (bytevector? utf32)
                    (= (bytevector-length utf32)
                       (* 4 (string-length str)))
                    (equal? (string->list str)
                            (map integer->char
                                 (bytevector->uint-list utf32
                                                        (endianness little) 4))))))

(test-assert "utf8->string"
             (let* ((utf8  (u8-list->bytevector (map char->integer
                                                     (string->list "hello, world"))))
                    (str   (utf8->string utf8)))
               (and (string? str)
                    (= (string-length str)
                       (bytevector-length utf8))
                    (equal? (string->list str)
                            (map integer->char (bytevector->u8-list utf8))))))

(test-assert "utf8->string [latin-1]"
             (let* ((utf8  (string->utf8 "hé, ça va bien ?"))
                    (str   (utf8->string utf8)))
               (and (string? str)
                    (= (string-length str)
                       (- (bytevector-length utf8) 2)))))

(test-equal "utf8->string [replacement character]"
             '(104 105 65533)
             (map char->integer
                  (string->list (utf8->string (u8-list->bytevector '(104 105 239 191 189))))))

(test-assert "utf16->string"
             (let* ((utf16  (uint-list->bytevector (map char->integer
                                                        (string->list "hello, world"))
                                                   (endianness big) 2))
                    (str   (utf16->string utf16)))
               (and (string? str)
                    (= (* 2 (string-length str))
                       (bytevector-length utf16))
                    (equal? (string->list str)
                            (map integer->char
                                 (bytevector->uint-list utf16 (endianness big)
                                                        2))))))

(test-assert "utf16->string [little]"
             (let* ((utf16  (uint-list->bytevector (map char->integer
                                                        (string->list "hello, world"))
                                                   (endianness little) 2))
                    (str   (utf16->string utf16 (endianness little))))
               (and (string? str)
                    (= (* 2 (string-length str))
                       (bytevector-length utf16))
                    (equal? (string->list str)
                            (map integer->char
                                 (bytevector->uint-list utf16 (endianness little)
                                                        2))))))
(test-assert "utf32->string"
             (let* ((utf32  (uint-list->bytevector (map char->integer
                                                        (string->list "hello, world"))
                                                   (endianness big) 4))
                    (str   (utf32->string utf32)))
               (and (string? str)
                    (= (* 4 (string-length str))
                       (bytevector-length utf32))
                    (equal? (string->list str)
                            (map integer->char
                                 (bytevector->uint-list utf32 (endianness big)
                                                        4))))))

(test-assert "utf32->string [little]"
             (let* ((utf32  (uint-list->bytevector (map char->integer
                                                        (string->list "hello, world"))
                                                   (endianness little) 4))
                    (str   (utf32->string utf32 (endianness little))))
               (and (string? str)
                    (= (* 4 (string-length str))
                       (bytevector-length utf32))
                    (equal? (string->list str)
                            (map integer->char
                                 (bytevector->uint-list utf32 (endianness little)
                                                        4))))))

(test-end "2.9 Operations on Strings")


(test-end "srfi-4")
