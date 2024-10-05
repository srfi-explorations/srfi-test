;; Tests are from Chibi scheme, modified to use SRFI-64
;; https://github.com/ashinn/chibi-scheme/blob/master/lib/srfi/33/test.sld
;; Copyright from:
;; https://github.com/ashinn/chibi-scheme/blob/master/COPYING

;;; Copyright (c) 2009-2021 Alex Shinn
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;; 3. The name of the author may not be used to endorse or promote products
;;;    derived from this software without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
;;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;;; IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
;;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


(test-begin "srfi-33")

(test-equal 0 (bitwise-and #b0 #b1))
(test-equal 1 (bitwise-and #b1 #b1))
(test-equal 0 (bitwise-and #b1 #b10))
(test-equal #b10 (bitwise-and #b11 #b10))
(test-equal #b101 (bitwise-and #b101 #b111))
(test-equal #b111 (bitwise-and -1 #b111))
(test-equal #b110 (bitwise-and -2 #b111))
(test-equal 3769478 (bitwise-and -4290775858 1694076839))
(test-equal 1680869008 (bitwise-and -193073517 1689392892))
(test-equal -4294967295 (bitwise-ior 1 (- -1 #xffffffff)))
(test-equal -18446744073709551615 (bitwise-ior 1 (- -1 #xffffffffffffffff)))
(test-equal -4294967126 (bitwise-xor #b10101010 (- -1 #xffffffff)))
(test-equal -18446744073709551446 (bitwise-xor #b10101010 (- -1 #xffffffffffffffff)))
(test-equal -2600468497 (bitwise-ior 1694076839 -4290775858))
(test-equal -184549633 (bitwise-ior -193073517 1689392892))
(test-equal -2604237975 (bitwise-xor 1694076839 -4290775858))
(test-equal -1865418641 (bitwise-xor -193073517 1689392892))
(test-equal 3769478 (bitwise-and 1694076839 -4290775858))
(test-equal 1680869008 (bitwise-and -193073517 1689392892))

(test-equal 1 (arithmetic-shift 1 0))
(test-equal 2 (arithmetic-shift 1 1))
(test-equal 4 (arithmetic-shift 1 2))
(test-equal 8 (arithmetic-shift 1 3))
(test-equal 16 (arithmetic-shift 1 4))
(test-equal (expt 2 31) (arithmetic-shift 1 31))
(test-equal (expt 2 32) (arithmetic-shift 1 32))
(test-equal (expt 2 33) (arithmetic-shift 1 33))
(test-equal (expt 2 63) (arithmetic-shift 1 63))
(test-equal (expt 2 64) (arithmetic-shift 1 64))
(test-equal (expt 2 65) (arithmetic-shift 1 65))
(test-equal (expt 2 127) (arithmetic-shift 1 127))
(test-equal (expt 2 128) (arithmetic-shift 1 128))
(test-equal (expt 2 129) (arithmetic-shift 1 129))
(test-equal 3028397001194014464 (arithmetic-shift 11829675785914119 8))

(test-equal -1 (arithmetic-shift -1 0))
(test-equal -2 (arithmetic-shift -1 1))
(test-equal -4 (arithmetic-shift -1 2))
(test-equal -8 (arithmetic-shift -1 3))
(test-equal -16 (arithmetic-shift -1 4))
(test-equal (- (expt 2 31)) (arithmetic-shift -1 31))
(test-equal (- (expt 2 32)) (arithmetic-shift -1 32))
(test-equal (- (expt 2 33)) (arithmetic-shift -1 33))
(test-equal (- (expt 2 63)) (arithmetic-shift -1 63))
(test-equal (- (expt 2 64)) (arithmetic-shift -1 64))
(test-equal (- (expt 2 65)) (arithmetic-shift -1 65))
(test-equal (- (expt 2 127)) (arithmetic-shift -1 127))
(test-equal (- (expt 2 128)) (arithmetic-shift -1 128))
(test-equal (- (expt 2 129)) (arithmetic-shift -1 129))

(test-equal 0 (arithmetic-shift 1 -63))
(test-equal 0 (arithmetic-shift 1 -64))
(test-equal 0 (arithmetic-shift 1 -65))

(test-equal #x1000000000000000100000000000000000000000000000000
    (arithmetic-shift #x100000000000000010000000000000000 64))
(test-equal #x8e73b0f7da0e6452c810f32b809079e5
    (arithmetic-shift #x8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b -64))

(test-not (bit-set? 64 1))
(test-assert (bit-set? 64 #x10000000000000000))

(test-equal 3 (bitwise-merge 1 1 2))
(test-equal #b00110011 (bitwise-merge #b00111100 #b11110000 #b00001111))

(test-end "srfi-33")
