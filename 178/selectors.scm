;;; SPDX-FileCopyrightText: 2020 Wolfgang Corcoran-Mathe <wcm@sigwinch.xyz>
;;;
;;; SPDX-License-Identifier: MIT

(define (check-selectors)
  (check (bitvector-length (bitvector))             => 0)
  (check (bitvector-length (bitvector 1 0 1 0))     => 4)
  (check (bitvector-ref/int (bitvector 1 0 1 0) 0)  => 1)
  (check (bitvector-ref/int (bitvector 1 0 1 0) 3)  => 0)
  (check (bitvector-ref/bool (bitvector 1 0 1 0) 0) => #t)
  (check (bitvector-ref/bool (bitvector 1 0 1 0) 3) => #f))

