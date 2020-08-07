#lang racket

(provide (all-defined-out))

;; Miscellaneous

; Emulate machine signed integers
(define (normalize-int x bits)
  (define shift (expt 2 (sub1 bits)))
  (- (modulo (+ (round x) shift) (expt 2 bits)) shift))

(define (valid-int? x bits)
  (-> any/c exact-positive-integer? boolean?)
  (match x
   [(? integer?)
    (define v (expt 2 (sub1 bits)))
    (< (- v) x (sub1 v))]
   [_ #f]))

(define (clamp-int x bits)
  (define v (expt 2 (sub1 bits)))
  (cond
   [(> x (sub1 v)) (sub1 v)]
   [(< x (- v)) (- v)]
   [else x]))

;; Operators

(define (int+ arg #:bits [bits 64] . rest)
  (normalize-int (apply + arg rest) bits))

(define (int- arg #:bits [bits 64] . rest)
  (normalize-int (apply - arg rest) bits))

(define (int* arg #:bits [bits 64] . rest)
  (normalize-int (apply * arg rest) bits))

(define (int/ arg #:bits [bits 64] . rest)
  (normalize-int (apply / arg rest) bits))