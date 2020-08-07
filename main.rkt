#lang racket

(require math/bigfloat herbie/plugin "integer.rkt")

(eprintf "Loading integer support...\n")

(define ((comparator test) . args)
  (for/and ([left args] [right (cdr args)])
    (test left right)))

(define (int->ordinal x bits)
  (+ x (expt 2 (sub1 bits))))

(define (ordinal->int x bits)
  (- x (expt 2 (sub1 bits))))

;; Integer type

(define-type integer (integer? bigfloat?)
  bf bigfloat->integer)

;; Integer representation

(define-representation (integer integer (curryr valid-int? 64))
  (compose (curryr normalize-int 64) (curryr clamp-int 64) bigfloat->real)
  bf
  (curryr ordinal->int 64)
  (curryr int->ordinal 64)
  64
  (const #f))

;; Operators

(define-operator (+ +.i64 integer integer) integer
  [fl int+] [bf bf+] [ival #f]
  [nonffi int+])

(define-operator (- neg.i64 integer) integer
  [fl int-] [bf bf-] [ival #f]
  [nonffi int-])

(define-operator (- -.i64 integer integer) integer
  [fl int-] [bf bf-] [ival #f]
  [nonffi int-])

(define-operator (* *.i64 integer integer) integer
  [fl int*] [bf bf*] [ival #f]
  [nonffi int*])

(define-operator (/ /.i64 integer integer) integer
  [fl int/] [bf bf/] [ival #f]
  [nonffi int/])

;; Comparators

(define (!=-fn . args)
  (not (check-duplicates args =)))

(define (bf!=-fn . args)
  (not (check-duplicates args bf=)))

(define-operator (== ==.i64 integer integer) bool
  [itype 'integer] [otype 'bool] ; Override number of arguments
  [fl (comparator =)] [bf (comparator bf=)] [ival #f]
  [nonffi (comparator =)])

(define-operator (!= !=.i64 integer integer) bool
  [itype 'integer] [otype 'bool] ; Override number of arguments
  [fl !=-fn] [bf bf!=-fn] [ival #f]
  [nonffi !=-fn])

(define-operator (< <.i64 integer integer) bool
  [itype 'integer] [otype 'bool] ; Override number of arguments
  [fl (comparator <)] [bf (comparator bf<)] [ival #f]
  [nonffi (comparator <)])

(define-operator (> >.i64 integer integer) bool
  [itype 'integer] [otype 'bool] ; Override number of arguments
  [fl (comparator >)] [bf (comparator bf>)] [ival #f]
  [nonffi (comparator >)])

(define-operator (<= <=.i64 integer integer) bool
  [itype 'integer] [otype 'bool] ; Override number of arguments
  [fl (comparator <=)] [bf (comparator bf<=)] [ival #f]
  [nonffi (comparator <=)])

(define-operator (>= >=.i64 integer integer) bool
  [itype 'integer] [otype 'bool] ; Override number of arguments
  [fl (comparator >=)] [bf (comparator bf>=)] [ival #f]
  [nonffi (comparator >=)])