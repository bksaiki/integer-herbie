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

(define (bfremainder x mod)
  (bf- x (bf* (bfround (bf/ x mod)) mod)))

(define-operator (remainder remainder.i64 integer integer) integer
  [fl intremainder] [bf bfremainder] [ival #f]
  [nonffi intremainder])

;; Bitwise operators

(define (bfshl x y)
  (bf* x (bfexpt 2.bf y)))

(define (bfshr x y)
  (bf/ x (bfexpt 2.bf y)))

(define (bfand x y)
  (bf 
    (bitwise-and 
      (normalize-int (clamp-int (bigfloat->real x) 1024) 1024)
      (normalize-int (clamp-int (bigfloat->real y) 1024) 1024))))

(define (bfor x y)
  (bf 
    (bitwise-ior
      (normalize-int (clamp-int (bigfloat->real x) 1024) 1024)
      (normalize-int (clamp-int (bigfloat->real y) 1024) 1024))))

(define (bfxor x y)
  (bf 
    (bitwise-and 
      (normalize-int (clamp-int (bigfloat->real x) 1024) 1024)
      (normalize-int (clamp-int (bigfloat->real y) 1024) 1024))))

(define-operator (shl shl.i64 integer integer) integer
  [fl intshl] [bf bfshl] [ival #f]
  [nonffi intshl])

(define-operator (shr shr.i64 integer integer) integer
  [fl intshr] [bf bfshr] [ival #f]
  [nonffi intshr])

(define-operator (and and.i64 integer integer) integer
  [fl intand] [bf bfand] [ival #f]
  [nonffi intand])

(define-operator (or or.i64 integer integer) integer
  [fl intor] [bf bfor] [ival #f]
  [nonffi intor])

(define-operator (xor xor.i64 integer integer) integer
  [fl intxor] [bf bfxor] [ival #f]
  [nonffi intxor])

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

;; Rewrite rules

(define-ruleset id-i64 (arithmetic simplify integer)
  #:type ([a integer])
  [+i64-lft-identity-reduce     (+.i64 0 a)     a]
  [+i64-rgt-identity-reduce     (+.i64 a 0)     a]
  [-i64-rgt-identity-reduce     (-.i64 a 0)     a]
  [*i64-lft-identity-reduce     (*.i64 1 a)     a]
  [*i64-rgt-identity-reduce     (*.i64 a 1)     a]
  [/i64-rgt-identity-reduce     (/.i64 a 1)     a])

(define-ruleset unid-i64 (arithmetic integer)
  #:type ([a integer])
  [+i64-lft-identity-expand    a    (+.i64 0 a)]
  [+i64-rgt-identity-expand    a    (+.i64 a 0)]
  [-i64-rgt-identity-expand    a    (-.i64 a 0)]
  [*i64-lft-identity-expand    a    (*.i64 1 a)]
  [*i64-rgt-identity-expand    a    (*.i64 a 1)]
  [/i64-rgt-identity-expand    a    (/.i64 a 1)])

(define-ruleset average-i64 (arithmetic integer)
  #:type ([a integer] [b integer])
  [i64-avg2   (/.i64 (+.i64 a b) 2)  (if (and (<.i64 (+.i64 a b) 0) (==.i64 (remainder.i64 (+.i64 a b) 2) 1))
                                         (+.i64 (+.i64 (and.i64 a b) (shr.i64 (xor.i64 a b) 1)) 1)
                                         (+.i64 (and.i64 a b) (shr.i64 (xor.i64 a b) 1)))])