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

;; Mixed integer-float operations

(require ffi/unsafe herbie/syntax/syntax herbie/errors) ; Need access to Herbie internals for this
(define-syntax (define-operator/libm stx)
  (syntax-case stx (real libm)
    [(_ (op opf64 opf32 atypes ...) rtype [libm id_d id_f] [key value] ...)
     (let ([atypes* (map syntax-e (cdddr (syntax-e (cadr (syntax-e stx)))))]
           [rtype* (syntax-e (caddr (syntax-e stx)))]
           [ffi-types (λ (args type)
                        (if (list? args) 
                            (for/list ([arg args]) (if (equal? arg 'integer) '_int type))
                            (if (equal? args 'integer) '_int type)))]
           [param-types (λ (args type)
                          (if (list? args) 
                            (for/list ([arg args]) (if (equal? arg 'real) type arg))
                            (if (equal? args 'real) type args)))])
       #`(begin
           (define (fallback prec . args)
             (warn 'fallback #:url "faq.html#native-ops"
                   "native `~a` not supported on your system, using fallback; ~a"
                   'op
                   "use --disable precision:fallback to disable fallbacks")
             (match prec
              ['double (apply (operator-info 'opf64 'nonffi) args)]
              ['float (apply (operator-info 'opf32 'nonffi) args)]))
           (define double-proc 
             (get-ffi-obj 'id_d #f (_fun #,@(ffi-types atypes* #'_double) -> #,(ffi-types rtype* #'_double))
                          (lambda () (*unknown-d-ops* (cons 'opf64 (*unknown-d-ops*))) (curry fallback #'double))))
           (define float-proc 
             (get-ffi-obj 'id_f #f (_fun #,@(ffi-types atypes* #'_float) -> #,(ffi-types rtype* #'_float))
                          (lambda () (*unknown-f-ops* (cons 'opf32 (*unknown-f-ops*))) (curry fallback #'float))))
           (define-operator (op opf64 #,@(param-types atypes* 'binary64)) #,(param-types rtype* 'binary64)
             [fl (λ args (apply double-proc args))]
             [key value] ...)
           (define-operator (op opf32 #,@(param-types atypes* 'binary32)) #,(param-types rtype* 'binary32)
             [fl (λ args (apply float-proc args))]
             [key value] ...)
       ))]))

(define (from-bigfloat bff)
  (λ args (bigfloat->flonum (apply bff (map bf args)))))

(define (bfldexp x y)
  (bf* x (bfexpt 2.bf y)))

(define (ldexp x y)
  (* x (expt 2 y)))

(define-operator/libm (ldexp ldexp.f64 ldexp.f32 real integer) real
  [libm ldexp ldexpf] [bf bfldexp] [ival #f]
  [nonffi ldexp])

(define (bfilogb x)
  (bigfloat->integer (bfround (bflog2 x))))

(define (ilogb x)
  (round (log x 2)))

(define-operator/libm (ilogb ilogb.f64 ilogb.f32 real) integer
  [libm ilogb ilogbf] [bf bfilogb] [ival #f]
  [nonffi ilogb])

(define (bf-bessel f) ; n is an integer
  (λ (n x) (f (bigfloat->integer (bfround n)) x)))

(define-operator/libm (jn jn.f64 jn.f32 integer real) real
  [libm jn jnf] [bf (bf-bessel bfbesj)] [ival #f]
  [nonffi (from-bigfloat (bf-bessel bfbesj))])

(define-operator/libm (yn yn.f64 yn.f32 integer real) real
  [libm yn ynf] [bf (bf-bessel bfbesy)] [ival #f]
  [nonffi (from-bigfloat (bf-bessel bfbesy))])

;; Rewrite rules

(define-ruleset associativity-i64 (arithmetic simplify)
  #:type ([a integer] [b integer] [c integer])
  [i64-associate-+r+     (+.i64 a (+.i64 b c))         (+.i64 (+.i64 a b) c)]
  [i64-associate-+l+     (+.i64 (+.i64 a b) c)         (+.i64 a (+.i64 b c))]
  [i64-associate-+r-     (+.i64 a (-.i64 b c))         (-.i64 (+.i64 a b) c)]
  [i64-associate-+l-     (+.i64 (-.i64 a b) c)         (-.i64 a (-.i64 b c))]
  [i64-associate--r+     (-.i64 a (+.i64 b c))         (-.i64 (-.i64 a b) c)]
  [i64-associate--l+     (-.i64 (+.i64 a b) c)         (+.i64 a (-.i64 b c))]
  [i64-associate--l-     (-.i64 (-.i64 a b) c)         (-.i64 a (+.i64 b c))]
  [i64-associate--r-     (-.i64 a (-.i64 b c))         (+.i64 (-.i64 a b) c)]
  [i64-associate-*r*     (*.i64 a (*.i64 b c))         (*.i64 (*.i64 a b) c)]
  [i64-associate-*l*     (*.i64 (*.i64 a b) c)         (*.i64 a (*.i64 b c))]
  [i64-associate-*r/     (*.i64 a (/.i64 b c))         (/.i64 (*.i64 a b) c)]
  [i64-associate-*l/     (*.i64 (/.i64 a b) c)         (/.i64 (*.i64 a c) b)]
  [i64-associate-/r*     (/.i64 a (*.i64 b c))         (/.i64 (/.i64 a b) c)]
  [i64-associate-/l*     (/.i64 (*.i64 b c) a)         (/.i64 b (/.i64 a c))]
  [i64-associate-/r/     (/.i64 a (/.i64 b c))         (*.i64 (/.i64 a b) c)]
  [i64-associate-/l/     (/.i64 (/.i64 b c) a)         (/.i64 b (*.i64 a c))])

(define-ruleset distributivity-i64 (arithmetic simplify)
  #:type ([a integer] [b integer] [c integer])
  [i64-distribute-lft-in      (*.i64 a (+.i64 b c))             (+.i64 (*.i64 a b) (*.i64 a c))]
  [i64-distribute-rgt-in      (*.i64 a (+.i64 b c))             (+.i64 (*.i64 b a) (*.i64 c a))]
  [i64-distribute-lft-out     (+.i64 (*.i64 a b) (*.i64 a c))   (*.i64 a (+.i64 b c))]
  [i64-distribute-lft-out--   (-.i64 (*.i64 a b) (*.i64 a c))   (*.i64 a (-.i64 b c))]
  [i64-distribute-rgt-out     (+.i64 (*.i64 b a) (*.i64 c a))   (*.i64 a (+.i64 b c))]
  [i64-distribute-rgt-out--   (-.i64 (*.i64 b a) (*.i64 c a))   (*.i64 a (-.i64 b c))]
  [i64-distribute-lft1-in     (+.i64 (*.i64 b a) a)             (*.i64 (+.i64 b 1) a)]
  [i64-distribute-rgt1-in     (+.i64 a (*.i64 c a))             (*.i64 (+.i64 c 1) a)])

(define-ruleset id-reduce-i64 (arithmetic simplify)
  #:type ([a integer])
  [i64-remove-double-div  (/.i64 1 (/.i64 1 a))     a]
  [i64-rgt-mult-inverse   (*.i64 a (/.i64 1 a))     1]
  [i64-lft-mult-inverse   (*.i64 (/.i64 1 a) a)     1]
  [i64-+-inverses         (-.i64 a a)               0]
  [i64-*-inverses         (/.i64 a a)               1]
  [i64-div0               (/.i64 0 a)               0]
  [i64-mul0-lft           (*.i64 0 a)               0]
  [i64-mul0-rgt           (*.i64 a 0)               0])


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

(define-ruleset mixed-int-f64 (arithmetic)
  #:type ([a binary64] [b integer])
  [f64-ldexp-def  (*.f64 a (pow.f64 2 b))       (ldexp.f64 a b)])

(define-ruleset mixed-int-f32 (arithmetic)
  #:type ([a binary32] [b integer])
  [f32-ldexp-def  (*.f32 a (pow.f32 2 b))       (ldexp.f32 a b)])