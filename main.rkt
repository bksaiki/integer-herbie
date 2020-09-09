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

(define-representation (integer integer (curryr valid-int? 32))
  (compose (curryr normalize-int 32) (curryr clamp-int 32) bigfloat->real)
  bf
  (compose (curryr normalize-int 32) (curryr ordinal->int 32))
  (compose (curryr int->ordinal 32) (curryr clamp-int 32))
  32
  (const #f))

;; Operators

(define-operator (+ +.i32 integer integer) integer
  [fl int+] [bf bf+] [ival #f]
  [nonffi int+])

(define-operator (- neg.i32 integer) integer
  [fl int-] [bf bf-] [ival #f]
  [nonffi int-])

(define-operator (- -.i32 integer integer) integer
  [fl int-] [bf bf-] [ival #f]
  [nonffi int-])

(define-operator (* *.i32 integer integer) integer
  [fl int*] [bf bf*] [ival #f]
  [nonffi int*])

(define-operator (/ /.i32 integer integer) integer
  [fl int/] [bf bf/] [ival #f]
  [nonffi int/])

(define (bfremainder x mod)
  (bf- x (bf* (bfround (bf/ x mod)) mod)))

(define-operator (remainder remainder.i32 integer integer) integer
  [fl intremainder] [bf bfremainder] [ival #f]
  [nonffi intremainder])

(define-operator (fabs abs.i32 integer) integer 
  [fl intabs] [bf bfabs] [ival #f]
  [nonffi intabs])

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

(define-operator (shl shl.i32 integer integer) integer
  [fl intshl] [bf bfshl] [ival #f]
  [nonffi intshl])

(define-operator (shr shr.i32 integer integer) integer
  [fl intshr] [bf bfshr] [ival #f]
  [nonffi intshr])

(define-operator (and and.i32 integer integer) integer
  [fl intand] [bf bfand] [ival #f]
  [nonffi intand])

(define-operator (or or.i32 integer integer) integer
  [fl intor] [bf bfor] [ival #f]
  [nonffi intor])

(define-operator (xor xor.i32 integer integer) integer
  [fl intxor] [bf bfxor] [ival #f]
  [nonffi intxor])

;; Comparators

(define (!=-fn . args)
  (not (check-duplicates args =)))

(define (bf!=-fn . args)
  (not (check-duplicates args bf=)))

(define-operator (== ==.i32 integer integer) bool
  [itype 'integer] [otype 'bool] ; Override number of arguments
  [fl (comparator =)] [bf (comparator bf=)] [ival #f]
  [nonffi (comparator =)])

(define-operator (!= !=.i32 integer integer) bool
  [itype 'integer] [otype 'bool] ; Override number of arguments
  [fl !=-fn] [bf bf!=-fn] [ival #f]
  [nonffi !=-fn])

(define-operator (< <.i32 integer integer) bool
  [itype 'integer] [otype 'bool] ; Override number of arguments
  [fl (comparator <)] [bf (comparator bf<)] [ival #f]
  [nonffi (comparator <)])

(define-operator (> >.i32 integer integer) bool
  [itype 'integer] [otype 'bool] ; Override number of arguments
  [fl (comparator >)] [bf (comparator bf>)] [ival #f]
  [nonffi (comparator >)])

(define-operator (<= <=.i32 integer integer) bool
  [itype 'integer] [otype 'bool] ; Override number of arguments
  [fl (comparator <=)] [bf (comparator bf<=)] [ival #f]
  [nonffi (comparator <=)])

(define-operator (>= >=.i32 integer integer) bool
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
           (define-operator (op opf64 #,@(param-types atypes* 'binary32)) #,(param-types rtype* 'binary32)
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

(define-ruleset commutativity-i32 (arithmetic simplify integer)
  #:type ([a integer] [b integer])
  [i32-commutative+     (+.i32 a b)       (+.i32 b a)]
  [i32-commutative*     (*.i32 a b)       (*.i32 b a)])

(define-ruleset add2-i32 (arithmetic simplify integer)
  #:type ([a integer])
  [i32-add2-mul   (+.i32 a a)     (*.i32 2 a)]
  [i32-add2-shl   (+.i32 a a)     (shl.i32 a 1)])

(define-ruleset associativity-i32 (arithmetic simplify integer)
  #:type ([a integer] [b integer] [c integer])
  [i32-associate-+r+     (+.i32 a (+.i32 b c))         (+.i32 (+.i32 a b) c)]
  [i32-associate-+l+     (+.i32 (+.i32 a b) c)         (+.i32 a (+.i32 b c))]
  [i32-associate-+r-     (+.i32 a (-.i32 b c))         (-.i32 (+.i32 a b) c)]
  [i32-associate-+l-     (+.i32 (-.i32 a b) c)         (-.i32 a (-.i32 b c))]
  [i32-associate--r+     (-.i32 a (+.i32 b c))         (-.i32 (-.i32 a b) c)]
  [i32-associate--l+     (-.i32 (+.i32 a b) c)         (+.i32 a (-.i32 b c))]
  [i32-associate--l-     (-.i32 (-.i32 a b) c)         (-.i32 a (+.i32 b c))]
  [i32-associate--r-     (-.i32 a (-.i32 b c))         (+.i32 (-.i32 a b) c)]
  [i32-associate-*r*     (*.i32 a (*.i32 b c))         (*.i32 (*.i32 a b) c)]
  [i32-associate-*l*     (*.i32 (*.i32 a b) c)         (*.i32 a (*.i32 b c))]
  [i32-associate-*r/     (*.i32 a (/.i32 b c))         (/.i32 (*.i32 a b) c)]
  [i32-associate-*l/     (*.i32 (/.i32 a b) c)         (/.i32 (*.i32 a c) b)]
  [i32-associate-/r*     (/.i32 a (*.i32 b c))         (/.i32 (/.i32 a b) c)]
  [i32-associate-/l*     (/.i32 (*.i32 b c) a)         (/.i32 b (/.i32 a c))]
  [i32-associate-/r/     (/.i32 a (/.i32 b c))         (*.i32 (/.i32 a b) c)]
  [i32-associate-/l/     (/.i32 (/.i32 b c) a)         (/.i32 b (*.i32 a c))])

(define-ruleset distributivity-i32 (arithmetic simplify integer)
  #:type ([a integer] [b integer] [c integer])
  [i32-distribute-lft-in      (*.i32 a (+.i32 b c))             (+.i32 (*.i32 a b) (*.i32 a c))]
  [i32-distribute-rgt-in      (*.i32 a (+.i32 b c))             (+.i32 (*.i32 b a) (*.i32 c a))]
  [i32-distribute-lft-out     (+.i32 (*.i32 a b) (*.i32 a c))   (*.i32 a (+.i32 b c))]
  [i32-distribute-lft-out--   (-.i32 (*.i32 a b) (*.i32 a c))   (*.i32 a (-.i32 b c))]
  [i32-distribute-rgt-out     (+.i32 (*.i32 b a) (*.i32 c a))   (*.i32 a (+.i32 b c))]
  [i32-distribute-rgt-out--   (-.i32 (*.i32 b a) (*.i32 c a))   (*.i32 a (-.i32 b c))]
  [i32-distribute-lft1-in     (+.i32 (*.i32 b a) a)             (*.i32 (+.i32 b 1) a)]
  [i32-distribute-rgt1-in     (+.i32 a (*.i32 c a))             (*.i32 (+.i32 c 1) a)])

(define-ruleset id-reduce-i32 (arithmetic simplify integer)
  #:type ([a integer])
  [i32-remove-double-div  (/.i32 1 (/.i32 1 a))     a]
  [i32-rgt-mult-inverse   (*.i32 a (/.i32 1 a))     1]
  [i32-lft-mult-inverse   (*.i32 (/.i32 1 a) a)     1]
  [i32-+-inverses         (-.i32 a a)               0]
  [i32-*-inverses         (/.i32 a a)               1]
  [i32-div0               (/.i32 0 a)               0]
  [i32-mul0-lft           (*.i32 0 a)               0]
  [i32-mul0-rgt           (*.i32 a 0)               0])

(define-ruleset id-i32 (arithmetic simplify integer)
  #:type ([a integer])
  [+i32-lft-identity-reduce     (+.i32 0 a)     a]
  [+i32-rgt-identity-reduce     (+.i32 a 0)     a]
  [-i32-rgt-identity-reduce     (-.i32 a 0)     a]
  [*i32-lft-identity-reduce     (*.i32 1 a)     a]
  [*i32-rgt-identity-reduce     (*.i32 a 1)     a]
  [/i32-rgt-identity-reduce     (/.i32 a 1)     a])

(define-ruleset unid-i32 (arithmetic integer)
  #:type ([a integer])
  [+i32-lft-identity-expand    a    (+.i32 0 a)]
  [+i32-rgt-identity-expand    a    (+.i32 a 0)]
  [-i32-rgt-identity-expand    a    (-.i32 a 0)]
  [*i32-lft-identity-expand    a    (*.i32 1 a)]
  [*i32-rgt-identity-expand    a    (*.i32 a 1)]
  [/i32-rgt-identity-expand    a    (/.i32 a 1)])

(define-ruleset average-i32 (arithmetic integer)
  #:type ([a integer] [b integer])
  [i32-avg2   (/.i32 (+.i32 a b) 2)   (+.i32 (+.i32 (and.i32 a b) (shr.i32 (xor.i32 a b) 1)) 
                                             (and.i32 (neg.i32 (shr.i32 (+.i32 (and.i32 a b) (shr.i32 (xor.i32 a b) 1)) 63))
                                                      (xor.i32 a b)))])

(define-ruleset saturate-i32 (arithmetic integer)
  #:type ([a integer] [b integer])
  [i32-saturate-add+  (+.i32 a b)     2147483647]
  [i32-saturate-add-  (+.i32 a b)     -2147483648]
  [i32-saturate-sub+  (-.i32 a b)     2147483647]
  [i32-saturate-sub-  (-.i32 a b)     -2147483648]
  [i32-saturate-mul+  (*.i32 a b)     2147483647]
  [i32-saturate-mul-  (*.i32 a b)     -2147483648])

(define-ruleset mixed-int-f64 (arithmetic)
  #:type ([a binary32] [b integer])
  [f64-ldexp-def  (*.f64 a (pow.f64 2 b))       (ldexp.f64 a b)])

(define-ruleset mixed-int-f32 (arithmetic)
  #:type ([a binary32] [b integer])
  [f32-ldexp-def  (*.f32 a (pow.f32 2 b))       (ldexp.f32 a b)])