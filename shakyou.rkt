#lang racket
#|
[Racket]: https://racket-lang.org
[SICP]: https://mitpress.mit.edu/sites/default/files/sicp/index.html
|#

#|
1. 手続きによる抽象の構築
---------------------

- 基本式
### 組み合わせ

- 組み合わせは演算子と引数の組
- 組み合わせでない式は特殊形式

### 抽象化

- 名前付け `define`

### 特殊形式

- `define`: `(define (<name> <formal parameters>) <body>)`
- `cond`: `(cond (<p1> <e1>)[...(<pn> <en>) (else <e>)])`
- `if`: `(if <predecate> <consequent> <alternative>)`
- `and`: `(and <e1> ... <en>)`
- `or`: `(or <e1> ... <en>)`
- `let`: `(let ((<var> <exp>)...(<var> <exp>)) <body>)`
`lambda` のシンタックスシュガー

- `lambda`: `(lambda (<formal-parameters>) <body>)`

### 置き換えモデル

- 手続きの作用を考えるときのモデル
- 解釈系の実際の働きとは異なる
|#

(define (square x) (* x x))
(define (cube x) (* x x x))
(define (sum-of-square x y)
  (+ (square x) (square y)))
(define (abs x)
  (if (< x 0)
      (- x)
      x))
(define (average x y)
  (/ (+ x y) 2))
;; sqrt
(define (sqrt x)
  "y -> x/y の不動点 y"
  (fixed-point-of-transform (lambda (y) (/ x y))
                            average-damp
                            1.0))
#|
(define (sqrt x)
  "y^2-x の根 y"
  (fixed-point-of-transform (lambda (y) (- (square y) x))
                            newton-transform
                            1.0))
|#
;; factorial
(define (factorial n)
  (fact-iter 1 1 n))
(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))
;; fibonacci
(define (fib n)
  (fib-iter 1 0 n))
(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))
;; count-change
(define (count-change amount)
  (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))
;; expt
(define (expt b n)
  "逐次平方版"
  (cond ((= n 0) 1)
        ((even? n) (square (expt b (/ n 2))))
        (else (* b (expt b (- n 1))))))
(define (even? n)
  (= (remainder n 2) 0))
;; gcd
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
;; prime?
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n ) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))
;; Fermat test
(define (expmod base exp m)
  "ある数の冪乗の別の数を法とした剰余"
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))
;; sum
(define (sum f a next b)
  (if (> a b)
      0
      (+ (f a)
         (sum f (next a) next b))))

(define (inc n) (+ n 1))
(define (sum-integers a b)
  (sum identity a inc b))
(define (integral f a b dx)
  (* (sum f
          (+ a (/ dx 2.0))
          (lambda (x) (+ x dx))
          b)
     dx))

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))
(define (close-enough? x y)
  (< (abs (- x y)) 0.001))
(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))
;; fixed point
(define (fixed-point f first-guess)
  (let ((tolerance 0.00001))
    (define (close-enough? v1 v2)
      (< (abs (- v1 v2)) tolerance))
    (define (try guess)
      (let ((next (f guess)))
        (if (close-enough? guess next)
            next
            (try next))))
    (try first-guess)))

(define (average-damp f)
  (lambda (x) (average x (f x))))
;; newton's method
(define (deriv g)
  (let ((dx 0.0001))
    (lambda (x)
      (/ (- (g (+ x dx)) (g x))
         dx))))
(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

#|
(define (fixed-point f first-guess)
  (let ((tolerance 0.00001))
    (define (close-enough? v1 v2)
      (< (abs (- v1 v2)) tolerance))
    (define (try guess)
      (let ((next (f guess)))
        (if (close-enough? guess next)
            next
            (try next))))
    (try first-guess)))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))
|#

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))





