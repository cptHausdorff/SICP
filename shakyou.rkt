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

### 置き換えモデル

- 手続きの作用を考えるときのモデル
- 解釈系の実際の働きとは異なる
|#

(define (square x) (* x x))
(define (sum-of-square x y)
  (+ (square x) (square y)))
(define (abs x)
  (if (< x 0)
      (- x)
      x))
(define (average x y)
  (/ (+ x y) 2))

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

(define (factorial n)
  (fact-iter 1 1 n))
(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))
(define (fib n)
  (fib-iter 1 0 n))
(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

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

;;      問題
; Q1.3
(define (q13 x y z)
  "大きい二つの数の二乗の和"
  (cond ((or (> x y z) (> y x z))
         (sum-of-square x y))
        ((or (> x z y) (> z x y))
         (sum-of-square x z))
        ((or (> y z x) (> z y x))
         (sum-of-square y z))))
; Q1.8
(define (cbrt x)
  (define (good-enough? guess)
    (< (abs (- (* guess guess guess) x)) 0.001))
  (define (improve guess)
    (/ (+ (/ x (square guess))
          (* 2 guess))
       3))
  (define (cbrt-iter guess)
    (if (good-enough? guess)
        guess
        (cbrt-iter (improve guess))))
  (cbrt-iter 1.0))














