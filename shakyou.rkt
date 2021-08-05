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


(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

#|
2. データによる抽象の構築
---------------------

- 合成データ，データ抽象，抽象化の壁，データ主導プログラミング
|#

;; 有理数
;; make-rat, numer, denom がすでに使えると仮定する(wishful thinkng)
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (numer y) (denom x))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

; 点　線分
(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (make-segment p q) (cons p q))
(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))
  

;; 区間算術
(define (make-interval a b) (cons a b))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound x))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))
(define (sub-interval x y)
  (add-interval x
                (make-interval (- (upper-bound y))
                               (- (lower-bound y)))))
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))
(define (make-center-percent c p)
  (make-interval (- c (* c p)) (+ c (* c p))))
(define (percent i)
  (/ (width i) (center i)))

; list
;; cdr down, cons up
(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))
(define (length items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))
(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1)
            (append (cdr list1) list2))))

(define (last-pair items)
  (if (null? (cdr items))
      items
      (last-pair (cdr items))))
(define (reverse items)
  (if (null? items)
      null
      (append (reverse (cdr items))
              (list (car items)))))

(define (map proc items)
  (if (null? items)
      null
      (cons (proc (car items))
            (map proc (cdr items)))))
;; tree
(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))
(define (tree-map proc tree)
  (cond ((null? tree) null)
        ((not (pair? (car tree)))
         (cons (proc (car tree))
               (tree-map proc (cdr tree))))
        (else (cons (tree-map proc (car tree))
                    (tree-map proc (cdr tree))))))

;; conventional interfaces
(define (filter predicate seq)
  (cond ((null? seq) null)
        ((predicate (car seq))
         (cons (car seq)
               (filter predicate (cdr seq))))
        (else (filter predicate (cdr seq)))))
(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq)
          (accumulate op initial (cdr seq)))))
(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))
(define (enumerate-tree tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))
(define (flatmap proc seq)
  (accumulate append null (map proc seq)))


;; 記号微分
; 代数式の表現
; 変数は記号とする．
(define (=number? exp num)
  (and (number? exp) (= exp num)))
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2) (+ a1 a2)))
        (else (list (quote +) a1 a2))))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list (quote *) m1 m2))))

(define (sum? x)
  (and (pair? x) (eq? (car x) (quote +))))
(define (addend s) (cadr s))
(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) (quote *))))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))



; 微分規則
(define (diff exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (diff (addend exp) var)
                   (diff (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (diff (multiplicand exp) var))
          (make-product (diff (multiplier exp) var)
                        (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))






  



