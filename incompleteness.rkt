#lang racket

(require rackunit)
(require profile)
(require (for-syntax syntax/parse))
(require (for-syntax racket/syntax))

; 装備を整える

; literalは何か定義しておかないとエラーになる
; とりあえず定義されていれば値はなんでもいい模様
; よくわかってない

(define ≦ #f)

(define-syntax (define-equipment stx)
  (syntax-parse stx
    ((_ name term notfound found)
     #:with fname (format-id stx "~a≦" #'name)
     #'(begin
         (define (fname max f)
           (let loop ((x 1))
             (cond ((> x max) (notfound x))
                   ((term (f x)) (found x))
                   (else (loop (+ x 1))))))
         (define-syntax (name stx)
           (syntax-parse stx
             #:literals (≦)
             [(_ v:id ≦ max:expr body:expr)
              #'(fname max (λ (v) body))]))))))

(define-equipment ∀ not (const #t) (const #f))
(define-equipment ∃ identity (const #f) (const #t))
(define-equipment Min identity (const 0) identity)

(check-true (∀ x ≦ 3 (< x 4)))
(check-false (∀ x ≦ 3 (< x 3)))
(check-true (∃ x ≦ 3 (= x 2)))
(check-false (∃ x ≦ 3 (= x 4)))
(check-eq? (Min x ≦ 3 (= x 2)) 2)
(check-eq? (Min x ≦ 3 (= x 4)) 0)

; 定数

(define c0 1)
(define cf 3)
(define cnot 5)
(define cor 7)
(define call 9)
(define clp 11)
(define crp 13)

; 変数

; 変数
; x1=(var 1 1)、y1=(var 2 1)、z3=(var 3 3)などと表記することにする
(define (var n c) (expt (P (+ 6 n)) c))

; テストはPの後で

; ⇒の定義
; これは関数だと余分な評価が走るのでマクロで
; (if x y #t) とどっちがわかりやすいかな

(define-syntax-rule (⇒ x y)
  (or (not x) y))

(check-true (⇒ #f #f))
(check-true (⇒ #f #t))
(check-false (⇒ #t #f))
(check-true (⇒ #t #t))

; 定義1

; もとの定義
;(define (CanDivide x d)
;  (∃ n ≦ x (= x (* d n))))

(define (CanDivide x d)
  (and (not (zero? d))
       (= (remainder x d) 0)))

(check-true (CanDivide 12 3))
(check-false (CanDivide 12 5))

; 定義2

; もとの定義
;(define (IsPrime x)
;  (and (> x 1)
;       (not (∃ d ≦ x (and (not (= d 1))
;                          (not (= d x))
;                          (CanDivide x d))))))

(define (IsPrime x)
  (and (> x 1)
       (let loop ((d 2))
         (cond ((> (* d d) x) #t)
               ((CanDivide x d) #f)
               (else (loop (+ d 1)))))))

(check-false (IsPrime 0))
(check-false (IsPrime 1))
(check-true (IsPrime 2))
(check-false (IsPrime 12))
(check-true (IsPrime 17))

; 定義3

; 元のソース
;(define (CanDivideByPrime x p)
;  (and (CanDivide x p) (IsPrime p)))
;
;(check-true (CanDivideByPrime 12 3))
;(check-false (CanDivideByPrime 12 5))
;(check-false (CanDivideByPrime 12 6))
;
;(define (prime n x)
;  (cond ((= n 0) 0)
;        (else (Min p ≦ x (and (< (prime (- n 1) x) p)
;                              (CanDivideByPrime x p))))))

(define (prime n x)
  (cond ((= n 0) 0)
        (else (let loop ((k 1) (cnt 0))
                (let* ((p (P k))
                      (c (if (CanDivide x p)
                            (+ cnt 1)
                            cnt)))
                  (cond ((= c n) p)
                        ((> p x) 0)
                        (else (loop (+ k 1) c))))))))

; Pを使っているのでテストは定義5で

; 定義4

(define (factorial n)
  (cond ((= n 0) 1)
        (else (* n (factorial (- n 1))))))

(check-eq? (factorial 0) 1)
(check-eq? (factorial 3) 6)

; 定義5

; 元のソース
;(define (M5 n)
;  (+ (factorial n) 1))
;
;(define (P n)
;  (cond ((= n 0) 0)
;        (else (Min p ≦ (M5 n) (and (< (P (- n 1)) p)
;                                   (IsPrime p))))))

(define primes (make-hash))
(hash-set! primes 0 0)
(hash-set! primes 1 2)

(define (P n)
  (cond ((hash-ref primes n #f))
        (else (let loop ((k (+ (P (- n 1)) 1)))
                (cond ((IsPrime k)
                       (hash-set! primes n k)
                       k)
                      (else (loop (+ k 1))))))))
; varのテスト
(check-eq? (var 1 1) 17)
(check-eq? (var 3 3) (expt 23 3))

; 定義3のテスト
(check-eq? (prime 0 2352) 0)
(check-eq? (prime 1 2352) 2)
(check-eq? (prime 2 2352) 3)
(check-eq? (prime 3 2352) 7)

; 定義5のテスト
(check-eq? (P 0) 0)
(check-eq? (P 1) 2)
(check-eq? (P 5) 11)

; 定義6 n番目の要素

; 元のソース
;(define (CanDivideByPower x n k)
;  (CanDivide x (expt (prime n x) k)))
;
;(define (elm x n)
;  (Min k ≦ x (and (CanDivideByPower x n k)
;                  (not (CanDivideByPower x n (+ k 1))))))

(define (elm x n)
  (let ((np (prime n x)))
    (let loop ((k 1) (x x))
      (cond ((not (CanDivide x np)) (- k 1))
            (else (loop (+ k 1) (/ x np)))))))

; 2^3*3^2*5^1=360
;(check-true (CanDivideByPower 360 3 1))
;(check-false (CanDivideByPower 360 3 2))

; (P n)ではなく(prime n x)を使っているので、歯抜けでも問題はないはず
; 2^3*3^2*7^1=504
;(check-true (CanDivideByPower 504 3 1))
;(check-false (CanDivideByPower 504 3 2))

(check-equal? (elm 360 1) 3)
(check-equal? (elm 360 2) 2)
(check-equal? (elm 360 3) 1)
(check-equal? (elm 504 3) 1)

; 定義7 列の長さ

(define (len x)
  (Min k ≦ x (and (> (prime k x) 0)
                  (= (prime (+ k 1) x) 0))))

(check-equal? (len 0) 0)
(check-equal? (len 1) 0)
(check-equal? (len 4) 1)
(check-equal? (len 360) 3)
(check-equal? (len 504) 3)

; 定義8 列の連結

; 元のソース
;(define (M8 x y)
;  (expt (P (+ (len x) (len y))) (+ x y)))
;
;(define (** x y)
;  (Min z ≦ (M8 x y)
;       (and (∀ m ≦ (len x)
;               (⇒ (<= 1 m) (= (elm z m) (elm x m))))
;            (∀ n ≦ (len y)
;               (⇒ (<= 1 n) (= (elm z (+ (len x) n)) (elm y n)))))))

(define (** x y)
  (let ((lenx (len x)))
    (let loop ((k 1) (n x))
      (let ((yk (elm y k)))
        (if (= yk 0)
            n
            (loop (+ k 1) (* n (expt (P (+ lenx k)) yk))))))))

(check-equal? (** 8 4) 72)
(check-equal? (** 6 2) 30)

; 定義9 xだけからなる列

(define (<> x) (expt 2 x))

; 定義10 xをカッコに入れた列

(define (paren x)
  (** (** (<> clp) x) (<> crp)))

(check-equal? (paren (<> c0)) (* (expt 2 clp) (expt 3 c0) (expt 5 crp)))

; 定義11 xは"第n型"の"変数"である

(define (IsVarBase p)
  (and (> p crp) (IsPrime p)))

(check-true (IsVarBase 17))
(check-false (IsVarBase 13))
(check-false (IsVarBase 18))

(define (IsVarType x n)
  (and (>= n 1)
       (∃ p ≦ x (and (IsVarBase p) (= x (expt p n))))))

(check-false (IsVarType 13 1))
(check-true (IsVarType 17 1))
(check-false (IsVarType 17 2))
(check-true (IsVarType (expt 17 3) 3))
