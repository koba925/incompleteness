#lang racket

(require rackunit)
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

; 定義1

; もとの定義
;(define (CanDivide x d)
;  (∃ n ≦ x (= x (* d n))))

(define (CanDivide x d)
  (= (remainder x d) 0))

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

(define (CanDivideByPrime x p)
  (and (CanDivide x p) (IsPrime p)))

(check-true (CanDivideByPrime 12 3))
(check-false (CanDivideByPrime 12 5))
(check-false (CanDivideByPrime 12 6))

(define (prime n x)
  (cond ((= n 0) 0)
        (else (Min p ≦ x (and (< (prime (- n 1) x) p)
                              (CanDivideByPrime x p))))))

(check-eq? (prime 0 2352) 0)
(check-eq? (prime 1 2352) 2)
(check-eq? (prime 2 2352) 3)
(check-eq? (prime 3 2352) 7)

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

(check-eq? (P 0) 0)
(check-eq? (P 1) 2)
(check-eq? (P 5) 11)

; 定義6 n番目の要素

(define (CanDivideByPower x n k)
  (CanDivide x (expt (prime n x) k)))

(define (elm x n)
  (Min k ≦ x (and (CanDivideByPower x n k)
                  (not (CanDivideByPower x n (+ k 1))))))

; 2^3*3^2*5^1=360
(check-true (CanDivideByPower 360 3 1))
(check-false (CanDivideByPower 360 3 2))

; (P n)ではなく(prime n x)を使っているので、歯抜けでも問題はないはず
; 2^3*3^2*7^1=504
(check-true (CanDivideByPower 504 3 1))
(check-false (CanDivideByPower 504 3 2))

(check-equal? (elm 360 1) 3)
(check-equal? (elm 360 2) 2)
(check-equal? (elm 360 3) 1)
(check-equal? (elm 504 3) 1)
