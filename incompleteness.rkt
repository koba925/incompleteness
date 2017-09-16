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

(define (CanDevide x d)
  (∃ n ≦ x (= x (* d n))))

(check-true (CanDevide 12 3))
(check-false (CanDevide 12 5))

; 定義2
(define (IsPrime x)
  (and (> x 1)
       (not (∃ d ≦ x (and (not (= d 1))
                          (not (= d x))
                          (CanDevide x d))))))

(check-false (IsPrime 0))
(check-false (IsPrime 1))
(check-true (IsPrime 2))
(check-false (IsPrime 12))
(check-true (IsPrime 17))

; 定義3

(define (CanDevideByPrime x p)
  (and (CanDevide x p) (IsPrime p)))

(check-true (CanDevideByPrime 12 3))
(check-false (CanDevideByPrime 12 5))
(check-false (CanDevideByPrime 12 6))

(define (prime n x)
  (cond ((= n 0) 0)
        (else (Min p ≦ x (and (< (prime (- n 1) x) p)
                              (CanDevideByPrime x p))))))

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

(define (M5 n)
  (+ (factorial n) 1))

(define (P n)
  (cond ((= n 0) 0)
        (else (Min p ≦ (M5 n) (and (< (P (- n 1)) p)
                                   (IsPrime p))))))

(check-eq? (P 0) 0)
(check-eq? (P 1) 2)
(check-eq? (P 5) 11)
