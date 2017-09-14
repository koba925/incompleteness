#lang racket

(require rackunit)
(require (for-syntax syntax/parse))

; 装備を整える

; literalは何か定義しておかないとエラーになる
; とりあえず定義されていれば値はなんでもいい模様
; よくわかってない

(define ≦ #f)

(define-syntax (define-equipment stx)
  (syntax-parse stx
    ((_ name func)
     #'(define-syntax (name stx)
        (syntax-parse stx
          #:literals (≦)
          [(_ v:id ≦ max:expr body:expr)
           #'(func max (λ (v) body))])))))

(define (∀≦ max f)
  (let loop ((x 1))
    (cond ((> x max) #t)
          ((not (f x)) #f)
          (else (loop (+ x 1))))))

(check-true (∀≦ 3 (λ (x) (< x 4))))
(check-false (∀≦ 3 (λ (x) (< x 3))))

(define-equipment ∀ ∀≦)

(check-true (∀ x ≦ 3 (< x 4)))
(check-false (∀ x ≦ 3 (< x 3)))

(define (∃≦ max f)
  (let loop ((x 1))
    (cond ((> x max) #f)
          ((f x) #t)
          (else (loop (+ x 1))))))

(check-true (∃≦ 3 (λ (x) (= x 2))))
(check-false (∃≦ 3 (λ (x) (= x 4))))

(define-equipment ∃ ∃≦)

(check-true (∃ x ≦ 3 (= x 2)))
(check-false (∃ x ≦ 3 (= x 4)))

(define (Min≦ max f)
  (let loop ((x 1))
    (cond ((> x max) 0)
          ((f x) x)
          (else (loop (+ x 1))))))

(check-eq? (Min≦ 3 (λ (x) (= x 2))) 2)
(check-eq? (Min≦ 3 (λ (x) (= x 4))) 0)

(define-equipment Min Min≦)

(check-eq? (Min x ≦ 3 (= x 2)) 2)
(check-eq? (Min x ≦ 3 (= x 4)) 0)
