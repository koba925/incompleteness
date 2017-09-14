#lang racket

(require rackunit)
(require (for-syntax syntax/parse))

; 装備を整える

(define (∀≦ max f)
  (let loop ((x 1))
    (cond ((> x max) #t)
          ((not (f x)) #f)
          (else (loop (+ x 1))))))

(check-true (∀≦ 3 (λ (x) (< x 4))))
(check-false (∀≦ 3 (λ (x) (< x 3))))

; literalは何か定義しないとエラーになる
; とりあえず定義されていれば値はなんでもいい模様

(define ≦ #f)

(define-syntax (∀ stx)
  (syntax-parse stx
    #:literals (≦)
    [(_ v:id ≦ max:expr body:expr)
     #'(∀≦ max (λ (v) body))]))

(check-true (∀ x ≦ 3 (< x 4)))
(check-false (∀ x ≦ 3 (< x 3)))

(define (∃≦ max f)
  (let loop ((x 1))
    (cond ((> x max) #f)
          ((f x) #t)
          (else (loop (+ x 1))))))

(check-true (∃≦ 3 (λ (x) (= x 2))))
(check-false (∃≦ 3 (λ (x) (= x 4))))

(define-syntax (∃ stx)
  (syntax-parse stx
    #:literals (≦)
    [(_ v:id ≦ max:expr body:expr)
     #'(∃≦ max (λ (v) body))]))

(check-true (∃ x ≦ 3 (= x 2)))
(check-false (∃ x ≦ 3 (= x 4)))

; (f x)を満たすxが見つからなかった場合は0を返す
; x=0から開始すると見つからなかったのかx=0で見つかったのかわからないので
; x=1から開始する

(define (Min≦ max f)
  (let loop ((x 1))
    (cond ((> x max) 0)
          ((f x) x)
          (else (loop (+ x 1))))))

(check-eq? (Min≦ 3 (λ (x) (= x 2))) 2)
(check-eq? (Min≦ 3 (λ (x) (= x 4))) 0)

(define-syntax (Min stx)
  (syntax-parse stx
    #:literals (≦)
    [(_ v:id ≦ max:expr body:expr)
     #'(Min≦ max (λ (v) body))]))

(check-eq? (Min x ≦ 3 (= x 2)) 2)
(check-eq? (Min x ≦ 3 (= x 4)) 0)


