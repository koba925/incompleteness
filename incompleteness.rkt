#lang racket

(require rackunit)
(require (for-syntax syntax/parse))

(define (∀≦ max f)
  (let loop ((x 0))
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
