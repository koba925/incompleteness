# ゲーデルの不完全性定理の証明のアレをRacketで書いてみる (2)

## 「10.8.1 装備を整える」の続き

同様に`∃x≦M[...]`と`min x≦M[...]`を定義します

```
(define (∃≦ max f)
  (let loop ((x 1))
    (cond ((> x max) #f)
          ((f x) #t)
          (else (loop (+ x 1))))))

(define-syntax (∃ stx)
  (syntax-parse stx
    #:literals (≦)
    [(_ v:id ≦ max:expr body:expr)
     #'(∃≦ max (λ (v) body))]))
     
(define (Min≦ max f)
  (let loop ((x 1))
    (cond ((> x max) 0)
          ((f x) x)
          (else (loop (+ x 1))))))

(define-syntax (Min stx)
  (syntax-parse stx
    #:literals (≦)
    [(_ v:id ≦ max:expr body:expr)
     #'(Min≦ max (λ (v) body))]))
```

`min`だと名前がカブるので`Min`にしました

`Min≦`や`Min`は`(f x)`を満たす`x`が見つからなかった場合は0を返します
`x`=0から開始すると見つからなかったのか`x`=0で見つかったのかわからないので
`x`=1から開始することにしました
`∀`や`∃`も同様に修正
