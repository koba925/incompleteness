# ゲーデルの不完全性定理の証明のアレをRacketで書いてみる (5)

## 「10.8.3 列」

定義6 n番目の要素

```
(define (CanDivideByPower x n k)
  (CanDivide x (expt (prime n x) k)))

(define (elm x n)
  (Min k ≦ x (and (CanDivideByPower x n k)
                  (not (CanDivideByPower x n (+ k 1))))))

```

`(P n)`ではなく`(prime n x)`を使っているので、歯抜けでも問題はないはず

```
> (elm 504 3)
1
```

おｋ

