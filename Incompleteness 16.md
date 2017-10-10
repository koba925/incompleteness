# ゲーデルの不完全性定理の証明のアレをRacketで書いてみる (16)

## 「10.8.4 変数・記号・論理式」の続き

```
(define (IsNotOp x a) (= x (Not a)))

(define (IsOrOp x a b) (= x (Or a b)))

(define (IsForallOp x a)
  (∃ v ≦ x (and (IsVar v) (= x (ForAll v a)))))

(define (IsOp x a b)
  (or (IsNotOp x a)
      (IsOrOp x a b)
      (IsForallOp x a)))
```