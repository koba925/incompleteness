# ゲーデルの不完全性定理の証明のアレをRacketで書いてみる (16)

## 「10.8.4 変数・記号・論理式」の続き

定義21 "¬(a)"または"(a)∨(b)"または"∀v(a)"である

とりあえずそのまま

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

案の定`IsForallOp`が（#fになるときに）遅いので書き直し

```
(define (IsForallOp x a)
  (let ((v (elm x 2)))
    (and (IsVar v)
         (= x (ForAll v a)))))
```

今日はこれだけ！