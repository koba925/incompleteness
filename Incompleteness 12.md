# ゲーデルの不完全性定理の証明のアレをRacketで書いてみる (12)

## 「10.8.4 変数・記号・論理式」の続き

定義18 "第n型の記号"である

```
(define (IsNthType x n)
  (or (and (= n 1) (IsNumberType x))
      (and (> n 1) (∃ v ≦ x (and (IsVarType v n) (= x (<> v)))))))
```
