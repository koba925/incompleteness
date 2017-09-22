# ゲーデルの不完全性定理の証明のアレをRacketで書いてみる (7)

## 「10.8.4 変数・記号・論理式」

ここらへんから証明っぽくなってきます

定義11 xは"第n型"の"変数"である

```
(define (IsVarBase p)
  (and (> p crp) (IsPrime p)))

(define (IsVarType x n)
  (and (>= n 1)
       (∃ p ≦ x (and (IsVarBase p) (= x (expt p n))))))
```

これはどうかな

```
> (time (IsVarType (+ (expt 17 3) 1) 3))
cpu time: 7 real time: 6 gc time: 0
#f
```

微妙なところ
型が大きいと時間がかかりそうですが先へ進みます

変数を`(expt 17 3)`とか書かないでいいようにしておきます
x1を`(var 1 1)`、z3を`(var 3 3)`などと書くことにします

```
(define (var n c) (expt (P (+ 6 n)) c))
```

定義12 xは"変数"である

```
(define (IsVar x)
  (∃ n ≦ x (IsVarType x n)))
```
