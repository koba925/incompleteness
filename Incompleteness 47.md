# ゲーデルの不完全性定理の証明のアレをRacketで書いてみる (47)

定義9 xだけからなる列

```
(: <> (-> GSymbol GSequence))
(define (<> x) (gsequence+ (expt 2 x)))
```

問題なし

定義10 xをカッコに入れた列

えーとこの`x`は列だな 記号じゃない
定義9の`x`は記号
まぎらわしい
こういうまぎらわしいところを型チェックが見てくれるだろうという期待でやってるわけだけど

```
(: paren (-> GSequence GSequence))
(define (paren x)
  (** (** (<> clp) x) (<> crp)))
```

定義11 xは"第n型"の"変数"である

```
(: IsVarBase (-> GSymbol Boolean))
(define (IsVarBase p)
  (and (> (cast p Natural) (cast crp Natural))
       (IsPrime p)))

(: IsVarType (-> GSymbol Natural Boolean))
(define (IsVarType x n)
  (and (>= n 1)
       (∃≦ x (λ (p)
               (and (IsVarBase (gsymbol+ p))
                    (= x (expt p n)))))))
```

また`cast`してやらないとうまくいかない案件に遭遇
何かなこれは
ちょっとつらい