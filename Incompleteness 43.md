# ゲーデルの不完全性定理の証明のアレをRacketで書いてみる (43)

そういえばGuideにパターンマッチをやってくれる`match`の例が上がってました

```
(: assert-symbols! ((Listof Any) -> (Listof Symbol)))
(define (assert-symbols! lst)
  (match lst
    [(list (? symbol? #{s : (Listof Symbol)}) ...) s]
    [_ (error "expected only symbols, given" lst)]))
```

ここで使えるはず

```
(: opt-apply (All (a b) (-> (Opt a) (-> a b) (Opt b))))
(define (opt-apply x func)
  (if (None? x)
      (None)
      (Some (func (Some-v x)))))
```

上の例を見て`#{}`の書き方についてひとしきり悩んでましたが
実は使わなくても書けてしまいました

```
(: opt-apply (All (a b) (-> (Opt a) (-> a b) (Opt b))))
(define (opt-apply x func)
  (match x
    [(None) (None)]
    [(Some y) (Some (func y))]))
```

できました
「`match`のように、Typed Racketに対応してない式では`#{}`を使うのが便利」って
書いてあったんですが、今は`match`も型に対応したってことなんでしょうね

なお`[(None) (None)]`のふたつめの`(None)`はないとエラーになります
`cond`だとふたつめの式がなければひとつめの式の値を返してくれてたと思うんですけど

`none-to-zero`も同様

```
(: none-to-zero (All (a) (-> (Opt a) (U Zero a))))
(define (none-to-zero optvar)
  (match optvar
    [(None) 0]
    [(Some y) y]))
```

本筋に戻って

定義6 n番目の要素

まずはもともとの定義から

```
(: CanDivideByPower (-> Natural Natural Natural Boolean))
(define (CanDivideByPower x n k)
  (CanDivide x (expt (prime n x) k)))
```

これは単なる自然数に関する述語と見ていいのでは
次は本体
`x`は少なくとも列 → `GSequene`
値は列かもしれないけど単なる記号かもしれない → `GNumber`

```
(: elm (-> GSequence Natural GNumber))
(define (elm x n)
  (gnumber
   (Min≦ x (λ (k) (and (CanDivideByPower x n k)
                       (not (CanDivideByPower x n (+ k 1))))))))
```

次は素因数分解版
型は同じ

```
(define (elm x n)
  (gnumber (none-to-zero (factor-expt (factorization-nth (factorization x) n)))))
```

ここから先はしばらく`Opt`のことは忘れてもいいはず