# ゲーデルの不完全性定理の証明のアレをRacketで書いてみる (42)

ここで自然数の世界からゲーデル数の世界に入ります
`GNumber`などの型がやっと登場

定数

```
(define c0 : GSymbol (gsymbol (gnumber 1)))
(define cf : GSymbol (gsymbol (gnumber 3)))
 :
 :
```

`(define c0 : GSymbol 1)`ではエラーになるのはしかたないんだけど
ちょっとめんどくさいなあ
こうかな？

```
(define gsymbol+
  (case-lambda [([x : Natural]) (gsymbol (gnumber x))]
               [([x : GNumber]) (gsymbol x)]))

(define c0 : GSymbol (gsymbol+ 1))
(define cf : GSymbol (gsymbol+ 3))
 :
 :
```

`case-lambda`はオーバーロードみたいなことをする式で
`case->`はその型
でもこの書き方だと、誰かが作った関数と同じ処理を新しい型について
実行しようとしたとき、その関数自体を書き換えなきゃいけなくて
ちょっとやりにくかったりしないかなあ
独立してもうひとつ関数を定義するだけでオーバーロードされるような
書き方はできないんだろうか

気になったのはReferenceに

> Note that each formals must have a different arity.

と書いてあったこと
arityって引数の数のことだよなあ？型のことは言ってないよな？
素のRacketには型がないから引数の数でしか区別つかないけど
Typed Racketではそんなことないと思いたい
ていうかちゃんと型でも区別してるっぽいけど
ドキュメントが間違ってるとか追いついてないとかそういう話？

でもそれくらいにして先に進む
`gsymbol+`みたいな関数を作って使うべきかどうかも考えつつ

変数

```
(: var (-> Natural Natural GSymbol))
(define (var n c) (gsymbol+ (expt (P (+ 6 n)) c)))
```

ゲーデル数を求めるユーティリティ関数

を作ってたらやっぱりこういうのがほしくなりました

```
(: gsequence+ (case-> (-> Natural GSequence)
                    (-> GNumber GSequence)))
(define gsequence+
  (case-lambda [([x : Natural]) (gsequence (gnumber x))]
               [([x : GNumber]) (gsequence x)]))
```

うーん毎回こういうのを書きたくなるとなると、なにか不自然なことを
しようとしているんじゃないかという不安が出てくるな
間違ってはいないと思うんだけど

で本体
値は列の列かもしれないけど少なくとも列なので`GSequence`

```
(: gnum (-> GNumber * GSequence))
(define (gnum . seq)
  (: iter (-> (Listof GNumber) Natural GSequence GSequence))
  (define (iter s k n)
    (if (null? s)
        n
        (iter (cdr s) (+ k 1) (gsequence+ (* (expt (P k) (car s)) n)))))
  (iter seq 1 (gsequence+ 1)))
```

1を`GSequence`の値として認めていいのかどうか微妙な気もするけど
長さ0の列と見る、としてもそれほど不自然ではないんじゃないかな

型エラーを出さずに書くのはなかなか気難しい
それくらい見逃してよ・・・と言いたくなる気もするけど
バグ取りしてもらうためにやってるんだからだまっとくか
いい仕事してくれること期待してますよ