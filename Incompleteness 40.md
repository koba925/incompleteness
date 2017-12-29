# ゲーデルの不完全性定理の証明のアレをRacketで書いてみる (40)

どうもカンがつかめなくていろいろ試行錯誤してたら収集つかなくなってきたんですが
なんとなくまとまった気がするので一度このへんで書いておきます

型の定義はこうなりました
`Factor`は`Pairof`ではなくてstructで表現することに
これは単にせっかくそういうのがあるんだからこうしとこうという程度の話

```
(struct None () #:transparent)
(struct (a) Some ([v : a]) #:transparent)
(define-type (Opt a) (U None (Some a)))

(struct Factor ([prime : Natural] [expt : Natural]) #:transparent)
(define-type Factorization (Listof Factor))
```

`#:transparent`っていうのは、値の表示を親切にしてくれるキーワードです
お勉強中はこの方が便利
デフォルトでそうなってないってことはたぶん性能が下がったりしてるのかと

こう定義したとして

```
(define fac1 : Factor (Factor 2 1))
(define optfac1 : (Opt Factor) (None))
(define optfac2 : (Opt Factor) (Some fac1))
```

今までの型定義だとこんな風に表示されて

```
> optfac1
- : (U (Some Factor) None)
#<None>
> optfac2
- : (U (Some Factor) None)
#<Some>
```

`#<None>`はともかく、`#<Some>`じゃ中身がわからんじゃないか、となってたんですが
`#:transparent`をつけてやるとこう表示されます

```
> optfac1
- : (U (Some Factor) None)
(None)
> optfac2
- : (U (Some Factor) None)
(Some (Factor 2 1))
```

素因数分解の結果もわかりやすく

```
> (factorization 40)
- : (U (Some Factorization) None)
(Some (list (Factor 2 3) (Factor 5 1)))
```

助かります

素因数分解をする関数はそのまま変化なしです
素因数分解の長さ（素因数の数っていうか）を求める関数はまずこうなりました

```
(: factorization-length (-> (Opt Factorization) (Opt Natural)))
(define (factorization-length facz)
  (if (None? facz)
      (None)
      (Some (length (Some-v facz)))))
```

`(if (None? ...) (None) (Some (... (Some-v ...))))`というところは
`Opt`の中身に関数を適用してやるイメージ
パターンぽいので関数にしてみると使いでがありそうな予感

```
(: opt-apply (All (a b) (-> (Opt a) (-> a b) (Opt b))))
(define (opt-apply x func)
  (if (None? x)
      (None)
      (Some (func (Some-v x)))))
```

これを使ったら`factorization-length`の定義はこうなりました

```
(: factorization-length (-> (Opt Factorization) (Opt Natural)))
(define (factorization-length facz)
  ((inst opt-apply Factorization Natural) facz length))
```

ほんとは単に`(opt-apply facz length)`って書きたいんですが
これだと型がおかしいって言われます

```
Type Checker: Polymorphic function `opt-apply' could not be applied to arguments:
Argument 1:
  Expected: (U (Some a) None)
  Given:    (U (Some Factorization) None)
Argument 2:
  Expected: (-> a b)
  Given:    (All (a) (-> (Listof a) Index))

Result type:     (U (Some b) None)
Expected result: (U (Some Nonnegative-Integer) None)
 in: (opt-apply facz length)
```

`(define (f g) ...)`という定義があって`g`が関数型の場合
`(f h)`という呼び出しを行うには、`h`の引数の型は`g`の引数の型よりも
広くないといけませんが
`(Some Factorization)`は`(Some a)`よりも狭いのでエラーとなります

・・・たぶん

そこで`(inst opt-apply Factorization Natural)`は何をやっているかというと
`(All (a b) (-> (Opt a) (-> a b) (Opt b)))`の
`a`を`Factorization`、`b`を`Natural`にした`opt-apply`を作ってる
ということになります
これでエラーが出なくなりました

っていうやり方がいいやり方なのかどうかは自信がありません
無理やりキャストするみたいなことをしてる感覚ではないと思いますけど
もうちょっと楽に書けるやりかたがあってもいいような

`((inst opt-apply Factorization Natural) facz length)`は
ちょっと見づらいのでそれだけの関数を定義するという手もありかなあ

```
(define opt-factorization-apply (inst opt-apply Factorization Natural))

(: factorization-length (-> (Opt Factorization) (Opt Natural)))
(define (factorization-length facz)
  (opt-factorization-apply facz length))
```

ちょっとやりすぎかもしれません
値が`Natural`でなければまた別の関数が必要になりますし