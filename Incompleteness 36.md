# ゲーデルの不完全性定理の証明のアレをRacketで書いてみる (36)

さて`P`が定義できたので素因数分解と定義3の再定義を
まずは補助関数から

```
(: times-divide (-> Natural Natural (Values Natural Natural)))
(define (times-divide x p)
  (let loop ((k : Natural 1) (x : Natural x))
    (cond ((not (CanDivide x p)) (values x (cast (- k 1) Natural)))
          (else (loop (+ k 1) (cast (/ x p) Natural))))))
```

`cast`はしかたないのかなあ
`k`は1から増えていくだけだから`(- k 1)`は`Natural`だよ、とか
`(CanDivide x p)`だから`(/ x p)`は`Natural`だよ、って
わかってくれるといいんだけど・・・

と思ってもういちど調べたら
後者は`quotient`に書き換えるだけでした
`div`がないなあと思ってあきらめてたんですが名前が違いました

前者はガイドに別の書き方が載ってました
`assert`を使います

```
(: times-divide (-> Natural Natural (Values Natural Natural)))
(define (times-divide x p)
  (let loop ((k : Natural 1) (x : Natural x))
    (cond ((not (CanDivide x p)) (values x (assert (- k 1) natural?)))
          (else (loop (+ k 1) (quotient x p))))))
```

`(- k 1)`が`natural?`であることを`assert`すると、
コンパイラの型チェックも`(- k 1)`も`natural?`と思って通してくれます
`cast`よりもちょっとだけスジがいい気がします
結果は変わらないですけど

もし何かの間違いで`(- k 1)`が負になってしまったら当然ランタイムエラーになります

ところで`natural?`は`typed/racket`モジュールに含まれていません
`racket/math`で定義されています
こんなふうに`require`してやりたいところ

```
(require/typed racket/math
               [natural? (-> Any Boolean : Natural)])
```

`: Natural`という部分は、`natural?`が真だったら
与えられた引数は`Natural`ですよ、と教えてあげるため
これがOccurence Typingのカラクリのひとつ

これが契約に変換されるはずなんですがそこでエラーになります

```
Type Checker: Type (-> Any Boolean : Nonnegative-Integer) could not be converted to a contract: cannot generate contract for function type with props or objects. in: (-> Any Boolean : Natural)
```

Referenceにも`number?`とかはうまくいかないんだよねーって書いてあります
もともと、`typed/racket`にある`exact-nonnegative-integer?`の
単なる別名なので自分で定義してやります

```
(: natural? (-> Any Boolean : Natural))
(define natural? exact-nonnegative-integer?)
```

これでうまくいくようになりました
この間書いた`cast`もこっちに直します

Occurence Typingがもっと進んでいくと今回の`(- k 1)`あたりも
勝手に`Natural`だってわかってくれるようになったりするんでしょうか
難しそうな分野ですが