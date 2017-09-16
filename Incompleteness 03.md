# ゲーデルの不完全性定理の証明のアレをRacketで書いてみる (3)

## 「10.8.2 整数論」

このへんは単なる数の計算

定義1 割り切れる？
関数名はできるだけ数学ガールに合わせていきます

```
(define (CanDevide x d)
  (∃ n ≦ x (= x (* d n))))
```

割り切れないときに`x`以下の`n`すべてについて試してしまってもったいないですが
とりあえずそういうことは言わないで進みます
言ってたらキリがないので！
超富豪です！

定義2 素数？

```
(define (IsPrime x)
  (and (> x 1)
       (not (∃ d ≦ x (and (not (= d 1))
                          (not (= d x))
                          (CanDevide x d))))))
```

言わないぞ言わないぞ・・・

定義3 n番目の素因数

```
(define (CanDevideByPrime x p)
  (and (CanDevide x p) (IsPrime p)))
(define (prime n x)
  (cond ((= n 0) 0)
        (else (Min p ≦ x (and (< (prime (- n 1) x) p)
                              (CanDevideByPrime x p))))))
```

例では2352=2^4×3^1×7^2が使われてます
5が抜けてますが歯抜けはアリってことですかね
ゲーデル数の定義ってそうなってるのかな

p.220

> この自然数の列を一つの自然数にまとめたいとき、素数を小さい方から
> 並べた列〈2,3,5,7,11,13...〉を別途用意する。

「小さい方から並べた列」は「小さい方からすべて」なのか
「2,3,5,7」はただの一例なのか
微妙なところです

そういうコード、じゃないな関数定義になってるますしまあ気にしないで進みます

定義4 nの階乗

```
(define (factorial n)
  (cond ((= n 0) 1)
        (else (* n (factorial (- n 1))))))
```

ふつうです
再帰入門みたいなのにでてくるサンプルと同じ
ていうかプログラミングよりもこういう世界が先にあったんですよね

`if`じゃなくて`cond`を使ってるのは、場合分けってなんとなく`if`より
`cond`っぽいかなーという気分だけの話です

定義5 n番目の素数

```
(define (M5 n)
  (+ (factorial n) 1))

(define (P n)
  (cond ((= n 0) 0)
        (else (Min p ≦ (M5 n) (and (< (P (- n 1)) p)
                                   (IsPrime p))))))
```

すごい数の計算が行われてそうだ・・・

```
> (time (P 5))
cpu time: 7 real time: 7 gc time: 0
11
> (time (P 6))
cpu time: 42 real time: 42 gc time: 0
13
> (time (P 7))
cpu time: 683 real time: 682 gc time: 83
17
> (time (P 8))
cpu time: 12989 real time: 12992 gc time: 1441
19
> (time (P 9))
cpu time: 304833 real time: 305456 gc time: 37796
23
```

普通に処理時間が爆発してますね (P 10)はちょっとやる気がしません
列が出てきたら苦しいだろうなとは思ってましたがその手前でもうこんな状態とは
