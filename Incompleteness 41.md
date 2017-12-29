# ゲーデルの不完全性定理の証明のアレをRacketで書いてみる (41)

`Factorization`からn番目の`Factor`を取り出します

```
(: factorization-nth (-> (Opt Factorization) Natural (Opt Factor)))
(define (factorization-nth facz n)
  (: F (-> Factorization Natural (Opt Factor)))
  (define (F f n)
    (cond ((null? f) (None))
          ((= n 1) (Some (car f)))
          (else (F (cdr f) (assert (- n 1) natural?)))))
  (if (or (None? facz) (= n 0))
      (None)
      (F (Some-v facz) n)))
```

これはこれで考えて作ったんですがどうもすっきりしてません
内部関数の`F`が`Factor`でなく`(Opt Factor)`を返すので
`opt-apply`が使えません
2箇所で`None`を返しているのもどことなく気に入らない

`(Opt Factor)`から`prime`や`expt`を取り出します

```
(: factor-prime (-> (Opt Factor) (Opt Natural)))
(define (factor-prime x)
  ((inst opt-apply Factor Natural) x Factor-prime))

(: factor-expt (-> (Opt Factor) (Opt Natural)))
(define (factor-expt x)
  ((inst opt-apply Factor Natural) x Factor-expt))
```

`car`とか`cdr`で済んでたんですけどねえ
`inst`くらい不要にできないかなあ

さてここまで失敗を`None`で表していますが
ゲーデルが作った範囲では例外を0で表しているので
どこかで`None`から0に変換する必要があります

ていうかもともと0でうまくいくようになってるので、`None`はあるだけ邪魔って話
がそこは勉強なので

というわけで`None`を0に変える関数

```
(: none-to-zero (All (a) (-> (Opt a) (U Zero a))))
(define (none-to-zero optvar)
  (if (None? optvar)
      0
      (Some-v optvar)))
```

`(Opt Natural)`型以外で使うことはあんまりなさそうですが
何でも動くので`(Opt a)`にしてあります

はーやっと元の流れに戻りました
`prime`はこうなってます

```
(: prime (-> Natural Natural Natural))
(define (prime n x)
  (none-to-zero (factor-prime (factorization-nth (factorization x) n))))
```

素因数分解して、n番目を持ってきて、素数パートを持ってきて、
途中で失敗してたら0にする
