# ゲーデルの不完全性定理の証明のアレをRacketで書いてみる (26)

定義35 xは公理II-nから得られる"論理式"である
定義36 xは公理IIから得られる"論理式"である

```
(define (IsSchemaII n x)
  (case n
    ((1) (∃ p ≦ x (and (IsForm p)
                       (= x (Implies (Or p p) p)))))
    ((2) (∃ p q ≦ x (and (IsForm p)
                         (IsForm q)
                         (= x (Implies p (Or p q))))))
    ((3) (∃ p q ≦ x (and (IsForm p)
                         (IsForm q)
                         (= x (Implies (Or p q) (Or q p))))))
    ((4) (∃ p q r ≦ x (and (IsForm p)
                           (IsForm q)
                           (IsForm r)
                           (= x (Implies (Implies p q) 
                                         (Implies (Or r p) (Or r q)))))))))

(define (IsAxiomII x)
  (or (IsSchema 1 x)
      (IsSchema 2 x)
      (IsSchema 3 x)
      (IsSchema 4 x)))
```

こんどは`p`,`q`が与えられてるのでいろんな式にマッチできますね

あんまり`n`を引数に入れてる意味がないですね
`IsSchemaII-1`とかってやるほうが自然な気も

そういえばそういうのもあったな、と思って`case`を使ってみましたが
動かしてみるってことをしなくなったのでちゃんと使えてるかどうかわかりません
ちょっとREPLで練習はしたので多分大丈夫だと思いますが

話はそれますが、こういうことがあると最初に戻って
(`case`の似つかわしいところは)全部、`case`に書き換えてしまいたくなります
自前で書いてたけどライブラリありましたとかも同様

そういうのがもったいない気がして、最初にいろいろ調べたくなりますが
調べてるばっかりで先へ進まないというね
最近はそれに気がついたので多少改善してますが
お勉強なら車輪の再発明も大いにアリだと思いますし
リファクタリングとか技術的負債とかいうキーワードがだんだん増えてきて
戻って書き直すというのが以前よりも価値のあるものと認められてきてる気もします

Racketはschemeから派生したわりに（派生したせいで？）いろいろ多機能なんで
全部把握しようってのはちょっと無理（他の言語でもたいてい無理な気はしますが）
その辺の兼ね合いが悩ましいところです

