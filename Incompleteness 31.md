# ゲーデルの不完全性定理の証明のアレをRacketで書いてみる (31)

さてTyped Racketもざっと見たので型をつけていってみます
動かせないのでテストもできませんでしたが
うまく型を付けられれば静的なチェックだけでもバグが見つかるのではと
（実際ときどき間違えてたし）

さてどんな型をつけましょうか
型っぽいものはゲーデル数、列、列の列、ですかね
型の関係としては自然数＞ゲーデル数＞列＞列の列という感じ
列の列は列でもある、とか列に含まれる、とか列の一種、とかそういう意味で

別の捉え方をすると記号、論理式、証明という型も考えられそうです
それぞれゲーデル数、列、列の列の一部となっています

で、そういったことをSubtypeで表現していけばいいかな、と思ったんですが
そういえばSubtypeを定義する式ってでてきたっけな？と思い当たりました
う、出てきてない
`define-type`は既存の型に別名をつけるだけなので
ゲーデル数は数である、列はゲーデル数である、とやっても
結局ゲーデル数と数を区別することはできず
列を渡すべきところでゲーデル数を渡してしまった、みたいなことを
チェックすることはできそうにありません

しかしぐぐってみると`define-new-subtype`というのが出てきました
GuideではなくReference https://docs.racket-lang.org/ts-reference/index.html に掲載されていてしかもExperimental Features https://docs.racket-lang.org/ts-reference/Experimental_Features.html だと
むう
もっと基本的な機能かと思いましたが

最新版のリリースノート https://download.racket-lang.org/releases/6.11/doc/release/HISTORY_11.txt を確認してみたところRacket 6.3で実装されたようです
PLT SchemeのころからTyped Schemeっていうのがあったらしいことを考えると
比較的新しい機能なんですね

まあいいか
ではちょっと練習

```
#lang typed/racket

(define-new-subtype GNumber (gnumber Natural))
(define-new-subtype GSequence (gsequence GNumber))

(define c0 (gnumber 1))
(define s1 (gsequence (gnumber 2)))

(: number-func (-> Natural Natural))
(define (number-func x)
  (* x 2))

(number-func 0)
(number-func c0)
(number-func s1)

(: gnumber-func (-> GNumber GNumber))
(define (gnumber-func x)
  (gnumber (* x 3)))

; (gnumber-func 0) <- Error
(gnumber-func c0)
(gnumber-func s1)
(gnumber-func ss1)

(: sequence-func (-> GSequence GSequence))
(define (sequence-func x)
  (gsequence (gnumber (* x 4))))

;(sequence-func 0) <- Error
;(sequence-func c0) <- Error
(sequence-func s1)
```

やりたいことはできそうな感じがしてきました