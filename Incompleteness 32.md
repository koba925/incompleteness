# ゲーデルの不完全性定理の証明のアレをRacketで書いてみる (32)
では

```
#lang typed/racket
```

動くだけは動いたりするかな？

```
Type Checker: insufficient type information to typecheck. please add more type annotations in: loop
```

ダメでした

```
(define-syntax (define-equipment stx)
  (syntax-parse stx
    ((_ name term notfound found)
     #:with fname (format-id stx "~a≦" #'name)
     #'(begin
         (define (fname max f)
           (let loop ((x 0))
                ^^^^
             (cond ((> x max) (notfound x))
                   ((term (f x)) (found x))
                   (else (loop (+ x 1))))))
           :
           :
```

ん、マクロのとこか
そういえばマクロってどうなるんだ
Guideにはマクロって出てこなかったなあ
Referenceにも`define-syntax`は出てこないし
ぐぐってもTyped Racket Macro入門みたいなのは出てこない

単純なケースなら型宣言をつければいいとして
型が決まってないところ、特にlambdaを渡すところとかどうするんだろう
型も渡す？それともAnyにしておくのかな？

loopに型をつける前に
まずはトップレベルの関数に型をつけるのが先かな
しかしfにどんな型をつければいいのやら
とりあえずAnyを並べてみたらエラーは出なかったけど

```
         (: fname (-> Natural (-> Any Any) Any))
         (define (fname max f)
```

とりあえず`#'`の中でも普通に型宣言が書けることはわかったけど
これはやってる意味がなくなりそうな気がしないでもない
Syntax Objectの中身の型を返してくれるような関数がほしくなる気がする？
リフレクションっていうか

ちょっと試行錯誤と練習をしておいたほうがいいかな
それとも先へ進んでみるべきか
