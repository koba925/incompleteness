# ゲーデルの不完全性定理の証明のアレをRacketで書いてみる (33)

関数を定義したりマクロを定義したりするややこしいマクロを直すよりも
まずはできそうなところから経験値を積んでいってみようと思います
どうしても詰まったらまじめに調べるとして

あと、既存のソースに型宣言だけ付け加えていくスタイルで進めようと思ってましたが
どうもあちこちで型宣言が必須っぽくてエラーがなくなるまで全部つけてやらないと
関数の型やマクロの展開具合も調べられないので
別ファイルにちょっとずつコピーしながら試していくことにします

なのでマクロ内で定義してる関数の型はとりあえずこれで

```
     #'(begin
         (: fname (-> Natural (-> Any Any) Any))
         (define (fname max f)
           (let loop ((x 0))
             (cond ((> x max) (notfound x))
                   ((term (f x)) (found x))
                   (else (loop (+ x 1))))))
```

とりあえずこれだけ入れたらマクロの呼び出しは成功するようになりました

```
(define-equipment ∀ not (const #t) (const #f))
(define-equipment ∃ identity (const #f) (const #t))
(define-equipment Min identity (const 0) identity)
```

動くかな

```
> (∀ x ≦ 3 (< x 4))
  Type Checker: type mismatch
  expected: Real
  given: Any in: x
```

うぐ
そうか`>`だもんなあ
こんなところでも`#{}`は使えるんだろうか

```
> (∀ #{x : Natural} ≦ 3 (< x 4))
  Type Checker: type mismatch
  expected: (-> Any Any)
  given: (-> Nonnegative-Integer Any) in: (lambda (x) (#%app < x (quote 4)))
```

`#{}`が使えたのはいいとして一難去ってまた一難
`(-> Any Any)`に`(-> Nonnegative-Integer Any)`を渡すのは変だっけ？
そうか変か
ゆるすぎた

```
         (: fname (-> Natural (-> Natural Any) Any))
         (define (fname max f)
           (let loop ((x : Natural 0))
             (cond ((> x max) (notfound x))
                   ((term (f x)) (found x))
                   (else (loop (+ x 1))))))
```

これでどうよ

```
> (∀ #{x : Natural} ≦ 3 (< x 4))
- : Any
#t
```

やっとひとつ動いた
しかしこのパターンがエラー

```
> (∀ #{x : Natural} (< x 3))
  Type Checker: type mismatch
  expected: Nonnegative-Integer
  given: +inf.0 in: (quote +inf.0)
```

`+inf.0`はFlonumだったか

```
> +inf.0
- : Flonum [more precisely: +inf.0]
+inf.0
```

まあこの形使うのって一番最後の式だけだし当面コメントアウトでもいいんだけど
なんかこういろいろ釈然としないなあ
Typed Racketのマクロってちょっと上級者向け？

さらに遡ってまずはマクロなしでやってみるか