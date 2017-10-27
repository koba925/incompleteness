# ゲーデルの不完全性定理の証明のアレをRacketで書いてみる (28)

`ElementForm`周辺が盛大にバグってました
ぐぬぬ

変数と、ひとつの変数だけからなる列が混乱しまくってる
どっちも整数だけど、別の型として扱ってれば検出できたかな
でもさらっと考えて見るとこのプログラムって型付けしにくそうだな
もう少しでひととおり終わるし
その後でTyped Racketでも調べながらちょっと考えてみるか・・・

もともとは

```
(define (ElementForm a b)
  (** a (paren b)))
```

と定義してたんですがこの`a`、`b`に`(var 1 1)`とか入れちゃってたんですね
そこは列を入れないと
でも`a`は必ず変数なので、こっちは`ElementForm`側で列にしてやることにします

```
(define (ElementForm a b)
  (** (<> a) (paren b)))
```

このへんに手直しが必要でした

```
(define AxiomI-1
  (Not (Equal (succ 1 (var 1 1)) (￣ 0))))

(define AxiomI-2
  (Implies (Equal (succ 1 (var 1 1))
                  (succ 1 (var 2 1)))
           (Equal (<> (var 1 1))
                  (<> (var 2 1)))))

(define AxiomI-3
  (Implies (And (ElementForm (var 1 2) (￣ 0))
                (ForAll (var 1 1)
                        (Implies (ElementForm (var 1 2) (<> (var 1 1)))
                                 (ElementForm (var 1 2) (succ 1 (var 1 1))))))
           (ForAll (var 1 1) (ElementForm (var 1 2) (<> (var 1 1))))))
```

正直ほかも大丈夫という気はしませんがもし気づいちゃったら直します

定義41 xは公理Vから得られる"論理式"である

```

```




