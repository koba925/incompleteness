# ゲーデルの不完全性定理の証明のアレをRacketで書いてみる (45)

```
(: Min≦ (All (a) (-> a (-> a Boolean) a)))
```

の`a`は`Number`かそのサブタイプです、っていう書き方はないのかな
ってところは何か名案が見つかることを期待して放置

`**`の素因数分解版

```
(define (** x y)
  (let ((lenx (len x)))
    (let loop ((k : Natural 1) (n : GSequence x))
      (let ((yk : GNumber (elm y k)))
        (printf "k ~a n ~a yk ~a~n" k n yk)
        (if (= yk 0)
            n
            (loop (+ k 1)
                  (gsequence+ (* n (expt (P (+ lenx k)) yk)))))))))
```

といきたいところなんですが・・・
止まらない・・・

`printf`入れてみると`yk`が0になってもループが終わってない・・・
もしかして`GNumber`型と`Natural`は比較できないとか？

```
> (= (gnumber 0) 0)
- : False
#t
```

え？
`False`型の`#t`？

```
> (= (gnumber 0) 1)
- : False
#f
```

まあこれはいい

```
> (= (gnumber 0) (gnumber 0))
- : False
#t
```

えーどうなってんのー

```
> (= 0 0)
- : Boolean [more precisely: True]
#t
> (= 0 1)
- : False
#f
```

だよねだよねー

Issue?Issueなの？
Experimentalだから？
いやー

