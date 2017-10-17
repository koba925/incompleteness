# ゲーデルの不完全性定理の証明のアレをRacketで書いてみる (21)

定義24 "変数"vはxのn番目の場所では"束縛"されている

```
(define (IsBoundAt v n x)
  (and (IsVar v)
       (IsForm x)
       (∃ a b c ≦ x (and (= x (** (** a (ForAll v b)) c))
                         (IsForm b)
                         (<= (+ (len a) 1) n)
                         (<= n (+ (len a) (len (ForAll v b))))))))
```

変数が束縛されるのは`∀v(b)`の形で`b`の中に`v`が含まれているとき、と

`b`の中にまた`∀v(c)`があっても大丈夫かな？
束縛されてることには変わりないからいいのか

定義25 "変数"vはxのn番目の場所では"束縛"されてない

```
(define (IsFreeAt v n x)
  (and (IsVar v)
       (IsForm x)
       (= v (elm x n))
       (<= n (len x))
       (not (IsBoundAt v n x))))
```

いろいろチェックがついてますが見たまま

定義26 vはxの"自由変数"である

```
(define (IsFree v x)
  (∃ n ≦ (len x) (IsFreeAt v n x)))
```

xのどこでもいいから1箇所でもvがFreeに現れていれば

さて、動かすのをあきらめてみるともうほとんど書き写してるだけっていう状態ですが
これでいいんでしょうか



