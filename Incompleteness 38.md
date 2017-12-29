# ゲーデルの不完全性定理の証明のアレをRacketで書いてみる (38)

> 0の素因数分解とか、2の素因数分解の3番目の因数とか変なやつは`'(0 .0)`で表しました
> 後で型による表現も試してみたいと思います

失敗を表せればいいので、ちょうどGuideに載ってた`Opt`型が使えそうです

```
(struct None ())
(struct (a) Some ([v : a]))
(define-type (Opt a) (U None (Some a)))
```

本格的に書き換える前にいったん練習

```
(: safediv (-> Number Number (Opt Number)))
(define (safediv x y)
  (if (= y 0)
      (None)
      (Some (/ x y))))

(: showdiv (-> Number Number Void))
(define (showdiv x y)
  (let ((z (safediv x y)))
    (if (None? z)
        (printf "division by zero~n")
        (printf "~a/~a=~a~n" x y (Some-v z)))))
```

これで最低限のことはできそう
あと何か追加すればモナドだかなんだかになるはずなんだけどよくわかってないし
とりあえず進めてみよう