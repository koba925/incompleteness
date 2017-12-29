# ゲーデルの不完全性定理の証明のアレをRacketで書いてみる (44)

しばらく`Opt`が出てこないというのは勘違い

定義7 列の長さ

```
(: len (-> GSequence Natural))

; 元のソース
(define (len x)
  (Min≦ x (λ (k) (and (> (prime k x) 0)
                      (= (prime (+ k 1) x) 0)))))

; 素因数分解を利用する版
(define (len x)
  (none-to-zero (factorization-length (factorization x))))
```

定義8 列の連結

まずは元の定義・・・

```
(: M8 (-> GSequence GSequence Natural))
(define (M8 x y)
  (expt (P (+ (len x) (len y))) (+ x y)))

(: ** (-> GSequence GSequence GSequence))
(define (** x y)
  (gsequence+
   (Min≦ (M8 x y)
         (λ (z)
           (and (∀≦ (len x)
                    (λ (m) (⇒ (<= 1 m)
                              (= (elm (gsequence+ z) m)
                                 (elm x m)))))
                (∀≦ (len y)
                    (λ (n) (⇒ (<= 1 n)
                              (= (elm (gsequence+ z)
                                      (+ (len x) n)) (elm y n))))))))))
```

これは若干不満
型のエラーが出ないようにしていったらこうなってしまったんですが
`z`が`Natural`型で`elm`に適用するとき`GSequence`型にしてるというのが
最初から`z`を`GSequence`型にしたいんですけど
そうすると`Min≦`の引数の型に合わなくなってしまうし
`Min≦`(や`∀≦`や`∃≦`)をPolymorficな関数にするのがいいのかな？

```
(: Min≦ (-> Natural (-> Natural Boolean) Natural))
```

の代わりに

```
(: Min≦ (All (a) (-> a (-> a Boolean) a)))
```

とか
そうすると今度は`inst`が必要になってきてしまうし・・・
`All`といっても何でもOKってわけじゃないし・・・
どう書くんでしょうね？

なかなか難しいです
