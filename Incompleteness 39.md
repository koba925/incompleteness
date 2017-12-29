# ゲーデルの不完全性定理の証明のアレをRacketで書いてみる (39)
では本体に戻って書き換え
型の部分を再掲

```
(struct None ())
(struct (a) Some ([v : a]))
(define-type (Opt a) (U None (Some a)))

(define-type Factor (Pairof Natural Natural))
(define-type Factorization (Listof Factor))
```

`Factorization`は`(Listof Factor)`か``(Listof (Opt Factor))`か
ちょっと悩みましたがところどころ`Factor`が`None`になってることは
ないだろうということで`(Listof Factor)`でいきます
うまく書けなかったら戻る

つまり`Factorization`全体がまるごと`None`になるという表現

```
(: factorizations (HashTable Natural (Opt Factorization)))
(define factorizations (make-hash))
(hash-set! factorizations 0 None)
(hash-set! factorizations 1 None)
```

でもなんで書き換え前はエラーの値を`()`じゃなくて`((0 . 0))`に
してたんだっけということが気になったり
なにかそのほうが便利だったかな
まあいいや

`None`を返さなければいけない特殊ケースはすでに
`factorizations`の中に登録してあるので
素因数分解を実際に計算する部分には`None`は出てきません
関数に型をつけただけ

```
(: factorization (-> Natural (Opt Factorization)))
(define (factorization x)
  (cond ((hash-ref factorizations x #f))
        (else
         (let loop ((n : Natural 1)
          :
```
