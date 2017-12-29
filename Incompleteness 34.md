# ゲーデルの不完全性定理の証明のアレをRacketで書いてみる (34)
というわけでマクロなし作戦

```
(: ∀≦ (-> Natural (-> Natural Boolean) Boolean))
(define (∀≦ max f)
  (let loop ((x : Natural 0))
    (cond ((> x max) #t)
          ((not (f x)) #f)
          (else (loop (+ x 1))))))
```

これならちゃんと理解できる範囲

`x`には型を書いてやらないとエラーになりました

```
  Type Checker: type mismatch
  expected: Nonnegative-Integer
  given: Integer in: x
```

0に初期化されてるからIntegerってことなんでしょうね
`f`の引数になってるってとこまで見てくれたらNaturalだってことがわかると思うんですが

逆に、fの引数をIntegerにしちゃってもそんなに問題はないと思うんですけど
このままのほうがていねいだと思うのでこのままで

```
> (∀≦ 3 (λ (x) (< x 4)))
- : Boolean
#t
```

あーすっきり

とりあえず`∃≦`と`Min≦`だけ作って先へ進みます
ほかは必要になってから

```
(: ∃≦ (-> Natural (-> Natural Boolean) Boolean))
(define (∃≦ max f)
  (let loop ((x : Natural 0))
    (cond ((> x max) #f)
          ((f x) #t)
          (else (loop (+ x 1))))))

(: Min≦ (-> Natural (-> Natural Boolean) Natural))
(define (Min≦ max f)
  (let loop ((x : Natural 0))
    (cond ((> x max) 0)
          ((f x) x)
          (else (loop (+ x 1))))))
```

型の定義
志高く始めておいて、困難にぶち当たったら緩めていく方針で

```
; ゲーデル数は自然数の一種
(define-new-subtype GNumber (gnumber Natural))

; 記号はゲーデル数の一種
(define-new-subtype GSymbol (gsymbol GNumber))

; 列はゲーデル数の一種
(define-new-subtype GSequence (gsequence GNumber))

; 論理式は列の一種
(define-new-subtype GForm (gform GSequence))

; 列の列は列の一種
(define-new-subtype GSeqSeq (gseqseq GSequence))

; 証明は列の列の一種
(define-new-subtype GProve (gprove GSeqSeq))
```

ではいよいよ

定義1 xはdで割り切れる

本のままの定義も書き直した定義も同じ型（当たり前）
実際にはどっちかコメントアウトしています

ここはまだ普通の自然数の話なので`x`や`d`は`GNumber`ではなく
`Natural`としました

```
(: CanDivide (-> Natural Natural Boolean))

; もとの定義
(define (CanDivide x d)
  (∃≦ x (λ (n) (= x (* d n)))))

; 書き直した定義
(define (CanDivide x d)
  (and (not (zero? d))
       (= (remainder x d) 0)))
```

どちらの定義でも型宣言がないとポロポロエラーが出ますので
やっぱり書いておかないといけないようです

Haskellだととりあえず書いてからこれ型なに？って聞ける感じでしたが
そこまで賢くはないのかな
なにか原理的にできない理由があるのか
まだTyped Haskellが発展途上だからなのか
それともNaturalを使ってるからなんだろうか

定義2 xは素数である

```
(: IsPrime (-> Natural Boolean))

; もとの定義
(define (IsPrime x)
  (and (> x 1)
       (not (∃≦ x (λ (d) (and (not (= d 1))
                              (not (= d x))
                              (CanDivide x d)))))))

; 書き直した定義
(define (IsPrime x)
  (and (> x 1)
       (let loop ((d : Natural 2))
         (cond ((> (* d d) x) #t)
               ((CanDivide x d) #f)
               (else (loop (+ d 1)))))))
```

定義3 n番目の、xの素因数

素因数分解を使う版は`P`を使ってるので後回し

```
; もとの定義

(: CanDivideByPrime (-> Natural Natural Boolean))
(define (CanDivideByPrime x p)
  (and (CanDivide x p) (IsPrime p)))

(: prime (-> Natural Natural Natural))
(define (prime n x)
  (cond ((= n 0) 0)
        (else (Min≦ x (λ (p)
                        (and (< (prime (- n 1) x) p)
                             (CanDivideByPrime x p)))))))
```

調子出てきた