# ゲーデルの不完全性定理の証明のアレをRacketで書いてみる (55)

定義33 xを、nだけ"型持ち上げ"したもの

```
(define (typelift [n : Natural]
                  [x : GForm])
  (Min y <= (expt x (expt x n))
       (∀ k <= (len x)
          (or (and (not (IsVar (elm x k)))
                   (= (elm (gform+ y) k) (elm x k)))
              (and (IsVar (elm x k))
                   (= (elm (gform+ y) k)
                      (* (elm x k)
                         (expt (prime 1 (elm x k)) n))))))))
```

定義34 xは公理Iから得られる"論理式"である

等号

```
(define (ElementForm [a : GSymbol]
                     [b : GSequence]) : GForm
  (gform+ (** (<> a) (paren b))))

(define (Equal [x : GSequence]
               [y : GSequence]) : GForm
  (ForAll (var 3 2) (Implies (ElementForm (var 3 2) x)
                             (ElementForm (var 3 2) y))))
```

ペアノの公理Iの本体

型付け前のコード

```
(define AxiomI-1
  (Not (Equal (succ 1 (var 1 1)) (￣ 0))))
```

おめでとうございます
型のおかげで間違いが見つかりました

型付け後のコード
といっても関数の定義じゃなければ型はつけなくてもいいみたい
つけたほうが行儀が良いのかもしれないけど

```
(define AxiomI-1
  (Not (Equal (succ 1 (<> (var 1 1))) (￣ 0))))
```

本には関数の形で書いてなくて、公理を見て自分で関数に書き下したところ
さっそくこれですよ
コンピュータもない時代によく間違いなく書けたもんだなあ

以下同様

```

(define AxiomI-2
  (Implies (Equal (succ 1 (<> (var 1 1)))
                  (succ 1 (<> (var 2 1))))
           (Equal (<> (var 1 1))
                  (<> (var 2 1)))))

(define AxiomI-3
  (Implies (And (ElementForm (var 1 2) (￣ 0))
                (ForAll (var 1 1)
                        (Implies (ElementForm (var 1 2) (<> (var 1 1)))
                                 (ElementForm (var 1 2) (succ 1 (<> (var 1 1)))))))
           (ForAll (var 1 1) (ElementForm (var 1 2) (<> (var 1 1))))))

(define (IsAxiomI [x : GForm])
  (or (= x AxiomI-1)
      (= x AxiomI-2)
      (= x AxiomI-3)))
```

定義35 xは公理II-nから得られる"論理式"である

型を教えてあげるのがちょいと面倒だ
うまくOccurence Typingが使えれば楽になりそうだけど

で

```
(define (IsSchemaII [n : Natural]
                    [x : GForm]) : Boolean
  (case n
    ((1) (∃ p <= x (and (IsForm (gsequence+ p)) ... )
    ((2) (∃ p q <= x (and (IsForm (gsequence+ p)) ... )
    ((3) (∃ p q <= x (and (IsForm (gsequence+ p)) ... )
    ((4) (∃ p q r <= x (and (IsForm (gsequence+ p)) ... ))
```

だとエラー
`expected: Boolean given: Void`だとおっしゃる

`case`に`else`がないからかな
厳密に言えばそうなのかもだけど

ということはUnion使って

```
(define (IsSchemaII [n : (U 1 2 3 4)]
                    [x : GForm]) : Boolean
  ...)
```

ならOccurence Typingが効いて大丈夫かも、と思ったけどダメ
うーんそうなの？うまくいってもいいと思うんだけど

```
(define (test1 [x : (U 1 2)]) : Boolean
  (case x
    ((1) #t)
    ((2) #f)))
```

ダメか
elseを付けて、

```
(define (test1 [x : (U 1 2)]) : Boolean
  (case x
    ((1) #t)
    ((2) #f)
    (else #f)))
```

動かしてみる

```
> (test1 1)
- : Boolean
#t
> (test1 2)
- : Boolean
#f
> (test1 3)
. Type Checker: type mismatch
  expected: (U 2 One)
  given: Positive-Byte in: 3
```

実行時の型チェックは思った通りに動いてるみたいなんだけど
ところでなんとなく`expected: (U 2 One)`が気になる
もしかして・・・

```
(define (test1 [x : (U 0 1)]) : Boolean
  (case x
    ((0) #t)
    ((1) #f)))
```

これだと通るとは

```
> test1
- : (-> (U One Zero) Boolean)
#<procedure:test1>
> (test1 1)
- : Boolean
#f
```

ふーん
じゃあこれならどうなのよ

```
(define-type Two 2)
(define (test1 [x : (U One Two)]) : Boolean
  (case x
    ((1) #f)
    ((2) #f)))
```

ダメでした
むう

ちなみにこれも（ちゃんと）エラーになりました

```
(define (test1 [x : (U 0 1)]) : Boolean
  (case x
    ((1) #f)))
```

このくらいにしておこう
というわけでこれ

```
(define (IsSchemaII [n : (U 1 2 3 4)]
                    [x : GForm]) : Boolean
  (case n
    ((1) (∃ p <= x (and (IsForm (gsequence+ p))
                        (= x (Implies (Or (gform+ p)
                                          (gform+ p))
                                      (gform+ p))))))
    ((2) (∃ p q <= x (and (IsForm (gsequence+ p))
                          (IsForm (gsequence+ q))
                          (= x (Implies (gform+ p)
                                        (Or (gform+ p)
                                            (gform+ q)))))))
    ((3) (∃ p q <= x (and (IsForm (gsequence+ p))
                          (IsForm (gsequence+ q))
                          (= x (Implies (Or (gform+ p)
                                            (gform+ q))
                                        (Or (gform+ q)
                                            (gform+ p)))))))
    ((4) (∃ p q r <= x (and (IsForm (gsequence+ p))
                            (IsForm (gsequence+ q))
                            (IsForm (gsequence+ r))
                            (= x (Implies (Implies (gform+ p)
                                                   (gform+ q))
                                          (Implies (Or (gform+ r)
                                                       (gform+ p))
                                                   (Or (gform+ r)
                                                       (gform+ q))))))))
    (else #f)))
```

