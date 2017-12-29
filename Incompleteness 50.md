# ゲーデルの不完全性定理の証明のアレをRacketで書いてみる (50)

定義21 "¬(a)"または"(a)∨(b)"または"∀v(a)"である

```
(: IsNotOp (-> GSequence GSequence Boolean))
(define (IsNotOp x a) (= x (Not a)))

(: IsOrOp (-> GSequence GSequence GSequence Boolean))
(define (IsOrOp x a b) (= x (Or a b)))

(: IsForallOp (-> GSequence GSequence Boolean))

;元のソース
(define (IsForallOp x a)
  (∃≦ x (λ ([v : Natural]) (and (IsVar (gsymbol+ v))
                                (= x (ForAll (gsymbol+ v) a))))))

;書き直した版
(define (IsForallOp x a)
  (let ((v : GSymbol (gsymbol+ (elm x 2))))
    (and (IsVar v)
         (= x (ForAll v a)))))

(: IsOp (-> GSequence GSequence GSequence Boolean))
(define (IsOp x a b)
  (or (IsNotOp x a)
      (IsOrOp x a b)
      (IsForallOp x a)))
```

次から列の列が現れてもう根本的に動かせないエリア

定義22 "基本論理式"から組み上げた"論理式"の列である

あ、`∃＜`作んなきゃな、と思ったけど
考えてみたら`<=`と`<`を引数で渡すようにするだけじゃない？
と思って直してみたら

```
(: ∃ (-> (-> Real Real Boolean)
         Natural
         (-> Natural Boolean) 
         Boolean))
(define (∃ cmp max f)
  (let loop ((x : Natural 0))
    (cond ((not (cmp x max)) #f)
          ((f x) #t)
          (else (loop (+ x 1))))))
```

普通に大丈夫そう

```
(: IsFormSeq (-> GSeqSeq Boolean))
(define (IsFormSeq x)
  (and (> (len x) 0)
       (∀ <= (len x)
           (λ (n)
             (⇒ (> n 0)
                (or (IsElementForm (gsequence+ (elm x n)))
                    (∃ < n
                       (λ (p)
                         (∃ < n
                            (λ (q)
                              (and (> p 0) (> q 0)
                                   (IsOp (gsequence+ (elm x n))
                                         (gsequence+ (elm x p))
                                         (gsequence+ (elm x q))))))))))))))
```

なんか前に考えて変なやり方した記憶が

17回目

> いったん逃避して
> 
> ```
> (define (IsFormSeq x)
>       :
>                  (∃ p q ＜ n
>                        :
> ```
> 
> と書けるようにしますかね

`<=`と`<`を引数で渡すと何かまずいからやめたんだっけ？
たぶん考えてなかったんだな・・・

関数の型宣言で仮引数を縦にならべるようになると、
こういう書き方のほうが見やすそうだな
ここからこっち式で書いてみよう

```
(define (∃ [cmp : (-> Real Real Boolean)]
           [max : Natural]
           [f : (-> Natural Boolean)]) : Boolean
  (let loop ((x : Natural 0))
    (cond ((not (cmp x max)) #f)
          ((f x) #t)
          (else (loop (+ x 1))))))
```