# ゲーデルの不完全性定理の証明のアレをRacketで書いてみる (53)

定義25 "変数"vはxのn番目の場所では"束縛"されてない

```
(define (IsFreeAt [v : GSymbol]
                  [n : Natural]
                  [x : GForm]) : Boolean
  (and (IsVar v)
       (IsForm x)
       (= v (elm x n))
       (<= n (len x))
       (not (IsBoundAt v n x))))
```

定義26 vはxの"自由変数"である

```
(define (IsFree [v : GSymbol]
                [x : GForm]) : Boolean
  (∃ n <= (len x) (IsFreeAt v n x)))
```


定義27 xのn番目の要素をcで置き換えたもの

```
(define (substAtWith [x : GSequence]
                     [n : Natural]
                     [c : GSequence])
  (Min z <= (M8 x c)
       (∃ a b <= x
          (and (= n (+ (len (gsequence+ a)) 1))
               (= x (** (** (gsequence+ a)
                            (<> (gsymbol (elm x n))))
                        (gsequence+ b)))
               (= z (** (** a c) b))))))
```

`c`は`GSymbol`じゃないかと一瞬思ったけど列だった

定義28 xでk+1番目の"自由"であるvの場所

```
(define (freepos [k : Natural]
                 [v : GSymbol]
                 [x : GForm]) : Natural
  (cond ((= k 0)
         (Min n <= (len x)
              (and (IsFreeAt v n x)
                   (not (∃ p <= (len x)
                           (and (< n p)
                                (IsFreeAt v p x)))))))
        (else
         (Min n < (freepos (- k 1) v x)
              (and (IsFreeAt v n x)
                   (not (∃ p < (freepos (- k 1) v x)
                           (and (< n p)
                                (IsFreeAt v p x)))))))))
```

定義29 xで、vが"自由"である場所の総数

```
(define (freenum [v : GSymbol]
                 [x : GForm]) : Natural
  (Min n <= (len x) (= (freepos n v x) 0)))
```

定義30 xの"自由"であるvの場所のうち、k個をcで置き換えた論理式

```
(define (substSome [k : Natural]
                   [x : GForm]
                   [v : GSymbol]
                   [c : GSequence]) : GForm
  (cond ((= k 0) x)
        (else
         (gform
          (substAtWith (substSome (- k 1) x v c)
                       (freepos (- k 1) v x)
                       c)))))
```
