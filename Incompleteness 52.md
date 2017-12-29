# ゲーデルの不完全性定理の証明のアレをRacketで書いてみる (52)

定義23 xは論理式である

```
(define (M23 [x : GSequence]) : Natural
  (expt (P (sqr (len x))) (* x (sqr (len x)))))

(define (IsEndedWith [n : GSeqSeq]
                     [x : GSequence]) : Boolean
  (= (elm n (len n)) x))

(define (IsForm [x : GSequence]) : Boolean
  (∃ n <= (M23 x) (and (IsFormSeq (gseqseq+ n))
                       (IsEndedWith (gseqseq+ n) x))))
```

定義24 "変数"vはxのn番目の場所では"束縛"されている


```
(define (IsBoundAt [v : GSymbol]
                   [n : Natural]
                   [x : GForm]) : Boolean
  (and (IsVar v)
       (IsForm x)
       (∃ a b c <= x (and (= x (** (** (gsequence+ a)
                                       (ForAll v (gsequence+ b)))
                                   (gsequence+ c)))
                          (IsForm b)
                          (<= (+ (len a) 1) n)
                          (<= n (+ (len a) (len (ForAll v b))))))))
```

ここで`x`は`GSequence`なのか`GForm`なのかまた悩んでたり
論理式だと確認されたものだけが入ってくるなら安心して`GForm`にすればいいけど
論理式だろうと期待されてるだけだったら`GSequence`にするほうがいいかも
うーんどっちなん
なにかすっきりする考え方ないかな


