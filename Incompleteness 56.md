# ゲーデルの不完全性定理の証明のアレをRacketで書いてみる (56)

定義36 xは公理IIから得られる"論理式"である

```
(define (IsAxiomII [x : GForm]) : Boolean
  (or (IsSchemaII 1 x)
      (IsSchemaII 2 x)
      (IsSchemaII 3 x)
      (IsSchemaII 4 x)))
```

定義37 zは、yの中でvが"自由"な範囲に、"束縛"された"変数"を持たない

```
(define (IsNotBoundIn [z : GForm]
                      [y : GForm]
                      [v : GSymbol]) : Boolean
  (not (∃ n <= (len y)
          (∃ m <= (len z)
             (∃ w <= z
                (and (= w (elm z m))
                     (IsBoundAt (gsymbol+ w) n y)
                     (IsFreeAt v n y)))))))
```

定義38 xは公理III-1から得られる"論理式"である
定義39 xは公理III-2から得られる"論理式"である
定義 xは公理IIIから得られる"論理式"である

```
(define (IsSchemaIII [k : Natural]
                     [x : GForm]) : Boolean
  (case k
    ((1) (∃ v y z n <= x
            (and (IsVarType (gsymbol+ v) n)
                 (IsNthType (gsequence+ z) n)
                 (IsForm (gsequence+ y))
                 (IsNotBoundIn (gsequence+ z) (gform+ y) (gsymbol+ v))
                 (= x (Implies (ForAll (gsymbol+ v)
                                       (gform+ y))
                               (subst (gform+ y)
                                      (gsymbol+ v)
                                      (gsequence+ z)))))))
    ((2) (∃ v q p <= x
            (and (IsVar (gnumber v))
                 (IsForm (gsequence+ p))
                 (not (IsFree (gsymbol+ v) (gform+ p)))
                 (IsForm (gsequence+ q))
                 (= x (Implies (ForAll (gsymbol+ v)
                                       (Or (gform+ p)
                                           (gform+ q)))
                               (Or (gform+ p)
                                   (ForAll (gsymbol+ v)
                                           (gform+ q))))))))
    (else #f)))

(define (IsAxiomIII [x : GForm])
  (or (IsSchemaIII 1 x)
      (IsSchemaIII 2 x)))
```