# ゲーデルの不完全性定理の証明のアレをRacketで書いてみる (57)

定義40 xは公理IVから得られる"論理式"である

```
(define (IsAxiomIV [x : GForm])
  (∃ u v y n <= x
     (and (IsVarType (gsymbol+ u) (+ n 1))
          (IsVarType (gsymbol+ v) n)
          (IsForm (gsequence+ y))
          (not (IsFree (gsymbol+ u) (gform+ y)))
          (= x
             (Exists (gsymbol+ u)
                     (ForAll (gsymbol+ v)
                             (Equiv (gform+
                                     (** (<> (gsymbol+ u))
                                         (paren (<> (gsymbol+ v)))))
                                    (gform+ y))))))))
```


定義41 xは公理Vから得られる"論理式"である

```
(define AxiomV : GForm
  (Implies (ForAll (var 1 1)
                   (Equiv (ElementForm (var 1 2) (<> (var 1 1)))
                          (ElementForm (var 2 2) (<> (var 1 1)))))
           (Equal (<> (var 1 2)) (<> (var 2 2)))))

(define (IsAxiomV [x : GForm]) : Boolean
  (∃ n <= x (= x (typelift n AxiomV))))
```

定義42 xは"公理"である

```
(define (IsAxiom [x : GForm]) : Boolean
  (or (IsAxiomI x)
      (IsAxiomII x)
      (IsAxiomIII x)
      (IsAxiomIV x)
      (IsAxiomV x)))
```

あとちょっと
