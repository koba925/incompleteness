# ゲーデルの不完全性定理の証明のアレをRacketで書いてみる (27)

定義37 zは、yの中でvが"自由"な範囲に、"束縛"された"変数"を持たない

```
(define (IsNotBoundIn z y v)
  (not (∃ n ≦ (len y)
          (∃ m ≦ (len z)
             (∃ w ≦ z
                (and (= w (elm z m))
                     (IsBoundAt w n y)
                     (IsFreeAt v n y)))))))
```

こういうの見るとつい
束縛が入れ子になってるところは・・・
変数名がカブってたら・・・
再帰的にやらなくて大丈夫かな・・・
みたいに心配になってしまいますが
大丈夫でない例は思いつきません
今まで数え切れないほどの人が検証してるはずですしね

定義38 xは公理III-1から得られる"論理式"である
定義39 xは公理III-2から得られる"論理式"である
定義 xは公理IIIから得られる"論理式"である

```
(define (IsSchemaIII k x)
  (case k
    ((1) (∃ v y z n ≦ x
            (and (IsVarType v n)
                 (IsNthType z n)
                 (IsForm y)
                 (IsNotBoundIn z y v)
                 (= x (Implies (ForAll v y)
                               (subst y v z))))))
    ((2) (∃ v q p ≦ x
            (and (IsVar v)
                 (IsForm p)
                 (not (IsFree v p))
                 (IsForm q)
                 (= x (Implies (ForAll v (Or p q))
                               (Or p (ForAll v q)))))))))

(define (IsAxiomIII x)
  (or (IsSchemaIII 1 x)
      (IsSchemaIII 2 x)))
```

`IsAxiomIII`はここにあるほうが似つかわしいですよね

定義40 xは公理IVから得られる"論理式"である

```
(define (IsAxiomIV x)
  (∃ u v y n ≦ x
     (and (IsVarType u (+ n 1))
          (IsVarType v n)
          (not (IsFree u y))
          (IsForm y)
          (= x
             (Exists u (Forall v (Equiv (** (<> u)
                                            (paren (<> v)))
                                          y)))))))
```

しょうもない話ですが
`(not (IsFree u y))`の前に`(IsForm y)`のチェックをするほうがいいような
気がしませんか
定義38とか39ではそんな感じの順になってますし
数式だと評価順とかどう考えるのかな
