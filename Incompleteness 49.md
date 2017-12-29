# ゲーデルの不完全性定理の証明のアレをRacketで書いてみる (49)

やっぱり型で悩んでいる
たとえば`(IsNumberType x)`で言うと
`x`は列であることを期待しているという意味では`GSequence`なんだけれども
`x`は1から順に総当りで試していくだけの数だから`GNumber`な気もする
一方、(2以上なら)どんな数だって素因数分解はできるわけだから
`GSequence`とみなせないわけじゃない

というわけでところどころ揺れている
このあとは`GSequence`と見なす方向で進めてみる

```
(: IsElementForm (-> GSequence Boolean))

;元のソース
(define (IsElementForm x)
  (∃≦ x (λ (a)
     (∃≦ x (λ (b)
        (∃≦ x (λ (n)
           (and (IsNthType (gsequence+ a) (+ n 1))
                (IsNthType (gsequence+ b) n)
                (= x (** (gsequence+ a)
                         (paren (gsequence+ b))))))))))))
```

`GSequence`かどうか確認してないのに`GSequence`に変換しているところが
気持ち悪いと言えば気持ち悪いんだけれども
0とか1とか素因数分解できないところは呼んだ先の関数でうまいことやってくれているはず
本当は「はず」をチェックしてくれるようにしたいんだけど
このへん、ゲーデルさんもけっこうギリギリの危ない橋を渡ってる気がするんだよな

```

; 素因数分解を使う版

; 変数の型
(: VarType (-> GNumber Natural))
(define (VarType x)
  (none-to-zero (factor-expt (factorization-nth (factorization x) 1))))

; 列の一部を取り出す

(: ExtractSequence (-> GSequence Natural Natural GSequence))
(define (ExtractSequence x s e)
  (match (factorization x)
    [(None) (gsequence+ 0)]
    [(Some f)
     (if (< (length f) e)
         (gsequence+ 0)
         (apply gnum
                (map gnumber
                     (map Factor-expt
                          (drop (take f e) (- s 1))))))]))

(define (IsElementForm x)
  (let ((l (len x)))
    (and (>= l 4)
         (let* ((a : GNumber (elm x 1))
                (b : GSequence (ExtractSequence x 3 (cast (- (len x) 1) Natural)))
                (n : Natural (VarType a)))
           (and (IsVar a)
                (= (nat (elm x 2)) (nat clp))
                (IsNthType b (cast (- n 1) Natural))
                (= (nat (elm x (len x))) (nat crp)))))))

```

うーん元よりけっこう行数が増える
進捗は今ひとつ

