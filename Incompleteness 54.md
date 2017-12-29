# ゲーデルの不完全性定理の証明のアレをRacketで書いてみる (54)

結局キャストをいっぱい書くのが面倒になってこんな関数を作っていたりする

```
(: gsequence+ (case-> (-> Natural GSequence)
                    (-> GNumber GSequence)))
(define gsequence+
  (case-lambda [([x : Natural]) (gsequence (gnumber x))]
               [([x : GNumber]) (gsequence x)]))

(: gform+ (case-> (-> Natural GForm)
                  (-> GNumber GForm)
                  (-> GSequence GForm)))
(define gform+
  (case-lambda [([x : Natural]) (gform (gsequence+ x))]
               [([x : GNumber]) (gform (gsequence+ x))]
               [([x : GSequence]) (gform (gsequence x))]))

(: gseqseq+ (case-> (-> Natural GSeqSeq)
                    (-> GNumber GSeqSeq)
                    (-> GSequence GSeqSeq)))
(define gseqseq+
  (case-lambda [([x : Natural]) (gseqseq (gsequence+ x))]
               [([x : GNumber]) (gseqseq (gsequence+ x))]
               [([x : GSequence]) (gseqseq  x)]))
```

こういうのを書いてるとうっかり型を間違えててもすんなり通してしまうような気もする
唯一`GForm`型から`GSeqSeq`型にはキャストしてないところだけが残ったこだわり

あとこれも
`gnum`の戻りの型を変えるためだけの関数

```
(: gfrm (-> GNumber * GForm))
(define (gfrm . seq)
  (gform (apply gnum seq)))
```

`(gform (gnum ...))`という書き方があまりに頻出だったので

引数が`GNumber`だったら値は`GSequence`で
引数が`GSequence`だったら値は`GSeqSeq`、なら`case-lambda`でカバーできそうだけど
`GSequence`か`GForm`かは引数の型からは判断できないし
第一自分でもあやふやだったりするから
いまでもああでもないこうでもないと型を変えたりしてるし

`GSymbol`、`GForm`、`GProve`という型は導入せずに
`GNumber`→`GSequence`→`GSeqSeq`という一列の型だけにしておくくらいが
いい塩梅だったかもしれない

定義31 aの"自由"であるvをすべてcで置換した"論理式"

```
(define (subst [a : GForm]
               [v : GSymbol]
               [c : GSequence])
  (substSome (freenum v a) a v c))
```

定義32 "(a)→(b)"、"(a)∧(b)"、"(a)⇄(b)"、"∃x(a)"を得る関数

```
(define (Implies [a : GForm]
                 [b : GForm]) : GForm
  (Or (Not a) b))
(define (And [a : GForm]
             [b : GForm]) : GForm
  (Not (Or (Not a) (Not b))))
(define (Equiv [a : GForm]
               [b : GForm]) : GForm
  (And (Implies a b) (Implies b a)))
(define (Exists [x : GSymbol]
                [a : GForm]) : GForm
  (Not (ForAll x (Not a))))
```
