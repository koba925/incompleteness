# ゲーデルの不完全性定理の証明のアレをRacketで書いてみる (8)

## 「10.8.4 変数・記号・論理式」

ここらへんから証明っぽくなってきます

定義11 xは"第n型"の"変数"である

```
(define (IsVarBase p)
  (and (> p crp) (IsPrime p)))

(define (IsVarType x n)
  (and (>= n 1)
       (∃ p ≦ x (and (IsVarBase p) (= x (expt p n))))))
```

これはどうかな

```
> (time (IsVarType (+ (expt 17 3) 1) 3))
cpu time: 7 real time: 6 gc time: 0
#f
```

微妙なところ
型が大きいと時間がかかりそうですが先へ進みます

ところで

> 僕「"変数"の" "の意味は？」
> ミルカ「メタ数学の概念という意味」

実はここで何を言っているかすっきりわかってなかったりするんですよね
P上の変数と、メタ数学上の"変数"があるってことですよね

ただの数であってP上の数(fff0とか)じゃないから、P上の変数じゃないってこと？
いやP上の変数はx1とかだし
xの実体はただの数で、メタ数学上の解釈で初めて変数になる、ってことですかね

たとえば2^9*3^17^2*5^11*7^17*11^13っていう数は
`∀x2(x1)`っていう式を表してるわけですけどこれはPの式？
それともメタ数学の概念？
一見Pの式には見えませんが、見慣れない文字と表記方法で書いてるってだけなんですよね
だから自分的にはPの式に思えるんですけどね

> ミルカ「つまり、xは意味の世界での変数を表しているのではない。形式的
> 　　体系Pのほうで定義された変数のゲーデル数を表しているんだ」

ミルカさんもそう言ってはいると思うんですが
「メタ数学の概念」という言葉がピンときてません
もしかして大事なことかもしれませんがこれくらいにして

このへんで、ゲーデル数を直接作る関数を作っておきます
これからテストで使いそうなので

まずは変数を`(expt 17 3)`とか書かないでいいようにしておきます
x1を`(var 1 1)`、z3を`(var 3 3)`などと書くことにします

```
(define (var n c) (expt (P (+ 6 n)) c))
```

ゲーデル数を作る方

```
(define (gnum . seq)
  (define (iter s k n)
    (if (null? s)
        n
        (iter (cdr s) (+ k 1) (* (expt (P k) (car s)) n))))
  (iter seq 1 1))
```

さっきの`∀x2(x1)`で試してみましょう

```
> (gnum call (var 1 2) clp (var 1 1) crp)
155150868122395845239730380258828948544470333645526905417109467051986545180149117421814751229978010870695495625428839601792638246935390849457292566300089904991446507775000000000
```

合ってるかどうかさっぱりわかりませんね

```
> (* (expt 2 call)
     (expt 3 (var 1 2))
     (expt 5 clp)
     (expt 7 (var 1 1))
     (expt 11 crp))
155150868122395845239730380258828948544470333645526905417109467051986545180149117421814751229978010870695495625428839601792638246935390849457292566300089904991446507775000000000
```

やや信頼性に欠けるテストですがまあいいことにしましょう

定義12 xは"変数"である

```
(define (IsVar x)
  (∃ n ≦ x (IsVarType x n)))
```

定義13 ¬(x)

素直に式の形を表現していきます
定義に寄ってxがひとつの記号を表してたり列を表してたりするので注意が必要
この定義では列ですね

```
(define (¬ x)
  (** (<> cnot) (paren x)))
```

```
> (profile (paren (<> 7)))
Profiling results
-----------------
  Total cpu time observed: 61815ms (out of 63058ms)
  Number of samples taken: 1121 (once every 55ms)
  (Hiding functions with self<1.0% and local<2.0%: 1 of 16 hidden)

============================================================================
                                  Caller
 Idx    Total         Self      Name+src                              Local%
        ms(pct)       ms(pct)     Callee
============================================================================
 :
 略
 :
------------------------------------------------------------------------
                                  run [10]                            100.0%
[11] 61815(100.0%)     0(0.0%)  ** ...tudy/Incompleteness/incompleteness.rkt:265:0
                                  loop [12]                           100.0%
----------------------------------------------------------------------------
                                  ** [11]                             100.0%
[12] 61815(100.0%)    50(0.1%)  loop ...dy/Incompleteness/incompleteness.rkt:242:6
                                  loop [13]                            99.9%
----------------------------------------------------------------------------
                                  loop [12]                           100.0%
[13] 61765(99.9%)    432(0.7%)  loop ...y/Incompleteness/incompleteness.rkt:135:14
                                  loop [14]                            98.9%
----------------------------------------------------------------------------
                                  loop [13]                           100.0%
[14] 61065(98.8%)  40141(64.9%) loop ...y/Incompleteness/incompleteness.rkt:172:14
                                  loop [15]                            34.3%
----------------------------------------------------------------------------
                                  loop [14]                           100.0%
[15] 20924(33.8%)  20924(33.8%) loop ...dy/Incompleteness/incompleteness.rkt:107:7
----------------------------------------------------------------------------
5467500000000000
> 
```

incompleteness.rkt:172:14は`primes`のloop、ncompleteness.rkt:107:7は`P`のloopです
ほとんどが素数を求めてる時間っぽいですね

同じことをもういちど実行すると瞬時に計算が終わります
素数をハッシュに覚えたままだからかと
もしかして今まで高速化できたと思ってたのもそのせいかも（涙

```
> (profile (paren (<> 7)))
Profiling results
-----------------
  Total cpu time observed: 98ms (out of 141ms)
  Number of samples taken: 2 (once every 49ms)

========================================================================
                              Caller
 Idx  Total      Self       Name+src                              Local%
      ms(pct)    ms(pct)      Callee
========================================================================
 :
 略
 :
------------------------------------------------------------------------
                              run [10]                            100.0%
[11]  98(100.0%)  0(0.0%)   ** ...tudy/Incompleteness/incompleteness.rkt:265:0
                              loop [12]                           100.0%
------------------------------------------------------------------------
                              ** [11]                             100.0%
[12]  98(100.0%)  0(0.0%)   loop ...dy/Incompleteness/incompleteness.rkt:242:6
                              loop [13]                           100.0%
------------------------------------------------------------------------
                              loop [12]                           100.0%
[13]  98(100.0%) 98(100.0%) loop ...y/Incompleteness/incompleteness.rkt:135:14
------------------------------------------------------------------------
5467500000000000
```

うーんどうするかなあ
ちょっとアイデアがありません
ここらへんであきらめて関数を定義するだけにする？



