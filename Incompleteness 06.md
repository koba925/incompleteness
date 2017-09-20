# ゲーデルの不完全性定理の証明のアレをRacketで書いてみる (6)

## 「10.8.3 列」の続き

> ```
> > (time (** 12 4))  ; [2 1]という列と[2]という列の連結
> cpu time: 35571 real time: 35565 gc time: 1153
> 300
> > (time (** 12 12)) ; [2 1]という列と[2 1]という列の連結
> 
> ```
> 
> お返事がありません

書き直し

```
; 元のソース
;(define (M8 x y)
;  (expt (P (+ (len x) (len y))) (+ x y)))
;
;(define (** x y)
;  (Min z ≦ (M8 x y)
;       (and (∀ m ≦ (len x)
;               (⇒ (<= 1 m) (= (elm z m) (elm x m))))
;            (∀ n ≦ (len y)
;               (⇒ (<= 1 n) (= (elm z (+ (len x) n)) (elm y n)))))))

(define (** x y)
  (let ((lenx (len x)))
    (let loop ((k 1) (n x))
      (let ((yk (elm y k)))
        (if (= yk 0)
            n
            (loop (+ k 1) (* n (expt (P (+ lenx k)) yk))))))))
```

実行

```
> (time (** 12 4))
cpu time: 0 real time: 0 gc time: 0
300
> (time (** 12 12))
cpu time: 0 real time: 1 gc time: 0
2100
```

おｋ

定義9 xだけからなる列

```
(define (<> x) (expt 2 x))
```

定義10 xをカッコに入れた列

```
(define (paren x)
  (** (** (<> clp) x) (<> crp)))
```

```
> (time (paren (<> c0)))
cpu time: 55863 real time: 55844 gc time: 3880
7500000000000
```

さっきのテストじゃ全然甘かった
つらい
ただの`(0)`という式を作るだけで1分かかることもつらければ
`(0)`のゲーデル数が7500000000000というのも相当なつらさ
いや相当などころではないか
式の列を作ったら最低でも2^7500000000000になるんだもんな

先のことは考えず目の前の問題を何とかする方向で
調べてみると簡単にプロファイルが取れるようなのでやってみます

```
> (require profile)
> (profile (paren (<> c0)))
Profiling results
-----------------
  Total cpu time observed: 53481ms (out of 53918ms)
  Number of samples taken: 936 (once every 57ms)

============================================================================
                                  Caller
 Idx    Total         Self      Name+src                              Local%
        ms(pct)       ms(pct)     Callee
============================================================================
                                  loop [14]                           100.0%
 [1] 53481(100.0%) 13314(24.9%) ??? ...udy/Incompleteness/incompleteness.rkt:30:27
                                  CanDivideByPower [3]                 59.3%
                                  loop [14]                            29.5%
----------------------------------------------------------------------------
 [2] 53481(100.0%)     0(0.0%)  loop ...ib/mred/private/wx/common/queue.rkt:400:18
                                  call-with-break-parameterization [4]100.0%
----------------------------------------------------------------------------
                                  ??? [1]                             100.0%
 [3] 53481(100.0%)  2100(3.9%)  CanDivideByPower ...eness/incompleteness.rkt:157:0
                                  loop [14]                            96.1%
----------------------------------------------------------------------------
                                  ??? [11]                             50.0%
                                  loop [2]                             50.0%
 [4] 53481(100.0%)     0(0.0%)  call-with-break-parameterization ...heme.rkt:147:2
                                  ??? [6]                              50.0%
                                  loop [5]                             50.0%
----------------------------------------------------------------------------
                                  call-with-break-parameterization [4]100.0%
 [5] 53481(100.0%)     0(0.0%)  loop .../drracket/drracket/private/rep.rkt:1131:24
                                  call-with-exception-handler [8]     100.0%
----------------------------------------------------------------------------
                                  call-with-break-parameterization [4]100.0%
 [6] 53481(100.0%)     0(0.0%)  ??? ...lib/mred/private/wx/common/queue.rkt:505:32
                                  ??? [7]                             100.0%
----------------------------------------------------------------------------
                                  ??? [6]                             100.0%
 [7] 53481(100.0%)     0(0.0%)  ??? ...-lib/mred/private/wx/common/queue.rkt:454:6
                                  loop [9]                            100.0%
----------------------------------------------------------------------------
                                  loop [5]                            100.0%
 [8] 53481(100.0%)     0(0.0%)  call-with-exception-handler ...re-scheme.rkt:264:2
                                  profile-thunk14 [10]                100.0%
----------------------------------------------------------------------------
                                  ??? [7]                             100.0%
 [9] 53481(100.0%)     0(0.0%)  loop .../drracket/drracket/private/rep.rkt:1426:17
                                  ??? [11]                            100.0%
----------------------------------------------------------------------------
                                  call-with-exception-handler [8]     100.0%
[10] 53481(100.0%)     0(0.0%)  profile-thunk14 ...e/pkgs/profile-lib/main.rkt:9:0
                                  run [12]                            100.0%
----------------------------------------------------------------------------
                                  loop [9]                            100.0%
[11] 53481(100.0%)     0(0.0%)  ??? ...gs/drracket/drracket/private/rep.rkt:1105:9
                                  call-with-break-parameterization [4]100.0%
----------------------------------------------------------------------------
                                  profile-thunk14 [10]                100.0%
[12] 53481(100.0%)     0(0.0%)  run ...t v6.2/share/pkgs/profile-lib/main.rkt:31:2
                                  loop [13]                           100.0%
----------------------------------------------------------------------------
                                  run [12]                            100.0%
[13] 53481(100.0%)     0(0.0%)  loop ...dy/Incompleteness/incompleteness.rkt:205:4
                                  loop [14]                           100.0%
----------------------------------------------------------------------------
                                  ??? [1]                              20.5%
                                  CanDivideByPower [3]                 37.8%
                                  loop [13]                            41.7%
[14] 53481(100.0%) 33580(62.8%) loop ...dy/Incompleteness/incompleteness.rkt:22:11
                                  ??? [1]                              73.4%
                                  CanDivideByPrime [15]                 3.2%
----------------------------------------------------------------------------
                                  loop [14]                           100.0%
[15]  4486(8.4%)    4486(8.4%)  CanDivideByPrime ...eness/incompleteness.rkt:102:0
----------------------------------------------------------------------------
7500000000000
> 
```

Nameは関数名のはずなんですが`???`は何でしょう
マクロかな？ → ラムダのことでした なるほど
それに`loop`もたくさん出てくるせいでわかりづらいですが何しろ

```
[14] 53481(100.0%) 33580(62.8%) loop ...dy/Incompleteness/incompleteness.rkt:22:11
```

が一番時間を取ってるみたいなのでここを見てます
`???`→`CanDivideByPower`→`loop`→`???`→CanDivideByPrimeという呼び出し順
`CanDivideByPower`は`elm`からしか呼ばれてませんね
そして`CanDivideByPower`が`prime`を、`prime`が`CanDivideByPrime`を
呼んでいます

```
(define (elm x n)
  (Min k ≦ x (and (CanDivideByPower x n k)
                  (not (CanDivideByPower x n (+ k 1))))))

(define (CanDivideByPower x n k)
  (CanDivide x (expt (prime n x) k)))

(define (prime n x)
  (cond ((= n 0) 0)
        (else (Min p ≦ x (and (< (prime (- n 1) x) p)
                              (CanDivideByPrime x p))))))

(define (CanDivideByPrime x p)
  (and (CanDivide x p) (IsPrime p)))
```

このへんの`Min`の入れ子で時間がかかってるというのはありそうな話です
`IsPrime`以下は一応書き換え済みなのでこの範囲で高速化してみますか

今日はここまで