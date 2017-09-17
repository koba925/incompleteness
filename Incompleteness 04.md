# ゲーデルの不完全性定理の証明のアレをRacketで書いてみる (4)

## 「10.8.2 整数論」の続き

（単位はミリ秒）

```
> (time (P 8))
cpu time: 12989 real time: 12992 gc time: 1441
19
```


これだとテストもままならなくなりそうなのでもうちょっと速くなるようにしてみますか

とりあえず簡単なところから

```
; もとの定義
;(define (CanDevide x d)
;  (∃ n ≦ x (= x (* d n))))

(define (CanDevide x d)
  (= (remainder x d) 0))
```

測定

```
> (time (P 8))
cpu time: 10410 real time: 10410 gc time: 1369
19
```

もうちょっと減るかと思ったんだけどなあ
こっちのほうが効くかなあ

```
; もとの定義
;(define (IsPrime x)
;  (and (> x 1)
;       (not (∃ d ≦ x (and (not (= d 1))
;                          (not (= d x))
;                          (CanDevide x d))))))

(define (IsPrime x)
  (and (> x 1)
       (let loop ((d 2))
         (cond ((> (* d d) x) #t)
               ((CanDevide x d) #f)
               (else (loop (+ d 1)))))))
```

どれ

```
> (time (P 8))
cpu time: 5008 real time: 5006 gc time: 1103
19
```

お、効いた

```
> (time (P 9))
cpu time: 92482 real time: 92457 gc time: 3484
23
```

前は300秒位かかってたからまあマシに
とはいえ分単位で待つわけには

やっぱここが無駄多すぎるんだな
ちゃんとプロファイリングしないとダメですよってことでしょうか

```
(define (M5 n)
  (+ (factorial n) 1))

(define (P n)
  (cond ((= n 0) 0)
        (else (Min p ≦ (M5 n) (and (< (P (- n 1)) p)
                                   (IsPrime p))))))
```

`(P 8)`を求めるとき正直に`p`=1から始めて
しかもそれぞれの`p`につき`(P 7)`から`(P 0)`を計算し直してます
で、`(P 7)`を計算するときには正直に以下同文

ぱっと見`(M5 n)`まで計算するのかとまずびっくりしますが
実際のところ`p`が`(M5 n)`まで達することはないので実は関係なし

いろいろやりかたはあると思いますがとりあえず見つけた素数は覚えておくことに

```
(define primes (make-hash))
; 0番目の素数は0という定義
(hash-set! primes 0 0)
(hash-set! primes 1 2)

(define (P n)
  (cond ((hash-ref primes n #f))
        (else (let loop ((k (+ (P (- n 1)) 1)))
                (cond ((IsPrime k)
                       (hash-set! primes n k)
                       k)
                      (else (loop (+ k 1))))))))
```

どれ

```
> (time (P 9))
cpu time: 0 real time: 0 gc time: 0
23
> (time (P 100))
cpu time: 1 real time: 0 gc time: 0
541
> (time (P 1000))
cpu time: 11 real time: 11 gc time: 0
7919
> (time (P 10000))
cpu time: 261 real time: 261 gc time: 0
104729
```

よしこれくらいなら先へ進んでももうしばらくは動かせるかな

このへんってただたまたま論理式の表現方法に素数指数表現を選んだから
こうなってるってだけで別に不完全性とは関係ないとこなんですよねえ
あ、モジュールに分けたほうがキレイかも？

コード的には中途半端な気もするけどこれくらいで先に進もう