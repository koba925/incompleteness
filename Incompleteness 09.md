# ゲーデルの不完全性定理の証明のアレをRacketで書いてみる (9)

## 「10.8.4 変数・記号・論理式」の続き

ものは試しで`prime`をこんな風にしてみました

```
(define (prime n x)
  (let ((p (P n)))
    (if (CanDivide x p) p 0)))
```

素因数分解には必ず2から歯抜け無しで素数が出てくるという前提
動かしてみると

```
> (profile (paren (<> 7)))
Profiling results
-----------------
  Total cpu time observed: 0ms (out of 0ms)
  Number of samples taken: 0 (once every 0ms)

====================================
                        Caller
Idx  Total    Self    Name+srcLocal%
     ms(pct)  ms(pct)   Callee
====================================
5467500000000000
```

一瞬にして終わってしまいました
ポイントは`prime`の方だったのか

```
(define (prime n x)
  (cond ((= n 0) 0)
        (else (let loop ((k 1) (cnt 0))
                (let* ((p (P k))
                      (c (if (CanDivide x p)
                            (+ cnt 1)
                            cnt)))
                  (cond ((= c n) p)
                        ((> p x) 0)
                        (else (loop (+ k 1) c))))))))
```

呼ばれるたびに素因数を試していくのでもったいないっちゃあもったいないとは
思ってたんですけどね

そうか、`len`がこれ以上素因数がないことを確認しようとしてるから
そのときxまでのすべての素数を求めることになりますね
これはかなり壮大な無駄でした

素因数が見つかったら割り算していけばいいか

```
(define (prime n x)
  ;(printf "prime ~a ~a~n" n x)
  (cond ((= n 0) 0)
        (else (let loop ((k 1) (cnt 0) (x x))
                (define (newc x p cnt)
                  (if (CanDivide x p) (+ cnt 1) cnt))
                (define (newx x p)
                  (if (CanDivide x p) (newx (/ x p) p) x))
                (let* ((p (P k))
                       (c (newc x p cnt))
                       (x (newx x p)))
                  (cond ((= c n) p)
                        ((= x 1) 0)
                        (else (loop (+ k 1) c x))))))))
```

やってることはもうほとんど素因数分解ですね
一度素因数分解して、結果を保持しておくという作戦もありそうです
遅そうだったらそこまでやろうかな

```
> (profile (paren (<> 7)))
Profiling results
-----------------
  Total cpu time observed: 0ms (out of 2ms)
  Number of samples taken: 0 (once every 0ms)

====================================
                        Caller
Idx  Total    Self    Name+srcLocal%
     ms(pct)  ms(pct)   Callee
====================================
5467500000000000
```

遅くなかったのでこれで
