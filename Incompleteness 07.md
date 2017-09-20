# ゲーデルの不完全性定理の証明のアレをRacketで書いてみる (7)

## 「10.8.3 列」の続きの続き

> このへんの`Min`の入れ子で時間がかかってるというのはありそうな話です
> `IsPrime`以下は一応書き換え済みなのでこの範囲で高速化してみますか

`prime`を書き直し
素数が歯抜け無しで出てくるとは限らない、という前提はキープ

```
; 元のソース
;(define (CanDivideByPrime x p)
;  (and (CanDivide x p) (IsPrime p)))
;
;(define (prime n x)
;  (cond ((= n 0) 0)
;        (else (Min p ≦ x (and (< (prime (- n 1) x) p)
;                              (CanDivideByPrime x p))))))

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

`elm`を書き直し
これで`∀`・`∃`・`Min`を使っている関数は`len`しかなくなりました

```
; 元のソース
;(define (CanDivideByPower x n k)
;  (CanDivide x (expt (prime n x) k)))
;
;(define (elm x n)
;  (Min k ≦ x (and (CanDivideByPower x n k)
;                  (not (CanDivideByPower x n (+ k 1))))))

(define (elm x n)
  (let ((np (prime n x)))
    (let loop ((k 1) (x x))
      (cond ((not (CanDivide x np)) (- k 1))
            (else (loop (+ k 1) (/ x np)))))))
```

実行

```
> (time (paren (<> c0)))
cpu time: 14 real time: 14 gc time: 0
7500000000000
```

おｋ

しかしこれも2^7500000000000とかが出てくるまでの命
