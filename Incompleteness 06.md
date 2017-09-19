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

つまりどんなときもかならず1から順番に探すという作戦が無理あるんだよなー
せっかくマクロ書いたけどさー
どうしようかなー