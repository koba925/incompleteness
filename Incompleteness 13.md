# ゲーデルの不完全性定理の証明のアレをRacketで書いてみる (13)

## 「10.8.4 変数・記号・論理式」の続き

さて前回、勘違いに気づくまではもっと高速化しないとと思って
素因数分解する版を書き始めてました
乗った船なので続けてみます

素因数分解は12を`((2 . 2) (3 . 1))`と表現してみました
`((0 . 0))`というのは0とか1を因数分解しようとしたり
0番目の素因数を参照したりした時の値として使っています
ゲーデルの関数はそういった場合に0を返すようになっていて、
これでうまくいくんじゃないかと
あちこちにifを入れるよりさっぱりしそうなのでこうしてみました
かえってわかりにくくしてるかもなー

```
(define (times-divide x p)
  (let loop ((k 1) (x x))
    (cond ((not (CanDivide x p)) (values x (- k 1)))
          (else (loop (+ k 1) (/ x p))))))

; 0や1を与えられることもあるので場合分けして対応
(define (factorization x)
  (if (or (= x 0) (= x 1))
      '((0 . 0))
      (let loop ((n 1) (x x) (f '()))
        (if (= x 1)
            (reverse f)
            (let*-values (((pn) (P n))
                          ((x k) (times-divide x pn)))
              (loop (+ n 1)
                    x
                    (if (= k 0)
                        f
                        (cons (cons pn k) f))))))))

; 素因数分解した結果を扱うための関数たち

;こう書きたいところだが例外ケースを扱う必要がある
;(define factor-length length)
;(define (factor-nth f n) (list-ref f (- n 1)))

(define (factor-length f)
  (if (equal? f '((0 . 0)))
      0
      (length f)))

(define (factor-nth f n)
  (cond ((null? f) '(0 . 0))
        ((= n 1) (car f))
        (else (factor-nth (cdr f) (- n 1)))))

(define factor-prime car)
(define factor-expt cdr)

(define (IsNthType x n)
  (cond ((= n 1) (IsNumberType x))
        ((not (= (len x) 1)) #f)
        (else (let ((f (factorization (elm x 1))))
                (and (= (factor-length f) 1)
                     (> (factor-prime (car f)) crp)
                     (= (factor-expt (car f)) n))))))
```

せっかく素因数分解するならもっと遡って書き替えたくなります
いくつかの関数は簡単になります

```
(define (prime n x)
  (factor-prime (factor-nth (factorization x) n)))

(define (elm x n)
  (factor-expt (factor-nth (factorization x) n)))

(define (len x)
  (factor-length (factorization x)))
```

