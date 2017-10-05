# ゲーデルの不完全性定理の証明のアレをRacketで書いてみる (13)

## 「10.8.4 変数・記号・論理式」の続き

さて前回、勘違いに気づくまではもっと高速化しないとと思って
素因数分解する版を書き始めてました
乗った船なので続けてみます

```
(define (times-divide x p)
  (let loop ((k 1) (x x))
    (cond ((not (CanDivide x p)) (values x (- k 1)))
          (else (loop (+ k 1) (/ x p))))))

(define (factorization x)
  (let loop ((n 1) (x x) (f '()))
    (if (= x 1)
        (reverse f)
        (let*-values (((pn) (P n))
                      ((x k) (times-divide x pn)))
          (loop (+ n 1)
                x
                (if (= k 0)
                    f
                    (cons (cons pn k) f)))))))

(define factor-length length)
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

