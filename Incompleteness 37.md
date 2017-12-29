# ゲーデルの不完全性定理の証明のアレをRacketで書いてみる (37)

素因数分解の本体
素因数分解は`(Listof (Pairof Natural Natural))`という型で表しています
0の素因数分解とか、2の素因数分解の3番目の因数とか変なやつは`'(0 .0)`で表しました
後で型による表現も試してみたいと思います

```
(define-type Factor (Pairof Natural Natural))
(define factor-error : Factor '(0 . 0))

(define-type Factorization (Listof Factor))
(define factorization-error (list factor-error))

(: factorizations (HashTable Natural Factorization))
(define factorizations (make-hash))
(hash-set! factorizations 0 factorization-error)
(hash-set! factorizations 1 factorization-error)

(: factorization (-> Natural Factorization))
(define (factorization x)
  ;(printf "factorization ~a~n" x)
  (cond ((hash-ref factorizations x #f))
        (else
         (let loop ((n : Natural 1)
                    (x1 : Natural x)
                    (f : Factorization '()))
           (if (= x1 1)
               (let ((f (reverse f)))
                 (hash-set! factorizations x f)
                 f)
               (let*-values (((pn) (P n))
                             ((x1 k) (times-divide x1 pn)))
                 (loop (+ n 1)
                       x1
                       (if (= k 0)
                           f
                           (cons (cons pn k) f)))))))))
```

アクセス関数たち

```
(: factor-length (-> Factorization Natural))
(define (factor-length f)
  (if (equal? f factor-error)
      0
      (length f)))

(: factor-nth (-> Factorization Natural Factor))
(define (factor-nth f n)
  (: F (-> Factorization Natural Factor))
  (define (F f n)
    (cond ((null? f) '(0 . 0))
        ((= n 1) (car f))
        (else (factor-nth (cdr f) (assert (- n 1) natural?)))))
  (if (= n 0)
      '(0 . 0)
      (F f n)))

(: factor-prime (-> Factor Natural))
(define factor-prime car)

(: factor-expt (-> Factor Natural))
(define factor-expt cdr)
```

実はここで型にバグを見つけていただきまして
元のソースはこうだったんですが

```
(define (factor-nth f n)
  (cond ((null? f) '(0 . 0))
        ((= n 1) (car f))
        (else (factor-nth (cdr f) (- n 1)))))
```

`n`=0のときの考慮が抜けてました
とりあえず型付けてよかったひとつめ

定義3はこうなります

```
(: prime (-> Natural Natural Natural))
(define (prime n x)
  (factor-prime (factor-nth (factorization x) n)))
```
