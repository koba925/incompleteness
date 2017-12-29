# ゲーデルの不完全性定理の証明のアレをRacketで書いてみる (35)

定義4 nの階乗

型をつけただけ

```
(: factorial (-> Natural Natural))
(define (factorial n)
  (cond ((= n 0) 1)
        (else (* n (factorial (- n 1))))))
```

定義5 n番目の素数

元の形なら型を付けるだけ

```
; もとの定義
(: M5 (-> Natural Natural))
(define (M5 n)
  (+ (factorial n) 1))

(: P (-> Natural Natural))
(define (P n)
  (cond ((= n 0) 0)
        (else (Min≦ (M5 n)
                    (λ (p) (and (< (P (- n 1)) p)
                                (IsPrime p)))))))
```

Hash Tableに覚えておく版
まずはHash Tableの定義
型は`(HashTable <key> <value>)`

```
(: primes (HashTable Natural Natural))
(define primes (make-hash))
(hash-set! primes 0 0)
(hash-set! primes 1 2)
```

本体
`let`の引数に型をつけてやるのはいつもどおり

```
(: P (-> Natural Natural))
(define (P n)
  (cond ((hash-ref primes n #f))
        (else (let loop ((k : Natural (+ (P (- n 1)) 1)))
                (cond ((IsPrime k)
                       (hash-set! primes n k)
                       k)
                      (else (loop (+ k 1))))))))
```

でもそれだけではエラー

```
  Type Checker: type mismatch
  expected: Nonnegative-Integer
  given: Integer in: (- n 1)
```

うんたしかに`Natural`から`Natural`引くと`Integer`ですね
でもHashTableにあらかじめ`n`=0と`n`=1の分は入れてあるから
実際には`n`≧2なんですよ
と言ってもわからないか

あ、でもそういうのあったな Occurence Typingだったか
そうすると、こう書けばわかってくれる？

```
(: P (-> Natural Natural))
(define (P n)
  (cond ((= n 0) 0) ; ここ追加
        ((= n 1) 2) ; ここ追加
        ((hash-ref primes n #f))
        (else (let loop ((k : Natural (+ (P (- n 1)) 1)))
                (cond ((IsPrime k)
                       (hash-set! primes n k)
                       k)
                      (else (loop (+ k 1))))))))
```

通りました
`(- n 1)`の`n`は2以上ってことがわかってるんですね
面白い

ここはこれでもいいんですが
今後も必要になりそうなのでキャスト？のしかたを確認しておきます

それっぽい関数がふたつあります
`(ann e t)`: 式eが型tであることをensureする
`(cast e t)`: 式eの型がなんであっても型tにする

最初ちょっと意味がわかりませんでしたが
`ann`の方は合法的に型が変換できる場合だけコンパイルが通り、
`cast`の方は型がtであることにしてコンパイルを通し、
実際に型tにできない値を渡された場合は契約違反にする、ということのようです

ここで使うのは`cast`の方ですね

```
(: P (-> Natural Natural))
(define (P n)
  (cond ((hash-ref primes n #f))
        (else (let loop
                ((k : Natural (+ (P (cast (- n 1) Natural))
                                 1)))
                (cond ((IsPrime k)
                       (hash-set! primes n k)
                       k)
                      (else (loop (+ k 1))))))))
```

動きました

Typed Racketはソースが横に伸びていきやすいなあと思う今日このごろです
どこで行を変えるか
