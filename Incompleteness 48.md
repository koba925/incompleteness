# ゲーデルの不完全性定理の証明のアレをRacketで書いてみる (48)

`IsVarType`の型は
`(: IsVarType (-> GSymbol Natural Boolean))`ではなくて
`(: IsVarType (-> GNumber Natural Boolean))`の方が適切でしたかね
`x`が記号かどうかもわかってないわけだから

型チェックがチェックしてくれるのはいいとして
自分が適切な型を与えているかどうかは保証の限りではないか
とはいえどこかで予想外のエラーが出て気づくことが多そう

定理証明見習いを買ってしまって早く読みたいのであまり考え込まず
手持ちの武器で進めていきます

定義12 xは"変数"である

```
(: IsVar (-> GNumber Boolean))
(define (IsVar x)
  (∃≦ x (λ (n) (IsVarType x n))))
```

定義13 ¬(x)

```
(: Not (-> GSequence GSequence))
(define (Not x)
  (** (<> cnot) (paren x)))
```

定義14 (x)∨(y)

```
(: Or (-> GSequence GSequence GSequence))
(define (Or x y)
  (** (** (paren x) (<> cor)) (paren y)))
```

定義15 ∀x(a)

```
(: ForAll (-> GSymbol GSequence GSequence))
(define (ForAll x a)
  (** (** (<> call) (<> x)) (paren a)))
```

定義16 xの、n番目の後続数

```
(: succ (-> Natural GSequence GSequence))
(define (succ n x)
  (cond ((= n 0) x)
        (else (** (<> cf) (succ (- n 1) x)))))
```

定義17 nに対する"数項"

```
(: ￣ (-> Natural GSequence))
(define (￣ n)
  (succ n (<> c0)))
```

定義18 "第1型の記号である"

```
(: IsNumberType (-> GSequence Boolean))

; 元のソース
(define (IsNumberType x)
  (∃≦ x
      (λ ([m : Natural])
        (∃≦ x
            (λ ([n : Natural])
              (and (or (= m c0) (IsVarType (gnumber m) 1))
                   (= x (succ n (<> (gsymbol+ m))))))))))
```

`and`の項のひとつめで`m`が記号であることが確かめられてるので
ふたつめの項では`m`を`GSymbol`型にしてもOK、って自分で考えないといけないのかな
Occurence Typingをうまく使えればこういうのも自動で何か判定されるのかな

書き換え版

```
(define (IsNumberType x)
  (let loop ((k : Natural 1))
    (let ((e (elm x k)))
      (cond ((not (= (nat e) (nat cf)))
             (and (or (= (nat e) (nat c0))
                      (IsVarType e 1))
                  (= (elm x (+ k 1)) 0)))
            (else (loop (+ k 1)))))))
```

`nat`は`Natural`型に`cast`するだけの関数

```
(: nat (-> GNumber Natural))
(define (nat x) (cast x Natural))
```

`Natural`と`GNumber`の比較はそのままだとうまくいかないことがわかってきたので
`Natural`にしてから比較してます

定義19 "第n型の記号"である

```
(: IsNthType (-> GSequence Natural Boolean))

; 元のソース
(define (IsNthType x n)
  (or (and (= n 1)
           (IsNumberType x))
      (and (> n 1)
           (∃≦ x (λ ([v : Natural])
                   (and (IsVarType (gnumber v) n)
                        (= x (<> (gsymbol+ v)))))))))

; 素因数分解を使う版
(define (IsNthType x n)
  (: I (-> (Opt Factor) Boolean))
  (define (I f1)
    (and (> (none-to-zero (factor-prime f1)) crp)
         (= (none-to-zero (factor-expt f1)) n)))
  (cond ((= n 1) (IsNumberType x))
        ((not (= (len x) 1)) #f)
        (else (let ((f (factorization (elm x 1))))
                (and (= (none-to-zero (factorization-length f)) 1)
                     (I (factorization-nth f 1)))))))
```



