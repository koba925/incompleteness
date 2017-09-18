# ゲーデルの不完全性定理の証明のアレをRacketで書いてみる (5)

## 「10.8.3 列」

定義6 n番目の要素

```
(define (CanDivideByPower x n k)
  (CanDivide x (expt (prime n x) k)))

(define (elm x n)
  (Min k ≦ x (and (CanDivideByPower x n k)
                  (not (CanDivideByPower x n (+ k 1))))))

```

`(P n)`ではなく`(prime n x)`を使っているので、歯抜けでも問題はないはず

```
> (elm 504 3)
1
```

おｋ

定義7 列の長さ

```
(define (len x)
  (Min k ≦ x (and (> (prime k x) 0)
                  (= (prime (+ k 1) x) 0))))
```

定義8 列の連結

⇒が出てきました

「ならば」とよみますけど`if`そのままってわけじゃないですね
`A⇒B`は`(if A B #t)`か
まあこれでもいいけど毎回`#t`を書くのもアレだから定義しましょう

あ、これ関数で定義するとマズいやつ？
副作用はないけど余分な計算が走るのは避けたいので

```
(define-syntax-rule (⇒ x y)
  (or (not x) y))
```

これくらいなら`syntax-parse`でなくてもいいでしょう
さて定義

```
(define (M8 x y)
  (expt (P (+ (len x) (len y))) (+ x y)))

(define (** x y)
  (Min z ≦ (M8 x y)
       (and (∀ m ≦ (len x)
               (⇒ (<= 1 m) (= (elm z m) (elm x m))))
            (∀ n ≦ (len y)
               (⇒ (<= 1 n) (= (elm z (+ (len x) n)) (elm y n)))))))
```

テスト

```
> (** 8 4)
remainder: undefined for 0
```

ぐふ
`CanDivide`はただの`reminder`じゃなかった
0で割ってるときもエラーにせず`#f`を返すようにしてないと

```
(define (CanDivide x d)
  (and (not (zero? d))
       (= (remainder x d) 0)))
```

もう少し大きな数でテスト
[2 1]という列と[2 1]という列を連結してみます

```
> (time (** 8 4))   ; [3]という列と[2]という列の連結
cpu time: 3 real time: 3 gc time: 0
72
> (time (** 12 4))  ; [2 1]という列と[2]という列の連結
cpu time: 35571 real time: 35565 gc time: 1153
300
> (time (** 12 12)) ; [2 1]という列と[2 1]という列の連結

```

お返事がありません
うーん
ほとんど最小レベルの列の連結ですらここまでとは

どんなときもかならず1から順番に探すという作戦が無理あるんだよなー
せっかくマクロ書いたけどさー
どうしようかなー