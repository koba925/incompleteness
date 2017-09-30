# ゲーデルの不完全性定理の証明のアレをRacketで書いてみる (10)

## 「10.8.4 変数・記号・論理式」の続き

定義16 xの、n番目の後続数
定義17 nに対する"数項"

```
(define (succ n x)
  (cond ((= n 0) x)
        (else (** (<> cf) (succ (- n 1) x)))))

(define (￣ n)
  (succ n (<> c0)))
```

数項の"￣"はちょっと無理がある気もしますが気持ちはわかってください

単なるゲーデル数でも巨大なのに、それを数項にしちゃうとそれはそれは

定義18 "第1型の記号である"

```
(define (IsNumberType x)
  (∃ m n ≦ x 
     (and (or (= m c0) (IsVarType m 1))
          (= x (succ n (<> m))))))
```

と書きたいところですがしまった
変数がふたつあります
∃のマクロは変数ひとつしか対応してませんでした

変数がふたつある場合はえーとこんな風に展開してほしいんだな

```
(define (IsNumberType x)
  (∃≦ x (λ (m)
          (∃≦ x (λ (n)
                  (and (or (= m c0) (IsVarType m 1))
                       (= x (succ n (<> m)))))))))
```

ということはマクロ定義は

```
(define-syntax (define-equipment stx)
  (syntax-parse stx
    ((_ name term notfound found)
     #:with fname (format-id stx "~a≦" #'name)
     #'(begin
         (define (fname max f)
           (let loop ((x 1))
             (cond ((> x max) (notfound x))
                   ((term (f x)) (found x))
                   (else (loop (+ x 1))))))
         (define-syntax (name stx)
           (syntax-parse stx
             #:literals (≦)
             [(_ v:id ≦ max:expr body:expr)
              #'(fname max (λ (v) body))]
             [(_ v1:id v2:id ≦ max:expr body:expr) ; ここから追加
              #'(fname max (λ (v1) (fname max (λ (v2) body))))]))))))
```

こうかな？
どれ

```
> (IsNumberType (gnum c0))
#f
> (IsNumberType (gnum cf c0))
#t
```

あれー0が数字じゃないって言われる
f0は大丈夫なのに
なんでなんで
マクロ変かなあ？
単純に置き換えるだけなのに
えーとえーと

えー何がおかしいのー
Macro stepperさん登場（インデントは手動で追加）

```
(define (IsNumberType x) 
  (∃≦ x (λ (m) 
          (∃≦ x (λ (n) 
               (and (or (= m c0) (IsVarType m 1)) 
                    (= x (succ n (<> m)))))))))
```

完全に一致

```
(IsNumberType (gnum c0))
(IsNumberType 2)
(∃≦ 2 (λ (m) (∃≦ 2 (λ (n) (and (or (= m c0) (IsVarType m 1)) 
                               (= 2 (succ n (<> m)))))))))
```

えーと`∃≦`はこうだから

```
(let loop ((x 1)) 
  (cond ((> x max) ((const #f) x))
        ((identity (f x)) ((const #t) x))
        (else (loop (+ x 1))))))
```

あっ
1から始まってる！
`n`=0でないといけないのに！

でもさーそれってさー

> `Min≦`や`Min`は`(f x)`を満たす`x`が見つからなかった場合は0を返します
> `x`=0から開始すると見つからなかったのか`x`=0で見つかったのかわからないので
> `x`=1から開始することにしました
> `∀`や`∃`も同様に修正
> カンが悪かった

で直したとこなのに・・・0から始めちゃって大丈夫かな？
それとも`Min`だけ0からとか？
いやそれはないよな

`∀`や`∃`のところは問題ないとして、`Min`を使ってるところは見直してみよう
（今や使ってるところないけど元のコードで）

```
(define (CanDivideByPower x n k)
  (CanDivide x (expt (prime n x) k)))

(define (elm x n)
  (Min k ≦ x (and (CanDivideByPower x n k)
                  (not (CanDivideByPower x n (+ k 1))))))
```

こことかちょっとマズくない？
0乗すると何でも1になって割れちゃうし
・・・いや、素因数があれば少なくとも1乗はしてるから大丈夫か
ギリギリだなあ
ほかも大丈夫そうだ
ゲーデルさんコンピュータもなしでそこまで考えてたのかね
すごいね

というわけで0から開始するようにして解決

```
           (let loop ((x 0))
```

ではあらためて

```
> (IsNumberType (gnum c0))
#t
> (IsNumberType (gnum cf c0))
#t
> (IsNumberType (gnum c0 c0))
#f
```

よしよし
次は`fx1`のパターン

```
> (IsNumberType (gnum (var 1 1)))

```

はい予想通り返ってきませんよ
でも今日はここまで
