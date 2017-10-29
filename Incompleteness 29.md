# ゲーデルの不完全性定理の証明のアレをRacketで書いてみる (29)

定義43 xはaとbの"直接の帰結"である

```
(define (IsConseq x a b)
  (or (= a (implies b x))
      (∃ v ≦ x (and (IsVar v) (= x (ForAll v a))))))
```

ところで推論規則って、これだけで足りるんですか
論理学とか見ると、もっといろいろ書いてあるっぽいんですけど
推論規則が不足してるから不完全なんだよ！とかツッコミ入んないんですか

あと 
aから∀v(a)を得る、ってやつ
いまだに自由変数ってのがなんなのかピンときてなくてもやっとしてます
aと∀v(a)の違いってなんですか

定義44 xは"形式的証明"である

```
(define (IsAxiomAt x n)
  (IsAxiom (elm x n)))

(define (ConseqAt x n)
  (∃ p q ＜ n
     (and (> p 0)
          (> q 0)
          (IsConseq (elm x n) (elm x p) (elm x q)))))

(define (IsProof x)
  (and (> (len x) 0)
       (∀ n ≦ (len x)
          (⇒ (> n 0)
             (or (IsAxiomAt x n)
                 (ConseqAt x n))))))
```

`ConseqAt`は、それまでに出てきた論理式のどれかからの直接の帰結、という意味

定義45 pはxの"形式的証明"である

```
(define (Proves p x)
  (and (IsProof p)
       (IsEndedWith p x)))
```

定義46 xには、"形式的証明"が存在する

ついに最後の定義です

```
(define (IsProvable x)
  (∃ p (Proves p x)))
```

ここで`p`に上限がつけられるようだと何でも証明できてしまうことになっちゃうので
まずいんでしょうけど
上限がつけられないことはどうやって確かめるんでしょうね
仮に上限つけられるとしても、つけなくたって間違いにはなりませんが
不完全性が証明されてしまえばああそうですね上限つきませんねってことになりますが
まだ証明は終わっていないし

それにこの定義、表現定理の対象外なんですよね
そこらへんがちょっとわかってません

さて

```
∃: expected identifier; expected the identifier `＜'; or expected the identifier `≦' in: (Proves p x)
```

ええわかってましたよエラーですね

ええとどうするかな
どうせここにしかでてこないけど、やっぱマクロを修正するか
パターンのところは問題ないとして`(> x max)`のところをどうする

```
         (define (fname max f)
           (let loop ((x 0))
             (cond ((> x max) (notfound x))
                   ((term (f x)) (found x))
                   (else (loop (+ x 1))))))
```

もうひとつ関数を作ってもいいけど
無限大を使ってみましょうか
Racketの(正の)無限大は`+inf.0`

```
> (+ +inf.0 1)
+inf.0
> (- +inf.0 1)
+inf.0
> (> AxiomV +inf.0)
#f
> (< AxiomV +inf.0)
#t
```

うん、使えそうです
それならパターンを追加するだけ

```
(define ≦ #f)
(define ＜ #f)

(define-syntax (define-equipment stx)
  (syntax-parse stx
    ((_ name term notfound found)
     #:with fname (format-id stx "~a≦" #'name)
     #'(begin
         (define (fname max f)
           (let loop ((x 0))
             (cond ((> x max) (notfound x))
                   ((term (f x)) (found x))
                   (else (loop (+ x 1))))))
         (define-syntax (name stx)
           (syntax-parse stx
             #:literals (≦ ＜)
             [(_ v:id ≦ max:expr body:expr)
              #'(fname max (λ (v) body))]
             [(_ v:id ...+ vn:id ≦ max:expr body:expr)
              #'(name v (... ...) ≦ max (fname max (λ (vn) body)))]
             [(_ v:id ＜ max:expr body:expr)
              #'(fname (- max 1) (λ (v) body))]
             [(_ v:id ...+ vn:id ＜ max:expr body:expr)
              #'(name v (... ...) ＜ max (fname (- max 1) (λ (vn) body)))]
             ; ここから追加
             [(_ v:id body:expr)
              #'(fname +inf.0 (λ (v) body))]
             [(_ v:id ...+ vn:id body:expr)
              #'(name v (... ...) (fname +inf.0 (λ (vn) body)))]))))))

```

2変数以上指定する意味があるかどうかはわかりませんが（たぶんない）
一応つくりました

これで46個の定義が完了しました
動かないけど

さてどうしよう
もうちょっと何かしてみたい気もします

不完全性定理の証明はたぶんこの続きの表現定理を使うところからが
本当の本番な気がしているんですよね
今回かなりじっくり見たのでそのあたりもより理解できるようになったかもしれないとは思いつつ
でもそこはもうすっかり数学の世界