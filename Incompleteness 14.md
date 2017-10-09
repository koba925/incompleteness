# ゲーデルの不完全性定理の証明のアレをRacketで書いてみる (14)

## 「10.8.4 変数・記号・論理式」の続き

定義20 xは"基本論理式"である

変数が三つになった
ぐぬぬ

さすがに３つめのパターンを追加するというわけにもいくまい
Fear of Macrosになんかあったな・・・いやないな
じゃあSMPHにも・・・あれないな
どっかで再帰的にマクロ定義を呼んでる例があったと思うんだけど

こんな感じでいけると思うんだが

```
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
             #:literals (≦)
             [(_ v:id ≦ max:expr body:expr)
              #'(fname max (λ (v) body))]
             ;こうだった
             ;[(_ v1:id v2:id ≦ max:expr body:expr)
             ; #'(fname max (λ (v1) (fname max (λ (v2) body))))]))))))
             ;こうする
             [(_ v:id ...+ vn:id ≦ max:expr body:expr)
              #'(name v ... ≦ max (fname max (λ (vn) body)))]))))))
```

やってみるとエラー

```
syntax: no pattern variables before ellipsis in template in: ...
```

`(name v ...`の`...`がダメって言われてるっぽいです
やりたいことはこのとおりだと思うんだけど・・・

わからん
ちょっとマクロが二重になってて何がなんだかわからないので
いったんシンプルなマクロで確認します

```
(define-syntax (∀ stx)
  (syntax-parse stx
    #:literals (≦)
    [(∀ v:id ≦ max:expr body:expr)
     #'(_∀<= max (λ (v) body))]
    [(∀ v:id ...+ vn:id ≦ max:expr body:expr)
     #'(∀ v ... ≦ max (_∀<= max (λ (vn) body)))]))
```

あれーこれだと動くなあ
いっしょじゃない？

・・・
・・・

そうか
マクロを定義するマクロで`...`を使うときにエスケープみたいなことするのがあった
`(... ...)`だ
単なる`...`だと、外側の`syntax-parse`が展開しようとして
`v`?そんなパターン変数ないけど？て言ってるわけか
`(... ...)`なら外側の`syntax-parse`がいったん`...`にしてくれるわけか

こうだな

```
             [(_ v:id ...+ vn:id ≦ max:expr body:expr)
              #'(name v (... ...) ≦ max (fname max (λ (vn) body)))]))))))
```

大丈夫そうな雰囲気
やっと書けます

```
(define (IsElementForm x)
  (∃ a b n ≦ x
     (and (IsNthType a (+ n 1))
          (IsNthType b n)
          (= x (** a (paren b))))))
```

まともに動きそうな気がしません
一番簡単な論理式はたぶんx2(x1)ですが

```
  x2(x1)のゲーデル数
= 2^17^2・3^11・5^17・7^13
= 13024682419354582233779462566753686805335294697495029574927977097869102772155580534084672913040998400000000000000000
```

ですから

