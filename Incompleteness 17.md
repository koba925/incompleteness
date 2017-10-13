# ゲーデルの不完全性定理の証明のアレをRacketで書いてみる (17)

## 「10.8.4 変数・記号・論理式」の続き

定義22 "基本論理式"から組み上げた"論理式"の列である

うーナントカ以下じゃなくてナントカ未満のパターンもあったか
まあいいやとりあえず1引けばおんなじことだし

```
(define (IsFormSeq x)
  (and (> (len x) 0)
       (∀ n ≦ (len x)
          (⇒ (> n 0)
             (or (IsElementForm (elm x n))
                 (∃ p q ≦ (- n 1)
                    (and (> p 0) (> q 0)
                         (IsOp (elm x n) (elm x p) (elm x q)))))))))
```

今回は列の列を相手にするということでかなりつらいはず・・・
一番簡単な「"基本論理式"から組み上げた"論理式"の列」である`x2(0)`で

```
> (IsFormSeq (gnum (gnum (var 1 2) clp c0 crp)))
Interactions disabled
```

ふむ

```
> (gnum (gnum (var 1 2) clp c0 crp))
out of memory 
```

ここのゲーデル数を求める時点でもうアウト

```
> (gnum (var 1 2) clp c0 crp)
85358558703482190127297085877476961847445387329503425822247990708594951927598812588177312402905487114240
```

ということは2の85358558703482190127297085877476961847445387329503425822247990708594951927598812588177312402905487114240乗ってことだもんな
ループの回数を減らすことはできてもゲーデル数を小さくするわけにはいかないもんなあ
完全に詰み

いったん逃避して

```
(define (IsFormSeq x)
      :
                 (∃ p q ＜ n
                       :
```

と書けるようにしますかね
`≦`が全角なので`＜`も全角にしました ただの気分

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
              #'(name v (... ...) ＜ max (fname (- max 1) (λ (vn) body)))]))))))
```

原始的にやりました
パターンが繰り返しになっているのは気になるところ
パターンがまだ増えるようならまた考えるかも

動かせないことを除けば今回の関数はそこまでループに無駄があるわけでもないので
このままにして進みます
