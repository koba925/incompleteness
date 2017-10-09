# ゲーデルの不完全性定理の証明のアレをRacketで書いてみる (14)

## 「10.8.4 変数・記号・論理式」の続き

定義20 xは"基本論理式"である

```
(define (IsElementForm x)
  (∃ a b n ≦ x
     (and (IsNthType a (+ n 1))
          (IsNthType b n)
          (= x (** a (paren b))))))
```

遅いのでなんとかしましょう

全然ダメなのも含めて全部総当りでやるのがなにしろ無理があるんだよな（n回目）
記号をひとつずつ見ていって正しい形かどうか確認すればいいかな

```
(define (IsElementForm x)
    (and (IsVar (elm x 1))
         (= (elm x 2) clp)
         ((IsNthType <xの3番目〜(- (len x) 1)番目の要素> 
                     (- <(elm x 1)の型> 1))
         (= (elm (len x)) crp)))
```

てことだけどこれどう書くといいのかなあ

第2型以上は全部変数ですので「(elm x 1)の型」はこんな関数で取れます

```
(define (VarType x)
  (factor-expt (car (factorization x))))
```

「xの3番目〜(- (len x) 1)番目の要素」はどうしましょうねえ
とりあえずそのまま書いてみましょうか
あんまりうまくないやり方な気はしますけど、具体的に困ってから考えることにします

列のs番目からe番目までの記号を取り出した列を作ります

```
(define (ExtractSequence x s e)
  (let ((f (factorization x)))
    (apply gnum (map factor-expt (drop (take f e) (- s 1))))))
```

リスト相手だとパーツがそろってて楽だなあ
全部リストでやりなおしてみる？
遅延リストになるかな？
でもここはそのまま突っ切る

```
(define (IsElementForm x)
  (let ((l (len x)))
    (and (>= l 4)
         (let* ((a (elm x 1))
                (b (ExtractSequence x 3 (- (len x) 1)))
                (n (VarType a)))
           (and (IsVar a)
                (= (elm x 2) clp)
                (IsNthType b (- n 1))
                (= (elm x (len x)) crp))))))
```

動いた
これくらいになってくると元の式と同じ仕様になっているかどうか不安
（ほかも不安ですが）