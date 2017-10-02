# ゲーデルの不完全性定理の証明のアレをRacketで書いてみる (11)

## 「10.8.4 変数・記号・論理式」の続き

時間がかかるのはいつものパターンで書き換え

```
(define (IsNumberType x)
  (let loop ((k 1))
    (let ((e (elm x k)))
      (cond ((not (= e cf))
             (and (or (= e c0) (IsVarType e 1))
                  (= (elm x (+ k 1)) 0)))
            (else (loop (+ k 1)))))))
```

瞬時

```
> (IsNumberType (gnum cf (var 1 1)))
#t
```

「いつものパターン」というのが本当に見えてれば抽象化できるはずなんだけど・・・
名前付きletを`for`か`do`にするくらいしか思いつかないなあ

関数型っぽく書くならどうやって書くんでしょうね？
foldかなんか使うのかな？

