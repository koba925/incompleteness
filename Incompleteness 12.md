# ゲーデルの不完全性定理の証明のアレをRacketで書いてみる (12)

## 「10.8.4 変数・記号・論理式」の続き

定義18 "第n型の記号"である

```
(define (IsNthType x n)
  (or (and (= n 1) (IsNumberType x))
      (and (> n 1) (∃ v ≦ x (and (IsVarType v n) (= x (<> v)))))))
```

さて高速化
もともと、`#t`が帰るようなときは遅くないんですがそうでないときはダメ
fx1ですらゲーデル数は1033121304になりますので
全部の数を試してるわけにはいきません

素数だけを試すようにします

```
(define (IsNthType x n)
  (cond ((= n 1) (IsNumberType x))
        (else
         (let ((e (elm x 1)))
           (let loop ((k 7))
             (let ((v (expt (P k) n)))
               (cond ((> v e) #f)
                     ((= v e) #t)
                     (else (loop (+ k 1))))))))))
```

どうもこういう繰り返しっぽい形でしか書けないのは頭が手続き型なんでしょうか

`x`が列なことを忘れて記号のつもりで書いてて勘違いに気が付かず
手間取ってしまったのでここまで

