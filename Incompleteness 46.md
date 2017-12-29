# ゲーデルの不完全性定理の証明のアレをRacketで書いてみる (46)

今日の成果

* `(if (= yk 0) ...)`ではダメ（元のコード）
* `(if (= yk (gnumber 0)) ...)`でもダメ
* `(if (= (ann yk Natural) 0) ...)`でもダメ
* `(if (= (cast yk Natural) 0) ...)`なら成功する
* `(if (= (+ yk 1) 1) ...)`でも成功する

関数ごと再掲

```
(define (** x y)
  (let ((lenx (len x)))
    (let loop ((k : Natural 1) (n : GSequence x))
      (let ((yk (elm y k)))
        ; (= yk 0)と書くとyk=0でも偽と判定されてしまうので苦肉の策
        (if (= (cast yk Natural) 0)
            n
            (loop (+ k 1) (gsequence+ (* n (expt (P (+ lenx k)) yk)))))))))
```

ここはコメントの書きどころ