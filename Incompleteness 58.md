# ゲーデルの不完全性定理の証明のアレをRacketで書いてみる (58)

大詰め

定義43 xはaとbの"直接の帰結"である

```
(define (IsConseq [x : GForm]
                  [a : GForm]
                  [b : GForm]) : Boolean
  (or (= a (Implies b x))
      (∃ v <= x (and (IsVar (gsymbol+ v))
                     (= x (ForAll (gsymbol+ v) a))))))
```

定義44 xは"形式的証明"である

```
(define (IsAxiomAt [x : GSeqSeq]
                   [n : Natural]) : Boolean
  (IsAxiom (gform+ (elm x n))))

(define (ConseqAt [x : GProof]
                  [n : Natural]) : Boolean
  (∃ p q < n
     (and (> p 0)
          (> q 0)
          (IsConseq (gform+ (elm x n))
                    (gform+ (elm x p))
                    (gform+ (elm x q))))))

(define (IsProof [x : GProof])
  (and (> (len x) 0)
       (∀ n <= (len x)
          (⇒ (> n 0)
             (or (IsAxiomAt x n)
                 (ConseqAt x n))))))
```

定義45 pはxの"形式的証明"である

```
(define (Proves [p : GProof]
                [x : GForm])
  (and (IsProof p)
       (IsEndedWith p x)))
```

ラスボスを倒すには特別な装備が必要
(上限なしの`∀`と`∃`)

```
(define-syntax (define-equipment stx)
  (syntax-parse stx
    ((_ name term notfound found)
     #:with fname (format-id stx "_~a" #'name)
     #'(begin
         (define (fname [cmp : (-> Natural Natural Boolean)]
                        [max : Natural]
                        [f : (-> Natural Boolean)]) : Boolean
           (let loop ((x : Natural 0))
             (cond ((not (cmp x max)) (notfound x))
                   ((term (f x)) (found x))
                   (else (loop (+ x 1))))))
         (define-syntax (name stx)
           (syntax-parse stx
             [(_ v:id cmp:id max:expr body:expr)
              #'(fname cmp max (λ (v) body))]
             [(_ v:id ...+ vn:id cmp max:expr body:expr)
              #'(name v (... ...) cmp max (fname cmp max (λ (vn) body)))]
             [(_ v:id body:expr)
              #'(fname (const #t) 0 (λ (v) body))]
             [(_ v:id ...+ vn:id body:expr)
              #'(name v (... ...)
                      (const #t)
                      0
                      (fname (const #t) 0 (λ (vn) body)))]))))))
```

かなり適当なパターンマッチでギリギリたまたま動いてるって感じ

定義46 xには、"形式的証明"が存在する

```
(define (IsProvable [x : GForm])
  (∃ p (Proves (gproof+ p) x)))
```

終わり！
（終わったからといって特に何もなし）
（定理証明手習いにとりかかれる）