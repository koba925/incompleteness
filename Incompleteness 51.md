# ゲーデルの不完全性定理の証明のアレをRacketで書いてみる (50)

やっぱりマクロで書いてみたくなったので手持ちの武器の範囲を超えない程度に
もういちどやってみる

Minは型だけじゃなくて構造に近いレベルで差異があるので分けた

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
              #'(name v (... ...) cmp max (fname cmp max (λ (vn) body)))]))))))

(define-equipment ∀ not (const #t) (const #f))
(define-equipment ∃ identity (const #f) (const #t))

(define (_Min [cmp : (-> Natural Natural Boolean)]
            [max : Natural]
            [f : (-> Natural Boolean)]) : Natural
  (let loop ((x : Natural 0))
    (cond ((not (cmp x max)) 0)
          ((f x) x)
          (else (loop (+ x 1))))))

(define-syntax (Min stx)
  (syntax-parse stx
    [(_ v:id cmp:id max:expr body:expr)
     #'(_Min cmp max (λ (v) body))]))
```

それにともなって全面的に修正

```
(: IsFormSeq (-> GSeqSeq Boolean))
(define (IsFormSeq x)
  (and (> (len x) 0)
       (∀ <= (len x)
           (λ (n)
             (⇒ (> n 0)
                (or (IsElementForm (gsequence+ (elm x n)))
                    (∃ < n
                       (λ (p)
                         (∃ < n
                            (λ (q)
                              (and (> p 0) (> q 0)
                                   (IsOp (gsequence+ (elm x n))
                                         (gsequence+ (elm x p))
                                         (gsequence+ (elm x q))))))))))))))
```

が

```
(: IsFormSeq (-> GSeqSeq Boolean))
(define (IsFormSeq x)
  (and (> (len x) 0)
       (∀ n <= (len x)
          (⇒ (> n 0)
             (or (IsElementForm (gsequence+ (elm x n)))
                 (∃ p q < n
                    (and (> p 0) (> q 0)
                         (IsOp (gsequence+ (elm x n))
                               (gsequence+ (elm x p))
                               (gsequence+ (elm x q))))))))))
```

となって微妙にうれしい

