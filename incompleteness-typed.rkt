#lang typed/racket

(require typed/rackunit)
(require (for-syntax syntax/parse))
(require (for-syntax racket/syntax))

; require/typedできないので自分で定義
; 本当はこう書きたい
; (require/typed racket/math
;                [natural? (-> Any Boolean : Natural)])

(: natural? (-> Any Boolean : Natural))
(define natural? exact-nonnegative-integer?)

;; 装備

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

(check-true (∀ x <= 3 (< x 4)))
(check-false (∀ x <= 3 (< x 3)))
(check-true (∀ x y <= 3 (< (* x y) 10)))
(check-false (∀ x y <= 3 (< (* x y) 9)))
(check-true (∀ x < 4 (< x 4)))
(check-false (∀ x < 4 (< x 3)))
(check-true (∀ x y < 4 (< (* x y) 10)))
(check-false (∀ x y < 4 (< (* x y) 9)))
(check-false (∀ x (< x 4)))

(check-true (∃ x <= 3 (= x 2)))
(check-false (∃ x <= 3 (= x 4)))
(check-true (∃ x y <= 3 (= (* x y) 6)))
(check-false (∃ x y <= 3 (= (* x y) 7)))
(check-true (∃ x < 4 (= x 2)))
(check-false (∃ x < 4 (= x 4)))
(check-true (∃ x y < 4 (= (* x y) 6)))
(check-false (∃ x y < 4 (= (* x y) 7)))
(check-true (∃ x (> x 4)))

(check-eq? (Min x <= 3 (= x 2)) 2)
(check-eq? (Min x <= 3 (= x 4)) 0)
(check-eq? (Min x < 4 (= x 2)) 2)
(check-eq? (Min x < 4 (= x 4)) 0)

; ⇒の定義
; これは関数だと余分な評価が走るのでマクロで
; (if x y #t) とどっちがわかりやすいかな

(define-syntax-rule (⇒ x y)
  (or (not x) y))

(check-true (⇒ #f #f))
(check-true (⇒ #f #t))
(check-false (⇒ #t #f))
(check-true (⇒ #t #t))

; 定義1 xはdで割り切れる

(: CanDivide (-> Natural Natural Boolean))

; もとの定義
;(define (CanDivide x d)
;  (∃ n <= x (= x (* d n))))

; 書き直した定義
(define (CanDivide x d)
  (and (not (zero? d))
       (= (remainder x d) 0)))

(check-true (CanDivide 12 3))
(check-false (CanDivide 12 5))

; 定義2 xは素数である

(: IsPrime (-> Natural Boolean))

; もとの定義
;(define (IsPrime x)
;  (and (> x 1)
;       (not (∃ d <= x (and (not (= d 1))
;                           (not (= d x))
;                           (CanDivide x d))))))

; 書き直した定義
(define (IsPrime x)
  (and (> x 1)
       (let loop ((d : Natural 2))
         (cond ((> (* d d) x) #t)
               ((CanDivide x d) #f)
               (else (loop (+ d 1)))))))

(check-false (IsPrime 0))
(check-false (IsPrime 1))
(check-true (IsPrime 2))
(check-false (IsPrime 12))
(check-true (IsPrime 17))

; 定義4 nの階乗

(: factorial (-> Natural Natural))
(define (factorial n)
  (cond ((= n 0) 1)
        (else (* n (factorial (- n 1))))))

(check-eq? (factorial 0) 1)
(check-eq? (factorial 3) 6)

; 定義5 n番目の素数

; もとの定義
(: M5 (-> Natural Natural))
(define (M5 n)
  (+ (factorial n) 1))

;(: P (-> Natural Natural))
;(define (P n)
;  (cond ((= n 0) 0)
;        (else (Min p <= (M5 n)
;                   (and (< (P (- n 1)) p)
;                        (IsPrime p))))))

; 書き直した定義

(: primes (HashTable Natural Natural))
(define primes (make-hash))
(hash-set! primes 0 0)
(hash-set! primes 1 2)

(: P (-> Natural Natural))
(define (P n)
  (cond ((hash-ref primes n #f))
        (else (let loop
                ((k : Natural (+ (P (assert (- n 1) natural?))
                                 1)))
                (cond ((IsPrime k)
                       (hash-set! primes n k)
                       ;(printf "New prime ~a ~a~n" n k)
                       k)
                      (else (loop (+ k 1))))))))

(check-eq? (P 0) 0)
(check-eq? (P 1) 2)
(check-eq? (P 5) 11)

; 素因数分解

(struct None () #:transparent)
(struct (a) Some ([v : a]) #:transparent)
(define-type (Opt a) (U None (Some a)))

(struct Factor ([prime : Natural] [expt : Natural]) #:transparent)
(define-type Factorization (Listof Factor))

(: factorizations (HashTable Natural (Opt Factorization)))
(define factorizations (make-hash))
(hash-set! factorizations 0 (None))
(hash-set! factorizations 1 (None))

(: times-divide (-> Natural Natural (Values Natural Natural)))
(define (times-divide x p)
  (let loop ((k : Natural 1) (x : Natural x))
    (cond ((not (CanDivide x p)) (values x (assert (- k 1) natural?)))
          (else (loop (+ k 1) (quotient x p))))))

(: factorization (-> Natural (Opt Factorization)))
(define (factorization x)
  ;(printf "factorization ~a~n" x)
  (cond ((hash-ref factorizations x #f))
        (else
         (let loop ((n : Natural 1)
                    (x1 : Natural x)
                    (f : Factorization '()))
           (if (= x1 1)
               (let ((f (reverse f)))
                 (hash-set! factorizations x (Some f))
                 (Some f))
               (let*-values (((pn) (P n))
                             ((x1 k) (times-divide x1 pn)))
                 (loop (+ n 1)
                       x1
                       (if (= k 0)
                           f
                           (cons (Factor pn k) f)))))))))

(check-equal? (factorization 0) (None))
(check-equal? (factorization 1) (None))
(check-equal? (factorization 40) (Some (list (Factor 2 3) (Factor 5 1))))  

; (Some x)のxにfを適用する
; NoneならNoneを返す
(: opt-apply (All (a b) (-> (Opt a) (-> a b) (Opt b))))
(define (opt-apply x func)
  (match x
    [(None) (None)]
    [(Some y) (Some (func y))]))

(check-equal? (opt-apply (Some 2) add1) (Some 3))
(check-equal? (opt-apply (None) add1) (None))

(: factorization-length (-> (Opt Factorization) (Opt Natural)))
(define (factorization-length facz)
  ((inst opt-apply Factorization Natural) facz length))

(check-equal? (factorization-length (factorization 0)) (None))
(check-equal? (factorization-length (factorization 0)) (None))
(check-equal? (factorization-length (factorization 40)) (Some 2))

; n番目のFactorを取り出す
(: factorization-nth (-> (Opt Factorization) Natural (Opt Factor)))
(define (factorization-nth facz n)
  (: F (-> Factorization Natural (Opt Factor)))
  (define (F f n)
    (cond ((null? f) (None))
          ((= n 1) (Some (car f)))
          (else (F (cdr f) (assert (- n 1) natural?)))))
  (if (or (None? facz) (= n 0))
      (None)
      (F (Some-v facz) n)))

(check-equal? (factorization-nth (factorization 40) 0) (None))
(check-equal? (factorization-nth (factorization 40) 1) (Some (Factor 2 3)))
(check-equal? (factorization-nth (factorization 40) 2) (Some (Factor 5 1)))
(check-equal? (factorization-nth (factorization 40) 3) (None))

; Noneを0に変換
; どっちがどっちがいいかなあ？
;(: none-to-zero (-> (Opt Natural) Natural))
(: none-to-zero (All (a) (-> (Opt a) (U Zero a))))
(define (none-to-zero optvar)
  (match optvar
    [(None) 0]
    [(Some y) y]))
  
; (Opt Factor)からprimeを取り出す
(: factor-prime (-> (Opt Factor) (Opt Natural)))
(define (factor-prime x)
  ((inst opt-apply Factor Natural) x Factor-prime))

(check-equal? (factor-prime (Some (Factor 3 2))) (Some 3))
(check-equal? (factor-prime (None)) (None))

; (Opt Factor)からexptを取り出す
(: factor-expt (-> (Opt Factor) (Opt Natural)))
(define (factor-expt x)
  ((inst opt-apply Factor Natural) x Factor-expt))

(check-equal? (factor-expt (Some (Factor 3 2))) (Some 2))
(check-equal? (factor-expt (None)) (None))

; 定義3 n番目の、xの素因数

(: CanDivideByPrime (-> Natural Natural Boolean))
(define (CanDivideByPrime x p)
  (and (CanDivide x p) (IsPrime p)))

(check-true (CanDivideByPrime 12 3))
(check-false (CanDivideByPrime 12 5))
(check-false (CanDivideByPrime 12 6))

(: prime (-> Natural Natural Natural))

; もとの定義
;(define (prime n x)
;  (cond ((= n 0) 0)
;        (else (Min p <= x
;                   (and (< (prime (- n 1) x) p)
;                        (CanDivideByPrime x p))))))

; 素因数分解を利用する版
(define (prime n x)
  ;(printf "prime ~a ~a~n" n x)
  (none-to-zero (factor-prime (factorization-nth (factorization x) n))))

(check-eq? (prime 0 2352) 0)
(check-eq? (prime 1 2352) 2)
(check-eq? (prime 2 2352) 3)
(check-eq? (prime 3 2352) 7)

;;; ここからゲーデル数の登場

;; 型

; ゲーデル数は自然数の一種
(define-new-subtype GNumber (gnumber Natural))

; 記号はゲーデル数の一種
(define-new-subtype GSymbol (gsymbol GNumber))

(: gsymbol+ (case-> (-> Natural GSymbol)
                    (-> GNumber GSymbol)))
(define gsymbol+
  (case-lambda [([x : Natural]) (gsymbol (gnumber x))]
               [([x : GNumber]) (gsymbol x)]))

(check-equal? (gsymbol+ 1) (gsymbol (gnumber 1)))
(check-equal? (gsymbol+ (gnumber 1)) (gsymbol (gnumber 1)))

; 列はゲーデル数の一種
(define-new-subtype GSequence (gsequence GNumber))

(: gsequence+ (case-> (-> Natural GSequence)
                    (-> GNumber GSequence)))
(define gsequence+
  (case-lambda [([x : Natural]) (gsequence (gnumber x))]
               [([x : GNumber]) (gsequence x)]))

(check-equal? (gsequence+ 1) (gsequence (gnumber 1)))
(check-equal? (gsequence+ (gnumber 1)) (gsequence (gnumber 1)))

; 論理式は列の一種
(define-new-subtype GForm (gform GSequence))

(: gform+ (case-> (-> Natural GForm)
                  (-> GNumber GForm)
                  (-> GSequence GForm)))
(define gform+
  (case-lambda [([x : Natural]) (gform (gsequence+ x))]
               [([x : GNumber]) (gform (gsequence+ x))]
               [([x : GSequence]) (gform (gsequence x))]))

; 列の列は列の一種
(define-new-subtype GSeqSeq (gseqseq GSequence))

(: gseqseq+ (case-> (-> Natural GSeqSeq)
                    (-> GNumber GSeqSeq)
                    (-> GSequence GSeqSeq)))
(define gseqseq+
  (case-lambda [([x : Natural]) (gseqseq (gsequence+ x))]
               [([x : GNumber]) (gseqseq (gsequence+ x))]
               [([x : GSequence]) (gseqseq  x)]))

(check-equal? (gseqseq+ 1) (gseqseq (gsequence (gnumber 1))))
(check-equal? (gseqseq+ (gnumber 1)) (gseqseq (gsequence (gnumber 1))))
(check-equal? (gseqseq+ (gsequence (gnumber 1))) (gseqseq (gsequence (gnumber 1))))

; 証明は列の列の一種
(define-new-subtype GProof (gproof GSeqSeq))

(: gproof+ (case-> (-> Natural GProof)
                   (-> GNumber GProof)
                   (-> GSequence GProof)))
(define gproof+
  (case-lambda [([x : Natural]) (gproof (gseqseq+ x))]
               [([x : GNumber]) (gproof (gseqseq+ x))]
               [([x : GSeqSeq]) (gproof  x)]))

; 短く書きたいだけ

(: nat (-> GNumber Natural))
(define (nat x) (cast x Natural))

; 定数

(define c0 : GSymbol (gsymbol+ 1))
(define cf : GSymbol (gsymbol+ 3))
(define cnot : GSymbol (gsymbol+ 5))
(define cor : GSymbol (gsymbol+ 7))
(define call : GSymbol (gsymbol+ 9))
(define clp : GSymbol (gsymbol+ 11))
(define crp : GSymbol (gsymbol+ 13))

; 変数

; x1=(var 1 1)、y1=(var 2 1)、z3=(var 3 3)などと表記することにする
(: var (-> Natural Natural GSymbol))
(define (var n c) (gsymbol+ (expt (P (+ 6 n)) c)))

(check-eq? (var 1 1) (gsymbol+ 17))
(check-eq? (var 3 3) (gsymbol+ (expt 23 3)))
(check-eq? (var 2 cf) (gsymbol+ (expt 19 3)))

; ゲーデル数

(: gnum (-> GNumber * GSequence))
(define (gnum . seq)
  (: iter (-> (Listof GNumber) Natural GSequence GSequence))
  (define (iter s k n)
    (if (null? s)
        n
        (iter (cdr s) (+ k 1) (gsequence+ (* (expt (P k) (car s)) n)))))
  (iter seq 1 (gsequence+ 1)))

(: gfrm (-> GNumber * GForm))
(define (gfrm . seq)
  (gform (apply gnum seq)))

(check-equal? (gnum) (gnumber 1))
(check-equal? (gnum (gnumber 3) (gnumber 2) (gnumber 1)) 360)
(check-equal? (gnum call (var 1 2) clp (var 1 1) crp)
              (* (expt 2 call)
                 (expt 3 (var 1 2))
                 (expt 5 clp)
                 (expt 7 (var 1 1))
                 (expt 11 crp)))

; 定義6 n番目の要素

; 元のソース
;(: CanDivideByPower (-> Natural Natural Natural Boolean))
;(define (CanDivideByPower x n k)
;  (CanDivide x (expt (prime n x) k)))
;
;; 2^3*3^2*5^1=360
;(check-true (CanDivideByPower 360 3 0))
;(check-true (CanDivideByPower 360 3 1))
;(check-false (CanDivideByPower 360 3 2))
;
;; (P n)ではなく(prime n x)を使っているので、歯抜けでも問題はないはず
;; 2^3*3^2*7^1=504
;(check-true (CanDivideByPower 504 3 1))
;(check-true (CanDivideByPower 504 3 1))
;(check-false (CanDivideByPower 504 3 2))

(: elm (-> GSequence Natural GNumber))
;(define (elm x n)
;  (gnumber
;   (Min k <= x (and (CanDivideByPower x n k)
;                    (not (CanDivideByPower x n (+ k 1)))))))

(define (elm x n)
  (gnumber (none-to-zero (factor-expt (factorization-nth (factorization x) n)))))

(check-equal? (elm (gsequence+ 0) 0) 0)
(check-equal? (elm (gsequence+ 0) 1) 0)
(check-equal? (elm (gsequence+ 0) 2) 0)
(check-equal? (elm (gsequence+ 1) 0) 0)
(check-equal? (elm (gsequence+ 1) 1) 0)
(check-equal? (elm (gsequence+ 1) 2) 0)
(check-equal? (elm (gsequence+ 360) 0) 0)
(check-equal? (elm (gsequence+ 360) 1) 3)
(check-equal? (elm (gsequence+ 360) 2) 2)
(check-equal? (elm (gsequence+ 360) 3) 1)
(check-equal? (elm (gsequence+ 360) 5) 0)
(check-equal? (elm (gsequence+ 504) 3) 1)

; 定義7 列の長さ

(: len (-> GSequence Natural))

; 元のソース
;(define (len x)
;  (Min k <= x (and (> (prime k x) 0)
;                   (= (prime (+ k 1) x) 0))))

; 素因数分解を利用する版
(define (len x)
  (none-to-zero (factorization-length (factorization x))))

(check-equal? (len (gsequence+ 0)) 0) ;意味がある?
(check-equal? (len (gsequence+ 1)) 0) 
(check-equal? (len (gsequence+ 4)) 1)
(check-equal? (len (gsequence+ 360)) 3)
(check-equal? (len (gsequence+ 504)) 3)

; 定義8 列の連結

(: M8 (-> GSequence GSequence Natural))
(define (M8 x y)
  (expt (P (+ (len x) (len y))) (+ x y)))

(: ** (-> GSequence GSequence GSequence))
; 元のソース
;(define (** x y)
;  (gsequence+
;   (Min z <= (M8 x y)
;        (and (∀ m <= (len x)
;                (⇒ (<= 1 m)
;                   (= (elm (gsequence+ z) m)
;                      (elm x m))))
;             (∀ n <= (len y)
;                (⇒ (<= 1 n)
;                   (= (elm (gsequence+ z)
;                           (+ (len x) n))
;                      (elm y n))))))))

(define (** x y)
  ; (printf "** ~a ~a~n" x y)
  (let ((lenx (len x)))
    (let loop ((k : Natural 1) (n : GSequence x))
      (let ((yk (elm y k)))
        ; (printf "~a ~a ~a~n" yk (= yk 0) (= (ann yk Natural) 0))
        ; (= yk 0)と書くとyk=0でも偽と判定されているようだ
        ; (= (ann yk Natural) 0)でも改善しない
        ;(if (= (cast yk Natural) 0)
        (if (= (cast yk Natural) 0)
            n
            (loop (+ k 1) (gsequence+ (* n (expt (P (+ lenx k)) yk)))))))))

(check-equal? (** (gsequence+ 8) (gsequence+ 4)) (gsequence+ 72))
(check-equal? (** (gsequence+ 6) (gsequence+ 2)) (gsequence+ 30))

; 定義9 xだけからなる列

(: <> (-> GSymbol GSequence))
(define (<> x) (gsequence+ (expt 2 x)))

(check-equal? (<> cf) 8)

; 定義10 xをカッコに入れた列

(: paren (-> GSequence GSequence))
(define (paren x)
  (** (** (<> clp) x) (<> crp)))

(check-equal? (paren (<> c0)) (* (expt 2 clp) (expt 3 c0) (expt 5 crp)))

; 定義11 xは"第n型"の"変数"である

(: IsVarBase (-> GSymbol Boolean))
(define (IsVarBase p)
  (and (> (cast p Natural) (cast crp Natural))
       (IsPrime p)))

(check-true (IsVarBase (gsymbol+ 17)))
(check-false (IsVarBase (gsymbol+ 13)))
(check-false (IsVarBase (gsymbol+ 18)))

(: IsVarType (-> GNumber Natural Boolean))
(define (IsVarType x n)
  (and (>= n 1)
       (∃ p <= x
          ;(printf "x=~a n=~a p=~a pn=~a var=~a =~a~n" x n p (expt p n) (IsVarBase (gsymbol+ p)) (= x (expt p n)))
          (and (IsVarBase (gsymbol+ p))
               (= x (expt p n))))))

(check-false (IsVarType (gsymbol+ 17) 0))
(check-false (IsVarType (gsymbol+ 13) 1))
(check-true (IsVarType (gsymbol+ 17) 1))
(check-false (IsVarType (gsymbol+ 17) 2))
(check-true (IsVarType (gsymbol+ (expt 17 3)) 3))

; 定義12 xは"変数"である

(: IsVar (-> GNumber Boolean))
(define (IsVar x)
  (∃ n <= x (IsVarType x n)))

(check-true (IsVar (gsymbol+ 17)))
(check-false (IsVar (gsymbol+ 13)))
(check-true (IsVar (gsymbol+ (expt 17 3))))

; 定義13 ¬(x)

(: Not (-> GForm GForm))
(define (Not x)
  (gform (** (<> cnot) (paren x))))

(check-equal? (Not (gform (<> (var 1 1)))) (gnum cnot clp (gsymbol+ 17) crp))

; 定義14 (x)∨(y)

(: Or (-> GForm GForm GForm))
(define (Or x y)
  (gform (** (** (paren x) (<> cor)) (paren y))))

(check-equal? (Or (gform (<> (var 1 1))) (gform (<> (var 2 1))))
              (gnum clp (var 1 1) crp cor clp (var 2 1) crp))

; 定義15 ∀x(a)

(: ForAll (-> GSymbol GForm GForm))
(define (ForAll x a)
  (gform (** (** (<> call) (<> x)) (paren a))))

(check-equal? (ForAll (var 1 1) (gform (<> (var 1 2))))
              (gnum call (var 1 1) clp (var 1 2) crp))

; 定義16 xの、n番目の後続数

(: succ (-> Natural GSequence GSequence))
(define (succ n x)
  (cond ((= n 0) x)
        (else (** (<> cf) (succ (- n 1) x)))))

(check-equal? (succ 0 (<> c0)) (gnum c0))
(check-equal? (succ 2 (<> c0)) (gnum cf cf c0))
(check-equal? (succ 1 (<> (var 2 2))) (gnum cf (var 2 2)))

; 定義17 nに対する"数項"

(: ￣ (-> Natural GSequence))
(define (￣ n)
  (succ n (<> c0)))

(check-equal? (￣ 0) (gnum c0))
(check-equal? (￣ 3) (gnum cf cf cf c0))

; 定義18 "第1型の記号である"

(: IsNumberType (-> GSequence Boolean))

; 元のソース
;(define (IsNumberType x)
;  (∃ m n <= x     
;     (and (or (= m c0) (IsVarType (gnumber m) 1))
;          (= x (succ n (<> (gsymbol+ m)))))))

(define (IsNumberType x)
  (let loop ((k : Natural 1))
    (let ((e (elm x k)))
      (cond ((not (= (nat e) (nat cf)))
             (and (or (= (nat e) (nat c0))
                      (IsVarType e 1))
                  (= (elm x (+ k 1)) 0)))
            (else (loop (+ k 1)))))))

(check-false (IsNumberType (gnum clp)))
(check-true (IsNumberType (gnum c0)))
(check-false (IsNumberType (gnum c0 c0)))
(check-true (IsNumberType (gnum cf c0)))
(check-false (IsNumberType (gnum cf c0 clp)))
(check-true (IsNumberType (gnum (var 1 1))))
(check-false (IsNumberType (gnum (var 1 1) c0)))
(check-true (IsNumberType (gnum cf (var 1 1))))
(check-false (IsNumberType (gnum cf (var 1 1) c0)))

; 定義19 "第n型の記号"である

(: IsNthType (-> GSequence Natural Boolean))

; 元のソース
;(define (IsNthType x n)
;  (or (and (= n 1)
;           (IsNumberType x))
;      (and (> n 1)
;           (∃ v <= x
;              (and (IsVarType (gnumber v) n)
;                   (= x (<> (gsymbol+ v))))))))

; 素因数分解を使う版
(define (IsNthType x n)
  (: I (-> (Opt Factor) Boolean))
  (define (I f1)
    (and (> (none-to-zero (factor-prime f1)) crp)
         (= (none-to-zero (factor-expt f1)) n)))
  (cond ((= n 1) (IsNumberType x))
        ((not (= (len x) 1)) #f)
        (else (let ((f (factorization (elm x 1))))
                (and (= (none-to-zero (factorization-length f)) 1)
                     (I (factorization-nth f 1)))))))

(check-true (IsNthType (gnum c0) 1))
(check-false (IsNthType (gnum c0) 2))
(check-true (IsNthType (gnum cf c0) 1))
(check-false (IsNthType (gnum cf c0) 2))
(check-true (IsNthType (gnum (var 1 1)) 1))
(check-false (IsNthType (gnum (var 1 1)) 2))
(check-true (IsNthType (gnum cf (var 1 1)) 1))
(check-false (IsNthType (gnum cf (var 1 1)) 2))
(check-false (IsNthType (gnum (var 1 2)) 1))
(check-true (IsNthType (gnum (var 1 2)) 2))
(check-false (IsNthType (gnum (var 2 2)) 1))
(check-true (IsNthType (gnum (var 2 2)) 2))
(check-false (IsNthType (gnum cf (var 2 2)) 2))

; 定義20 xは"基本論理式"である

(: IsElementForm (-> GSequence Boolean))

;元のソース
;(define (IsElementForm x)
;  (∃ a b n <= x
;     (and (IsNthType (gsequence+ a) (+ n 1))
;          (IsNthType (gsequence+ b) n)
;          (= x (** (gsequence+ a)
;                   (paren (gsequence+ b)))))))

; 素因数分解を使う版

; 変数の型 (xは列ではなく記号)
(: VarType (-> GNumber Natural))
(define (VarType x)
  (none-to-zero (factor-expt (factorization-nth (factorization x) 1))))

(check-eq? (VarType c0) 0)
(check-eq? (VarType (var 1 1)) 1)
(check-eq? (VarType (var 1 2)) 2)
(check-eq? (VarType (var 1 3)) 3)
(check-eq? (VarType (var 2 1)) 1)
(check-eq? (VarType (var 2 2)) 2)
(check-eq? (VarType (var 2 3)) 3)

; 列の一部を取り出す

(: ExtractSequence (-> GSequence Natural Natural GSequence))
(define (ExtractSequence x s e)
  (match (factorization x)
    [(None) (gsequence+ 0)]
    [(Some f)
     (if (< (length f) e)
         (gsequence+ 0)
         (apply gnum
                (map gnumber
                     (map Factor-expt
                          (drop (take f e) (- s 1))))))]))

(define (IsElementForm x)
  (let ((l (len x)))
    (and (>= l 4)
         (let* ((a : GNumber (elm x 1))
                (b : GSequence (ExtractSequence x 3 (cast (- (len x) 1) Natural)))
                (n : Natural (VarType a)))
           (and (IsVar a)
                (= (nat (elm x 2)) (nat clp))
                (IsNthType b (cast (- n 1) Natural))
                (= (nat (elm x (len x))) (nat crp)))))))

(check-true (IsElementForm (gnum (var 1 2) clp (var 1 1) crp)))
(check-true (IsElementForm (gnum (var 1 2) clp cf (var 1 1) crp)))
(check-true (IsElementForm (gnum (var 1 2) clp cf cf (var 1 1) crp)))

(check-false (IsElementForm (gnum (var 1 2) clp crp)))
(check-false (IsElementForm (gnum c0 clp (var 1 1) crp)))
(check-false (IsElementForm (gnum (var 1 2) crp (var 1 1) crp)))
(check-false (IsElementForm (gnum (var 1 2) crp (var 1 2) crp)))
(check-false (IsElementForm (gnum (var 1 2) crp (var 1 2) clp)))

; 定義21 "¬(a)"または"(a)∨(b)"または"∀v(a)"である

(: IsNotOp (-> GForm GForm Boolean))
(define (IsNotOp x a) (= x (Not a)))

(check-true (IsNotOp (gfrm cnot clp (var 1 2) clp cf cf (var 1 1) crp crp)
                     (gfrm (var 1 2) clp cf cf (var 1 1) crp)))
(check-false (IsNotOp (gfrm c0 clp (var 1 2) clp cf cf (var 1 1) crp crp)
                      (gfrm (var 1 2) clp cf cf (var 1 1) crp)))

(: IsOrOp (-> GForm GForm GForm Boolean))
(define (IsOrOp x a b) (= x (Or a b)))

(check-true (IsOrOp (gfrm clp (var 1 2) clp cf cf (var 1 1) crp crp cor clp (var 2 2) clp cf cf (var 2 1) crp crp)
                    (gfrm (var 1 2) clp cf cf (var 1 1) crp)
                    (gfrm (var 2 2) clp cf cf (var 2 1) crp)))
(check-false (IsOrOp (gfrm clp (var 1 2) clp cf cf (var 1 1) crp crp call clp (var 2 2) clp cf cf (var 2 1) crp crp)
                     (gfrm (var 1 2) clp cf cf (var 1 1) crp)
                     (gfrm (var 2 2) clp cf cf (var 2 1) crp)))

(: IsForallOp (-> GForm GForm Boolean))

;元のソース
;(define (IsForallOp x a)
;  (∃ v <= x (and (IsVar (gsymbol+ v))
;                 (= x (ForAll (gsymbol+ v) a)))))

(define (IsForallOp x a)
  (let ((v : GSymbol (gsymbol+ (elm x 2))))
    (and (IsVar v)
         (= x (ForAll v a)))))

(check-true (IsForallOp (gfrm call (var 1 1) clp (var 1 2) clp cf cf (var 1 1) crp crp)
                        (gfrm (var 1 2) clp cf cf (var 1 1) crp)))
(check-false (IsForallOp (gfrm call (var 1 1) clp (var 1 2) clp cf cf (var 1 1) crp crp)
                         (gfrm (var 1 1) clp cf cf (var 1 1) crp)))

(: IsOp (-> GForm GForm GForm Boolean))
(define (IsOp x a b)
  (or (IsNotOp x a)
      (IsOrOp x a b)
      (IsForallOp x a)))

(check-true (IsOp (gfrm cnot clp (var 1 2) clp cf cf (var 1 1) crp crp)
                  (gfrm (var 1 2) clp cf cf (var 1 1) crp)
                  (gfrm (var 1 2) clp cf cf (var 1 1) crp)))
(check-true (IsOp (gfrm clp (var 1 2) clp cf cf (var 1 1) crp crp cor clp (var 2 2) clp cf cf (var 2 1) crp crp)
                  (gfrm (var 1 2) clp cf cf (var 1 1) crp)
                  (gfrm (var 2 2) clp cf cf (var 2 1) crp)))
(check-true (IsOp (gfrm call (var 1 1) clp (var 1 2) clp cf cf (var 1 1) crp crp)
                  (gfrm (var 1 2) clp cf cf (var 1 1) crp)
                  (gfrm (var 1 2) clp cf cf (var 1 1) crp)))
(check-false (IsOp (gfrm clp (var 1 2) clp cf cf (var 1 1) crp crp call clp (var 2 2) clp cf cf (var 2 1) crp crp)
                   (gfrm (var 1 2) clp cf cf (var 1 1) crp)
                   (gfrm (var 2 2) clp cf cf (var 2 1) crp)))

; 定義22 "基本論理式"から組み上げた"論理式"の列である

(: IsFormSeq (-> GSeqSeq Boolean))
(define (IsFormSeq x)
  (and (> (len x) 0)
       (∀ n <= (len x)
          (⇒ (> n 0)
             (or (IsElementForm (gsequence+ (elm x n)))
                 (∃ p q < n
                    (and (> p 0) (> q 0)
                         (IsOp (gform+ (elm x n))
                               (gform+ (elm x p))
                               (gform+ (elm x q))))))))))

(check-false (IsFormSeq (gseqseq+ (gnum (gnum c0)))))
; 列の列は無理
;(check-true (IsFormSeq (gseqseq+ (gnum (gnum (var 1 2) clp c0 crp)))))

; 定義23 xは論理式である

(define (M23 [x : GSequence]) : Natural
  (expt (P (sqr (len x))) (* x (sqr (len x)))))

;実行できるわけがない ゲーデル数の計算すらできまい
;(M23 (gnum (gnum (var 1 2) clp c0 crp)
;           (gnum cnot clp (var 1 2) clp c0 crp crp)))
;正しい論理式の列ではないが背に腹は変えられない
(check-equal? (M23 (gsequence+ 6)) (expt 7 24))

(define (IsEndedWith [n : GSeqSeq]
                     [x : GSequence]) : Boolean
  (= (elm n (len n)) x))

;正しい論理式の列ではない
(check-true (IsEndedWith (gseqseq+ (gnum (gnum cf c0) (gnum c0))) (gnum c0)))
(check-false (IsEndedWith (gseqseq+ (gnum (gnum cf c0) (gnum c0))) (gnum cf)))

(define (IsForm [x : GSequence]) : Boolean
  (∃ n <= (M23 x) (and (IsFormSeq (gseqseq+ n))
                       (IsEndedWith (gseqseq+ n) x))))

;(IsForm (gnum (gnum (var 1 2) clp c0 crp)
;              (gnum cnot clp (var 1 2) clp c0 crp crp)))

; 定義24 "変数"vはxのn番目の場所では"束縛"されている

(define (IsBoundAt [v : GSymbol]
                   [n : Natural]
                   [x : GForm]) : Boolean
  (and (IsVar v)
       (IsForm x)
       (∃ a b c <= x (and (= x (** (** (gsequence+ a)
                                       (ForAll v (gform+ b)))
                                   (gsequence+ c)))
                          (IsForm b)
                          (<= (+ (len a) 1) n)
                          (<= n (+ (len a) (len (ForAll v b))))))))

;(check-false (IsBoundAt (var 1 1) 1 (gfrm call (var 1 1) clp (var 1 2) clp (var 1 1) crp crp)))
;(check-true (IsBoundAt (var 1 1) 6 (gfrm call (var 1 1) clp (var 1 2) clp (var 1 1) crp crp)))

; 定義25 "変数"vはxのn番目の場所では"束縛"されてない

(define (IsFreeAt [v : GSymbol]
                  [n : Natural]
                  [x : GForm]) : Boolean
  (and (IsVar v)
       (IsForm x)
       (= v (elm x n))
       (<= n (len x))
       (not (IsBoundAt v n x))))

;(check-true (IsFreeAt (var 1 1) 1 (gfrm call (var 1 1) clp (var 1 2) clp (var 1 1) crp crp)))
;(check-false (IsFreeAt (var 1 1) 6 (gfrm call (var 1 1) clp (var 1 2) clp (var 1 1) crp crp)))

; 定義26 vはxの"自由変数"である

(define (IsFree [v : GSymbol]
                [x : GForm]) : Boolean
  (∃ n <= (len x) (IsFreeAt v n x)))

;(check-true (IsFree (var 1 1) (gfrm call (var 1 1) clp (var 1 2) clp (var 1 1) crp crp)))
;(check-false (IsFree (var 2 1) (gfrm call (var 1 1) clp (var 1 2) clp (var 1 1) crp crp)))


; 定義27 xのn番目の要素をcで置き換えたもの

(define (substAtWith [x : GSequence]
                     [n : Natural]
                     [c : GSequence]) : GSequence
  (gsequence+
   (Min z <= (M8 x c)
        (∃ a b <= x
           (and (= n (+ (len (gsequence+ a)) 1))
                (= x (** (** (gsequence+ a)
                             (<> (gsymbol (elm x n))))
                         (gsequence+ b)))
                (= z (** (** a c) b)))))))

;(check-equal? (substAtWith (gnum call (var 1 1) clp (var 1 2) clp (var 1 1) crp crp)
;                           6
;                           (gnum cf c0))
;              (gnum call (var 1 1) clp (var 1 2) clp cf c0 crp crp))

; 定義28 xでk+1番目の"自由"であるvの場所
; ただし、k+1番目というのは列の末尾から逆向きに数える

(define (freepos [k : Natural]
                 [v : GSymbol]
                 [x : GForm]) : Natural
  (cond ((= k 0)
         (Min n <= (len x)
              (and (IsFreeAt v n x)
                   (not (∃ p <= (len x)
                           (and (< n p)
                                (IsFreeAt v p x)))))))
        (else
         (Min n < (freepos (- k 1) v x)
              (and (IsFreeAt v n x)
                   (not (∃ p < (freepos (- k 1) v x)
                           (and (< n p)
                                (IsFreeAt v p x)))))))))

; 定義29 xで、vが"自由"である場所の総数

(define (freenum [v : GSymbol]
                 [x : GForm]) : Natural
  (Min n <= (len x) (= (freepos n v x) 0)))

; 定義30 xの"自由"であるvの場所のうち、k個をcで置き換えた論理式

(define (substSome [k : Natural]
                   [x : GForm]
                   [v : GSymbol]
                   [c : GSequence]) : GForm
  (cond ((= k 0) x)
        (else
         (gform
          (substAtWith (substSome (- k 1) x v c)
                       (freepos (- k 1) v x)
                       c)))))

; 定義31 aの"自由"であるvをすべてcで置換した"論理式"

(define (subst [a : GForm]
               [v : GSymbol]
               [c : GSequence])
  (substSome (freenum v a) a v c))

; 定義32 "(a)→(b)"、"(a)∧(b)"、"(a)⇄(b)"、"∃x(a)"を得る関数

(define (Implies [a : GForm]
                 [b : GForm]) : GForm
  (Or (Not a) b))
(define (And [a : GForm]
             [b : GForm]) : GForm
  (Not (Or (Not a) (Not b))))
(define (Equiv [a : GForm]
               [b : GForm]) : GForm
  (And (Implies a b) (Implies b a)))
(define (Exists [x : GSymbol]
                [a : GForm]) : GForm
  (Not (ForAll x (Not a))))

; 定義33 xを、nだけ"型持ち上げ"したもの

(define (typelift [n : Natural]
                  [x : GForm])
  (Min y <= (expt x (expt x n))
       (∀ k <= (len x)
          (or (and (not (IsVar (elm x k)))
                   (= (elm (gform+ y) k) (elm x k)))
              (and (IsVar (elm x k))
                   (= (elm (gform+ y) k)
                      (* (elm x k)
                         (expt (prime 1 (elm x k)) n))))))))

; 定義34 xは公理Iから得られる"論理式"である

; ペアノの公理ね

; まずは等号の定義
; ∀u(u(x1)→u(y1))
; x1、y1は任意の変数、ただし第1型に限るということでいいのかな？
; uは第2型の任意の変数？なにか決める必要があるのでz2に（なんか嫌な感じだけど

(define (ElementForm [a : GSymbol]
                     [b : GSequence]) : GForm
  (gform+ (** (<> a) (paren b))))

(define (Equal [x : GSequence]
               [y : GSequence]) : GForm
  (ForAll (var 3 2) (Implies (ElementForm (var 3 2) x)
                             (ElementForm (var 3 2) y))))

(define AxiomI-1
  (Not (Equal (succ 1 (<> (var 1 1))) (￣ 0))))

(define AxiomI-2
  (Implies (Equal (succ 1 (<> (var 1 1)))
                  (succ 1 (<> (var 2 1))))
           (Equal (<> (var 1 1))
                  (<> (var 2 1)))))

(define AxiomI-3
  (Implies (And (ElementForm (var 1 2) (￣ 0))
                (ForAll (var 1 1)
                        (Implies (ElementForm (var 1 2) (<> (var 1 1)))
                                 (ElementForm (var 1 2) (succ 1 (<> (var 1 1)))))))
           (ForAll (var 1 1) (ElementForm (var 1 2) (<> (var 1 1))))))

(define (IsAxiomI [x : GForm])
  (or (= x AxiomI-1)
      (= x AxiomI-2)
      (= x AxiomI-3)))

; 定義35 xは公理II-nから得られる"論理式"である

(define (IsSchemaII [n : (U 1 2 3 4)]
                    [x : GForm]) : Boolean
  (case n
    ((1) (∃ p <= x (and (IsForm (gsequence+ p))
                        (= x (Implies (Or (gform+ p)
                                          (gform+ p))
                                      (gform+ p))))))
    ((2) (∃ p q <= x (and (IsForm (gsequence+ p))
                          (IsForm (gsequence+ q))
                          (= x (Implies (gform+ p)
                                        (Or (gform+ p)
                                            (gform+ q)))))))
    ((3) (∃ p q <= x (and (IsForm (gsequence+ p))
                          (IsForm (gsequence+ q))
                          (= x (Implies (Or (gform+ p)
                                            (gform+ q))
                                        (Or (gform+ q)
                                            (gform+ p)))))))
    ((4) (∃ p q r <= x (and (IsForm (gsequence+ p))
                            (IsForm (gsequence+ q))
                            (IsForm (gsequence+ r))
                            (= x (Implies (Implies (gform+ p)
                                                   (gform+ q))
                                          (Implies (Or (gform+ r)
                                                       (gform+ p))
                                                   (Or (gform+ r)
                                                       (gform+ q))))))))
    (else #f)))

; 定義36 xは公理IIから得られる"論理式"である

(define (IsAxiomII [x : GForm]) : Boolean
  (or (IsSchemaII 1 x)
      (IsSchemaII 2 x)
      (IsSchemaII 3 x)
      (IsSchemaII 4 x)))

; 定義37 zは、yの中でvが"自由"な範囲に、"束縛"された"変数"を持たない

(define (IsNotBoundIn [z : GSequence]
                      [y : GForm]
                      [v : GSymbol]) : Boolean
  (not (∃ n <= (len y)
          (∃ m <= (len z)
             (∃ w <= z
                (and (= w (elm z m))
                     (IsBoundAt (gsymbol+ w) n y)
                     (IsFreeAt v n y)))))))

; 定義38 xは公理III-1から得られる"論理式"である
; 定義39 xは公理III-2から得られる"論理式"である
; 定義 xは公理IIIから得られる"論理式"である

(define (IsSchemaIII [k : Natural]
                     [x : GForm]) : Boolean
  (case k
    ((1) (∃ v y z n <= x
            (and (IsVarType (gsymbol+ v) n)
                 (IsNthType (gsequence+ z) n)
                 (IsForm (gsequence+ y))
                 (IsNotBoundIn (gsequence+ z) (gform+ y) (gsymbol+ v))
                 (= x (Implies (ForAll (gsymbol+ v)
                                       (gform+ y))
                               (subst (gform+ y)
                                      (gsymbol+ v)
                                      (gsequence+ z)))))))
    ((2) (∃ v q p <= x
            (and (IsVar (gnumber v))
                 (IsForm (gsequence+ p))
                 (not (IsFree (gsymbol+ v) (gform+ p)))
                 (IsForm (gsequence+ q))
                 (= x (Implies (ForAll (gsymbol+ v)
                                       (Or (gform+ p)
                                           (gform+ q)))
                               (Or (gform+ p)
                                   (ForAll (gsymbol+ v)
                                           (gform+ q))))))))
    (else #f)))

(define (IsAxiomIII [x : GForm]) : Boolean
  (or (IsSchemaIII 1 x)
      (IsSchemaIII 2 x)))

; 定義40 xは公理IVから得られる"論理式"である

(define (IsAxiomIV [x : GForm]) : Boolean
  (∃ u v y n <= x
     (and (IsVarType (gsymbol+ u) (+ n 1))
          (IsVarType (gsymbol+ v) n)
          (IsForm (gsequence+ y))
          (not (IsFree (gsymbol+ u) (gform+ y)))
          (= x
             (Exists (gsymbol+ u)
                     (ForAll (gsymbol+ v)
                             (Equiv (gform+
                                     (** (<> (gsymbol+ u))
                                         (paren (<> (gsymbol+ v)))))
                                    (gform+ y))))))))

; 定義41 xは公理Vから得られる"論理式"である

(define AxiomV : GForm
  (Implies (ForAll (var 1 1)
                   (Equiv (ElementForm (var 1 2) (<> (var 1 1)))
                          (ElementForm (var 2 2) (<> (var 1 1)))))
           (Equal (<> (var 1 2)) (<> (var 2 2)))))

(define (IsAxiomV [x : GForm]) : Boolean
  (∃ n <= x (= x (typelift n AxiomV))))

; 定義42 xは"公理"である

(define (IsAxiom [x : GForm]) : Boolean
  (or (IsAxiomI x)
      (IsAxiomII x)
      (IsAxiomIII x)
      (IsAxiomIV x)
      (IsAxiomV x)))

; 定義43 xはaとbの"直接の帰結"である

(define (IsConseq [x : GForm]
                  [a : GForm]
                  [b : GForm]) : Boolean
  (or (= a (Implies b x))
      (∃ v <= x (and (IsVar (gsymbol+ v))
                     (= x (ForAll (gsymbol+ v) a))))))

; 定義44 xは"形式的証明"である

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

; 定義45 pはxの"形式的証明"である

(define (Proves [p : GProof]
                [x : GForm])
  (and (IsProof p)
       (IsEndedWith p x)))

; 定義46 xには、"形式的証明"が存在する

(define (IsProvable [x : GForm])
  (∃ p (Proves (gproof+ p) x)))
