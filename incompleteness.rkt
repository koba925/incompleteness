#lang racket

(require rackunit)
(require profile)
(require (for-syntax syntax/parse))
(require (for-syntax racket/syntax))

; 装備を整える

; literalは何か定義しておかないとエラーになる
; とりあえず定義されていれば値はなんでもいい模様
; よくわかってない

(define ≦ #f)

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
             [(_ v:id ...+ vn:id ≦ max:expr body:expr)
              #'(name v (... ...) ≦ max (fname max (λ (vn) body)))]))))))

(define-equipment ∀ not (const #t) (const #f))
(define-equipment ∃ identity (const #f) (const #t))
(define-equipment Min identity (const 0) identity)

(check-true (∀ x ≦ 3 (< x 4)))
(check-false (∀ x ≦ 3 (< x 3)))
(check-true (∃ x ≦ 3 (= x 2)))
(check-false (∃ x ≦ 3 (= x 4)))
(check-eq? (Min x ≦ 3 (= x 2)) 2)
(check-eq? (Min x ≦ 3 (= x 4)) 0)

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

; もとの定義
;(define (CanDivide x d)
;  (∃ n ≦ x (= x (* d n))))

(define (CanDivide x d)
  (and (not (zero? d))
       (= (remainder x d) 0)))

(check-true (CanDivide 12 3))
(check-false (CanDivide 12 5))

; 定義2 xは素数である

; もとの定義
;(define (IsPrime x)
;  (and (> x 1)
;       (not (∃ d ≦ x (and (not (= d 1))
;                          (not (= d x))
;                          (CanDivide x d))))))

(define (IsPrime x)
  (and (> x 1)
       (let loop ((d 2))
         (cond ((> (* d d) x) #t)
               ((CanDivide x d) #f)
               (else (loop (+ d 1)))))))

(check-false (IsPrime 0))
(check-false (IsPrime 1))
(check-true (IsPrime 2))
(check-false (IsPrime 12))
(check-true (IsPrime 17))

; 定義4 nの階乗

(define (factorial n)
  (cond ((= n 0) 1)
        (else (* n (factorial (- n 1))))))

(check-eq? (factorial 0) 1)
(check-eq? (factorial 3) 6)

; 定義5 n番目の素数

; 元のソース
;(define (M5 n)
;  (+ (factorial n) 1))
;
;(define (P n)
;  (cond ((= n 0) 0)
;        (else (Min p ≦ (M5 n) (and (< (P (- n 1)) p)
;                                   (IsPrime p))))))

(define primes (make-hash))
(hash-set! primes 0 0)
(hash-set! primes 1 2)

(define (P n)
  (cond ((hash-ref primes n #f))
        (else (let loop ((k (+ (P (- n 1)) 1)))
                (cond ((IsPrime k)
                       (hash-set! primes n k)
                       ;(printf "New prime ~a ~a~n" n k)
                       k)
                      (else (loop (+ k 1))))))))

(check-eq? (P 0) 0)
(check-eq? (P 1) 2)
(check-eq? (P 5) 11)

; 素因数分解

(define (times-divide x p)
  (let loop ((k 1) (x x))
    (cond ((not (CanDivide x p)) (values x (- k 1)))
          (else (loop (+ k 1) (/ x p))))))

; 0や1の素因数分解は特別扱いする
; ゲーデルの関数は定義域をはずれたとき0を返すようになっているので
; これでうまくいく

(define factorizations (make-hash))
(hash-set! factorizations 0 '((0 . 0)))
(hash-set! factorizations 1 '((0 . 0)))

(define (factorization x)
  ;(printf "factorization ~a~n" x)
  (cond ((hash-ref factorizations x #f))
        (else
         (let loop ((n 1) (x1 x) (f '()))
           (if (= x1 1)
               (let ((f (reverse f)))
                 (hash-set! factorizations x f)
                 f)
               (let*-values (((pn) (P n))
                             ((x1 k) (times-divide x1 pn)))
                 (loop (+ n 1)
                       x1
                       (if (= k 0)
                           f
                           (cons (cons pn k) f)))))))))

;こう書きたいところだが、nが範囲外のときは0を返す必要がある
;(define factor-length length)
;(define (factor-nth f n) (list-ref f (- n 1)))

(define (factor-length f)
  (if (equal? f '((0 . 0)))
      0
      (length f)))

(define (factor-nth f n)
  (cond ((null? f) '(0 . 0))
        ((= n 1) (car f))
        (else (factor-nth (cdr f) (- n 1)))))
(define factor-prime car)
(define factor-expt cdr)

(check-equal? (factorization 0) '((0 . 0)))
(check-equal? (factorization 1) '((0 . 0)))
(check-equal? (factorization 40) '((2 . 3) (5 . 1)))  

(check-eq? (factor-length (factorization 0)) 0)
(check-eq? (factor-length (factorization 40)) 2)
(check-equal? (factor-nth (factorization 40) 0) '(0 . 0))
(check-equal? (factor-nth (factorization 40) 1) '(2 . 3))
(check-equal? (factor-nth (factorization 40) 2) '(5 . 1))
(check-equal? (factor-nth (factorization 40) 3) '(0 . 0))

; 定義3 n番目の、xの素因数

; 元のソース
;(define (CanDivideByPrime x p)
;  (and (CanDivide x p) (IsPrime p)))
;
;(check-true (CanDivideByPrime 12 3))
;(check-false (CanDivideByPrime 12 5))
;(check-false (CanDivideByPrime 12 6))
;
;(define (prime n x)
;  (cond ((= n 0) 0)
;        (else (Min p ≦ x (and (< (prime (- n 1) x) p)
;                              (CanDivideByPrime x p))))))

; 素因数分解を利用する版
; nが範囲外だったり、xが0や1でもうまくいくようにfactor*が吸収してくれる
(define (prime n x)
  ;(printf "prime ~a ~a~n" n x)
  (factor-prime (factor-nth (factorization x) n)))

(check-eq? (prime 0 2352) 0)
(check-eq? (prime 1 2352) 2)
(check-eq? (prime 2 2352) 3)
(check-eq? (prime 3 2352) 7)

; 定数

(define c0 1)
(define cf 3)
(define cnot 5)
(define cor 7)
(define call 9)
(define clp 11)
(define crp 13)

; 変数

; x1=(var 1 1)、y1=(var 2 1)、z3=(var 3 3)などと表記することにする
(define (var n c) (expt (P (+ 6 n)) c))

(check-eq? (var 1 1) 17)
(check-eq? (var 3 3) (expt 23 3))

; ゲーデル数

(define (gnum . seq)
  (define (iter s k n)
    (if (null? s)
        n
        (iter (cdr s) (+ k 1) (* (expt (P k) (car s)) n))))
  (iter seq 1 1))

(check-equal? (gnum) 1)
(check-equal? (gnum 3 2 1) 360)
(check-equal? (gnum call (var 1 2) clp (var 1 1) crp)
              (* (expt 2 call)
                 (expt 3 (var 1 2))
                 (expt 5 clp)
                 (expt 7 (var 1 1))
                 (expt 11 crp)))

; 定義6 n番目の要素

; 元のソース
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
;
;(define (elm x n)
;  (Min k ≦ x (and (CanDivideByPower x n k)
;                  (not (CanDivideByPower x n (+ k 1))))))

; 素因数分解を利用する版
; nが範囲外だったり、xが0や1でもうまくいくようにfactor*が吸収してくれる
(define (elm x n)
  (factor-expt (factor-nth (factorization x) n)))

(check-equal? (elm 0 0) 0)
(check-equal? (elm 0 1) 0)
(check-equal? (elm 0 2) 0)
(check-equal? (elm 1 0) 0)
(check-equal? (elm 1 1) 0)
(check-equal? (elm 1 2) 0)
(check-equal? (elm 360 0) 0)
(check-equal? (elm 360 1) 3)
(check-equal? (elm 360 2) 2)
(check-equal? (elm 360 3) 1)
(check-equal? (elm 360 5) 0)
(check-equal? (elm 504 3) 1)

; 定義7 列の長さ

; 元のソース
;(define (len x)
;  (Min k ≦ x (and (> (prime k x) 0)
;                  (= (prime (+ k 1) x) 0))))

; 素因数分解を利用する版
; xが0や1でもうまくいくようにfactor*が吸収してくれる
(define (len x)
  (factor-length (factorization x)))

(check-equal? (len 0) 0) ;意味がある？
(check-equal? (len 1) 0) 
(check-equal? (len 4) 1)
(check-equal? (len 360) 3)
(check-equal? (len 504) 3)

; 定義8 列の連結

; 元のソース
;(define (M8 x y)
;  (expt (P (+ (len x) (len y))) (+ x y)))
;
;(define (** x y)
;  (Min z ≦ (M8 x y)
;       (and (∀ m ≦ (len x)
;               (⇒ (<= 1 m) (= (elm z m) (elm x m))))
;            (∀ n ≦ (len y)
;               (⇒ (<= 1 n) (= (elm z (+ (len x) n)) (elm y n)))))))

(define (** x y)
  ;(printf "** ~a ~a~n" x y)
  (let ((lenx (len x)))
    (let loop ((k 1) (n x))
      (let ((yk (elm y k)))
        (if (= yk 0)
            n
            (loop (+ k 1) (* n (expt (P (+ lenx k)) yk))))))))

(check-equal? (** 8 4) 72)
(check-equal? (** 6 2) 30)

; 定義9 xだけからなる列

(define (<> x) (expt 2 x))

; 定義10 xをカッコに入れた列

(define (paren x)
  (** (** (<> clp) x) (<> crp)))

(check-equal? (paren (<> c0)) (* (expt 2 clp) (expt 3 c0) (expt 5 crp)))

; 定義11 xは"第n型"の"変数"である

(define (IsVarBase p)
  (and (> p crp) (IsPrime p)))

(check-true (IsVarBase 17))
(check-false (IsVarBase 13))
(check-false (IsVarBase 18))

(define (IsVarType x n)
  (and (>= n 1)
       (∃ p ≦ x (and (IsVarBase p) (= x (expt p n))))))

(check-false (IsVarType 13 1))
(check-true (IsVarType 17 1))
(check-false (IsVarType 17 2))
(check-true (IsVarType (expt 17 3) 3))

; 定義12 xは"変数"である

(define (IsVar x)
  (∃ n ≦ x (IsVarType x n)))

(check-true (IsVar 17))
(check-false (IsVar 13))
(check-true (IsVar (expt 17 3)))

; 定義13 ¬(x)

(define (Not x)
  (** (<> cnot) (paren x)))

(check-equal? (Not (<> (var 1 1))) (gnum cnot clp 17 crp))

; 定義14 (x)∨(y)

(define (Or x y)
  (** (** (paren x) (<> cor)) (paren y)))

(check-equal? (Or (<> (var 1 1)) (<> (var 2 1)))
              (gnum clp (var 1 1) crp cor clp (var 2 1) crp))

; 定義15 ∀x(a)

(define (ForAll x a)
  (** (** (<> call) (<> x)) (paren a)))

(check-equal? (ForAll (var 1 1) (<> (var 1 2)))
              (gnum call (var 1 1) clp (var 1 2) crp))

; 定義16 xの、n番目の後続数

(define (succ n x)
  (cond ((= n 0) x)
        (else (** (<> cf) (succ (- n 1) x)))))

(check-equal? (succ 0 (<> c0)) (gnum c0))
(check-equal? (succ 2 (<> c0)) (gnum cf cf c0))
(check-equal? (succ 1 (<> (var 2 2))) (gnum cf (var 2 2)))

; 定義17 nに対する"数項"

(define (￣ n)
  (succ n (<> c0)))

(check-equal? (￣ 0) (gnum c0))
(check-equal? (￣ 3) (gnum cf cf cf c0))

; 定義18 "第1型の記号である"

; 元のソース
;(define (IsNumberType x)
;  (∃ m n ≦ x 
;     (begin
;       (printf "~a ~a ~a~n" m n x)
;       (and (or (= m c0) (IsVarType m 1))
;          (= x (succ n (<> m)))))))

(define (IsNumberType x)
  (let loop ((k 1))
    (let ((e (elm x k)))
      (cond ((not (= e cf))
             (and (or (= e c0) (IsVarType e 1))
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

; 元のソース
;(define (IsNthType x n)
;  (or (and (= n 1) (IsNumberType x))
;      (and (> n 1) (∃ v ≦ x (and (IsVarType v n) (= x (<> v)))))))


; 素因数分解を使う版
(define (IsNthType x n)
  (cond ((= n 1) (IsNumberType x))
        ((not (= (len x) 1)) #f)
        (else (let ((f (factorization (elm x 1))))
                (and (= (factor-length f) 1)
                     (> (factor-prime (car f)) crp)
                     (= (factor-expt (car f)) n))))))

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

;元のソース
;(define (IsElementForm x)
;  (∃ a b n ≦ x
;     (and (IsNthType a (+ n 1))
;          (IsNthType b n)
;          (= x (** a (paren b))))))

; 変数の型 (xは列ではなく記号)
(define (VarType x)
  (factor-expt (car (factorization x))))

(check-eq? (VarType (var 1 1)) 1)
(check-eq? (VarType (var 1 2)) 2)
(check-eq? (VarType (var 1 3)) 3)
(check-eq? (VarType (var 2 1)) 1)
(check-eq? (VarType (var 2 2)) 2)
(check-eq? (VarType (var 2 3)) 3)

; 列の一部を取り出す

(define (ExtractSequence x s e)
  (let ((f (factorization x)))
    (apply gnum (map factor-expt (drop (take f e) (- s 1))))))

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

(check-true (IsElementForm (gnum (var 1 2) clp (var 1 1) crp)))
(check-true (IsElementForm (gnum (var 1 2) clp cf (var 1 1) crp)))
(check-true (IsElementForm (gnum (var 1 2) clp cf cf (var 1 1) crp)))

(check-false (IsElementForm (gnum (var 1 2) clp crp)))
(check-false (IsElementForm (gnum c0 clp (var 1 1) crp)))
(check-false (IsElementForm (gnum (var 1 2) crp (var 1 1) crp)))
(check-false (IsElementForm (gnum (var 1 2) crp (var 1 2) crp)))
(check-false (IsElementForm (gnum (var 1 2) crp (var 1 2) clp)))

; 定義21 "¬(a)"または"(a)∨(b)"または"∀v(a)"である

(define (IsNotOp x a) (= x (Not a)))

(check-true (IsNotOp (gnum cnot clp (var 1 2) clp cf cf (var 1 1) crp crp)
                     (gnum (var 1 2) clp cf cf (var 1 1) crp)))
(check-false (IsNotOp (gnum c0 clp (var 1 2) clp cf cf (var 1 1) crp crp)
                      (gnum (var 1 2) clp cf cf (var 1 1) crp)))

(define (IsOrOp x a b) (= x (Or a b)))

(check-true (IsOrOp (gnum clp (var 1 2) clp cf cf (var 1 1) crp crp cor clp (var 2 2) clp cf cf (var 2 1) crp crp)
                    (gnum (var 1 2) clp cf cf (var 1 1) crp)
                    (gnum (var 2 2) clp cf cf (var 2 1) crp)))
(check-false (IsOrOp (gnum clp (var 1 2) clp cf cf (var 1 1) crp crp call clp (var 2 2) clp cf cf (var 2 1) crp crp)
                     (gnum (var 1 2) clp cf cf (var 1 1) crp)
                     (gnum (var 2 2) clp cf cf (var 2 1) crp)))

;元のソース
;(define (IsForallOp x a)
;  (∃ v ≦ x (and (IsVar v) (= x (ForAll v a)))))

(define (IsForallOp x a)
  (let ((v (elm x 2)))
    (and (IsVar v)
         (= x (ForAll v a)))))

(check-true (IsForallOp (gnum call (var 1 1) clp (var 1 2) clp cf cf (var 1 1) crp crp)
                        (gnum (var 1 2) clp cf cf (var 1 1) crp)))
(check-false (IsForallOp (gnum call (var 1 1) clp (var 1 2) clp cf cf (var 1 1) crp crp)
                         (gnum (var 1 1) clp cf cf (var 1 1) crp)))

(define (IsOp x a b)
  (or (IsNotOp x a)
      (IsOrOp x a b)
      (IsForallOp x a)))

(check-true (IsOp (gnum cnot clp (var 1 2) clp cf cf (var 1 1) crp crp)
                  (gnum (var 1 2) clp cf cf (var 1 1) crp)
                  (gnum (var 1 2) clp cf cf (var 1 1) crp)))
(check-true (IsOp (gnum clp (var 1 2) clp cf cf (var 1 1) crp crp cor clp (var 2 2) clp cf cf (var 2 1) crp crp)
                  (gnum (var 1 2) clp cf cf (var 1 1) crp)
                  (gnum (var 2 2) clp cf cf (var 2 1) crp)))
(check-true (IsOp (gnum call (var 1 1) clp (var 1 2) clp cf cf (var 1 1) crp crp)
                  (gnum (var 1 2) clp cf cf (var 1 1) crp)
                  (gnum (var 1 2) clp cf cf (var 1 1) crp)))
(check-false (IsOp (gnum clp (var 1 2) clp cf cf (var 1 1) crp crp call clp (var 2 2) clp cf cf (var 2 1) crp crp)
                   (gnum (var 1 2) clp cf cf (var 1 1) crp)
                   (gnum (var 2 2) clp cf cf (var 2 1) crp)))

; 定義22 "基本論理式"から組み上げた"論理式"の列である

(define (IsFormSeq x)
  (and (> (len x) 0)
       (∀ n ≦ (len x)
          (⇒ (> n 0)
             (or (IsElementForm (elm x n))
                 (∃ p q ≦ (- n 1)
                    (and (> p 0) (> q 0)
                         (IsOp (elm x n) (elm x p) (elm x q)))))))))

;
;(check-false (IsFormSeq (gnum (gnum c0))))
;(check-true (IsFormSeq (gnum (gnum (var 1 2) clp c0 crp))))
;(check- (IsFormSeq (gnum )))
