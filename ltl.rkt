#lang racket

(define Atom? symbol?)
(struct Neg (e) #:transparent)
(struct Lor (e1 e2) #:transparent)
(struct Land (e1 e2) #:transparent)
(struct Next (e) #:transparent)
(struct Until (a b) #:transparent)
(struct Release (a b) #:transparent)
(struct Global (e) #:transparent)
(struct Future (e) #:transparent)

(define t 'true)
(define f 'false)
(define (- e)
  (cond [(Neg? e) (Neg-e e)]
        [(eq? e t) f]
        [(eq? e f) t]
        [else (Neg e)]))
(define (O e1 e2)
  (cond [(eq? e1 f) e2]
        [(eq? e2 f) e1]
        [(or (eq? e1 t) (eq? e2 t)) t]
        [else (Lor e1 e2)]))
(define (X-true? e) (and (Next? e) (eq? (Next-e e) t)))
(define-syntax +
  (syntax-rules ()
    [(_ l ...)
     (foldl (λ(x acc) (O acc x)) f (list l ...))]))
(define (A e1 e2)
  (cond [(eq? e1 t) e2]
        [(eq? e2 t) e1]
        [(or (eq? e1 f) (eq? e2 f)) f]
        [else (Land e1 e2)]))
(define-syntax *
  (syntax-rules ()
    [(_ l ...)
     (foldl (λ(x acc) (A acc x)) t (list l ...))]))
(define-syntax pipe
  (syntax-rules ()
    [(_ l ...)
     (λ(x)(foldl (λ(f acc) (f acc)) x (list l ...)))]))
(define (X e) (Next e))
(define (F e) (Future e))
(define (G e) (Global e))
(define (U e1 e2) (Until e1 e2))
(define (R e1 e2) (Release e1 e2))

(define (BoolOnly? e)
  (cond [(Atom? e) #t]
        [(Neg? e) (BoolOnly? (Neg-e e))]
        [(Land? e) (and (BoolOnly? (Land-e1 e))
                        (BoolOnly? (Land-e2 e)))]
        [(Lor? e) (and (BoolOnly? (Lor-e1 e))
                       (BoolOnly? (Lor-e2 e)))]
        [else #f]))
(define (NF e)
  (cond [(BoolOnly? e) (list (* e (Next t)))]
        [(Lor? e) (append (NF (Lor-e1 e)) (NF (Lor-e2 e)))]
        [(Land? e) (let ([nf1 (NF (Land-e1 e))]
                         [nf2 (NF (Land-e2 e))])
                     (append-map
                      (λ(x)(map
                            ;; 这里多套一层是为了化简
                            (λ(y)(Land (* (Land-e1 x) (Land-e1 y))
                                       (X (* (Next-e (Land-e2 x)) (Next-e (Land-e2 y))))))
                            nf2)) nf1))]
        [(Next? e) (list (Land t e))]
        [(Until? e) (append (NF (Until-b e)) (NF (Land (Until-a e) (X e))))]
        [(Future? e)
         (NF (Lor (Future-e e) (Next e)))]
        [(Global? e) (map (λ(x) (Land (Land-e1 x)
                                      (X (* (Next-e (Land-e2 x))
                                            (Next e)))))
                          (NF (Global-e e)))]
        [(Release? e) (append (NF (Land (Release-a e)
                                        (Release-b e)))
                              (NF (Land (Release-b e)
                                        (X e))))]
        [else   '()]))
(define (NF-str x) (map ltl->string (NF x)))
(define (expand-G e)
  (cond [(Global? e) (Land (Global-e e) (X e))]
        [else (error "Unsupported operation.")]))
(define (expand-F e)
  (cond [(Future? e) (Lor (Future-e e) (X e))]
        [else (error "Unsupported operation.")]))
(define (expand-until e)
  (cond [(Until? e) (Lor (Until-b e) (* (Until-a e) (X e)))]
        [else (error "Unsupported operation.")]))
(define (expand-release e)
  (cond [(Release? e) (let ([a (Release-a e)]
                            [b (Release-b e)])
                        (+ (* a b)
                           (* b (X (R a b)))))]
        [else (error "Unsupported operation.")]))

(define (simplify e)
  (cond [(Atom? e) e]
        [(Neg? e) (- (- e))]
        [(Land? e)
         (cond [(X-true? (Land-e1 e)) (simplify (Land-e2 e))]
               [(X-true? (Land-e2 e)) (simplify (Land-e1 e))]
               [else (* (simplify (Land-e1 e))
                        (simplify (Land-e2 e)))])]
        [(Lor? e) (+ (simplify (Lor-e1 e))
                     (simplify (Lor-e2 e)))]
        [(Next? e) (X (simplify (Next-e e)))]
        [(Future? e) (F (simplify (Future-e e)))]
        [(Until? e) (U (simplify (Until-a e))
                       (simplify (Until-b e)))]
        [(Release? e) (R (simplify (Release-a e))
                         (simplify (Release-b e)))]
        [(Global? e) (G (simplify (Global-e e)))]))

(define (ltl->string e)
  (let* ([e (simplify e)])
    (string-append "("
                   (cond [(Atom? e) (symbol->string e)]
                         [(Neg? e) (string-append "!" (ltl->string (Neg-e e)))]
                         [(Land? e) (string-append (ltl->string (Land-e1 e)) "&" (ltl->string (Land-e2 e)))]
                         [(Lor? e) (string-append (ltl->string (Lor-e1 e)) "|"(ltl->string (Lor-e2 e)))]
                         [(Next? e) (string-append "X" (ltl->string (Next-e e)))]
                         [(Future? e) (string-append "F" (ltl->string (Future-e e)))]
                         [(Until? e) (string-append (ltl->string (Until-a e)) "U"
                                                    (ltl->string (Until-b e)))]
                         [(Release? e) (string-append (ltl->string (Release-a e)) "R"
                                                      (ltl->string (Release-b e)))]
                         [(Global? e) (string-append "G" (ltl->string (Global-e e)))])
                   ")")))
