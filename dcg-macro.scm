(define appendo
  (lambda (l s out)
    (conde
      ((== '() l) (== s out))
      ((fresh (a d res)
         (== `(,a . ,d) l)
         (== `(,a . ,res) out)
         (appendo d s res))))))

(define-syntax term
  (syntax-rules (quote == escape quasiquote)
    ((_ (quote t) in out) (appendo (quote t) out in))
    ((_ (quasiquote t) in out) (appendo (quasiquote t) out in))
    ((_ (== x y) in out) (fresh () (== x y) (== out in)))
    ((_ (escape g ...) in out) (fresh () (== out in) g ...))
    ((_ (t ...) in out) (t ... in out))))

(define-syntax conj
  (syntax-rules ()
    ((_ (f) in out) (term f in out))
    ((_ (f r ...) in out)
     (fresh (mid)
       (term f in mid)
       (conj (r ...) mid out)))))

(define-syntax -->
  (syntax-rules (fresh conde)
  
    ;; fresh is optional
    ((_ (name args ...)
        (conde (g ...) ...))
     (--> (name args ...)
          (fresh ()
            (conde (g ...) ...))))
    
    ((_ (name args ...)
        (fresh (vars ...)
          (conde (g ...) ...)))
     (define (name args ... in out)
       (fresh (vars ...)
         (conde ((conj (g ...) in out)) ...))))))
