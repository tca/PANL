(import (scheme base)
        (scheme write)
        
        (miruKanren mk-diseq))

(define appendo
  (lambda (l s out)
    (conde
      ((== '() l) (== s out))
      ((fresh (a d res)
         (== `(,a . ,d) l)
         (== `(,a . ,res) out)
         (appendo d s res))))))

(define-syntax term
  (syntax-rules (quote)
    ((_ in out (quote t)) (appendo (quote t) out in))
    ((_ in out (t ...)) (t ... in out))))

(define-syntax conj
  (syntax-rules ()
    ((_ in out (f)) (term in out f))
    ((_ in out (f r ...))
     (fresh (mid)
       (term in mid f)
       (conj mid out (r ...))))))

(define-syntax -->
  (syntax-rules ()
    ((_ (name args ...) (g ...) ...)
     (define (name in out args ...)
       (conde ((conj in out (g ...))) ...)))))

(--> (s) ((np) (vp)))
(--> (np) ((det) (n) (optrel))
          ((np) (pn)))
(--> (vp) ((tv) (np))
     ((iv)))
(--> (optrel) ('())
     ('(that) (vp)))
(--> (pn) ('(terry))
     ('(shrdlu)))
(--> (iv) ('(halts)))
(--> (det) ('(a)))
(--> (n) ('(program)))
(--> (tv) ('(writes)))

(runi (lambda (q)
        (fresh (in out)
          (== q (list in out))
          (s in out))))
