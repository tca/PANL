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

(define (shuffleo l r s)
  (conde ((== r '()) (== l s))
         ((fresh (l1 lrest e rrest srest)
            (== r `(,e . ,rrest))
            (appendo l1 lrest l)
            (shuffleo lrest rrest srest)
            (appendo l1 `(,e . ,srest) s)))))

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


(define (reduceo ae a e) (== ae `(lambda (,a) ,e)))


(--> (iv form-)
     (fresh (iv-)
       (conde (`(,iv-) (escape (ivo iv- form-))))))

(define (ivo iv- form-)
  (conde ((== iv- 'halts) (== form- 'finite))
         ((== iv- 'halt) (== form- 'nonfinite))
         ((== iv- 'halting) (== form- 'present_participle))
         ((== iv- 'halted) (== form- 'past_participle))))

(--> (aux form-)
     (fresh (aux-)
       (conde (`(,aux-) (escape (auxo aux- form-))))))

(define (auxo aux- form-)
  (conde ((== aux- 'could) (== form- `(/ finite nonfinite)))
         ((== aux- 'have) (== form- `(/ nonfinite past_participle)))
         ((== aux- 'has) (== form- `(/ finite past_participle)))
         ((== aux- 'been) (== form- `(/ past_participle present_participle)))
         ((== aux- 'be) (== form- `(/ nonfinite present_participle)))))

(--> (vp form-)
     (fresh (require-)
       (conde ((iv form-))
              ((tv form-) (np))
              ((aux `(/ ,form- ,require-)) (vp require-)))))

(--> (s) (conde ((np) (vp 'finite))))

(--> (np)
     (conde ('(bretrand))
            ('(bob))))

(--> (tv form-)
     (fresh (tv-)
       (conde (`(,tv-) (escape (tvo tv- form-))))))

(define (tvo tv- form-)
  (conde ((== tv- 'killed) (== form- 'past_participle))))


;sinv --> aux(finite/Required), np, vp(Required).
(--> (sinv)
 (fresh (required)
  (conde ((aux `(/ finite ,required)) (np) (vp required)))))

(--> (q)
  (conde ((sinv) '(?))))


(runi (lambda (x) (s x '())))

(runi (lambda (x) (q x '())))
