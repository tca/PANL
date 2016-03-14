(use-modules (minikanren language)
             (minikanren dcg))

(define (shuffleo l r s)
  (conde ((== r '()) (== l s))
         ((fresh (l1 lrest e rrest srest)
            (== r `(,e . ,rrest))
            (appendo l1 lrest l)
            (shuffleo lrest rrest srest)
            (appendo l1 `(,e . ,srest) s)))))

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
