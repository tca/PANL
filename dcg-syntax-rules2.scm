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
  (syntax-rules (quote ==)
    ((_ (quote t) in out) (appendo (quote t) out in))
    ((_ (== x y) in out) (fresh () (== x y) (== out in)))
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


;s(s(NP,VP)) --> np(NP), vp(VP).
(--> (s x)
  (fresh (np- vp-)
     (conde ((== x `(s ,np- ,vp-)) (np np-) (vp vp-)))))


;np(np(Det,N,Rel)) --> det(Det), n(N), optrel(Rel).
;np(np(PN)) --> pn(PN).
(--> (np x)
 (fresh (det- n- rel- np- pn-)
   (conde
     ((== x `(np ,det- ,n- ,rel-))
      (det det-) (n n-) (optrel rel-))
     ((== x `(np ,pn-)) (pn pn-)))))

;vp(vp(TV,NP)) --> tv(TV), np(NP).
;vp(vp(IV)) --> iv(IV).
(--> (vp x) 
     (fresh (tv- np- iv-)
       (conde ((== x `(vp ,tv- ,np-)) (tv tv-) (np np-))
              ((== x `(vp ,iv-)) (iv iv-)))))

;optrel(rel(epsilon)) --> [].
;optrel(rel(that,VP)) --> [that], vp(VP).
(--> (optrel x)
     (fresh (vp-)
      (conde
       ((== x `(rel epsilon)) '())
       ((== x `(rel that ,vp-)) '(that) (vp vp-)))))

;pn(pn(terry)) --> [terry].
;pn(pn(shrdlu)) --> [shrdlu].
(--> (pn x)
     (conde
      ((== x `(pn terry)) '(terry))
      ((== x `(pn shrdlu)) '(shrdlu))))
 
 
;iv(iv(halts)) --> [halts].
(--> (iv x)
     (conde ((== x `(iv halts)) '(halts))))
 
;det(det(a)) --> [a].
(--> (det x)
     (conde ((== x `(det a)) '(a))))
 
;n(n(program)) --> [program].
(--> (n x)
     (conde ((== x `(n program)) '(program))))
 
;tv(tv(writes)) --> [writes].
(--> (tv x)
     (conde ((== x `(tv writes)) '(writes))))

(--> (test x)
     (conde ((pn x))))

(runi (lambda (m)
        (fresh (x q)
          (== m `(,x - ,q))
          (s q '(terry writes a program that halts) '()))))


(runi (lambda (m)
        (fresh (x q)
          (== m `(,x - ,q))
          (np q '(a program) '()))))
