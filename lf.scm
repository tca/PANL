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


;;Program 4.1
;; reduce(ArgˆExpr, Arg, Expr).
(define (reduceo ae a e) (== ae `(lambda (,a) ,e)))

;; ?- reduce(Xˆhalts(X), shrdlu, LF).
;; (runi (lambda (lf) (fresh (x) (reduceo `(lambda (,x) (halts ,x)) 'shrdlu lf))))


;;s(S) --> np(NP), vp(VP), {reduce(VP,NP,S)}. 
(--> (s s-)
     (fresh (np- vp-)
       (conde ;; ((np np-) (vp vp-) (escape (reduceo vp- np- s-)))
              ;; partially executed
              ;; np(NP), vp(VP^S). 
              ((np np-) (vp `(lambda (,np-) ,s-))))))


;; vp(VP) --> tv(TV), np(NP), {reduce(TV, NP, VP)}.
;; vp(VP) --> iv(VP).
(--> (vp vp-)
     (fresh (tv- np-)
       (conde ;;((tv tv-) (np np-) (escape (reduceo tv- np- vp-)))
              ;; partially executed
              ;; tv(NPˆVP), np(NP).
              ((tv `(lambda (,np-) ,vp-)) (np np-))
              ((iv vp-)))))

;; tv(XˆYˆwrote(Y,X)) --> [wrote].
(--> (tv exp)
     (fresh (x y)
       (conde ((== exp `(lambda (,x) (lambda (,y) (wrote ,y ,x))))
               '(wrote)))))

;; iv(Xˆhalts(X)) --> [halts]. 
(--> (iv exp)
     (fresh (x)
       (conde ((== exp `(lambda (,x) (halts ,x)))
               '(halts)))))

;; np(shrdlu) --> [shrdlu].
;; np(terry) --> [terry]. 
(--> (np x)
     (conde ((== x 'shrdlu)
             '(shrdlu))
            ((== x 'terry)
             '(terry))))


;; ?- s(LF, [shrdlu, halts], []).
;; LF = halts(shrdlu) yes 
(runi (lambda (lf) (s lf '(shrdlu halts) '())))

;; ?- s(LF, [terry, wrote, shrdlu], []).
;; LF = wrote(terry, shrdlu) yes
(runi (lambda (lf) (s lf '(terry wrote shrdlu) '())))
