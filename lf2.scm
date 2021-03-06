(use-modules (minikanren language)
             (minikanren dcg))

;;Program 4.1
;; reduce(ArgˆExpr, Arg, Expr).
(define (reduceo ae a e) (== ae `(lambda (,a) ,e)))


;; s(S) --> np(VPˆS), vp(VP).
(--> (s s-)
     (fresh (np- vp-)
       (conde ((np `(lambda (,vp-) ,s-)) (vp vp-)))))


(--> (n n-)
     (fresh (x-)
       (conde ('(program)
               ;; Xˆprogram(X)
               (== lf- `(lambda (,x-) (program ,x-)))))))

;; λp.λq.(∀x)p(x) ⇒ q(x)
;; `(lambda (,p) `(lambda (,q) (all ,x (=> (,p ,x) (,q ,x)))))
;; partially executed
;; det( (X^P)^(X^Q)^all(X,(P => Q)) ) --> [every].
(--> (det a)
     (fresh (x p q x^p x^q)
       (conde ((== x^p `(lambda (,x) ,p))
               (== x^q `(lambda (,x) ,q))
               (== a `(lambda (,x^p)
                        (lambda (,x^q)
                          (all ,x (=> ,p ,q)))))
               '(every)))))


;; vp(VP) --> tv(TV), np(NP), {reduce(TV, NP, VP)}.
;; vp(VP) --> iv(VP).
(--> (vp vp-)
     (fresh (tv- np-)
       (conde ;;((tv tv-) (np np-) (escape (reduceo tv- np- vp-)))
              ;; partially executed
              ;; tv(NPˆVP), np(NP).
              ((tv `(lambda (,np-) ,vp-)) (np np-))
              ((iv vp-)))))


;; iv(LF) --> [IV], {iv(IV, LF)}.
;; iv( halts, Xˆhalts(X) ).
(--> (iv lf-)
     (fresh (x-)
       (conde ('(halts) (== lf- `(lambda (,x-) (halts ,x-)))))))

(--> (tv lf-)
     (fresh (x-)
       (conde ('(wrote) (== lf- `(lambda (,x-) (wrote ,x-)))))))


;; np(NP) --> det(NˆNP), n(N).
(--> (np np-)
     (fresh (n-)
       (conde ((det `(lambda (,n-) ,np-)) (n n-)))))

(runi (lambda (lf) (s lf '(every program halts) '())))
