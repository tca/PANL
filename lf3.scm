(use-modules (minikanren language)
             (minikanren dcg))

(define (reduceo ae a e) (== ae `(lambda (,a) ,e)))
;; Program 4.2

;; s(S) --> np(VPˆS), vp(VP).
(--> (s s-)
     (fresh (np- vp-)
       (conde ((np `(lambda (,vp-) ,s-)) (vp vp-)))))

;; np(NP) --> det(N2ˆNP), n(N1), optrel(N1ˆN2).
;; np((EˆS)ˆS) --> pn(E).
(--> (np np-)
     (fresh (n1- n2- e- s-)
       (conde ((det `(lambda (,n2-) ,np-))
               (n n1-)
               (optrel `(lambda (,n1-) ,n2-)))
              ((== np- `(lambda ((lambda (,e-) ,s-)) ,s-))
               (pn e-)))))


;; vp(XˆS) --> tv(XˆIV), np(IVˆS).
;; vp(IV) --> iv(IV).
(--> (vp lf-)
     (fresh (x- s- iv-)
       (conde ((== lf- `(lambda (,x-) ,s-))
               (tv `(lambda (,x-) ,iv-))
               (np `(lambda (,iv-) ,s-)))
              ((iv lf-)))))

;; optrel((XˆS1)ˆ(Xˆ(S1 & S2))) --> [that], vp(XˆS2).
;; optrel(NˆN) --> [].
(--> (optrel lf-)
     (fresh (x x^s1 s1 s2)
       (conde ((== x^s1 `(lambda (,x) ,s1))
               (== lf- `(lambda (,x^s1) (lambda (,x) (& ,s1 ,s2))))
               '(that)
               (vp `(lambda (,x) ,s2)))
              ('()
               (== lf- `(lambda (,x) ,x))))))


;; det(LF) --> [D], {det(D, LF)}.
;; det( every, (XˆS1)ˆ(XˆS2)ˆall(X,(S1=>S2)) ).
;; det( a, (XˆS1)ˆ(XˆS2)ˆexists(X,S1&S2) ).
(--> (det lf-)
     (fresh (d-)
       (conde (`(,d-) (escape (deto d- lf-))))))

(define (deto d lf)
  (fresh (dq x q p x^p x^q)
    (conde ((== d 'every)
            (== dq `(all ,x (=> ,p ,q))))
           ((== d 'a)
            (== dq `(exists ,x (& ,p ,q)))))
    (== x^p `(lambda (,x) ,p))
    (== x^q `(lambda (,x) ,q))
    (== lf `(lambda (,x^p) (lambda (,x^q) ,dq)))))


;; n(LF) --> [N], {n(N, LF)}.
;; n( program, Xˆprogram(X) ).
;; n( student, Xˆstudent(X) ).
(--> (n lf-) (fresh (n-) (conde (`(,n-) (escape (no n- lf-))))))

(define (no n lf)
  (fresh (x)
    (conde ((== n 'program))
           ((== n 'student)))
    (== lf `(lambda (,x) (,n ,x)))))

;; pn(E) --> [PN], {pn(PN, E)}.
;; pn( terry, terry ).
;; pn( shrdlu, shrdlu ).
(--> (pn e-)
     (fresh (pn-)
       (conde (`(,pn-) (escape (pno pn- e-))))))

(define (pno pn- e-) 
 (conde ((== pn- 'terry) (== e- 'terry))
         ((== pn- 'shrdlu) (== e- 'shrdlu))))

;; tv(LF) --> [TV], {tv(TV, LF)}.
;; tv( wrote, XˆYˆwrote(X,Y) ).
(--> (tv lf-)
     (fresh (tv- x y)
       (conde (`(,tv-)
               (== tv- 'wrote)
               (== lf- `(lambda (,x)
                          (lambda (,y)
                            (wrote ,x ,y))))))))

;; iv(LF) --> [IV], {iv(IV, LF)}.
;; iv( halts, Xˆhalts(X) ).
(--> (iv lf-)
     (fresh (x-)
       (conde ('(halts) (== lf- `(lambda (,x-) (halts ,x-)))))))


(runi (lambda (lf) (fresh (x) (s lf `(terry wrote shrdlu) '()))))

(runi (lambda (lf) (s lf `(every program halts) '())))

(runi (lambda (lf) (s lf `(every student wrote a program) '())))
