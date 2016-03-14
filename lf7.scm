(use-modules (minikanren language)
             (minikanren dcg))

;; Program 4.5

;; q(VP) --> whpron, vp(VP, nogap).
;; q(XˆS) --> whpron, sinv(S, gap(np, X)).
;; q(yesˆS) --> sinv(S, nogap).
(--> (q vp-)
     (fresh (x s-)
       (conde
        ('whpron (vp vp- 'nogap))
        ('whpron (sinv s- `(gap np ,x)))
        ((sinv s 'nogap)))))


;; s(S) --> s(S, nogap).
;; s(S, Gap) --> np(VPˆS, nogap), vp(VP, Gap).
(--> (s s- gap)
     (fresh (vp-)
       (conde ((s s- 'nogap))
              ((np `(lambda (,vp-) ,s-) 'nogap)
               (vp vp- gap)))))

;; sinv(S, GapInfo) -->
;; aux, np(VPˆS, nogap), vp(VP, GapInfo).
(--> (sinv s- gapinfo-)
     (fresh (vp-)
       (conde ('aux
               (np `(lambda (,vp-) ,s-) 'nogap)
               (vp vp- gapinfo-)))))

;; np(NP, nogap) --> det(N2ˆNP), n(N1), optrel(N1ˆN2).
;; np((EˆS)ˆS, nogap) --> pn(E).
;; np((XˆS)ˆS, gap(np, X)) --> [].
(--> (np np- gap)
     (fresh (e x n1 n2 np- s- t)
       (conde ((== gap 'nogap)
               (det `(lambda (,n2) ,np-))
               (n n1 t)
               (optrel `(lambda (,n1) ,n2)))
              ((== gap 'nogap)
               (== np- `(lambda ((lambda (,e) ,s-)) ,s-))
               (pn e))
              ((== gap `(gap np ,x))
               (== np- `(lambda ((lambda (,x) ,s-)) ,s-))
               '()))))

;; vp(XˆS, Gap) --> tv(XˆVP), np(VPˆS, Gap).
;; vp(VP, nogap) --> iv(VP).
(--> (vp vp- gap)
     (fresh (x s-)
       (conde ((== vp- `(lambda (,x) ,s-))
               (tv `(lambda (,x) ,vp-))
               (np `(lambda (,vp-) ,s-) gap))
              ((== gap 'nogap)
               (iv vp-)))))


;; optrel(NˆN) --> [].
;; optrel((XˆS1)ˆ(Xˆ(S1&S2))) -->
;;   relpron, vp(XˆS2, nogap).
;; optrel((XˆS1)ˆ(Xˆ(S1&S2))) -->
;;   relpron, s(S2, gap(np, X)).
(--> (optrel optrel-)
     (fresh (n- x- s1 s2)
       (conde ((== optrel- `(lambda (,n-) ,n-))
               '())
              ((== optrel- `(lambda ((lambda (,x-) ,s1)) (lambda (,x-) (& ,s1 ,s2))))
               (relpron)
               (vp `(lambda (,x-) ,s2) 'nogap))
              ((== optrel- `(lambda ((lambda (,x-) ,s1)) (lambda (,x-) (& ,s1 ,s2))))
               (relpron)
               (s s2 `(gap np x-))))))

;; det(LF) --> [D], {det(D, LF)}.
;; det( every, (XˆS1)ˆ(XˆS2)ˆall(X,(S1=>S2)) ).
;; det( a, (XˆS1)ˆ(XˆS2)ˆexists(X,S1&S2) ).
(--> (det t)
     (fresh (det-)
       (conde (`(,det-) (escape (deto det- t))))))


(define (deto s t)
  (conde ((== s 'a) (== t 'consonant))
         ((== s 'an) (== t 'vowel))
         ((== s 'every))
         ((== s 'some))
         ((== s 'the))))

;; n(LF) --> [N], {n(N, LF)}.
;; n( program, Xˆprogram(X) ).
;; n( student, Xˆstudent(X) ).
(--> (n lf- t)
     (fresh (n-)
       (conde (`(,n-) (escape (no n- lf- t))))))

(define (no n- lf- t)
  (fresh (x)
    (conde ((== n- 'program) (== t 'consonant)
            (== lf- `(lambda (,x) (program ,x))))
           ((== n- 'student) (== t 'consonant)
            (== lf- `(lambda (,x) (student ,x)))))))

;; pn(E) --> [PN], {pn(PN, E)}.
;; pn( terry, terry ).
;; pn( shrdlu, shrdlu ).
(--> (pn e) (fresh (pn-) (conde (`(,pn-) (escape (pno pn- e))))))

(define (pno s e)
  (conde ((== s 'shrdlu) (== e 'shrdlu))
         ((== s 'terry) (== e 'terry))))


;; tv(LF) --> [TV], {tv(TV, LF)}.
;; tv( wrote, XˆYˆwrote(X,Y) ).
(--> (tv lf-)
     (fresh (tv-)
       (conde (`(,tv-) (escape (tvo tv- lf-))))))

(define (tvo s lf-)
  (fresh (x y)
    (conde ((== s 'wrote)
            (== lf- `(lambda (,x) (lambda (,y) (wrote ,x ,y))))))))

;; iv(LF) --> [IV], {iv(IV, LF)}.
;; iv( halts, Xˆhalts(X) ).
(--> (iv lf-)
     (fresh (iv-)
       (conde (`(,iv-) (escape (ivo iv- lf-))))))

(define (ivo s lf-)
  (fresh (x)
    (== s 'halted)
    (== lf- `(lambda (,x) (halts ,x)))))

;; relpron --> [RelPron], {relpron(Relpron)}.
;; relpron(that). relpron(who).
;; relpron(whom).
(--> (relpron)
     (fresh (x)
       (conde (`(,x) (escape (relprono x))))))
 
(define (relprono x)
 (conde ((== x 'that))
        ((== x 'who))
        ((== x 'whom))))

(runi (lambda (q) (fresh (a) (s a '(gap np) q '()))))
