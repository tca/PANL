(use-modules (minikanren language)
             (minikanren dcg))

(define (reduceo ae a e) (== ae `(lambda (,a) ,e)))
;; Program 4.3

;; s(S) --> np(VPˆS), vp(VP).
(--> (s t-)
     (fresh (np- vp- s-)
       (conde ((np `(lambda (,vp-) ,s-)) (vp vp-)
               (escape (pullo/2 s- t-))
               ))))

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
;; det( every, (XˆS1)ˆ(XˆS2)ˆq(PˆQˆall(X,P=>Q),S1,S2) ).
;; det( a, (XˆS1)ˆ(XˆS2)ˆq(PˆQˆexists(X,P&Q),S1,S2) ).
(--> (det lf-)
     (fresh (d-)
       (conde (`(,d-) (escape (deto d- lf-))))))

(define (deto d lf)
  (fresh (dq x s1 s2 x^s1 x^s2 p q)
    (== x^s1 `(lambda (,x) ,s1))
    (== x^s2 `(lambda (,x) ,s2))
    (== lf `(lambda (,x^s1) (lambda (,x^s2) ,dq)))
    (conde ((== d 'every)
            (== dq `(q (lambda (,p) (lambda (,q) (all ,x (=> ,p ,q)))) ,s1 ,s2)))
           ((== d 'a)
            (== dq `(q (lambda (,p) (lambda (,q) (exists ,x (& ,p ,q)))) ,s1 ,s2))))))

;; n(LF) --> [N], {n(N, LF)}.
;; n( book, Xˆ(‘book(X)) ). 
;; n( professor, Xˆ(‘professor(X)) ). 
;; n( program, Xˆ(‘program(X)) ).
;; n( student, Xˆ(‘student(X)) ).
(--> (n lf-) (fresh (n-) (conde (`(,n-) (escape (no n- lf-))))))

(define (no n lf)
  (fresh (x)
    (conde ((== n 'program))
           ((== n 'student))
           ((== n 'book))
           ((== n 'professor)))
    (== lf `(lambda (,x) (unscoped (,n ,x))))))

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
;; tv( ran, XˆYˆ(‘ran(X,Y)) ).
;; tv( wrote, XˆYˆ(‘wrote(X,Y)) ).
(--> (tv lf-)
     (fresh (tv-)
       (conde (`(,tv-) (escape (tvo tv- lf-))))))

(define (tvo tv- lf-)
  (fresh (x y lf1-)
    (== lf- `(lambda (,x) (lambda (,y) (unscoped ,lf1-))))
    (conde
     ((== tv- 'wrote) (== lf1- `(wrote ,x ,y)))
     ((== tv- 'ran) (== lf1- `(ran ,x ,y))))))

;; iv(LF) --> [IV], {iv(IV, LF)}.
;; iv( halts, Xˆhalts(X) ).
(--> (iv lf-)
     (fresh (x-)
       (conde ('(halts) (== lf- `(lambda (,x-) (unscoped (halts ,x-))))))))

;; pull(‘Predication, Predication, []).
;; pull(QuantTree1 & QuantTree2, Formula1 & Formula2, Store) :-
;;   pull(QuantTree1, Matrix1, Store1),
;;   pull(QuantTree2, Matrix2, Store2),
;;   conc(Pass1, Apply1, Store1),
;;   conc(Pass2, Apply2, Store2),
;;   apply_quants(Apply1, Matrix1, Formula1),
;;   apply_quants(Apply2, Matrix2, Formula2),
;;   shuffle(Pass1, Pass2, Store).

;; pull(q(Quantifier, RangeTree, ScopeTree), Matrix, Store) :-
;;   pull(RangeTree, RangeMatrix, RangeStore),
;;   pull(ScopeTree, Matrix, ScopeStore),
;;   conc(RangePass, RangeApply, RangeStore),
;;   apply_quants(RangeApply, RangeMatrix, Range),
;;   reduce(Quantifier, Range, StoreElement),
;;   conc(RangePass, [StoreElement], Pass),

(define (pullo qt matrix store)
  (conde
   ((== qt `(unscoped ,matrix)) (== store '()))
   ((fresh (qt1 qt2 f1 f2 store1 store2
            pass1 pass2 apply1 apply2 matrix1 matrix2)
       (== `(& ,qt1 ,qt2) qt)
       (== `(& ,f1 ,f2) matrix)
       (pullo qt1 matrix1 store1)
       (pullo qt2 matrix2 store2)
       (appendo pass1 apply1 store1)
       (appendo pass2 apply2 store2)
       (apply-quantso apply1 matrix1 f1)
       (apply-quantso apply2 matrix2 f2)
       (shuffleo pass1 pass2 store)))
   ((fresh (quantifier range-tree scope-tree range range-store range-matrix scope-store
            range-pass range-apply store-element pass)
      (== qt `(q ,quantifier ,range-tree ,scope-tree))
      (pullo range-tree range-matrix range-store)
      (pullo scope-tree matrix scope-store)
      (appendo range-pass range-apply range-store)
      (apply-quantso range-apply range-matrix range)
      (reduceo quantifier range store-element)
      (appendo range-pass `(,store-element) pass)
      (shuffleo pass scope-store store)))))

;; apply_quants([], Formula, Formula).
;; apply_quants([StoreElement|Elements], Matrix, Formula) :-
;;   apply_quants(Elements, Matrix, SubFormula),
;;   reduce(StoreElement, SubFormula, Formula).
(define (apply-quantso store matrix formula)
  (conde
   ((== store '()) (== matrix formula))
   ((fresh (store-element elements subformula)
      (== store `(,store-element . ,elements))
      (apply-quantso elements matrix subformula)
      (reduceo store-element subformula formula)))))

(define (pullo/2 quant-tree formula)
  (fresh (matrix store)
    (pullo quant-tree matrix store)
    (apply-quantso store matrix formula)))

;; - s(LF, [every,professor,that,wrote,a,book,ran,a,program], []).
(runi (lambda (lf) (fresh (x) (s lf `(every professor that wrote a book ran a program) '()))))

;; without pull
;; ---
;; (q (lambda (_.0) (lambda (_.1) (all _.2 (=> _.0 _.1))))
;;    (& (unscoped (professor _.2))
;;       (q (lambda (_.3) (lambda (_.4) (exists _.5 (& _.3 _.4))))
;;          (unscoped (book _.5))
;;          (unscoped (wrote _.2 _.5))))
;;    (q (lambda (_.6) (lambda (_.7) (exists _.8 (& _.6 _.7))))
;;       (unscoped (program _.8))
;;       (unscoped (ran _.2 _.8))))
;; ---
;;
;; ((exists _.0 (& (program _.0)
;;                 (all _.1 (=> (& (professor _.1)
;;                                 (exists _.2 (& (book _.2)
;;                                                (wrote _.1 _.2))))
;;                              (ran _.1 _.0))))) where)
;; (another? y/n)
;; y
;; ((all _.0 (=> (& (professor _.0)
;;                  (exists _.1 (& (book _.1) (wrote _.0 _.1))))
;;               (exists _.2 (& (program _.2) (ran _.0 _.2))))) where)
;; (another? y/n)
;; y
;; ((exists _.0 (& (program _.0)
;;                 (all _.1 (=> (exists _.2 (& (book _.2) (& (professor _.1) (wrote _.1 _.2))))
;;                              (ran _.1 _.0))))) where)
;; (another? y/n)
;; y
;; ((exists _.0 (& (program _.0)
;;                 (exists _.1 (& (book _.1) (all _.2 (=> (& (professor _.2) (wrote _.2 _.1)) (ran _.2 _.0))))))) where)
;; (another? y/n)
;; y
;; ((all _.0 (=> (exists _.1 (& (book _.1) (& (professor _.0) (wrote _.0 _.1))))
;;               (exists _.2 (& (program _.2) (ran _.0 _.2))))) where)
;; (another? y/n)
;; y
;; ((exists _.0 (& (book _.0)
;;                 (exists _.1 (& (program _.1)
;;                                (all _.2 (=> (& (professor _.2) (wrote _.2 _.0))
;;                                             (ran _.2 _.1))))))) where)
;; (another? y/n)
;; y
;; ((exists _.0 (& (book _.0)
;;                 (all _.1 (=> (& (professor _.1) (wrote _.1 _.0))
;;                              (exists _.2 (& (program _.2) (ran _.1 _.2))))))) where)
;; (another? y/n)
;; y
;; thats-all!
