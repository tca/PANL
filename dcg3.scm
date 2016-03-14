(use-modules (minikanren language))

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

;; s --> np, vp.
(--> (s) (conde ((np) (vp))))

;; np --> det, n, optrel.
;; np --> pn.
(--> (np) (conde ((det) (n) (optrel))
                 ((pn))))

;; vp --> tv, np.
;; vp --> iv.
(--> (vp) (conde ((tv) (np))
                 ((iv))))

;; optrel --> [].
;; optrel --> [that], vp.
(--> (optrel) (conde ('())
                     ('(that) (vp))))

;; det --> [Det], {det(Det)}.
(--> (det)
     (fresh (det-)
       (conde (`(,det-) (escape (deto det-))))))

;; det(a). det(every).
;; det(some). det(the).
(define (deto s)
  (conde ((== s 'a)) ((== s 'every))
         ((== s 'some)) ((== s 'the))))

;; n --> [N], {n(N)}.
(--> (n) (fresh (n-) (conde (`(,n-) (escape (no n-))))))

;; n(author). n(book).
;; n(professor). n(program).
;; n(programmer). n(student).
(define (no s)
  (conde ((== s 'author)) ((== s 'book))
         ((== s 'professor)) ((== s 'program))
         ((== s 'programmer)) ((== s 'student))))


;; pn --> [PN], {pn(PN)}.
(--> (pn) (fresh (pn-) (conde (`(,pn-) (escape (pno pn-))))))

;; pn(begriffsschrift). pn(bertrand).
;; pn(bill). pn(gottlob).
;; pn(lunar). pn(principia).
;; pn(shrdlu). pn(terry).
(define (pno s)
  (conde ((== s 'begriffsschrift)) ((== s 'bertrand))
         ((== s 'bill)) ((== s 'gottlob))
         ((== s 'lunar)) ((== s 'principia))
         ((== s 'shrdlu)) ((== s 'terry))))


;; tv --> [TV], {tv(TV)}.
(--> (tv)
     (fresh (tv-)
       (conde (`(,tv-) (escape (tvo tv-))))))

;; tv(concerns). tv(met).
;; tv(ran). tv(wrote).
(define (tvo s)
  (conde ((== s 'concerns)) ((== s 'met))
         ((== s 'ran)) ((== s 'wrote))))

;; iv --> [IV], {iv(IV)}.
(--> (iv)
     (fresh (iv-)
       (conde (`(,iv-) (escape (ivo iv-))))))

(define (ivo s) (fresh () (== s 'halted)))

(runi (lambda (q) (s q '())))
