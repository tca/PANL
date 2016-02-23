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

;; s --> s(nogap).
;; s(Gap) --> np(nogap), vp(Gap).
(--> (s gap) (conde ((np 'nogap) (vp gap))))


;; np(nogap) --> det, n, optrel.
;; np(nogap) --> pn.
;; np(gap(np)) --> [].
(--> (np gap)
 (fresh (t)
     (conde ((== gap 'nogap) (det t) (n t) (optrel))
            ((== gap 'nogap) (pn))
            ((== gap '(gap np)) '()))))

;; vp(Gap) --> tv, np(Gap).
;; vp(nogap) --> iv.
(--> (vp gap)
  (conde ((tv) (np gap))
         ((iv))))

;;optrel --> [].
;;optrel --> relpron, vp(nogap).
;;optrel --> relpron, s(gap(np)).
(--> (optrel)
  (conde ('())
         ((relpron) (vp 'nogap))
         ((relpron) (s '(gap np)))))

;; det --> [Det], {det(Det)}.
(--> (det t)
     (fresh (det-)
       (conde (`(,det-) (escape (deto det- t))))))

;; det(a). det(every).
;; det(some). det(the).

(define (deto s t)
  (conde ((== s 'a) (== t 'consonant))
         ((== s 'an) (== t 'vowel))
         ((== s 'every))
         ((== s 'some))
         ((== s 'the))))

;; n --> [N], {n(N)}.
(--> (n t) (fresh (n-) (conde (`(,n-) (escape (no n- t))))))

;; n(author). n(book).
;; n(professor). n(program).
;; n(programmer). n(student).
(define (no s t)
  (conde ((== s 'author) (== t 'vowel))
         ((== s 'book) (== t 'consonant))
         ((== s 'professor) (== t 'consonant))
         ((== s 'program) (== t 'consonant))
         ((== s 'programmer) (== t 'consonant))
         ((== s 'student) (== t 'consonant))))


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

;relpron --> [RelPron], {relpron(Relpron)}.
(--> (relpron)
(fresh (x)
 (conde (`(,x) (escape (relprono x))))))
 
(define (relprono x)
 (conde ((== x 'that))
        ((== x 'who))
        ((== x 'whom))))

(runi (lambda (q) (s '(gap np) q '())))
(runi (lambda (q) (s 'nogap q '())))

