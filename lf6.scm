(use-modules (minikanren language)
             (minikanren dcg))

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

