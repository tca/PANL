(define appendo
  (lambda (l s out)
    (conde
      ((== '() l) (== s out))
      ((fresh (a d res)
         (== `(,a . ,d) l)
         (== `(,a . ,res) out)
         (appendo d s res))))))

;(--> (s) ((np) (vp))
(define (s in out)
 (fresh (mid1)
 (conde
  ((np in mid1)
   (vp mid1 out)))))

;(--> (np) ((det) (n) (optrel))
;          ((np) (pn)))
(define (np in out)
 (fresh (mid1 mid2)
 (conde
  ((det in mid1)
   (n mid1 mid2)
   (optrel mid2 out))
  ((np in mid1)
   (pn mid1 out)))))

;(--> (vp) ((tv) (np))
;          ((iv)))
(define (vp in out)
 (fresh (mid1)
 (conde
  ((tv in mid1)
   (np mid1 out))
  ((iv in out)))))

;(--> (optrel) ('())
;              ('(that) (vp)))
(define (optrel in out)
 (fresh (mid1)
 (conde
  ((== in out))
  ((appendo '(that) mid1 in)
   (vp mid1 out)))))

;(--> (pn) ('(terry))
;          ('(shrdlu)))
(define (pn in out)
 (conde
  ((appendo '(terry) out in))
  ((appendo '(shrdlu) out in))))

;(--> (iv) ('(halts)))
(define (iv in out)
 (conde
  ((appendo '(halts) out in))))

;(--> (det) ('(a)))
(define (det in out)
 (conde
  ((appendo '(a) out in))))

;(--> (n) ('(program)))
(define (n in out)
 (conde
   ((appendo '(program) out in))))

;(--> (tv) ('(writes)))))
(define (tv in out)
  (conde
    ((appendo '(writes) out in))))

