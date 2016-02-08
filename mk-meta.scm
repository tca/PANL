(define (membero x l)
 (fresh (head tail)
  (== l `(,head . ,tail))
  (conde
   ((== x head))
   ((membero x tail)))))

(define (mko t)
 (conde
  ((== t `(true)))
  ((fresh (x) (== t `(== ,x ,x))))
  ((fresh (clause clauses)
    (== t `(conde . ,clauses))
    (membero clause clauses)
    (mkos clause)))
  ((fresh (pred args clause)
    (== t `(,pred . ,args))
    (=/= pred 'conde)
    (=/= pred 'true)
    (=/= pred '==)
    (clauseo pred args clause)
    (mko clause)))))

(define (mkos ts)
  (conde
   ((== ts '()))
   ((fresh (u us)
     (== ts `(,u . ,us))
     (mko u)
     (mkos us)))))

(define (clauseo p args clause)
 (conde
  ((== p 'membero)
   (fresh (x head tail)
    (== args `(,x (,head . ,tail)))
    (== clause `(conde ((== ,x ,head))
                       ((membero ,x ,tail))))))))



(mko '(true))

(mko '(false))

(mko '(== a b))

(mko '(== 1 1))

(mko '(conde ((== 1 2))
             ((== 1 1) (== 2 3))))

(mko '(membero x (a x b x y)))

(mko `(membero ,x (a x b x y)))
