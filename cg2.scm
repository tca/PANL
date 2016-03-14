(use-modules (minikanren language)
             (minikanren dcg))

;; Problem 4.10
(define (lexo in cat)
  (conde ((== 'bertrand in) (== 'np cat))
         ((== 'terry in) (== 'np cat))
         ((== 'principia in) (== 'np cat))
         ((== 'halts in) (== '(\ s np) cat))
         ((== 'wrote in) (== '(/ (\ s np) np) cat))
         ((== 'met in) (== '(/ (\ s np) np) cat))))

(define (combine a b r)
  (conde ((== a `(/ ,r ,b)))
         ((== b `(\ ,r ,a)))))

(--> (lex in ty)
  (conde (`(,in) (escape (lexo in ty)))))

;; with parse tree
(--> (parse ty tree)
  (fresh (tok a b l r)
    (conde ((== tree `(,tok : ,ty))
            (lex tok ty))
           ((escape (combine a b ty))
            (== tree `(,l ,r : ,ty))
            (parse a l)
            (parse b r)))))

(runi (lambda (q)
        (fresh (q1 q2)
          (== q (list q1 q2))
          (parse 's q1 q2 '()))))
