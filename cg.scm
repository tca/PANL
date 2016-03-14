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

(--> (lex ty)
  (fresh (in)
    (conde (`(,in) (escape (lexo in ty))))))

(--> (parse ty)
  (fresh (a b)
    (conde ((lex ty))
           ((escape (combine a b ty))
            (parse a)
            (parse b)))))

(runi (lambda (q) (parse 's q '())))
