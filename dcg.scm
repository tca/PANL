(define (membero x l)
  (fresh (head tail)
    (== l `(,head . ,tail))
    (conde
     ((== x head))
     ((membero x tail)))))

(define (connects w w-r r)
  (== w-r (cons w r)))

(define (parse-body e p0 p defs)
  (fresh (body1 body2 p1)
    (== e `(,body1 . ,body2))
    (conde
     ((== body2 '())
      (parse body1 p0 p defs))
     ((=/= body2 '())
      (parse body1 p0 p1 defs)
      (parse-body body2 p1 p defs)))))

(define (parse e p0 p defs)
  (conde
   ((fresh (body)
      (membero `(--> ,e . ,body) defs)
      (parse-body body p0 p defs)))
   ((== e `()) (== p0 p))
   ((fresh (word rest p1)
      (== e `(,word . ,rest))
      (connects word p0 p1)
      (parse rest p1 p defs)))))

(define my-lang
  `((--> noun '(cat))
    (--> noun '(bat))
    (--> verb '(eats))
    (--> det '(the))
    (--> det '(a))

    (--> s det noun verb det noun)))

;; (parse 's x '() my-lang)

