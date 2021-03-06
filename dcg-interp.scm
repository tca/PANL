
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
      (parse-inner body1 p0 p defs))
     ((=/= body2 '())
      (parse-inner body1 p0 p1 defs)
      (parse-body body2 p1 p defs)))))

(define (parse-list e p0 p defs)
  (conde
   ((== e `()) (== p0 p))
   ((fresh (word rest p1)
      (== e `(,word . ,rest))
      (connects word p0 p1)
      (parse-list rest p1 p defs)))))

(define (parse-inner e p0 p defs)
  (conde
   ((fresh (body)
      (membero `(--> ,e . ,body) defs)
      (parse-body body p0 p defs)))
   ((fresh (e1)
      (== e `(quote ,e1))
      (parse-list e1 p0 p defs)))))

(define (parse e p0 p defs)
  (conde
   ((fresh (es)
      (== e `(begin . ,es))
      (parse-body es p0 p defs)))
   ((parse-inner e p0 p defs))))

(define my-lang
  `((--> (noun) '(cat))
    (--> (noun) '(bat))
    (--> (verb) '(eats))
    (--> (det) '(the))
    (--> (det) '(a))

    (--> (s) (det) (noun) (verb) (det) (noun))
    (--> (s!) (s) '(!))))

;; (parse '(s!) x '() my-lang)

(define prog3-10
  '((--> (s) (np) (vp))
    (--> (np) (det) (n) (optrel))
    (--> (np) (pn))
    (--> (vp) (tv) (np))
    (--> (vp) (iv))
    (--> (optrel) '())
    (--> (optrel) '(that) (vp))
    (--> (pn) '(terry))
    (--> (pn) '(shrdlu))
    (--> (iv) '(halts))
    (--> (det) '(a))
    (--> (n) '(program))
    (--> (tv) '(writes))))

;; (parse '(s) x '() prog3-10)
