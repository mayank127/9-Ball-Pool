;vector representation is list of x,y,z of a vector
;calculating dot-product
(define (dot-prod vec1 vec2)
  (foldr + 0 (map * vec1 vec2)))
;magnitude^2 of of a vector
(define(magnitude^2 vec)
  (foldr (λ(x acc) (+ (* x x) acc)) 0 vec))
;multeplying constant to vector
(define (mult-cons c vec)
  (map (λ(x) (* c x)) vec))
;vector1 component along vector2
(define (along-component vec1 vec2)
  (mult-cons(/ (dot-prod vec1 vec2)(magnitude^2 vec2))vec2))
;perp component of vec1 on vec2
(define (perp-component vec1 vec2)
  (map - vec1 (along-component vec1 vec2)))
;cross product
(define (cross-prod vec1 vec2)
  (let*([x1 (car vec1)]
        [y1 (cadr vec1)]
        [z1 (caddr vec1)]
        [x2 (car vec2)]
        [y2 (cadr vec2)]
        [z2 (caddr vec2)]
        [i (- (* x1 z2) (* x2 z1))]
        [j (- (* y2 z1) (* y1 z2))]
        [k (- (* x1 y2) (* x2 y1))])
    (list i j k)))
;area of a triangle with three points a b c
(define (area1 a b c)
  (abs (- (+ (* (car a) (cadr b)) (* (car b) (cadr c)) (* (car c) (cadr a)))
          (+ (* (cadr a) (car b)) (* (cadr b) (car c)) (* (cadr c) (car a))))))

