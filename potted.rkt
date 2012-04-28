(define hole-rad 18)
;checking potting of ball
(define (potted? b l)
  (define (potted p)
    (let*([x (map - (send b get-list-pos)  p)]
          [mag^2 (magnitude^2 x)]
          [rad^2 (* hole-rad hole-rad)])
      (if(< mag^2 rad^2)(begin (send b pos-set! (cons 190 41))
                               (send b vel-set! (cons 9 0))
                               #t)
         (potted? b (cdr l)))))
  (if(null? l) #f
     (potted (car l))))
;looping through all balls and adding number of ball which are potted in event-list to increase score mechanism
(define (check-potted w)
  (define (helper i)
    (if(> i 9) "Nothing"
       (begin(if(potted? (vector-ref (caddr w) i) hole-pts)
                (begin (if(equal? "Pseudo" (car w)) "nothing"
                          (send (car w) add-event i))
                       (send (vector-ref (caddr w) i) potted))
                "Nothing")
             (helper (+ i 1)))))
  (helper 0))