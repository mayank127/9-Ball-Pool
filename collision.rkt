;takes to balls to check for collision
(define (collision b1 b2)
  (let* ([x1 (send b1 get-list-pos)]
         [x2 (send b2 get-list-pos)]
         [rad1 (get-field rad b1)]
         [rad2 (get-field rad b2)]
         [v1 (send b1 get-vel-xy)]
         [v2 (send b2 get-vel-xy)])
    ;checks distance of balls centers is optimal for collision
    (define (distance?)
      (let* ( [d (map - x1 x2)]
              [d^2 (magnitude^2 d)]
              [cd (+ rad1 rad2)])
        (<= d^2 (* cd cd))))
    ;checks if balls are aproaching
    (define (towards?)
      (let*([v12 (map - v1 v2)]
            [r12 (map - x1 x2)]
            [dot (dot-prod v12 r12)])
        (< dot 0)))
    ;called if conditions are optimum for collisions than changes velocity
    (define (after-collision)
      ;this function reduces error by tick i.e. if ball travels inside other ball in between tick it pushes it back to
      ;2r distance points and applies collision theory
      (define (make-dis-2r x1 x2 v1 v2)
        (define (helper i x1new x2new)
          (let* ([d (map - x1new x2new)]
                 [magd (magnitude^2 d)])
            (if (>= magd 1024)(list x1 x2 i)
                (helper (+ i 0.001) (map - x1 (mult-cons i v1)) (map - x2 (mult-cons i v2))))))
        (helper 0.001 x1 x2))
      (let* ([a (make-dis-2r x1 x2 v1 v2)]
             [x1n (car a)]
             [x2n (cadr a)]
             [r2 (map - x1n x2n)]
             [r1 (map - x2n x1n)]
             [v1a (along-component v1 r1)]
             [v2a (along-component v2 r2)]
             [v1p (map - v1 v1a)]
             [v2p (map - v2 v2a)]
             [v1new (map + v2a v1p)]
             [v2new (map + v1a v2p)])
        (begin  (send b1 set-vel-xy-to-rt v1new)
                (send b2 set-vel-xy-to-rt v2new)
                #f)))
    (if (and (distance?) (towards?)) (after-collision)
        "Nothing")))
;looping for all ball collisions
(define (check-all-ball-collision ball)
  (define (helper i j)
    (if(= i 10)"Done"
       (if(= j 10) (helper (+ i 1) 0)
          (if(= i j) (helper i (+ j 1))
             (begin (if(= i 0) (if (collision (vector-ref ball i)(vector-ref ball j))
                                   "Nothing"
                                   (if first-hit "Nothing"
                                       (set! first-hit j)))
                       (collision (vector-ref ball i)(vector-ref ball j)))
                    (helper i (+ j 1)))))))
  (helper 0 0))