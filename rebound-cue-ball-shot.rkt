(define (check-path2 r-per ball point) ;rper per vec to wall
  (let*([rb (send ball get-list-pos)]
        [r (map - point rb)])
    (< 0 (dot-prod r-per r))))

;checking if shot is possible directionally retuning list of possible wall list
(define (check-rebound-shot1 l wall) ;l list of holes
  (let*([i (lowest-ball)])
    (if(null? l) '()
       (filter (λ(x) (check-path2 (car wall) (vector-ref ball i) x)) l))))

;checking if path is clear from cue-ball to r-mid,r-mid to ball and ball to hole retuning possible wall-list
(define (check-rebound-shot2 l wall);rmid point frm which reflection takes place
  (let*([i (lowest-ball)]
        [rb (send (vector-ref ball i) get-list-pos)]
        [rcue (send (vector-ref ball 0) get-list-pos)]
        [r-mid (if (= 0 (cadr wall)) 
                   (let*([y (caddr wall)]
                         [cue-wall  (- y (cadr rcue))]
                         [rb-wall  (- y (cadr rb))]
                         [sum (+ cue-wall rb-wall)])
                     (list (/ (+ (* rb-wall (car rcue)) (* cue-wall (car rb))) sum) y 0))
                   
                   (let*([x (cadr wall)]
                         [cue-wall  (- x (car rcue))]
                         [rb-wall  (- x (car rb))]
                         [sum (+ cue-wall rb-wall)])
                     (list x (/ (+ (* rb-wall (cadr rcue)) (* cue-wall (cadr rb))) sum) 0)))]
        [ball-to-centre (no-obstacle-path rb  r-mid i)]
        [cueb-to-centre (no-obstacle-path rcue r-mid i)])
    (filter  (λ(x) (and ball-to-centre cueb-to-centre (no-obstacle-path  rb x i))) l)))

;checking possiblity of velocity and than returning list of all possible shots
(define (check-rebound-shot3 l wall)
  (let*([i (lowest-ball)]
        [rb (send (vector-ref ball i) get-list-pos)]
        [rcue (send (vector-ref ball 0) get-list-pos)])
    (define (helper l )
      (if(null? l) '()
         (let*([x (car l)]
               [r1 (map - x rb)]
               [magr1 (sqrt (magnitude^2 r1))]
               [r2 (mult-cons (/ 32 magr1) r1)]
               [r4 (map - rb r2)]
               [rmid (if (= 0 (cadr wall)) 
                         (let*([y (caddr wall)]
                               [cue-wall  (- y (cadr rcue))]
                               [r4-wall  (- y (cadr r4))]
                               [sum (+ cue-wall r4-wall)])
                           (list (/ (+ (* r4-wall (car rcue)) (* cue-wall (car r4))) sum) y 0))
                         
                         (let*([x (cadr wall)]
                               [cue-wall  (- x (car rcue))]
                               [r4-wall  (- x (car r4))]
                               [sum (+ cue-wall r4-wall)])
                           (list x (/ (+ (* r4-wall (cadr rcue)) (* cue-wall (cadr r4))) sum) 0)))]
               [rm-c (sqrt (magnitude^2 (map - rcue rmid)))]
               [rm-b (sqrt (magnitude^2 (map - rb rmid)))]
               [r (+ rm-c rm-b)]
               [r3 (map - rmid rcue)]
               [r5 (map - rb rmid)]
               [magr5 (sqrt (magnitude^2 r5))]
               [cosphi (abs (/ (dot-prod r1 r5) magr1 magr5))]
               [minv (sqrt (* .12  magr1))])
             (if  (> 15 (+ (sqrt (* .12 r)) (/ minv cosphi)))
                 (cons (cons r3 (+ (sqrt (* .12 r)) (/ minv cosphi))) (helper (cdr l)))
                 (helper (cdr l))))))
    (helper l)))
;retuns possible shot's list
(define (play-rebound-shot)
  (let([shot 
        (foldr append '() (map (λ(wall)
                                 (check-rebound-shot3 
                                  (check-rebound-shot2
                                   (check-rebound-shot1 hole-start wall) wall) wall)) wall-list))])
    (map (λ(x) (cons x (lowest-ball)))shot)))
;this takes list of all shots from all the 4 functions and than plays pseudo shot on then ant than executes the shot
(define (play-comp shot)
      (if(null? shot) (random-shot)
         (let([shot2 (choose-shot2 shot)])
                  (send (vector-ref ball 0) set-vel-xy-to-rt (mult-cons (/ (cdr shot2) (sqrt (magnitude^2 (car shot2)))) (car shot2))))))
;if no shot possible plays randomly towards the center of other ball
(define(random-shot)
  (let* ([i (lowest-ball)]
        [rb (send (vector-ref ball i) get-list-pos)]
        [rcue (send (vector-ref ball 0) get-list-pos)]
        [shot (cons (map - rb rcue) 15)])
           (send (vector-ref ball 0) set-vel-xy-to-rt (mult-cons (/ (cdr shot) (sqrt (magnitude^2 (car shot)))) (car shot)))))