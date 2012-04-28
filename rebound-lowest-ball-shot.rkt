;checking if shot is directionally possible than returns possible ball-wall-hole list
(define (check-rebound-ball1 wall)
  (let*([i (lowest-ball)]
        [rb (send (vector-ref ball i) get-list-pos)]
        [rcue (send (vector-ref ball 0) get-list-pos)]
        [dot (dot-prod (map - rb rcue) (car wall))])
    (< dot 0)))
;finds where ball will hot the wall
(define (find-rmid wall  hole)
  (let*([i (lowest-ball)]
        [rb (send (vector-ref ball i) get-list-pos)])
    (if(= 0 (caadr wall)) 
       (let*([y (cadadr wall)]
             [rb-wall (- y (cadr rb))]
             [hole-wall (- y (cadr hole))]
             [sum (+ hole-wall rb-wall)])
         (list (/ (+ (* rb-wall (car hole)) (* hole-wall (car rb))) sum) y 0))
       (let*([x (caadr wall)]
             [rb-wall (- x (car rb))]
             [hole-wall (- x (car hole))]
             [sum (+ hole-wall rb-wall)])
         (list x (/ (+ (* rb-wall (cadr hole)) (* hole-wall (cadr rb))) sum) 0)))))
;checkinf if path from cue to ball, ball to r-mid and r--mid to hole is clear
(define (check-rebound-ball2 wall)
  (let*([i (lowest-ball)]
        [rb (send (vector-ref ball i) get-list-pos)]
        [rcue (send (vector-ref ball 0) get-list-pos)]
        [l (caddr wall)]
        [rmid-list (map (λ(x) (find-rmid wall x)) l)]
        [rcue-to-rb (no-obstacle-path rcue rb i)]
        [l-new (map cons l rmid-list)])
    (filter (λ(x) (and 
                   rcue-to-rb
                   (no-obstacle-path (car x) (cdr x) i)
                   (no-obstacle-path (cdr x) rb i))) l-new)))
;checking velocity possibility and than retuning possible shots
(define (check-rebound-ball3 hole-rmid-list)
  (let*([l (foldr (λ(x y) (cons (cdr x) y)) '() hole-rmid-list)]
        [hole-list (foldr (λ(x y) (cons (car x) y)) '() hole-rmid-list)]
        [dist-list (map (λ(x y) (sqrt (magnitude^2 (map - x y)))) l hole-list)]
        [i (lowest-ball )]
        [rb (send (vector-ref ball i) get-list-pos )]
        [rcue (send (vector-ref ball 0) get-list-pos)]
        [r (sqrt (magnitude^2 (map - rcue rb)))])
    (define (helper l dist-list)
      (if(null? l) '()
         (let*([dist-hole-rmid (car dist-list)]
               [x (car l)]
               [r1 (map - x rb)]
               [magr1 (sqrt (magnitude^2 r1))]
               [r2 (mult-cons (/ 32 magr1) r1)]
               [r4 (map - rb r2)]
               [r3 (map - r4 rcue)]
               [magr3 (sqrt (magnitude^2 r3))]
               [cosphi (abs (/ (dot-prod r1 r3) magr1 magr3))]
               [minv (sqrt (* .12  (+ dist-hole-rmid magr1)))])
           (if (> 15 (+ (sqrt (* .12 r)) (/ minv cosphi)))
               (cons (cons r3 (+ (sqrt (* .12 r)) (/ minv cosphi))) (helper (cdr l) (cdr dist-list)))
               (helper (cdr l) (cdr dist-list))))))
    (helper l dist-list)))

;playing all three checks and than retunring list of shots
(define (play-rebound-ball-shot)
  (let*([l (filter check-rebound-ball1 wall-hole-list)]
        [shot (foldr append '() (map (λ(x) (check-rebound-ball3 (check-rebound-ball2 x))) l))])
    (map (λ(x) (cons x (lowest-ball)))shot)))