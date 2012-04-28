;check if its possible to shoot ball in hole directionally retuning possible holes
(define (check-path1 ball1 ball2 point)
  (let* ([r1 (send ball1 get-list-pos)]
         [r2 (send ball2 get-list-pos)]
         [rp1 (map - point r1)]
         [r12 (map - r1 r2)])
    (< 0 (dot-prod rp1 r12))))
(define (check-direct-shot1 l)
  (let* ([i (lowest-ball)])
    (if(null? l) '()
       (filter (λ(x)(check-path1 (vector-ref ball i) (vector-ref ball 0) x)) l))))
;checking if path is clear from cue-ball to ball and ball to hole retuning the possible holes
(define (check-direct-shot2 l)
    (let*([i (lowest-ball)]
          [rb (send (vector-ref ball i) get-list-pos )]
          [rcue (send (vector-ref ball 0) get-list-pos )]
          [ball-to-cueb (no-obstacle-path rb rcue i)])
      (if(null? l) '() 
           (filter (λ(x) (and ball-to-cueb (no-obstacle-path rb x i))) l))))
;checking if e lies in rectanle abcd
(define (check-obstacle a b c d e)
  (= (+ (area1 a b c) (area1 a c d)) (+ (area1 a b e) (area1 b e c) (area1 a e d) (area1 c e d))))
(define (make-rec r1 r2)
  (let* ([r (map - r1 r2)]
         [r-per (list (cadr r) (-(car r)) 0)]
         [r-peru (mult-cons (/ 32 (sqrt (magnitude^2 r-per))) r-per)]
         [a (map + r1 r-peru)]
         [b (map - r1 r-peru)]
         [c (map - r2 r-peru)]
         [d (map + r2 r-peru)])
    (list a b c d)))
(define (no-obstacle-path v r i)
  (let*([l (make-rec v r)]
        [a (car l)]
        [b (cadr l)]
        [c (caddr l)]
        [d (cadddr l)])
    (define (helper i)
      (if(<= i 9) 
         (let([e (send (vector-ref ball i) get-list-pos)])          
           (if(check-obstacle a b c d e) #f (helper (+ i 1)) ))
         #t))
    (helper (+ i 1))))
;checking if possible velocity is available and returns list of shots with direction and min velocity
(define (check-direct-shot3 l)
    (let*([i (lowest-ball )]
          [rb (send (vector-ref ball i) get-list-pos )]
          [rcue (send (vector-ref ball 0) get-list-pos)]
          [r (sqrt (magnitude^2 (map - rcue rb)))])
      (define (helper l)
        (if(null? l) '()
           (let*([x (car l)]
                 [r1 (map - x rb)]
                 [magr1 (sqrt (magnitude^2 r1))]
                 [r2 (mult-cons (/ 32 magr1) r1)]
                 [r4 (map - rb r2)]
                 [r3 (map - r4 rcue)]
                 [magr3 (sqrt (magnitude^2 r3))]
                 [cosphi (abs (/ (dot-prod r1 r3) magr1 magr3))]
                 [minv (sqrt (* .12  magr1))])
               (if(> 15 (+ (sqrt (* .12 r)) (/ minv cosphi)))
                  (cons (cons r3 (+ (sqrt (* .12 r)) (/ minv cosphi))) (helper (cdr l)))
                  (helper (cdr l))))))
      (helper l)))
;this takes list of shot and aimed ball than it applies pseudo shot each shot and choses best among them and than 
;check for 5 differnet velocities which of them is best to play
(define (choose-shot2 shot)
  (define (helper max shot cnt fshot v)
    (if(< cnt 6)
       (let ([ps (pseudo-shot (car shot) (cdr shot))])
         (if(> ps max) (helper ps (cons (cons (caar shot) (+ (cdar shot) v))(cdr shot)) (+ cnt 1) (cons (cons (caar shot) (+ (cdar shot) v))(cdr shot)) v)
            (helper max (cons (cons (caar shot) (+ (cdar shot) v))(cdr shot)) (+ cnt 1) shot v)))
       (begin (print max)
              (car fshot))))
  (define (helper2 shot)
    (let* ([v (/ (- 15 (cdr (car shot))) 5)])
      (helper 0 shot 1 shot v)))
  (define (helper3 max shot fshot)
    (if(null? shot) (helper2 fshot)
       (let ([ps (pseudo-shot (caar shot) (cdar shot))])
         (if(> ps max) (helper3 ps (cdr shot) (car shot))
            (helper3 max (cdr shot) fshot)))))
  (helper3 0 shot (car shot)))


;checks 1,2,3 and than returns possible list of shot
(define (play-direct-shot)
  (let([shot (check-direct-shot3 
              (check-direct-shot2
               (check-direct-shot1 hole-start)))])
    (map (λ(x)(cons x (lowest-ball))) shot)))