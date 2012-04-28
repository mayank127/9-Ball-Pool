;checking possible shots from cue to i-th to j-th to hole directionally
(define (check-indirect-shot1 l i j)
  (if(null? l) '()
     (filter (λ(x) (check-path1 (vector-ref ball j) (vector-ref ball i) x)) l)))
;;checks if path of shot is clear
(define (check-indirect-shot2 l i j)
  (let*( [rb (send (vector-ref ball i) get-list-pos )]
         [rb2 (send (vector-ref ball j) get-list-pos )]
         [ball-to-ball (no-obstacle-path2 rb rb2 i j)])
    (if(null? l) '() 
       (filter (λ(x) (and ball-to-ball (no-obstacle-path2 rb2 x i j))) l))))
(define (no-obstacle-path2 v r i j)
  (let*([l (make-rec v r)]
        [a (car l)]
        [b (cadr l)]
        [c (caddr l)]
        [d (cadddr l)])
    (define (helper k)
      (if(and (<= k 9) (not(= k j)) (not(= k i))) 
         (let([e (send (vector-ref ball i) get-list-pos)])          
           (if(check-obstacle a b c d e) #f (helper (+ k 1))))
         #t))
    (helper (+ i 1))))
;checks if velocity is possible than returns list of shots for ball to be played
(define (check-indirect-shot3 l i j )
  (let*([rb (send (vector-ref ball i) get-list-pos )]
        [rb2 (send (vector-ref ball j) get-list-pos)]
        [r (sqrt (magnitude^2 (map - rb2 rb)))])
    (define (helper l)
      (if(null? l) '()
         (let*([x (car l)]
               [r1 (map - x rb2)]
               [magr1 (sqrt (magnitude^2 r1))]
               [r2 (mult-cons (/ 32 magr1) r1)]
               [r4 (map - rb2 r2)]
               [r3 (map - r4 rb)]
               [magr3 (sqrt (magnitude^2 r3))]
               [cosphi (abs (/ (dot-prod r1 r3) magr1 magr3))]
               [minv (sqrt (* .12  magr1))])
           (if (> 15 (+ (sqrt (* .12 r)) (/ minv cosphi)))
               (cons (cons r4 (cons r3 (+ (sqrt (* .12 r)) (/ minv cosphi)))) (helper (cdr l)))
               (helper (cdr l))))))
    (helper l)))
;returning cue-ball shots to be played and than list of shots is returned
(define (check-indirect-shot4 l i j v)
  (let*([rb (send (vector-ref ball i) get-list-pos )]
        [rb2 (send (vector-ref ball j) get-list-pos)]
        [r (sqrt (magnitude^2 (map - rb2 rb)))])
    (define (helper l v)
      (if(null? l) '()
         (let*([minv1 (car v)]
               [x (car l)]
               [r1 (map - x rb2)]
               [magr1 (sqrt (magnitude^2 r1))]
               [r2 (mult-cons (/ 32 magr1) r1)]
               [r4 (map - rb2 r2)]
               [r3 (map - r4 rb)]
               [magr3 (sqrt (magnitude^2 r3))]
               [cosphi (abs (/ (dot-prod r1 r3) magr1 magr3))]
               [minv (sqrt (* .12  magr1))])
           (if (> 20 (+ (sqrt (* .12 r)) (/ (+ minv minv1) cosphi)))
           (cons (cons  r3 (+ (sqrt (* .12 r)) (/ (+ minv minv1) cosphi))) (helper (cdr l) (cdr v)))
         (helper (cdr l) (cdr v))))))
    (helper l v)))

;checking all shots than playing for all possible balls
(define (play-indirect-shot)
  (let ([i (lowest-ball)])
    (define (helper j)
      (if(< j 10)
         (let*([shot1 (check-indirect-shot3 (check-indirect-shot2 (check-indirect-shot1 hole-start i j) i j) i j)]
               [shot2 (map (λ(x) (cons x j)) (check-indirect-shot4 
                                              (check-indirect-shot2 
                                               (check-indirect-shot1 
                                                (foldr (λ(x y) (cons (car x) y)) '() shot1)  0 i) 0 i) 0 i (foldr (λ(x y) (cons (cddr x) y)) '() shot1)))])
             (append shot2 (helper (+ j 1))))
         '()))
    (helper (+ i 1))))