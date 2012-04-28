;table-collision takes ball and cue? boolean
(define (table-collision b cue?)
  (let*([p3 (send b get-list-pos)]
        [v (send b get-vel-xy)]
        [rad (get-field rad b)]
        [magv (sqrt (magnitude^2 v))])
    (define (helper p1 p2)
      ;checking distance factor
      (define (line-collision?)
        (let*([l1 (magnitude^2 (map - p3 p1))]
              [l2 (magnitude^2 (map - p3 p2))]
              [l3 (magnitude^2 (map - p2 p1))]
              [sl3 (sqrt l3)]
              [x (/ (+ l1 l3 (- l2)) (* 2 sl3))])
          (and (> x 0) (< x sl3) (< (- l1 (* x x)) (* rad rad)))))
      ;checking if ball is moving towards wall
      (define (towards-line?)
        [let* ([p31 (map - p3 p1)]
               [p21 (map - p2 p1)]
               [dot (along-component p31 p21)]
               [per (map - p31 dot)]
               [x (dot-prod v per)])
                 (< x 0)])
      ;for balls except cue-ball changinf of velocity
      (define (after-line-collision)
        (let* ([r (map - p1 p2)]
               [2dot (mult-cons 2 (along-component  v r))]
               [mag (sqrt(magnitude^2 (map - 2dot v)))]
               [vnew (mult-cons (/ magv mag) (map - 2dot v))])
          (send b set-vel-xy-to-rt vnew)))
      ;for cue ball inclusion of rotation feature
      (define(deviated-collision)
        (let* ([w (get-field w b)]
               [r (map - p1 p2)]
               [v-par (along-component v r)]
               [v-per (map - v v-par)])
          (let*([cross (cross-prod v-per w)]
                [magvper (sqrt (magnitude^2 v-per))]
                [magcross  (magnitude^2 cross)]
                [cross-xy (list (car cross) (cadr cross) 0)]
                [cross-z (map - cross cross-xy)]
                [v-par-new (map + v-par (mult-cons (* 0.4 magvper (sqrt (magnitude^2 cross-xy)) (/ 1 magcross)) cross-xy))]
                [v-per-new (if(>= (caddr cross-z) 0) (mult-cons -1 v-per)
                              (mult-cons (* -0.92 (magnitude^2 cross-z) (/ 1 magcross)) v-per))]
                [vfinal (map + v-per-new v-par-new)])
              (send b set-vel-xy-to-rt vfinal))))
      
      (if(and(line-collision?)(towards-line?)) (if cue? (deviated-collision) 
                                                   (after-line-collision))
         "Nothing"))
    ;looping through all the holes
    (define(helper2 l)
      (define (helper3 l1)
        (if (null? (cdr l1)) (helper2 (cdr l))
            (begin (helper (car l1) (cadr l1))
                   (helper3 (cdr l1)))))
      (if(null? l) "Nothing"
         (helper3 (car l))))
    (if (< magv 0.00001)"Nothing"
        (helper2 table-pts))))
;looping through all balls
(define (check-table-collision ball)
  (define(helper i)
    (if(= i 10) #f
       (if (= i 0) (begin(if (= 0(magnitude^2 (get-field w (vector-ref ball i))))
                             (table-collision (vector-ref ball i) #f)
                             (table-collision (vector-ref ball i) #t))
                         (helper (+ i 1)))
           (begin (table-collision (vector-ref ball i) #f)
                  (helper (+ i 1))))))
  (helper 0))

;(define (make-pts l)
;  (if(null? l)(place-image table 450 300(empty-scene 900 600))
 ;    (place-image (circle 2 "solid" "red") (caar l) (cadar l) (make-pts (cdr l)))))