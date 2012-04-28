;these makes list of pos ant pot of balls so that table can be resetted to initial after boolean
(define (make-pos-list)
  (define (helper i)
    (if (> i 9) '()
        (cons (get-field pos (vector-ref ball i)) (helper (+ i 1)))))
  (helper 0))
(define (make-pot-list)
  (define (helper i)
    (if (> i 9) '()
        (cons (get-field potted? (vector-ref ball i)) (helper (+ i 1)))))
  (helper 0))
;playing pseudo shot and than resetting it to initial
(define (pseudo-shot shot i)
  (let* ([pos-list (make-pos-list)]
         [pot-list (make-pot-list)]
         [a 0])
    (define (play-shot)
      (define (helper w)
        (begin
          (vector-map (λ(x)(send x change-t)) (caddr w))
          (check-all-ball-collision (caddr w))
          (check-table-collision (caddr w))
          (check-potted w)
          (set! all-stable (chk-motion (caddr w)))
          (if (and (< a 20) (get-field potted? (vector-ref ball i)))
              (if(= i 9) (set! a (+ a 40))
                 (set! a (+ a 20)))
              "Nothing")
          (if all-stable (begin (set! a (+ a (check-position-play)))
                                (restore-table pos-list pot-list))
              (helper (list "Pseudo" stick ball)))))
      (begin (send (vector-ref ball 0) set-vel-xy-to-rt (mult-cons (/ (cdr shot) (sqrt (magnitude^2 (car shot)))) (car shot)))
             (helper (list "Pseudo" stick ball))
             a))
    (play-shot)))
;restoring table
(define (restore-table pos-list pot-list)
  (define (helper i pos pot)
    (if (> i 9) "Nothing"
        (begin (send (vector-ref ball i) pos-set! (car pos))
               (send (vector-ref ball i) pot-set! (car pot))
               (set! first-hit #f)
               (set! ball-in-hand #f)
               (helper (+ i 1) (cdr pos) (cdr pot)))))
  (helper 0 pos-list pot-list))
;checking position plays which checks how many direct shots are possible after the shot so it can land the cue-ball 
;at a postion beneficial for it
(define (check-position-play)
  (if(> (lowest-ball) 9)0
     (length (check-direct-shot3 (check-direct-shot2 (check-direct-shot1 hole-start))))))