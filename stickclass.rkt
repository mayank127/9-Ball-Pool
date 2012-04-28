;class with properties of stick
(define stick% 
  (class object%
    (super-new)
    (init-field image)
    (define d-min 136);min distance of stick-center from cue-ball center
    (define d 136);initial d
    (define c (send (vector-ref ball 0) get-list-pos));to get pos of cue-ball
    (define pos-def (list 450 520 0));intial pos of cue
    (define pos pos-def)
    (define theta 0)
    (define start pos-def);start point of click
    (define end pos-def);end point of drag
    ;click mechanism for stick
    (define/public(click r type)
      (cond [(equal? type "move")  (cursor r)]
            [(equal? type "button-down") (hit-start r)]
            [(equal? type "drag") (hit-drag r)]
            [(equal? type "button-up") (shoot r)]))
    ;to rotate stick in direction of mouse pointer
    (define (cursor r)
      (begin (set! c (send (vector-ref ball 0) get-list-pos))
             (let* ([st (map - c r)]
                    [mag (sqrt (magnitude^2 st))])
               (if (= mag 0) "Nothing"
                   (let*([st* (mult-cons (/ d mag) st)]
                         [stick (map + c st*)]
                         [the (if (> (cadr st)0)(acos (/ (-(car st)) mag))
                                  (- (* 2 pi)(acos (/ (-(car st)) mag))))])
                     (begin (set! pos stick)
                            (set! theta  (floor ( * the 57.2957795)))))))))
    ;show functions using background and placing image on back
    (define/public (show back)
      (place-image (rotate theta image) (car pos) (cadr pos) back))
    ;when clicked start is set
    (define (hit-start r)
      (set! start r))
    ;dragging increasing distance of stick from ball
    (define (hit-drag r)
      (begin (set! end r)
             (set! d (if(>= d 286) 286
                        (+ d-min (sqrt (magnitude^2 (map - end start))))))
             (cursor start)))
    ;playing shot when mouse click is button-up
    (define (shoot r)
      (begin (set! end r)
             (let* ([dis (map - start c)]
                    [mag  (magnitude^2 dis)]
                    [pow (/ (/ (- d d-min) 10) (sqrt mag))])
               (begin (set! d d-min)
                      (send (vector-ref ball 0) set-vel-xy-to-rt (mult-cons pow dis))
                      (set! start pos-def)
                      (set! end pos-def)
                      (set! pos pos-def)
                      (set! theta 0)))))
    (define/public (set-pos x t)
      (begin (set! pos x)
             (set! theta 0)))))