(define ball% 
  (class object%
    (init-field pos);cons pair of pos
    (init-field image);image of ball
    (init-field vel);vel of ball in (cons r theta)
    (init-field (r (list 0 0 0)));only for cue-ball hitting coordinates of mouse
    (field (potted? #f));boolean for checkign potting characterstic
    (field (w 0));omega of ball
    (field (rad (/ (image-height image) 2)));radius
    (field (acc 0.06));acceleration
    (define t 1);value of a tick
    (super-new)
    (define/public (pos-set! posnew)
      (set! pos posnew))
    (define/public (vel-set! velnew)
      (set! vel velnew))
    ;change with time pos and vel
    (define/public (change-t)
      (begin (pos-set! (cons (+ (car pos) (*  (car vel)(cos (cdr vel)) t))
                             (+ (cdr pos) (*  (car vel)(sin (cdr vel)) t))))
             (vel-set! (if (> (car vel) 0.00066667)(cons  (- (car vel) (* acc t))
                                                         (cdr vel)) (cons 0 0)))
             (set! w (calcw))))
    ;show function which takes background as argument and place ball image at x and y on the background
    (define/public (show background)
      (place-image image (car pos) (cdr pos) background))
    ;get vel vector
    (define/public (get-vel-xy)
      (list (*(car vel)(cos (cdr vel)))
            (*(car vel)(sin (cdr vel)))
            0))
    ;setting vel from vector to r-theta form
    (define/public (set-vel-xy-to-rt v)
      (let* ([vm (sqrt (+(* (car v)(car v))
                         (* (cadr v)(cadr v))))]
             [theta1 (if (= 0 (car v)) (/ pi 2)
                         (atan (/ (cadr v) (car v))))]
             [theta (if(> (car v) 0) theta1
                       (+ pi theta1))])
        (set! vel (cons vm theta))))
    ;getting pos in vector form
    (define/public (get-list-pos)
      (list (car pos) (cdr pos) 0))
    ;calculating omega
    (define (calcw)
      (cross-prod (get-vel-xy) r))
    ;sending function to change potted?
    (define/public (potted)
      (set! potted? #t))
    (define/public (pot-set! val)
      (set! potted? val))
    (define/public (r-set! r1)
      (set! r r1))))
;draws all 10 ball by looping through the vector and at i = 10 returns background
(define (draw-all ball)
  (define (helper i)
    (if(= i 10) (send choti-ball show all)
       (send (vector-ref ball i) show (helper (+ i 1)))))
  (helper 0))