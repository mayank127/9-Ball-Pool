;player object including name and how a player plays in human player it just make play to #t allowing mouse mechanism and 
;playing of shot
(define player% 
  (class object%
    (super-new)
    (init-field name)
    (field (score 0))
    (define/public (increase-score val)
      (set! score (+ score val)))
    (define/public (play-game)
      (begin (send (vector-ref ball 0) r-set! (list 0 0 0))
             (send choti-ball pos-set! (cons xb yb))
             (set! play #t)))))
;comp-player with name -CPU and play mechanisme includes plaing of ball at center if ball in hand otherwise playing the shot 
;according to play-comp which takes list of shots calculated from different types of functions
(define comp-player%
  (class object%
    (super-new)
    (field (name "CPU"))
    (field (score 0))
    (define/public (increase-score val)
      (set! score (+ score val)))
    (define/public (play-game)
      (begin (set! play #f)
             (send (vector-ref ball 0) r-set! (list 0 0 0))
             (send choti-ball pos-set! (cons xb yb))
             (if ball-in-hand (begin (send (vector-ref ball 0) pos-set! (cons 257 281))
                                     (send (vector-ref ball 0) pot-set! #f)
                                     (set! ball-in-hand #f)
                                     (draw-all ball)
                                     (play-game))
                 (play-comp (append (play-direct-shot) 
                                    (play-indirect-shot) 
                                    (play-rebound-shot) 
                                    (play-rebound-shot)
                                    )))
             (set! play #t)))))
;game class which take two players and a window to show scores and manages swapping on foul shot, increment of score and printing
; of different results if game
(define game%
  (class object%
    (super-new)
    (init-field p1)
    (init-field p2)
    (init-field window)
    (field (current p1))
    (define event-list '())
    (define lb 1)
    (define/public (scored l)
      (if (null? l) (begin (show-score)
                           (set!  event-list '()))
          (begin (send current increase-score (* (car l) 10))
                 (scored (cdr l)))))
    (define (swap)
      (begin (if (equal? current p1)(set! current p2)
                 (set! current p1))
             (play-game)
             (newline)))
    (define/public (add-event e)
      (set! event-list (cons e event-list)))
    ;processing event list which includes all balls which were potted in the shot
    (define/public (process-events)
      (if(equal? lb 10)
         (begin(show-score)
               (result))
         
         (if (equal? first-hit lb)
             (cond [(and (search 0 event-list) (not (search 9 event-list))) (foul1)]
                   [(and (search 0 event-list) (search 9 event-list)) (foul2)]
                   [(search 9 event-list)(begin (scored (append '(2) event-list))
                                                (result))]
                   [(null? event-list) (begin (scored '(2)) (foul3))]
                   [else (begin (scored (append '(2) event-list))
                                (play-game))])
             
             (cond [(search 9 event-list) (foul2)]
                   [(search 0 event-list) (foul1)]
                   [else (foul3)]))))
    ;foul when no ball is potted
    (define (foul3)
      (begin (set!  event-list '())
             (swap)))
    ;when ball 9 is potted on illegal shot current player loses
    (define (foul2)
      (begin (set!  event-list '())
             (lose)))
    ;if white ball is potted chance is swapped
    (define (foul1)
      (begin (set! ball-in-hand #t)
             (set!  event-list '())
             (swap)))
    ;showing result
    (define (result)
      (begin (set! play #f)
             (show-score)
             (if (> (get-field score p1) (get-field score p2)) (begin (set! current p1) (win))
                 (begin (set! current p2) (win)))))
    (define (win) 
      (begin((draw-pixmap wscore) "images/pool5.jpg" (make-posn 0 0))
            (show-names (string->list (get-field name p1)) 
                        (string->list (get-field name p2))
                        (string->list (get-field name current))
                        (string->list "Wins...!!"))
            (show-scores (get-field score p1) (get-field score p2))
            (copy-viewport wscore window)
            (set! end-world #t)))
    (define (lose)
      (begin ((draw-pixmap wscore) "images/pool5.jpg" (make-posn 0 0))
             (show-names (string->list (get-field name p1)) 
                         (string->list (get-field name p2))
                         (string->list (get-field name current)) 
                         (string->list "Looses...!!"))
             (show-scores (get-field score p1) (get-field score p2))
             (copy-viewport wscore window)
             (set! end-world #t)))
    
    (define (show-score)
      (begin((draw-pixmap wscore) "images/pool5.jpg" (make-posn 0 0))
            (show-names (string->list (get-field name p1)) 
                        (string->list (get-field name p2))
                        (string->list "current")
                        (string->list (get-field name current)))
            (show-scores (get-field score p1) (get-field score p2))
            (copy-viewport wscore window)))
    ;starting of game lowest ball is set before the shot is played and current player plays the game
    (define (play-game)
      (begin (show-score)
             (set! lb (lowest-ball))
             (send current play-game)))
    (play-game)))