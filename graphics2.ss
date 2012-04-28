;defining rules-window
(define r-w (open-pixmap "PooL ThE BaLL" 900 600))
((draw-pixmap r-w) "images/scoring.jpg"  (make-posn 0 0) "black")

;defining credits-window
(define c-w (open-pixmap "PooL ThE BaLL" 900 600))
((draw-pixmap c-w) "images/credits.jpg"(make-posn 0 0) "black")

;defining controls-window
(define co-w (open-pixmap "PooL ThE BaLL" 900 600))
((draw-pixmap co-w) "images/controls.png"(make-posn 0 0) "black")

;defining single-player window
(define s-w (open-pixmap "PooL ThE BaLL" 900 600))
((draw-pixmap s-w )  "images/p1.jpg" (make-posn 0 0) "white")

;defining multi-player window
(define m-w (open-pixmap "PooL ThE BaLL" 900 600))
((draw-pixmap m-w )  "images/mp1.png" (make-posn 0 0) "white")

;Main Window 
(define w1 (open-viewport "PooL ThE BaLL" 900 600))
((draw-pixmap w1) "images/9-Ball.jpg" (make-posn 0 0) "white")

(define p1-name "P1")
(define p2-name "P2")
;function to for main screen of start
(define (start)  
  (let([clicku (mouse-click-posn (get-mouse-click w1))])
    (cond
      [(and(<= 560 (posn-x clicku))(>= 760 (posn-x clicku));Single-Player
           (<= 275 (posn-y clicku))(>= 310 (posn-y clicku)))
       (begin         
         (copy-viewport s-w w1)
         (set! p1-name (get-name1))
         (single-player))]
      [(and(<= 580 (posn-x clicku))(>= 750 (posn-x clicku));Multiplayer
           (<= 325 (posn-y clicku))(>= 355 (posn-y clicku)))
       (begin
         (copy-viewport m-w w1)
         (set! p1-name (get-name1))
         (mult1))]
      [(and(<= 600 (posn-x clicku))(>= 730 (posn-x clicku));Controls
           (<= 370 (posn-y clicku))(>= 405 (posn-y clicku)))
       (begin         
         (copy-viewport co-w w1)
         (controls))]
      [(and(<= 625 (posn-x clicku))(>= 705 (posn-x clicku));Rules
           (<= 420 (posn-y clicku))(>= 455 (posn-y clicku)))
       (begin         
         (copy-viewport r-w w1)
         (rules))]
      [(and(<= 610 (posn-x clicku))(>= 720 (posn-x clicku));Credits
           (<= 470 (posn-y clicku))(>= 505 (posn-y clicku)))
       (begin
         (copy-viewport c-w w1)
         (credits))]
      [else (start)])))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (rules)
  (let([clicku (mouse-click-posn (get-mouse-click w1))])
    (cond
      [(and(<= 725 (posn-x clicku))(>= 876 (posn-x clicku))
           (<= 554 (posn-y clicku))(>= 576 (posn-y clicku)))
       (begin 
         ((draw-pixmap w1) "images/9-Ball.jpg" (make-posn 0 0) "white")
         (start))]
      [else (rules)])))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (controls)
  (let([clicku (mouse-click-posn (get-mouse-click w1))])
    (cond
      [(and(<= 738 (posn-x clicku))(>= 884 (posn-x clicku))
           (<= 538 (posn-y clicku))(>= 580 (posn-y clicku)))
       (begin 
         ((draw-pixmap w1) "images/9-Ball.jpg" (make-posn 0 0) "white")
         (start))]
      [else (controls)])))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (credits)
  (let([clicku (mouse-click-posn (get-mouse-click w1))])
    (cond
      [(and(<= 625 (posn-x clicku))(>= 860 (posn-x clicku))
           (<= 520 (posn-y clicku))(>= 570 (posn-y clicku)))
       (begin 
         ((draw-pixmap w1) "images/9-Ball.jpg" (make-posn 0 0) "white")
         (start))
       ]
      [else (credits)])))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (single-player)
  (let([clicku (mouse-click-posn (get-mouse-click w1))])
    (cond
      [(and(<= 625 (posn-x clicku))(>= 860 (posn-x clicku));back
           (<= 520 (posn-y clicku))(>= 570 (posn-y clicku)))
       (begin 
         ((draw-pixmap w1) "images/9-Ball.jpg" (make-posn 0 0) "white")
         (start))]
       [(and(<= 360 (posn-x clicku))(>= 490 (posn-x clicku));play
           (<= 465 (posn-y clicku))(>= 577 (posn-y clicku)))
        (begin 
          (close-viewport w1)
          (new-game-singles p1-name))]      
      [else (single-player)])))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (mult1)
  (let([clicku (mouse-click-posn (get-mouse-click w1))])
    (cond
      [(and(<= 625 (posn-x clicku))(>= 860 (posn-x clicku));back
           (<= 520 (posn-y clicku))(>= 570 (posn-y clicku)))
       (begin 
         ((draw-pixmap w1) "images/9-Ball.jpg" (make-posn 0 0) "white")
         (start))]
      [(and(<= 342 (posn-x clicku))(>= 478 (posn-x clicku));next
           (<= 479 (posn-y clicku))(>= 573 (posn-y clicku)))
       (begin 
         ((draw-pixmap w1) "images/p2.png" (make-posn 0 0) "white")
         (mult2))]
      [else(mult1)])))
(define (mult2)
  (begin 
    (set! p2-name (get-name1))
  (let([clicku (mouse-click-posn (get-mouse-click w1))])
    (cond
      [(and(<= 625 (posn-x clicku))(>= 860 (posn-x clicku));back
           (<= 520 (posn-y clicku))(>= 570 (posn-y clicku)))
       (begin 
         ((draw-pixmap w1) "images/mp1.png" (make-posn 0 0) "white")
         (mult1))]
       [(and(<= 360 (posn-x clicku))(>= 490 (posn-x clicku));play
           (<= 465 (posn-y clicku))(>= 577 (posn-y clicku)))
        (begin 
          (close-viewport w1)
          (new-game-doubles p1-name p2-name))]
      [else (mult2)]))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Display Name
;function to get key-value
(define (press-val) (key-value (get-key-press w1)))
;taking input of 6 alph-num-symbolic keys
(define (get-name1)
  (define (helper i l)
    (if(= i 6)
       (list->string (reverse l))
       (let ([a (press-val)])
         (if (not(char? a))
             (helper i l)
             (if (equal? a #\return) (begin (show (reverse l) 470 w1)
                                            (list->string (reverse l)))
                 (if(or (char-alphabetic? a) 
                        (char-numeric? a) (char-symbolic? a))
                    (begin (show (reverse(cons a l)) 470 w1)
                           (helper (+ i 1) (cons a l)))
                    (begin (show (reverse l) 470 w1)
                           (helper i l))))))))
  (helper 0 '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;loops through each character and than converts it to ascii code and than places image corresponding to its ascii value
(define (show l h window)
  (define (help i l)
    (if(null? l)void
       (let((str (string-append "images/alpha/" 
                                (number->string (char->integer (car l))) ".jpg")))
         (begin
           ((draw-pixmap window) str (make-posn (* 30 i) h) "white")
           (help (+ i 1) (cdr l))))))
  (help 1 l))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(start)
