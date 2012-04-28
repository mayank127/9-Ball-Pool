;graphics for score window
(open-graphics)
(define win-height 300)
(define win-width 600)
(define wscore (open-pixmap "Scores"  win-height win-width))
(define (press-val) (key-value (get-key-press wscore)))
(define (show-string str)
  (print str))
((draw-pixmap wscore) "images/pool5.jpg" (make-posn 0 0))
;show-score places pictures of number at the given positions
(define  (show-scores n1 n2)
  (if(not(or(number? n1)
            (number? n2)))void
                          (begin
                            (show (string->list(number->string n1)) 110 wscore)
                            (show (string->list(number->string n2)) 410 wscore))))
;shows names of players and current players name and also the win and loose at the end
(define  (show-names l1 l2 cur status)
  (if(not(or(list? l1)
            (list? l2)))void
                          (begin
                            (show l1 60 wscore)
                            (show cur 170 wscore)
                            (show l2 360 wscore)
                            (show status 220 wscore))))