;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |DVD SCREEN SAVER|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require spd/tags)

(@htdw Ball)

;; Constants:
(define WIDTH  605)
(define HEIGHT 535)


(define BALL-RADIUS 10)



(define MTS (rectangle WIDTH HEIGHT "solid" "black"))

(define TEXT-SIZE-ABOVE 50)

(define TEXT-SIZE-BELOW 15)

(define ELLIPSE-WIDTH 90)

(define ELLIPSE-HEIGHT 20)

(define IMAGE (above (text "DVD" TEXT-SIZE-ABOVE "white")
                      (place-image/align (text "VIDEO" TEXT-SIZE-BELOW "black") 45 10 "center" "center"
                                         (ellipse ELLIPSE-WIDTH ELLIPSE-HEIGHT "solid" "white"))))
                         


;; ===========================================================================
;; ===========================================================================
;; Data definitions:

(@htdd Ball)

(define-struct ball (x y dx dy colour))
;; Ball is (make-ball Number Number Number Number String)
;; interp. (make-ball x y dx dy) is ball
;;   - position x, y    in screen coordinates
;;   - velocity dx, dy  in pixels/tick
;;   - Colour is colour of ball
(define B1 (make-ball (- WIDTH 1) (/ HEIGHT 2) -4 -4 "red"))

(@dd-template-rules compound)

(define (fn-for-ball b)
  (... (ball-x b) 
       (ball-y b) 
       (ball-dx b) 
       (ball-dy b)
       (ball-colour b)))


;; ===========================================================================
;; ===========================================================================
;; Functions:

(@htdf main)
(@signature Ball -> Ball)
;; start the game, call with (main B1)
;; <no tests for main functions>

(@template htdw-main)

(define (main b)
  (big-bang b
    (on-draw   render-ball)   ;Ball -> Image
    (on-tick   next-ball)     ;Ball -> Ball
    (on-mouse  handle-mouse)));Ball MouseEvent Integer Integer -> Ball
            



(@htdf render-ball)
(@signature Ball -> Image)
;; place BALL on image at appropriate x, y coordinate
(check-expect (render-ball (make-ball WIDTH HEIGHT 4 4 "white"))
              (place-image (above (text "DVD" 50 "white")
                                  (place-image/align (text/font "VIDEO" 20 "black" "face" "decorative" "italic" "bold" false)
                                                     50 10 "center" "center" (ellipse 100 20 "solid" "white")))
                           WIDTH
                           HEIGHT
                           MTS))
(check-expect (render-ball (make-ball (/ WIDTH 2) (/ HEIGHT 2) 4 4 "white"))
              (place-image (above (text "DVD" 50 "white")
                                  (place-image/align (text/font "VIDEO" 20 "black" "face" "decorative" "italic" "bold" false)
                                                     50 10 "center" "center" (ellipse 100 20 "solid" "white")))
                             (/ WIDTH 2)
                           (/ HEIGHT 2)
                           MTS)) 
#;
(define (render-ball b) empty-image);stub

(@template Ball)

(define (render-ball b)
  (place-image (above (text "DVD" 50 (ball-colour b))
                      (place-image/align (text/font "VIDEO" 20 "black" "face" "decorative" "italic" "bold" false)
                                                     50 10 "center" "center" (ellipse 100 20 "solid" (ball-colour b))))
               (ball-x b) 
               (ball-y b) 
               MTS))

(@htdf next-ball)
(@signature Ball -> Ball)
;; produce ball at next x,y; checks bounces off top/right/bottom/left wall and change colour
                        
(check-expect (next-ball (make-ball 100 3 3 -4 "red"))
              (bounce-top (make-ball 100 3 3 -4 "red")))
(check-expect (next-ball (make-ball 100 HEIGHT 3 4 "red")) 
              (bounce-bottom (make-ball 100 HEIGHT 3 4 "red")))
(check-expect (next-ball (make-ball 0 100 -3 1 "red")) 
              (bounce-left (make-ball 0 100 -3 1 "red")))
(check-expect (next-ball (make-ball WIDTH 100 3 4 "red")) 
              (bounce-right (make-ball WIDTH 100 3 4 "red")))
(check-expect (next-ball (make-ball 100 110 3 4 "red")) 
              (glide (make-ball 100 110 3 4 "red")))
#;
(define (next-ball b) b)

(@template Number) ;(@template Number) because b is treated as atomic

(define (next-ball b)
  (cond [(touch-top?    b) (bounce-top b)]
        [(touch-bottom? b) (bounce-bottom b)]
        [(touch-right?  b) (bounce-right b)]
        [(touch-left?   b) (bounce-left b)]
        [else
         (glide b)]))


(@htdf handle-mouse)
(@signature Ball Integer Integer MouseEvent -> Ball)
;; replace ball with new ball on mouse click
;; NOTE: uses random, so testing has to use check-random
(check-random (handle-mouse (make-ball 1 2 3  "white" 4) 100 200 "button-down")
              (make-ball 100 200 (- 5 (random 11)) (- 5 (random 11)) "white"))
(check-random (handle-mouse (make-ball 1 2 3 4 "red") 100 200 "button-up")
              (make-ball 1 2 3 4 "red"))
#;
(define (handle-mouse b x y me) b)

(@template MouseEvent)

(define (handle-mouse b x y me)
  (cond [(mouse=? me "button-down")
         (make-ball x y (- 5 (random 11)) (- 5 (random 11)) "white")]
        [else b]))
        

(@htdf touch-top?)
(@signature Ball -> Boolean)
;; produce true if ball is going up and running into top edge of box
(check-expect (touch-top?    (make-ball 1  (- 100 (/(image-height IMAGE) 2)) 3 -4 "red")) false)
(check-expect (touch-top?    (make-ball 1  (- 3 (/(image-height IMAGE) 2))   3 -2 "red")) false)
(check-expect (touch-top?    (make-ball 1  (- 2 (/(image-height IMAGE) 2))   3 -2 "red")) true)
(check-expect (touch-top?    (make-ball 1  (- 1 (/(image-height IMAGE) 2))   3 -4 "red")) true)
#;
(define (touch-top? b) false)

(@template Ball)

(define (touch-top? b) 
  (<= (+ (- (ball-y b) (/(image-height IMAGE) 2))(ball-dy b)) 0))

(@htdf touch-bottom?)
(@signature Ball -> Boolean)
;; produce true if ball is going down and running into bottom edge of box
(check-expect (touch-bottom? (make-ball 1           (+ 1 (/ (image-height IMAGE) 2))           3 4 "red"))
              false)
(check-expect (touch-bottom? (make-ball (- WIDTH 1) (+ 1 (/ (image-height IMAGE) 2))            3 4 "red"))
              false)
(check-expect (touch-bottom? (make-ball (- WIDTH 1) (+ (- HEIGHT 3)(/ (image-height IMAGE) 2)) 3 4 "red"))
              true)  ;at edge
(check-expect (touch-bottom? (make-ball (/ WIDTH 2) (+ HEIGHT (/ (image-height IMAGE) 2))       3 4 "red"))
              true)  ;goes past edge

#;
(define (touch-bottom? b) false);stub

(@template Ball)

(define (touch-bottom? b)
  (>= (+ (+ (ball-y b) (/(image-height IMAGE) 2))(ball-dy b)) HEIGHT))

(@htdf touch-left?)
(@signature Ball -> Boolean)
;; produce true if ball is going left and running into left edge of box
(check-expect (touch-left? (make-ball (- (- WIDTH 3)(/ (image-width IMAGE) 2)) (- HEIGHT 3) 2 2 "red"))
              false)
(check-expect (touch-left? (make-ball (- (/ WIDTH 2)(/ (image-width IMAGE) 2)) (/ HEIGHT 2) 4 4 "red"))
              false)
(check-expect (touch-left? (make-ball (- 1 (/ (image-width IMAGE) 2))           1           -1 4 "red"))
              true) ;at left edge
(check-expect (touch-left? (make-ball (- 1 (/ (image-width IMAGE) 2))           1           -2 4 "red"))
              true) ;goes past edge

#;
(define (touch-left? b) false);stub

(@template Ball)

(define (touch-left? b)
  (<= (+ (- (ball-x b)(/ (image-width IMAGE) 2))(ball-dx b)) 0))


(@htdf touch-right?)
(@signature Ball -> Boolean)
;; produce true if ball is going right and running into right edge of box
(check-expect (touch-right? (make-ball (+ (/ WIDTH 2)(/ (image-width IMAGE) 2)) 1  4 4 "red"))
              false)
(check-expect (touch-right? (make-ball (+ 1 (/ (image-width IMAGE) 2))          1  4 4 "red"))
              false)
(check-expect (touch-right? (make-ball (+ (- WIDTH 4)(/ (image-width IMAGE) 2)) 1  4 4 "red"))
              true) ;at right edge
(check-expect (touch-right? (make-ball (+ WIDTH (/ (image-width IMAGE) 2))      1  4 4 "red"))
              true) ;goes past edge

#;
(define (touch-right? b) false) ;stub

(@template Ball)

(define (touch-right? b)
  (>= (+ (+ (ball-x b)(/ (image-width IMAGE) 2))(ball-dx b)) WIDTH))


(@htdf bounce-top)
(@signature Ball -> Ball)
;; produce a ball that has bounced 1 pixel off of top edge of the box and change colour
;; CONSTRAINT: assume ball is close to top edge and moving up
(check-random (bounce-top (make-ball 4 5 2 -3 "red"))
              (make-ball 3 (+ 0 (/ (image-height IMAGE) 2)) 2 (random 4) "red"))
(check-random (bounce-top (make-ball 4 -1 5 -6 "red"))
              (make-ball 4 (+ 0 (/ (image-height IMAGE) 2)) 5 (random 4) "red"))
#;
(define (bounce-top b) b)

(@template Ball)

(define (bounce-top b) 
  (make-ball
   (ball-x b)
   (+ 0 (/ (image-height IMAGE) 2))
   (ball-dx b)
   (random 4)
   (random-colour b)))
 
(@htdf bounce-bottom)
(@signature Ball -> Ball)
;; produce a ball that has bounced 1 pixel off of bottom edge of the box and change colour
;; CONSTRAINT: assume ball is close to bottom edge and moving down
(check-expect (bounce-bottom (make-ball 1 HEIGHT 4 4 "red"))
              (make-ball 1 (+ HEIGHT (/ (image-height IMAGE) 2)) 4 (- (random 4)) "red"))
(check-expect (bounce-bottom (make-ball 1 (+ HEIGHT 2) 4 4 "red"))
              (make-ball 1 (+ HEIGHT (/ (image-height IMAGE) 2)) 4 (- (random 4)) "red")) ;assume ball going past edge 

#;
(define (bounce-bottom b) b)  ;stub

(@template Ball)

(define (bounce-bottom b)
  (make-ball (ball-x b) 
             (- HEIGHT (/ (image-height IMAGE) 2))        ;maybe i need to make this (- HEIGHT BALL-RADIUS)
             (ball-dx b) 
             (- (random 4))
             (random-colour b)))


(@htdf bounce-left)
(@signature Ball -> Ball)
;; produce a ball that has bounced 1 pixel off of left edge of the box and change colour
;; CONSTRAINT: assume ball is close to left edge and moving left
(check-expect (bounce-left (make-ball 0 1 -1 1 "red"))
              (make-ball (+ 0 (/ (image-width IMAGE) 2)) 1 (random 4) 1 "red"))
(check-expect (bounce-left (make-ball -1 10 -5 5 "red"))
              (make-ball (+ 0 (/ (image-width IMAGE) 2)) 10 (random 4) 5 "red")) ;assume all touch-left? true cases

#;
(define (bounce-left b) b) ;stub

(@template Ball)

(define (bounce-left b)
  (make-ball (+ 0 (/ (image-width IMAGE) 2))
             (ball-y b) 
             (random 4) 
             (ball-dy b)
             (random-colour b)))


(@htdf bounce-right)
(@signature Ball -> Ball)
;; produce a ball that has bounced 1 pixel off of right edge of the box and change colour
;; CONSTRAINT: assume ball is close to right edge and moving right
(check-expect (bounce-right (make-ball WIDTH 1 5 5 "red"))
              (make-ball (- WIDTH (/ (image-width IMAGE) 2)) 1 (- (random 4)) 5 "red"))
(check-expect (bounce-right (make-ball (+ WIDTH 1) 1 5 5 "red"))
              (make-ball (- WIDTH (/ (image-width IMAGE) 2)) 1 (- (random 4)) 5 "red")) ;assume all touch-right? true cases

#;
(define (bounce-right b) b) ;stub

(@template Ball)

(define (bounce-right b)
  (make-ball (- WIDTH (/ (image-width IMAGE) 2))
             (ball-y b) 
             (- (random 4))
             (ball-dy b)
             (random-colour b)))



(@htdf glide)
(@signature Ball -> Ball)
;; move ball by dx dy
;; CONSTRAINT: ball is not touching or about to touch any edge of the box
(check-expect (glide (make-ball (/ WIDTH 2) (/ HEIGHT 2) 4 4 "red"))
              (make-ball (+ (/ WIDTH 2) 4) (+ (/ HEIGHT 2) 4) 4 4 "red"))
(check-expect (glide (make-ball (/ WIDTH 2) (/ HEIGHT 2) 4 -4 "red"))
              (make-ball (+ (/ WIDTH 2) 4) (+ (/ HEIGHT 2) -4) 4 -4 "red"))
(check-expect (glide (make-ball (/ WIDTH 2) (/ HEIGHT 2) -4 4 "red"))
              (make-ball (+ (/ WIDTH 2) -4) (+ (/ HEIGHT 2) 4) -4 4 "red"))
(check-expect (glide (make-ball (/ WIDTH 2) (/ HEIGHT 2) -4 -4 "red"))
              (make-ball (+ (/ WIDTH 2) -4) (+ (/ HEIGHT 2) -4) -4 -4 "red"))

#;
(define (glide b) b) ;stub

(@template Ball)

(define (glide b)
  (make-ball (+ (ball-x b) (ball-dx b)) 
             (+ (ball-y b) (ball-dy b))
             (ball-dx b) 
             (ball-dy b)
             (ball-colour b)))


(@htdf random-colour)
(@signature String -> Number)
;; change colour to another random colour
(check-random (random-colour "red") (give-colour (random 10)))
(check-random (random-colour "blue") (give-colour (random 10)))

#;
(define (random-colour b) 1) ;stub

(@template String)

(define (random-colour s)
  (give-colour (random 10)))

(@htdf give-colour)
(@signature Number -> String)
;; number correlates to colour
;;   1 = red
;;   2 = blue
;;   3 = purple
;;   4 = green
;;   5 = magenta
;;   6 = white
;;   7 = indigo
;;   8 = orange
;;   9 = yellow
(check-expect (give-colour 5) "magenta")
(check-expect (give-colour 1) "red")
(check-expect (give-colour 2) "blue")
(check-expect (give-colour 3) "purple")
(check-expect (give-colour 4) "green")
(check-expect (give-colour 6) "white")
(check-expect (give-colour 7) "indigo")
(check-expect (give-colour 8) "orange")
(check-expect (give-colour 9) "yellow")


;(define (give-colour n) "") ;stub

(define (give-colour n)
  (cond [(= n 1) "red"]
        [(= n 2) "blue"]
        [(= n 3) "purple"]
        [(= n 4) "green"]
        [(= n 5) "magenta"]
        [(= n 6) "white"]
        [(= n 7) "indigo"]
        [(= n 8) "orange"]
        [else "yellow"]))
