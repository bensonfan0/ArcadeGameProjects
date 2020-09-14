;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Tank Game|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require spd/tags)
(require 2htdp/image)
(require 2htdp/universe)
; signature ListOfNumber -> ListOfNumber

(@htdd ListOfNumber)
(@htdd WS)

(@template htdw-main)

(define (main ws)
  (big-bang ws                   ; WS
    (state true)
    (on-tick   tock (/ 1 24))     ; WS -> WS
    (to-draw   render)   ; WS -> Image
    ;(stop-when ...)      ; WS -> Boolean
    (on-mouse  handle-mouse)      ; WS Integer Integer MouseEvent -> WS
    ;(on-key    ...)       ; WS KeyEvent -> WS
    ))   

#;
(@htdf tock)
(@signature ListOfNumber -> ListOfNumber)
;; produce the next ...
#|
(check-expect (tock (list 5 6 7 8 9) 13) (list 5 6 7 8 9 10 11 12 13))
(check-expect (tock empty 3) (list 1 2 3))
(check-expect (tock (list 3) 3) (list 3))
|#


#;
(define (tock lon x)
  (cond [(empty? lon) empty]
        [else (if (= (first lon) (+ x 1))
                  empty
                  (if (empty? (rest lon))
                      (cons (first lon)(tock (cons (+ (first lon) 1)
                                                   (rest lon)) x))
                      (cons (first lon) (tock (rest lon) x))))]))
        


#;
(define (tock lon x)
  (cond [(empty? lon) lon]
        [else (if (= (first lon) x)
                  lon
                  (append lon (cons (first lon)
                                    (if  (empty? (rest lon))
                                         (tock (list (+ (first lon) 1)) x)
                                         (tock (rest lon) x)))))]))




;; signature X -> X
;; will take dx and dy (only apply - 0.4 to dy every tick)
;; -> (-9.8 pixel/s * (1/24)s/tick) = -0.4 pixel/tick

;; the shell's dx and dy will be based on where user clicks

;; the longer the user holds, the greater the dx will be (dx is constant after
;; release)
;; the y-axis that the user clicks will be the initial dy
(define HEIGHT 500)
(define WIDTH 500)

(define TANK-X 25)
(define TANK-Y (- HEIGHT 200))

(define CHARGE-BAR (rectangle 100 20 "outline" "black"))

(define SCROLLER (rectangle 5 30 "solid" "black"))

(define MTS (rectangle WIDTH HEIGHT "solid" "white"))

(define ACCELERATION 0.4)

(define-struct shell (x y dx dy))

(check-expect (tock (make-shell TANK-X TANK-Y 10 -10))
              (make-shell (+ TANK-X 10) (+ TANK-Y -9.6) 10 -9.6))

(define w0 (make-shell TANK-X TANK-Y 0 0))



(define (tock s)
  (past-height (move-shell (calc-dy s))))

;; Signature Shell -> Shell

(define (move-shell s)
  (make-shell (+ (shell-x s) (shell-dx s))
              (+ (shell-y s) (shell-dy s))
              (shell-dx s)
              (shell-dy s)))
 

;; Signature Shell -> Shell

(define (calc-dy s)
  (make-shell (shell-x s)
              (shell-y s)
              (shell-dx s)
              (+ (shell-dy s) ACCELERATION)))

;;

(define (past-height s)
  (if (> (shell-y s) HEIGHT)
      (make-shell 0 0 0 0)
      s))




;; Signature Shell Number Number MouseEvent -> Shell
;; holding down mouse will charge x -> letting go will release shell




(define (handle-mouse s x y me)
  (cond [(mouse=? me "button-up") (make-shell TANK-X TANK-Y (shell-dx s)
                                              (- (/ (- TANK-Y y) 20)))]
        [(mouse=? me "drag") (make-shell TANK-X TANK-Y (+ (shell-dx s) 0.2)   ;; when you click and drag it has to hold at tank
                                         (shell-dy s))]
        [else
         s]))
              








#|
(define (calc-dy loy)
  (cond [(empty? loy) empty]
|#
        



                  
                  
                 

(@htdf render)
(@signature WS -> Image)
;; render ... 
;; !!!
(define (render ws)


  (place-image/align SCROLLER
                     5
                     10
                     "center"
                     "center"
                     (place-image CHARGE-BAR
                                  50
                                  (- HEIGHT 20)
              
                                  (place-image (circle 3 "solid" "black")
                                               (shell-x ws)
                                               (shell-y ws)
                                               (rectangle WIDTH HEIGHT "solid" "white")))))

