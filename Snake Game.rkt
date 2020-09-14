;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Snake Game|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require spd/tags)
(require 2htdp/image)
(require 2htdp/universe)

;; My world program  (make this more specific)

;; use W A S D to control snake movement
;; segments of snake follow after the "head" of snake
;; random foods appear and if collected by head of snake increase size of snake
;; if the head touches one of its own segments or wall then game stops

(@htdw Snake) 

;; =================
;; Constants:

(define WIDTH 300)
(define HEIGHT 300)

(define MTS (empty-scene WIDTH HEIGHT))

(define SEGMENT-RADIUS 7)

(define SEGMENT (square SEGMENT-RADIUS "solid" "black"))

(define SNAKE-COLOUR "black")

(define FOOD-COLOUR "goldenrod")

(define FOOD-RADIUS 4)

(define FOOD-IMAGE (circle FOOD-RADIUS "solid" FOOD-COLOUR))

(define CTR-X (/ WIDTH 2))

(define CTR-Y (/ HEIGHT 2))

(define SPEED-X 8)

(define SPEED-Y 8)


;; =================
;; Data definitions:

(@htdd Food)
(define-struct food (x y))
;; food is (make-food Number Number)
;; interp. food has x and y positions on MTS
;; CONSTRAINT:(x =/= 0, WIDTH) (y =/= 0, HEIGHT)
(define f1 (make-food 50 50))
(define f2 (make-food 100 100))

(@dd-template-rules compound)

(define (fn-for-food f)
  (... (food-x f)
       (food-y f)))

(@htdd Segment)
(define-struct segment (x y))
;; segment is (make-segment Number Number Number Number)
;; interp. one segment of snake. x and y are positions on MTS. dx and dy are change in x and y
(define segment1 (make-segment CTR-X CTR-Y))
(define segment2 (make-segment 500 500))


(@dd-template-rules compound) ;4 fields
                    

(define (fn-for-segment s)
  (... (segment-x s)
       (segment-y s)))

(@htdd Snake)
;; Snake is one of
;;    - empty
;;    - (cons segment Snake)
;; interp. snake is a list of segments 
(define Snake1 empty)
(define Snake2 (cons (make-segment 100 100) (cons (make-segment 200 200) empty)))

(@dd-template-rules one-of
                    atomic-distinct
                    compound
                    ref
                    self-ref)

(define (fn-for-snake snk)
  (cond [(empty? snk) empty]
        [else (... (fn-for-segment (first snk))
                   (fn-for-snake (rest snk)))]))

(@htdd World)
(define-struct world (snake dx dy food))
;; world is (make-world Snake Food)
;; interp. all moving world of world
(define w1 (make-world (cons (make-segment CTR-X CTR-Y)
                             (cons (make-segment (- CTR-X SEGMENT-RADIUS) CTR-Y)
                                   (cons (make-segment (- CTR-X (* SEGMENT-RADIUS 2)) CTR-Y) empty)))
                       0
                       0
                       (make-food (random (- WIDTH 5)) (random (- HEIGHT 5)))))
(define w2 (make-world (cons (make-segment CTR-X CTR-Y) empty)
                       0
                       0
                       (make-food (random WIDTH) (random HEIGHT))))

(@dd-template-rules compound
                    ref
                    ref)

(define (fn-for-world w)
  (... (fn-for-snake (world-snake w))
       (world-dx w)
       (world-dy w)
       (fn-for-food (world-food w))))
                     

;; =================
;; Functions:

(@htdf main)
(@signature World -> World)
;; start the world with (main w1)
;; 

(@template htdw-main)

(define (main w)
  (big-bang w                  ; World
    (state true)
    (on-tick   next-world (/ 1 6))       ; World -> World
    (to-draw   render)           ; World -> Image
    (stop-when you-lose)         ; World -> Boolean
    (on-key    key-handle)))     ; World KeyEvent -> world 


(@htdf render)
(@signature World -> Image) 
;; render segments on correct x and y and food at correct x and y
(check-expect (render (make-world empty 0 0 (make-food 10 10)))(place-image FOOD-IMAGE 10 10  MTS))
(check-expect (render (make-world (cons (make-segment 50 50) empty) 0 0 (make-food 20 20))) (place-image SEGMENT
                                                                                                         50
                                                                                                         50
                                                                                                         (place-image
                                                                                                          FOOD-IMAGE
                                                                                                          20
                                                                                                          20  MTS)))
(check-expect (render (make-world (cons (make-segment 50 50) (cons (make-segment 52 52) empty)) 0 0 (make-food 10 10)))
              (place-image SEGMENT
                           50
                           50
                           (place-image SEGMENT
                                        52
                                        52 (place-image FOOD-IMAGE 10 10 MTS))))
                                                                                                         
#;
(define (render w) empty-image) ;stub

(@template World)

(define (render w)
  (place-segment (world-snake w) (place-food (world-food w))))

(@htdf place-segment)
(@signature Snake Image -> Image)
;; place segments on image
(check-expect (place-segment empty MTS) MTS)
(check-expect (place-segment (cons (make-segment 50 50) (cons (make-segment 52 52) empty)) MTS)
              (place-image SEGMENT
                           50
                           50
                           (place-image SEGMENT
                                        52
                                        52
                                        MTS)))

#;
(define (place-segment snk img) img) ;stub

(@template Snake)

(define (place-segment snk img)
  (cond [(empty? snk) img]
        [else (place-image SEGMENT
                           (segment-x (first snk))
                           (segment-y (first snk))
                           (place-segment (rest snk) img))]))

(@htdf place-food)
(@signature Food -> Image)
;; place on correctly on x y on MTS
(check-expect (place-food (make-food 10 10)) (place-image FOOD-IMAGE
                                                          10
                                                          10
                                                          MTS))

#;
(define (place-food f) img) ;stub

(@template Food)

(define (place-food f)
  (place-image FOOD-IMAGE
               (food-x f)
               (food-y f)
               MTS))

(@htdf next-world)
(@signature World -> World)
;; produce next snake from world create new food if food is eaten
(check-expect (next-world (make-world (cons (make-segment 50 50)
                                            (cons (make-segment 55 55) empty)) 0 0 (make-food 10 10)))
              (make-world
               (next-snake1 (cons (make-segment 50 50)
                                  (cons (make-segment 55 55) empty)) 0 0 (make-food 10 10)) 0 0
                                                                                            (make-food 10 10)))
(check-expect (next-world (make-world (cons (make-segment 50 50)
                                            (cons (make-segment 55 55) empty)) 2 0 (make-food 52 50)))
              (make-world 
               (next-snake1 (cons (make-segment 50 50)
                                  (cons (make-segment 55 55) empty)) 2 0 (make-food 52 50)) (+ 2 (/ 1 2)) (+ 0 (/ 1 2))
                                                                                            (next-food (make-food 52 50)))) ;random
              
#;
(define (next-world w) w) ;stub

(@template World)

(define (next-world w)
  (if (eat-food (first (world-snake w)) (world-dx w) (world-dy w) (world-food w))
      (make-world (next-snake1 (world-snake w)
                               (+ (world-dx w)(/ 1 2))
                               (+ (world-dy w) (/ 1 2))
                               (world-food w)) (world-dx w) (world-dy w) (next-food (world-food w)))
      (make-world (next-snake1 (world-snake w)
                               (world-dx w)
                               (world-dy w)
                               (world-food w)) (world-dx w) (world-dy w) (world-food w)))) 
 
(@htdf eat-food)
(@signature Segment Number Number Food -> Boolean)
;; true if head intersects with food after adding dx and dy to x and y
(check-expect (eat-food (make-segment 50 50) 2 0 (make-food 52 52)) true)
(check-expect (eat-food (make-segment 20 20) 2 0 (make-food 52 52)) false)

#;
(define (eat-food s dx dy f) true) ;stub

(@template Food)

(define (eat-food s dx dy f)
  (and (and (>= (+ (segment-x s) dx) (- (food-x f) FOOD-RADIUS))
            (<= (+ (segment-x s) dx) (+ (food-x f) FOOD-RADIUS)))
       (and (>= (+ (segment-y s) dy) (- (food-y f) FOOD-RADIUS))
            (<= (+ (segment-y s) dy) (+ (food-y f) FOOD-RADIUS)))))

(@htdf next-food)
(@signature Food -> Food)
;; produce new random food spawn
(check-random (next-food (make-food 52 52)) (make-food (random (- WIDTH 3)) (random (- HEIGHT 3))))
(check-random (next-food (make-food 100 100)) (make-food (random (- WIDTH 3)) (random (- HEIGHT 3))))

#;
(define (next-food f) f);stub

(@template Food)

(define (next-food f)
  (make-food (random (- WIDTH 3)) (random (- HEIGHT 3))))

(@htdf next-snake1)
(@signature Snake Number Number Food -> Snake)
;; move the first segment dx dy to next segment then send to move-segment

(check-expect (next-snake1 empty 2 0 f1) empty)

(check-expect (next-snake1 (cons (make-segment 50 50)
                                 (cons (make-segment 48 50) empty)) 2 0 (make-food 10 10))                             ;moves normally
              (cons (make-segment 52 50)
                    (cons (make-segment 50 50) empty)))

(check-expect (next-snake1 (cons (make-segment 48 50)
                                 (cons (make-segment 46 50) empty)) 2 0 (make-food 20 20))   ;moves normally
              (cons (make-segment 50 50)
                    (cons (make-segment 48 50) empty)))

(check-expect (next-snake1 (cons (make-segment 48 50)
                                 (cons (make-segment 46 50) empty)) 2 0 (make-food 50 50))
              (cons (make-segment 48 50)
                    (cons (make-segment 46 50)
                          (cons (last-segment (make-segment 46 50)) empty))))  ;head eats food

#;
(define (next-snake1 snk f) snk) ;stub

(@template Snake)

(define (next-snake1 snk dx dy f)
  (cond [(empty? snk) empty]
        [else (if (eat-food (first snk) dx dy f)
                  (add-segment snk)
                  (change-y-x snk dx dy))]))

(@htdf change-y-x)
(@signature Snake Food -> Snake)
;; change first snake to given dx and dy 
(check-expect (change-y-x  (cons (make-segment 50 50) (cons (make-segment 48 50) empty)) 5 5)
              (cons (make-segment 55 55) (cons (make-segment 50 50) empty)))

#;
(define (change-y-x snk f) snk) ;stub

(@template Snake)

(define (change-y-x snk dx dy)
  (cons (make-segment (+ (segment-x (first snk)) dx)
                      (+ (segment-y (first snk)) dy)) (move-over snk)))

(@htdf move-over)
(@signature Snake -> Snake)
;; removes last segment
(check-expect (move-over (cons (make-segment 1 1) empty)) empty)
(check-expect (move-over (cons (make-segment 20 20) (cons (make-segment 50 50) empty))) (cons (make-segment 20 
                                                                                                            20)
                                                                                              empty))
(define (move-over snk)
  (cond [(empty? snk) empty]
        [else (if (empty? (rest snk))
                  empty
                  (cons (make-segment (segment-x (first snk))
                                      (segment-y (first snk)))
                        (move-over (rest snk))))]))

(@htdf add-segment)
(@signature Snake -> Snake)
;; add segment to the end of snake with gap before last segment
(check-expect (add-segment empty) empty)
(check-expect (add-segment (cons (make-segment 49 50) empty))
              (cons (make-segment 49 50)
                    (cons (make-segment (+ 49 1) 50) empty)))
(check-expect (add-segment (cons (make-segment 51 50) (cons (make-segment 52 52) (cons (make-segment 54 54) empty))))
              (cons (make-segment 51 50)
                    (cons (make-segment 52 52)
                          (cons (make-segment 54 54)
                                (cons (make-segment (+ 54 1) 54) empty)))))
  
#;
(define (add-segment snk) snk) ;stub

(@template Snake)

(define (add-segment snk)
  (cond [(empty? snk) empty]
        [else (if (empty? (rest snk))
                  (cons (first snk) (cons (last-segment (first snk)) empty))
                  (cons (first snk) (add-segment (rest snk))))]))

(@htdf last-segment)
(@signature Segment -> Segment)
;; make segment with diff in x-position of one radius-segment to given segment
(check-expect (last-segment (make-segment 50 50 )) (make-segment (+ 50 1) 50))

#;
(define (last-segment s) s) ;stub

(@template Segment)

(define (last-segment s)
  (make-segment (+ (segment-x s) 1)
                (segment-y s)))

(@htdf you-lose)
(@signature World -> Boolean)
;; segment hits another segment or hits wall
(check-expect (you-lose (make-world empty 0 0 f1)) false)
(check-expect (you-lose (make-world (cons (make-segment 10 10)
                                          (cons (make-segment 12 12)
                                                (cons (make-segment 10 10) empty))) 0 0 f1)) true)
(check-expect (you-lose (make-world (cons (make-segment 10 10)
                                          (cons (make-segment 12 12) empty)) 0 0 f1)) false)
(check-expect (you-lose (make-world (cons (make-segment WIDTH 10) empty) 0 0 f1)) true)
(check-expect (you-lose (make-world (cons (make-segment 10 HEIGHT) empty) 0 0 f1)) true)
(check-expect (you-lose (make-world (cons (make-segment 0 HEIGHT) empty) 0 0 f1)) true)
(check-expect (you-lose (make-world (cons (make-segment 10 0) empty) 0 0 f1)) true)

#;
(define (you-lose w) true) ;stub

(@template World)

(define (you-lose w)
  (stop! (world-snake w) (world-dx w) (world-dy w)))

(@htdf stop!)
(@signature Snake -> Boolean)
;; when head segment intersects with another segment or edge of screen after adding dx/dy
;; produce true
(check-expect (stop! empty 0 0) false)
(check-expect (stop! (cons (make-segment 10 10)
                           (cons (make-segment 12 12)
                                 (cons (make-segment 10 10) empty))) 0 0) true)
(check-expect (stop! (cons (make-segment 10 10)
                           (cons (make-segment 12 12) empty)) 5 0) false)
(check-expect (stop! (cons (make-segment WIDTH 10) empty) 5 0) true)
(check-expect (stop! (cons (make-segment 10 HEIGHT) empty) 0 5) true)
(check-expect (stop! (cons (make-segment 0 10) empty) -5 0) true)
(check-expect (stop! (cons (make-segment 10 0) empty) 0 -5) true)
   
#;
(define (stop! snk dx dy) true) ;stub

(@template Snake)

(define (stop! snk dx dy)
  (cond [(empty? snk) false]
        [else (if (or (intersect-edge (first snk) dx dy) (intersect-self (first snk) (rest snk)))
                  true
                  (stop! (rest snk) dx dy))]))

(@htdf intersect-edge)
(@signature Segment -> Boolean)
;; if segment is past edge of MTS after adding dx and dy to x and y produce true
(check-expect (intersect-edge (make-segment (+ WIDTH 1) 10) 2 0) true)
(check-expect (intersect-edge (make-segment 10 (+ HEIGHT 1)) 0 2) true)
(check-expect (intersect-edge (make-segment -1 10) -2 0) true)
(check-expect (intersect-edge (make-segment 10 -1) 0 -2) true) 

#;
(define (intersect-edge s) true) ;stub

(@template Segment)

(define (intersect-edge s dx dy)
  (or  (>= (segment-x s) WIDTH)
       (>= (segment-y s) HEIGHT)
       (<= (segment-x s) 0)
       (<= (segment-y s) 0)))
       
(@htdf intersect-self)
(@signature Segment Snake -> Boolean)
;; if segment intersects with any other segment x and y after adding dx and dy produce true
(check-expect (intersect-self (make-segment 10 10) empty) false)
(check-expect (intersect-self (make-segment 10 10) (cons (make-segment 20 20)
                                                         (cons (make-segment 10 10) empty))) true)

#;
(define (intersect-self s snk) true) ;stub

(@template Snake)

(define (intersect-self s snk)
  (cond [(empty? snk) false]
        [else (if (and (= (segment-x s) (segment-x (first snk))) (= (segment-y s) (segment-y (first snk))))
                  true
                  (intersect-self s (rest snk)))]))

(@htdf key-handle)
(@signature World KeyEvent -> World)
;; w s a d control direction of head of snake
(check-expect (key-handle empty " ") empty)
(check-expect (key-handle (make-world (cons (make-segment 53 53) (cons (make-segment 51 51) empty)) 0 0 f1) "w")
              (make-world (cons (make-segment 53 53) (cons (make-segment 51 51) empty)) 0 (- SPEED-Y) f1))
(check-expect (key-handle (make-world (cons (make-segment 53 53) (cons (make-segment 51 51) empty)) 0 0 f1) "s")
              (make-world (cons (make-segment 53 53) (cons (make-segment 51 51) empty)) 0 SPEED-Y f1))
(check-expect (key-handle (make-world (cons (make-segment 53 53) (cons (make-segment 51 51) empty)) 0 0 f1) "a")
              (make-world (cons (make-segment 53 53) (cons (make-segment 51 51) empty)) (- SPEED-X) 0 f1))
(check-expect (key-handle (make-world (cons (make-segment 53 53) (cons (make-segment 51 51) empty)) 0 0 f1) "d")
              (make-world (cons (make-segment 53 53) (cons (make-segment 51 51) empty)) SPEED-X 0 f1))
(check-expect (key-handle (make-world (cons (make-segment 53 53) (cons (make-segment 51 51) empty)) 0 0 f1) " ")
              (make-world (cons (make-segment 53 53) (cons (make-segment 51 51) empty)) 0 0 f1))
(check-expect (key-handle (make-world (cons (make-segment 53 53) (cons (make-segment 51 51) empty)) SPEED-X 0 f1) "a")
              (make-world (cons (make-segment 53 53) (cons (make-segment 51 51) empty)) SPEED-X 0 f1))
(check-expect (key-handle (make-world (cons (make-segment 53 53) (cons (make-segment 51 51) empty)) (- SPEED-X) 0 f1) "d")
              (make-world (cons (make-segment 53 53) (cons (make-segment 51 51) empty)) (- SPEED-X) 0 f1))
(check-expect (key-handle (make-world (cons (make-segment 53 53) (cons (make-segment 51 51) empty)) 0 (- SPEED-Y) f1) "s")
              (make-world (cons (make-segment 53 53) (cons (make-segment 51 51) empty)) 0 (- SPEED-Y) f1))
(check-expect (key-handle (make-world (cons (make-segment 53 53) (cons (make-segment 51 51) empty)) 0 SPEED-Y f1) "w")
              (make-world (cons (make-segment 53 53) (cons (make-segment 51 51) empty)) 0 SPEED-Y f1))



#;
(define (key-handle w kevt) w) ;stub

(@template KeyEvent)

(define (key-handle w ke)
  (cond [(and (key=? ke "w") (not (empty? (world-snake w))) (not (= (world-dy w) SPEED-Y)))
         (make-world (world-snake w) 0 (change-dy-up (world-dy w)) (world-food w))]
        [(and (key=? ke "s") (not (empty? (world-snake w))) (not (= (world-dy w) (- SPEED-Y))))
         (make-world (world-snake w) 0 (change-dy-down (world-dy w)) (world-food w))]
        [(and (key=? ke "a") (not (empty? (world-snake w))) (not (= (world-dx w)  SPEED-X)))
         (make-world (world-snake w) (change-dx-left (world-dx w)) 0 (world-food w))]
        [(and (key=? ke "d") (not (empty? (world-snake w))) (not (= (world-dx w) (- SPEED-X))))
         (make-world (world-snake w) (change-dx-right (world-dx w)) 0 (world-food w))]
        [else 
         w]))

(@htdf change-dy-up)
(@signature Number -> Number)
;; change dy to positive
(check-expect (change-dy-up 0) (- SPEED-Y))

#;
(define (change-dy-up n) n) ;stub

(@template Number)

(define (change-dy-up n) (- SPEED-Y))

(@htdf change-dy-down)
(@signature Number -> Number)
;; change dy to positive
(check-expect (change-dy-down 0) SPEED-Y)
(check-expect (change-dy-down -2) SPEED-Y)

#;
(define (change-dy-down n) n) ;stub

(@template Segment)

(define (change-dy-down n) SPEED-Y)

(@htdf change-dx-left)
(@signature Number -> Number)
;; change dy to positive
(check-expect (change-dx-left 0) (- SPEED-X))
(check-expect (change-dx-left 2) (- SPEED-X))

#;
(define (change-dx-left n) n) ;stub

(@template Segment)

(define (change-dx-left n)(- SPEED-X))


(@htdf change-dx-right)
(@signature Segment -> Segment)
;; change dy to positive
(check-expect (change-dx-right 2) SPEED-X)
(check-expect (change-dx-right -2) SPEED-X)

#;
(define (change-dx-right n) n) ;stub

(@template Segment)

(define (change-dx-right n) SPEED-X)
                










