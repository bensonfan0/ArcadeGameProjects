;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |2D Ray Casting|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require spd/tags)

#|
2D Ray Casting
Summarized:

https://www.redblobgames.com/articles/visibility/

https://www.techopedia.com/definition/21614/ray-casting

- "Rays can be cast and traced in groups based on certain
geometric constraints."

- "The pixel value from the closest intersection is obtained and
 is further set as the base for the projection"

- Calculate the edge of walls present and only render those

- Use lines (first -> try triangles later) which start from center of person

- 24 Lines in total -> 6 lines in each quadrant of graph

- End of line will be another object "TRACER" (w/ x and y coord) -> checks
if TRACE intersects with any walls on screen

- Consecutive TRACER will follow first and replace old TRACERS when intersection
occurs

- WASD will allow person to move around.

- TRACER will originate from person (x y)

- when an intersection is true then change the sight line to the current tracer
|#


;; My world program  (make this more specific)

(@htdw World) 

;; =================
;; Constants:

(define WIDTH 400)
(define HEIGHT 400)
(define MTS (empty-scene WIDTH HEIGHT))

(define PERSON-SIZE 5)
(define PERSON-COLOR "black")
(define PERSON (circle 10 "solid" "black"))

(define OBJECT1 (rectangle 100 50 "solid" "black"))
(define OBJECT2 (rectangle 75 50 "solid" "black"))
(define OBJECT3 (square    75     "solid" "black"))
(define OBJECT4 (square    180    "solid" "black"))

(define TRACE-IMAGE empty-image)

(define PDX 2)
(define PDY 2)

;;bottom left tracers

(define tbl1dx -10)
(define tbl1dy 10)
(define tbl2dx -20)
(define tbl2dy 10)
(define tbl3dx -10)
(define tbl3dy 20)
(define tbl4dx -10)
(define tbl4dy 40)
(define tbl5dx -40)
(define tbl5dy 10)
(define tbl6dx 0)   ;straight down
(define tbl6dy 10)   ;straight down

(define tbl1dx_1 -10)
(define tbl1dy_1 10)
(define tbl2dx_2 -20)
(define tbl2dy_2 10)
(define tbl3dx_3 -10)
(define tbl3dy_3 20)
(define tbl4dx_4 -10)
(define tbl4dy_4 40)
(define tbl5dx_5 -40)
(define tbl5dy_5 10)
(define tbl6dx_6 0)   ;straight down
(define tbl6dy_6 10)   ;straight down

;;bottom right tracers

(define tbr1dx 10)
(define tbr1dy 10)
(define tbr2dx 20)
(define tbr2dy 10)
(define tbr3dx 10)
(define tbr3dy 20)
(define tbr4dx 10)
(define tbr4dy 40)
(define tbr5dx 40)
(define tbr5dy 10)
(define tbr6dx 10)   ;straight right
(define tbr6dy 0)   ;straight right

(define tbr1dx_1 1)
(define tbr1dy_1 1)
(define tbr2dx_2 2)
(define tbr2dy_2 1)
(define tbr3dx_3 1)
(define tbr3dy_3 2)
(define tbr4dx_4 1)
(define tbr4dy_4 4)
(define tbr5dx_5 4)
(define tbr5dy_5 1)
(define tbr6dx_6 1)   ;straight right
(define tbr6dy_6 0)   ;straight right


;;top left tracers

(define ttl1dx -10)
(define ttl1dy -10)
(define ttl2dx -10)
(define ttl2dy -20)
(define ttl3dx -20)
(define ttl3dy -10)
(define ttl4dx -10)
(define ttl4dy -40)
(define ttl5dx -40)
(define ttl5dy -10)
(define ttl6dx -10)  ;straight left
(define ttl6dy 0)  ;straight left

(define ttl1dx_1 -1)
(define ttl1dy_1 -1)
(define ttl2dx_2 -1)
(define ttl2dy_2 -2)
(define ttl3dx_3 -2)
(define ttl3dy_3 -1)
(define ttl4dx_4 -1)
(define ttl4dy_4 -4)
(define ttl5dx_5 -4)
(define ttl5dy_5 -1)
(define ttl6dx_6 -1)  ;straight left
(define ttl6dy_6 0)  ;straight left

;;top right tracers

(define ttr1dx 10)
(define ttr1dy -10)
(define ttr2dx 10)
(define ttr2dy -20)
(define ttr3dx 20)
(define ttr3dy -10)
(define ttr4dx 10)
(define ttr4dy -40)
(define ttr5dx 40)
(define ttr5dy -10)
(define ttr6dx 0)  ;straight up
(define ttr6dy -10)  ;straight up

(define ttr1dx_1 1)
(define ttr1dy_1 -1)
(define ttr2dx_2 1)
(define ttr2dy_2 -2)
(define ttr3dx_3 2)
(define ttr3dy_3 -1)
(define ttr4dx_4 1)
(define ttr4dy_4 -4)
(define ttr5dx_5 4)
(define ttr5dy_5 -1)
(define ttr6dx_6 0)  ;straight up
(define ttr6dy_6 -1)  ;straight up




;; =================
;; Data definitions
(@htdd Person)
(define-struct person (px py))
;; Person is (make-person Number Number)
;; interp. the x and y coord of person on screen
(define P0 (make-person (/ WIDTH 2) (/ HEIGHT 2)))
(define P1 (make-person 50 50))
(define P2 (make-person 20 20))
(define P3 (make-person 10 150))

(define (fn-for-person p)
  (local [(define (fn-for-person p)
            (... (person-px p)
                 (person-py p)))]
    (fn-for-person p)))

(@htdd Quadrant)
;; Quadrant is String:
;;   - "Top-Level"
;;   - "Bot-Left"
;;   - "Bot-Right"
;;   - "Top-Left"
;;   - "Top-Right"
;;   - "Bottom-Level"
;; breaking up sectors of graph based on +/- x coord and +/- y coord
(define t_level "Top-Level")
(define b_l "Bot-Left")
(define b_r "Bot-Right")
(define t_l "Top-Left")
(define t_r "Top-Right")

(define (fn-for-quadrant q)
  (local [(define (fn-for-quadrant q)
            (... ))]
    (fn-for-quadrant q)))

(@htdd Tracer)
(define-struct tracer (tx ty tdx tdy))    ;; each tracer independent slope y/x
;; Tracer is (make-tracer Number Number Integer Integer)
;; interp. tracer moves across screen starting from person center
(define T1  (make-tracer (/ WIDTH 2) (/ HEIGHT 2) tbl1dx tbl1dy))
(define T2  (make-tracer (/ WIDTH 2) (/ HEIGHT 2) tbl2dx tbl2dy))
(define T3  (make-tracer (/ WIDTH 2) (/ HEIGHT 2) tbl3dx tbl3dy))
(define T4  (make-tracer (/ WIDTH 2) (/ HEIGHT 2) tbl4dx tbl4dy))
(define T5  (make-tracer (/ WIDTH 2) (/ HEIGHT 2) tbl5dx tbl5dy))
(define T6  (make-tracer (/ WIDTH 2) (/ HEIGHT 2) tbl6dx tbl6dy))

(define T7  (make-tracer (/ WIDTH 2) (/ HEIGHT 2) tbr1dx tbr1dx))
(define T8  (make-tracer (/ WIDTH 2) (/ HEIGHT 2) tbr2dx tbr2dy))
(define T9  (make-tracer (/ WIDTH 2) (/ HEIGHT 2) tbr3dx tbr3dy))
(define T10 (make-tracer (/ WIDTH 2) (/ HEIGHT 2) tbr4dx tbr4dy))
(define T11 (make-tracer (/ WIDTH 2) (/ HEIGHT 2) tbr5dx tbr5dy))
(define T12 (make-tracer (/ WIDTH 2) (/ HEIGHT 2) tbr6dx tbr6dy))

(define T13 (make-tracer (/ WIDTH 2) (/ HEIGHT 2) ttl1dx ttl1dy))
(define T14 (make-tracer (/ WIDTH 2) (/ HEIGHT 2) ttl2dx ttl2dy))
(define T15 (make-tracer (/ WIDTH 2) (/ HEIGHT 2) ttl3dx ttl3dy))
(define T16 (make-tracer (/ WIDTH 2) (/ HEIGHT 2) ttl4dx ttl4dy))
(define T17 (make-tracer (/ WIDTH 2) (/ HEIGHT 2) ttl5dx ttl5dy))
(define T18 (make-tracer (/ WIDTH 2) (/ HEIGHT 2) ttl6dx ttl6dy))

(define T19 (make-tracer (/ WIDTH 2) (/ HEIGHT 2) ttr1dx ttr1dy))
(define T20 (make-tracer (/ WIDTH 2) (/ HEIGHT 2) ttr2dx ttr2dy))
(define T21 (make-tracer (/ WIDTH 2) (/ HEIGHT 2) ttr3dx ttr3dy))
(define T22 (make-tracer (/ WIDTH 2) (/ HEIGHT 2) ttr4dx ttr4dy))
(define T23 (make-tracer (/ WIDTH 2) (/ HEIGHT 2) ttr5dx ttr5dy))
(define T24 (make-tracer (/ WIDTH 2) (/ HEIGHT 2) ttr6dx ttr6dy))


(define (fn-for-tracer t)
  (local [(define (fn-for-tracer t)
            (... (tracer-tx t)
                 (tracer-ty t)
                 (tracer-tdx t)
                 (tracer-tdy t)))]
    (fn-for-tracer t)))

(@htdd Intersection)
(define-struct intersection (ix iy))
;; Intersection is (make-intersection Number Number)
;; interp. intersection has x and y
(define I1 (make-intersection (person-px P0) (person-py P0)))
(define I2 (make-intersection (person-px P0) (person-py P0)))
(define I3 (make-intersection (person-px P0) (person-py P0)))
(define I4 (make-intersection (person-px P0) (person-py P0)))
(define I5 (make-intersection (person-px P0) (person-py P0)))
(define I6 (make-intersection (person-px P0) (person-py P0)))
(define I7 (make-intersection (person-px P0) (person-py P0)))
(define I8 (make-intersection (person-px P0) (person-py P0)))
(define I9 (make-intersection (person-px P0) (person-py P0)))
(define I10 (make-intersection (person-px P0) (person-py P0)))
(define I11 (make-intersection (person-px P0) (person-py P0)))
(define I12 (make-intersection (person-px P0) (person-py P0)))
(define I13 (make-intersection (person-px P0) (person-py P0)))
(define I14 (make-intersection (person-px P0) (person-py P0)))
(define I15 (make-intersection (person-px P0) (person-py P0)))
(define I16 (make-intersection (person-px P0) (person-py P0)))
(define I17 (make-intersection (person-px P0) (person-py P0)))
(define I18 (make-intersection (person-px P0) (person-py P0)))
(define I19 (make-intersection (person-px P0) (person-py P0)))
(define I20 (make-intersection (person-px P0) (person-py P0)))
(define I21 (make-intersection (person-px P0) (person-py P0)))
(define I22 (make-intersection (person-px P0) (person-py P0)))
(define I23 (make-intersection (person-px P0) (person-py P0)))
(define I24 (make-intersection (person-px P0) (person-py P0)))
(define I_92 (make-intersection 50 50))
(define I_91 (make-intersection 100 75))
(define I_90 (make-intersection 2 2))

(define (fn-for-intersection i)
  (local [(define (fn-for-intersection i)
            (... (intersection-ix i)
                 (intersection-iy i)))]
    (fn-for-intersection i)))
      

(@htdd Sight ListOfSight)
(define-struct horizon (tracer intersection))   
(define-struct level (quadrant los))
;; Sight is one of:
;;   - (make-horizon Tracer Intersection)
;;   - (make-level (Quadrant ListOfSight))
;; interp. a tree with the tracers broken up into graph segments

;; ListOfSight is one of:
;;   - empty
;;   - (cons Sight ListOfSight)
;; interp. a list of sight

;; H is horizon and L is level
(define H1 (make-horizon T1 I1))
(define H2 (make-horizon T2 I2))
(define H3 (make-horizon T3 I3))
(define H4 (make-horizon T4 I4))
(define H5 (make-horizon T5 I5))
(define H6 (make-horizon T6 I6))
(define H7 (make-horizon T7 I7))
(define H8 (make-horizon T8 I8))
(define H9 (make-horizon T9 I9))
(define H10 (make-horizon T10 I10))
(define H11 (make-horizon T11 I11))
(define H12 (make-horizon T12 I12))
(define H13 (make-horizon T13 I13))
(define H14 (make-horizon T14 I14))
(define H15 (make-horizon T15 I15))
(define H16 (make-horizon T16 I16))
(define H17 (make-horizon T17 I17))
(define H18 (make-horizon T18 I18))
(define H19 (make-horizon T19 I19))
(define H20 (make-horizon T20 I20))
(define H21 (make-horizon T21 I21))
(define H22 (make-horizon T22 I22))
(define H23 (make-horizon T23 I23))
(define H24 (make-horizon T24 I24))

(define L2 (make-level b_l (list H1 H2 H3 H4 H5 H6)))
(define L3 (make-level b_r (list H7 H8 H9 H10 H11 H12)))
(define L4 (make-level t_l (list H13 H14 H15 H16 H17 H18)))
(define L5 (make-level t_r (list H18 H19 H20 H21 H22 H24)))
(define L1 (make-level t_level (list L2 L3 L4 L5)))

(define (fn-for-sight s) 
  (local [(define (fn-for-sight s)
            (cond [(horizon? s)
                   (... (fn-for-tracer (horizon-tracer s))
                        (fn-for-intersection (horizon-intersection s)))]
                  [else (... (level-quadrant s)
                             (fn-for-los (level-los s)))]))
            
          (define (fn-for-los s)
            (cond [(empty? s) (...)]
                  [else (... (fn-for-sight (first s))
                             (fn-for-los (rest s)))]))]
    (fn-for-sight s)))

(@htdd Obstacle) 
(define-struct obstacle (ox oy img))
;; Obstacle is (make-obstacle Number Number Image)
;; interp. the structures to be rendered on screen given x and y coord
(define obs1 (make-obstacle 30 40 OBJECT1)) 
(define obs2 (make-obstacle (- WIDTH 50) (- HEIGHT 70) OBJECT2))
(define obs3 (make-obstacle (- WIDTH 100) (- HEIGHT 125) OBJECT3))
(define obs4 (make-obstacle (- WIDTH 160) (- HEIGHT 25) OBJECT4))

(define (fn-for-obstacle o)
  (local [(define (fn-for-obstacle o)
            (... (obstacle-ox o)
                 (obstacle-oy o)
                 (obstacle-img o)))]
    (fn-for-obstacle o)))
 

(@htdd Map)
;; Map is one of:
;;   - empty
;;   - (cons Obstacle Map)
;; interp. the objects on screen
(define M1 empty)
(define M2 (list obs1 obs2 obs3 obs4))

(define (fn-for-map m)
  (local [(define (fn-for-map m)
            (cond [(empty? m) (...)]
                  [else (... (fn-for-obstacle (first m))
                             (fn-for-map (rest m)))]))]
    (fn-for-map m))) 

(@htdd World)
(define-struct world (person sight map))
;; World is (make-world Person Sight ListOfMap)
;; interp. all things to be rendered in world
(define W0 (make-world P0 L1 M2))
(define W1 (make-world (make-person 0 0) L1 empty))
(define W2 (make-world P0 (make-level "bottom" (list H6 H12 H18 H24)) M2))
(define W3 (make-world P0 (make-level "bottom" (list H12 H2)) M2))

(define (fn-for-world w)
  (local [(define (fn-for-world w)
            (... (fn-for-person (world-person w))
                 (fn-for-sight (world-sight w))
                 (world-map w)))]
    (fn-for-world w)))  
                                    
;; =================
;; Functions:

(@htdf main)
(@signature World -> World)
;; start the world with ...
;; 

(@template htdw-main)

(define (main w)
  
  (big-bang w                   ; World
    (state true)
    (on-tick   next-world (/ 1 24))     ; World -> World
    (to-draw   render)   ; World -> Image
    ; (stop-when ...)      ; World -> Boolean
    (on-mouse  mouse-handle)      ; World Integer Integer MouseEvent -> World
    (on-key    key-handle)))    ; World KeyEvent -> World

(@htdf next-world)
(@signature World -> World)
;; produce the next world by moving tracers and seeing if intersects


(@template World fn-composition )

(define (next-world w)
  (move-intersection (move-tracer w)))



(@htdf move-tracer)
(@signature World -> World)
;; add tdx and tdy to x and y
(check-expect (move-tracer W0) (make-world (world-person W0)
                                           (find-horizon (world-sight W0))
                                           (world-map W0)))
(check-expect (move-tracer W1) (make-world (world-person W1)
                                           (find-horizon (world-sight W1))
                                           (world-map W1)))
#;
(define (move-tracer w) w) ;stub


(@template World)

(define (move-tracer w)
  (local [(define (fn-for-world w)
            (make-world  (world-person w)
                         (find-horizon (world-sight w))
                         (world-map w)))]
    (fn-for-world w)))




(@htdf find-horizon)
(@signature Sight -> ListOfSight)
;;go into sight and send tracers in horizons to add-tracer

(check-expect (find-horizon L2)(make-level (level-quadrant L2)
                                           (list
                                            (make-horizon
                                             (add-tracer (horizon-tracer H1))
                                             (horizon-intersection H1))
                                            (make-horizon
                                             (add-tracer (horizon-tracer H2))
                                             (horizon-intersection H2))
                                            (make-horizon
                                             (add-tracer (horizon-tracer H3))
                                             (horizon-intersection H3))
                                            (make-horizon
                                             (add-tracer (horizon-tracer H4))
                                             (horizon-intersection H4))
                                            (make-horizon
                                             (add-tracer (horizon-tracer H5))
                                             (horizon-intersection H5))
                                            (make-horizon
                                             (add-tracer (horizon-tracer H6))
                                             (horizon-intersection H6)))))
(check-expect (find-horizon L3) (make-level (level-quadrant L3)
                                            (list
                                             (make-horizon
                                              (add-tracer (horizon-tracer H7))
                                              (horizon-intersection H7))
                                             (make-horizon
                                              (add-tracer (horizon-tracer H8))
                                              (horizon-intersection H8))
                                             (make-horizon
                                              (add-tracer (horizon-tracer H9))
                                              (horizon-intersection H9))
                                             (make-horizon
                                              (add-tracer (horizon-tracer H10))
                                              (horizon-intersection H10))
                                             (make-horizon
                                              (add-tracer (horizon-tracer H11))
                                              (horizon-intersection H11))
                                             (make-horizon
                                              (add-tracer (horizon-tracer H12))
                                              (horizon-intersection H12)))))
(check-expect (find-horizon L4) (make-level (level-quadrant L4)
                                            (list
                                             (make-horizon
                                              (add-tracer (horizon-tracer H13))
                                              (horizon-intersection H13))
                                             (make-horizon
                                              (add-tracer (horizon-tracer H14))
                                              (horizon-intersection H14))
                                             (make-horizon
                                              (add-tracer (horizon-tracer H15))
                                              (horizon-intersection H15))
                                             (make-horizon
                                              (add-tracer (horizon-tracer H16))
                                              (horizon-intersection H16))
                                             (make-horizon
                                              (add-tracer (horizon-tracer H17))
                                              (horizon-intersection H17))
                                             (make-horizon
                                              (add-tracer (horizon-tracer H18))
                                              (horizon-intersection H18))))) 
                                             
              
#;
(define (find-horizon L1) L1) ;stub

(@template Sight)

(define (find-horizon s) 
  (local [(define (fn-for-sight s)
            (cond [(horizon? s)
                   (make-horizon (add-tracer (horizon-tracer s))
                                 (horizon-intersection s))]
                  [else (make-level (level-quadrant s)
                                    (fn-for-los (level-los s)))]))
            
          (define (fn-for-los s)
            (cond [(empty? s) empty]
                  [else (cons (fn-for-sight (first s))
                              (fn-for-los (rest s)))]))]
    (fn-for-sight s)))




(@htdf add-tracer)
(@signature Tracer -> Tracer)
;; add tdx and tdy to tx and ty

(check-expect (add-tracer T1) (make-tracer (+ (tracer-tx T1) (tracer-tdx T1))
                                           (+ (tracer-ty T1) (tracer-tdy T1))
                                           (tracer-tdx T1)
                                           (tracer-tdy T1)))
(check-expect (add-tracer T2) (make-tracer (+ (tracer-tx T2) (tracer-tdx T2))
                                           (+ (tracer-ty T2) (tracer-tdy T2))
                                           (tracer-tdx T2)
                                           (tracer-tdy T2)))
(check-expect (add-tracer T3) (make-tracer (+ (tracer-tx T3) (tracer-tdx T3))
                                           (+ (tracer-ty T3) (tracer-tdy T3))
                                           (tracer-tdx T3)
                                           (tracer-tdy T3)))

#;
(define (add-tracer T3) T3) ;stub

(define (add-tracer f)
  (local [(define (fn-for-tracer f)
            (make-tracer (+ (tracer-tx f) (tracer-tdx f))
                         (+ (tracer-ty f) (tracer-tdy f))
                         (tracer-tdx f)
                         (tracer-tdy f)))]
    (fn-for-tracer f)))





(@htdf move-intersection)
(@signature World -> World)
;; change the ix and iy of intersection in each branch to new intersection point
(check-expect (move-intersection W0)
              (make-world (world-person W0)
                          (collision (world-sight W0)(world-map W0)(world-person W0))
                          (world-map W0)))
#;
(define (move-intersection w) w) ;stub

(@template World)

(define (move-intersection w)
  (local [(define (fn-for-world w)
            (make-world
             (world-person w)
             (collision (world-sight w) (world-map w) (world-person w))
             (world-map w)))]
    (fn-for-world w)))


(@htdf collision)
(@signature Sight Map -> Sight)
;; change intersection ix and iy to tx and ty at collision point
(check-expect (collision (make-level "testing" (list (make-horizon
                                                      (make-tracer 50 50 1 1)
                                                      (make-intersection 0 0))
                                                     (make-horizon
                                                      (make-tracer 200 200 1 1)
                                                      (make-intersection 0 0))))
                         (list (make-obstacle
                                50 50 (square 10 "solid" "black"))
                               (make-obstacle
                                0  0  (square 20 "solid" "black")))
                         P0)
              (make-level "testing" (list (make-horizon
                                           (make-tracer (person-px P0)
                                                        (person-py P0) 1 1)
                                           (make-intersection 50 50))
                                          (make-horizon
                                           (make-tracer 200 200 1 1)
                                           (make-intersection 0 0))))) 
(check-expect (collision (make-level "testing" (list (make-horizon
                                                      (make-tracer 50 50 1 1)
                                                      (make-intersection 0 0))
                                                     (make-horizon
                                                      (make-tracer 200 200 1 1)
                                                      (make-intersection 0 0))))
                         (list (make-obstacle
                                0  0 (square 10 "solid" "black"))
                               (make-obstacle
                                200 200  (square 20 "solid" "black")))
                         P0)
              (make-level "testing" (list (make-horizon
                                           (make-tracer 50 50 1 1)
                                           (make-intersection 0 0))
                                          (make-horizon
                                           (make-tracer (person-px P0)
                                                        (person-py P0) 1 1)
                                           (make-intersection 200 200)))))
(check-expect (collision (make-level "testing" (list (make-horizon
                                                      (make-tracer 50 50 1 1)
                                                      (make-intersection 0 0))
                                                     (make-horizon
                                                      (make-tracer 200 200 1 1)
                                                      (make-intersection 0 0))))
                         (list (make-obstacle
                                50  50 (square 10 "solid" "black"))
                               (make-obstacle
                                200 200  (square 20 "solid" "black")))
                         P0)
              (make-level "testing" (list (make-horizon
                                           (make-tracer (person-px P0)
                                                        (person-py P0) 1 1)
                                           (make-intersection 50 50))
                                          (make-horizon
                                           (make-tracer (person-px P0)
                                                        (person-py P0) 1 1)
                                           (make-intersection 200 200)))))
#;
(define (collision s m p) s) ;stub

(@template Sight)

(define (collision s m p) 
  (local [(define (fn-for-sight s)
            (cond [(horizon? s)
                   (if  (intersects? (horizon-tracer s) m)
                        (make-horizon (make-tracer (person-px p)
                                                   (person-py p)
                                                   (tracer-tdx (horizon-tracer s))
                                                   (tracer-tdy (horizon-tracer s)))
                                      (change-intersection
                                       (horizon-intersection s)
                                       (horizon-tracer s)))
                        (make-horizon (horizon-tracer s)
                                      (horizon-intersection s)))]
                  
                  [else (make-level (level-quadrant s)
                                    (fn-for-los (level-los s)))]))
            
          (define (fn-for-los s)
            (cond [(empty? s) empty]
                  [else (cons (fn-for-sight (first s))
                              (fn-for-los (rest s)))]))]
    (fn-for-sight s)))

(@htdf intersects?)
(@signature Tracer Map -> Boolean)
;; checks tracer against all obstacles in map, if collides then true
(check-expect (intersects? (make-tracer 50 50 1 1) (list (make-obstacle
                                                          50
                                                          50
                                                          (square 10 "solid"
                                                                  "black"))
                                                         (make-obstacle
                                                          200
                                                          200
                                                          (square 10 "solid"
                                                                  "black"))))
              true)
(check-expect (intersects? (make-tracer 200 200 1 1) (list (make-obstacle
                                                            50
                                                            50
                                                            (square 10 "solid"
                                                                    "black"))
                                                           (make-obstacle
                                                            200
                                                            200
                                                            (square 10 "solid"
                                                                    "black"))))
              true)
(check-expect (intersects? (make-tracer 100 100 1 1) (list (make-obstacle
                                                            50
                                                            50
                                                            (square 10 "solid"
                                                                    "black"))
                                                           (make-obstacle
                                                            200
                                                            200
                                                            (square 10 "solid"
                                                                    "black"))))
              false)
(check-expect (intersects? (make-tracer 401 100 1 1) (list (make-obstacle
                                                            50
                                                            50
                                                            (square 10 "solid"
                                                                    "black"))
                                                           (make-obstacle
                                                            200
                                                            200
                                                            (square 10 "solid"
                                                                    "black"))))
              true)
(check-expect (intersects? (make-tracer 100 401 1 1) (list (make-obstacle
                                                            50
                                                            50
                                                            (square 10 "solid"
                                                                    "black"))
                                                           (make-obstacle
                                                            200
                                                            200
                                                            (square 10 "solid"
                                                                    "black"))))
              true)
(check-expect (intersects? (make-tracer 100 401 1 1) empty)
              true)
(check-expect (intersects? (make-tracer 100 201 1 1) empty)
              false)


#;                                                         
(define (intersects? t m) true) ;stub


(@template Map)

(define (intersects? t m)
  (local [(define (fn-for-map m)
            (cond [(empty? m) (touch-edge? t)]
                  [else (if (inside-obstacle? t (first m))
                            true
                            (fn-for-map (rest m)))]))]
    (fn-for-map m)))


(@htdf touch-edge?)
(@signature Tracer -> Boolean)
;;


(define (touch-edge? t)
  (or (>= (tracer-tx t) WIDTH)
      (<= (tracer-tx t) 0)
      (>= (tracer-ty t) HEIGHT)
      (<= (tracer-ty t) 0)))


(@htdf inside-obstacle?)
(@signature Tracer Obstacle -> Boolean)
;; if tracer is inside obstacle produce true
(check-expect (inside-obstacle? (make-tracer 100 100 1 1) (make-obstacle
                                                           100
                                                           100
                                                           (rectangle
                                                            20 50
                                                            "solid"
                                                            "black")))
              true)
(check-expect (inside-obstacle? (make-tracer 100 100 1 1) (make-obstacle
                                                           95
                                                           100
                                                           (rectangle
                                                            50 50
                                                            "solid"
                                                            "black")))
              true)
(check-expect (inside-obstacle? (make-tracer 100 100 1 1) (make-obstacle
                                                           10
                                                           10
                                                           (rectangle
                                                            20 50
                                                            "solid"
                                                            "black")))
              false)
(check-expect (inside-obstacle? (make-tracer 0 100 1 1) (make-obstacle
                                                         10
                                                         10
                                                         (rectangle
                                                          20 50
                                                          "solid"
                                                          "black")))
              true)
(check-expect (inside-obstacle? (make-tracer WIDTH 100 1 1) (make-obstacle
                                                             10
                                                             10
                                                             (rectangle
                                                              20 50
                                                              "solid"
                                                              "black")))
              true)
(check-expect (inside-obstacle? (make-tracer 50 0 1 1) (make-obstacle
                                                        10
                                                        10
                                                        (rectangle
                                                         20 50
                                                         "solid"
                                                         "black")))
              true)
(check-expect (inside-obstacle? (make-tracer 50 HEIGHT 1 1) (make-obstacle
                                                             10
                                                             10
                                                             (rectangle
                                                              20 50
                                                              "solid"
                                                              "black")))
              true)
(check-expect (inside-obstacle? (make-tracer 50 HEIGHT 1 1) (make-obstacle
                                                             10
                                                             10
                                                             (rectangle
                                                              20 50
                                                              "solid"
                                                              "black")))
              true)


#;
(define (inside-obstacle? t o) true) ;stub

(@template Tracer Obstacle)

(define (inside-obstacle? t o)
  (local [(define (fn-for-tracer t)
            (or (and (< (- (obstacle-ox o)
                           (/ (image-width (obstacle-img o)) 2))
                        (tracer-tx t)
                        (+ (obstacle-ox o)
                           (/ (image-width (obstacle-img o)) 2)))
                     (< (- (obstacle-oy o)
                           (/ (image-height (obstacle-img o)) 2))
                        (tracer-ty t)
                        (+ (obstacle-oy o)
                           (/ (image-height (obstacle-img o)) 2))))

                (>= (tracer-tx t) WIDTH)
                (<= (tracer-tx t) 0)
                (>= (tracer-ty t) HEIGHT)
                (<= (tracer-ty t) 0)))]
    (fn-for-tracer t)))





(@htdf change-intersection)
(@signature Intersection Tracer -> Intersection)
;; change ix and iy to tx and ty
(check-expect (change-intersection (make-intersection 0 0)
                                   (make-tracer 50 50 1 1))
              (make-intersection 50 50))
(check-expect (change-intersection (make-intersection 75 60)
                                   (make-tracer 150 160 1 1))
              (make-intersection 150 160))

#;
(define (change-intersection i t) i) ;stub

(@template Intersection Tracer)

(define (change-intersection i t)
  (local [(define (fn-for-intersection i)
           

            (make-intersection (tracer-tx t)
                               (tracer-ty t)))]
    (fn-for-intersection i)))
  




(@htdf render)   ;; SUPER IMPORTANT -> THIS IS WHERE THE LINE WILL BE ADDED
(@signature World -> Image)
;; render ... 
(check-expect (render (make-world P0
                                  (make-level "hi"
                                              (list
                                               (make-horizon
                                                T1
                                                (make-intersection 50 50))
                                               (make-horizon
                                                T1
                                                (make-intersection 25 25))))
                                  (list obs1)))
              (add-line (add-line (place-image PERSON
                                               (person-px P0)
                                               (person-py P0)
                                               (place-image (obstacle-img obs1)
                                                            (obstacle-ox obs1)
                                                            (obstacle-oy obs1)
                                                            MTS))
                                  (person-px P0)
                                  (person-py P0)
                                  50
                                  50
                                  "black")
                        (person-px P0)
                        (person-py P0)
                        25
                        25
                        "black")) 
(check-expect (render W0)
              (put-lines-in W0
                            (place-person W0
                                          (place-obstacles W0))))
                                                                                                            

#;
(define (render w) empty-image) ;stub

(@template fn-composition World)

(define (render w)
  (put-lines-in w (place-person w (place-obstacles w))))


(@htdf place-obstacles)
(@signature World -> Image)
;; put all obstacles on MTS with correct ox and oy
;; !!!
(check-expect (place-obstacles (make-world P0
                                           (make-level "hi"
                                                       (list
                                                        (make-horizon
                                                         (make-tracer 50 50 1 1)
                                                         I1)
                                                        (make-horizon
                                                         (make-tracer 25 25 1 1)
                                                         I2))) 
                                           (list obs1 obs2)))
              (place-image (obstacle-img obs1) 
                           (obstacle-ox obs1)
                           (obstacle-oy obs1)
                           (place-image (obstacle-img obs2) 
                                        (obstacle-ox obs2)
                                        (obstacle-oy obs2)
                                        MTS)))

#;
(define (place-obstacles w) img) ;stub

(@template World)

(define (place-obstacles w)
  (local [(define (fn-for-world w)
            (each-obstacle (world-map w)))]
    (fn-for-world w)))


(define (each-obstacle m)
  (local [(define (fn-for-map m)
            (cond [(empty? m) MTS]
                  [else (place-image
                         (obstacle-img (first m))
                         (obstacle-ox (first m))
                         (obstacle-oy (first m))
                         (fn-for-map (rest m)))]))]
    (fn-for-map m)))






(@htdf place-person)
(@signature World Image -> Image)
;; place person on world
(check-expect (place-person (make-world P0
                                        (make-level "hi"
                                                    (list
                                                     (make-horizon
                                                      (make-tracer 50 50 1 1)
                                                      I1)
                                                     (make-horizon
                                                      (make-tracer 25 25 1 1)
                                                      I2))) 
                                        (list obs1 obs2))
                            (place-image (obstacle-img obs1) 
                                         (obstacle-ox obs1)
                                         (obstacle-oy obs1)
                                         (place-image (obstacle-img obs2) 
                                                      (obstacle-ox obs2)
                                                      (obstacle-oy obs2)
                                                      MTS)))
              (place-image PERSON
                           (person-px P0)
                           (person-py P0)
                           (place-image (obstacle-img obs1) 
                                        (obstacle-ox obs1)
                                        (obstacle-oy obs1)
                                        (place-image (obstacle-img obs2) 
                                                     (obstacle-ox obs2)
                                                     (obstacle-oy obs2)
                                                     MTS))))
#;
(define (place-person w img) img) ;stub

(@template World)

(define (place-person w img)
  (local [(define (fn-for-world w)
            (put-person (world-person w) img))]
    (fn-for-world w)))


(define (put-person p img)
  (place-image PERSON
               (person-px p)
               (person-py p)
               img))
               
                    



(@htdf put-lines-in)
(@signature World Image -> Image)
;; add lines into world by connecting to horizon intersections
(check-expect (put-lines-in (make-world P0
                                        (make-level "hi"
                                                    (list
                                                     (make-horizon
                                                      T1
                                                      (make-intersection 100 100))
                                                     (make-horizon
                                                      T2
                                                      (make-intersection 150 150)))) 
                                        (list obs1 obs2))
                            (place-image PERSON
                                         (person-px P0)
                                         (person-py P0)
                                         (place-image (obstacle-img obs1) 
                                                      (obstacle-ox obs1)
                                                      (obstacle-oy obs1)
                                                      (place-image (obstacle-img obs2) 
                                                                   (obstacle-ox obs2)
                                                                   (obstacle-oy obs2)
                                                                   MTS))))
              (add-line (add-line (place-image PERSON
                                               (person-px P0)
                                               (person-py P0)
                                               (place-image (obstacle-img obs1) 
                                                            (obstacle-ox obs1)
                                                            (obstacle-oy obs1)
                                                            (place-image (obstacle-img obs2) 
                                                                         (obstacle-ox obs2)
                                                                         (obstacle-oy obs2)
                                                                         MTS)))
                                  (person-px P0)
                                  (person-py P0)
                                  100
                                  100
                                  "black")
                        (person-px P0)
                        (person-py P0)
                        150
                        150
                        "black"))

#;
(define (put-lines-in w img) img) ;stub

(@template World)

(define (put-lines-in w img)
  (place-lines (make-list-intersection (world-sight w))
               (world-person w)
               img))

(@template Sight)

(@htdf make-list-intersection)
;(@signature Sight -> ListOfIntersection)
;; ...
(check-expect (make-list-intersection L2) (list I1 I2 I3 I4 I5 I6))
(check-expect (make-list-intersection L3) (list I7 I8 I9 I10 I11 I12))
(check-expect (make-list-intersection L1) (list I1 I2 I3 I4 I5 I6
                                                I7 I8 I9 I10 I11 I12
                                                I13 I14 I15 I16 I17 I18
                                                I19 I20 I21 I22 I23 I24))


(define (make-list-intersection s) 
  (local [(define (fn-for-sight s)
            (cond [(horizon? s)
                   (list (horizon-intersection s))]
                  [else 
                   (fn-for-los (level-los s))]))
            
          (define (fn-for-los s)
            (cond [(empty? s) empty]
                  [else (append (fn-for-sight (first s))
                                (fn-for-los (rest s)))]))]
    (fn-for-sight s)))

(@htdf place-lines)
;(@signature ListOfIntersection Person Image -> Image)
(check-expect (place-lines (list (make-intersection 50 25)
                                 (make-intersection 200 225))
                           P0
                           MTS)
              (add-line (add-line MTS
                                  (person-px P0)
                                  (person-py P0)
                                  50
                                  25
                                  "black")
                        (person-px P0)
                        (person-py P0)
                        200
                        225
                        "black"))
                       
(define (place-lines loi p img)
  (cond [(empty? loi) img]
        [else (place-lines (rest loi) p (add-line img
                                                  (person-px p)
                                                  (person-py p)
                                                  (intersection-ix (first loi))
                                                  (intersection-iy (first loi))
                                                  "black"))]))


(@htdf mouse-handle)
(@signature World Number Number MouseEvent -> World)
;; dragging will move person


(@template MouseEvent)

(define (mouse-handle w x y me)
  (cond [(mouse=? me "button-up") (make-world (make-person
                                               x y)
                                              (world-sight w)
                                              (world-map w))]
        [else
         w])) 








(@htdf key-handle)
(@signature World KeyEvent -> World)
;; WASD moves person
(check-expect (key-handle W0 "w") (make-world (make-person (person-px (world-person W0))
                                                           (- (person-py (world-person W0)) PDY))
                                              (world-sight W0)
                                              (world-map W0)))
(check-expect (key-handle W0 "s") (make-world (make-person (person-px (world-person W0))
                                                           (+ (person-py (world-person W0)) PDY))
                                              (world-sight W0)
                                              (world-map W0)))
(check-expect (key-handle W0 "a") (make-world (make-person (- (person-px (world-person W0)) PDX)
                                                           (person-py (world-person W0)))
                                              (world-sight W0)
                                              (world-map W0)))
(check-expect (key-handle W0 "d") (make-world (make-person (+ (person-px (world-person W0)) PDX)
                                                           (person-py (world-person W0)))
                                              (world-sight W0)
                                              (world-map W0)))

#;
(define (key-handle w ke) w)

(@template KeyEvent)


(define (key-handle w ke)
  (cond [(key=? ke "w") (make-world (make-person (person-px (world-person w))
                                                 (- (person-py (world-person w)) PDY))
                                    (world-sight w)
                                    (world-map w))]
        [(key=? ke "a") (make-world (make-person (- (person-px (world-person w)) PDX)
                                                 (person-py (world-person w)))
                                    (world-sight w)
                                    (world-map w))]
        [(key=? ke "s") (make-world (make-person (person-px (world-person w))
                                                 (+ (person-py (world-person w)) PDY))
                                    (world-sight w)
                                    (world-map w))]
        [(key=? ke "d") (make-world (make-person (+ (person-px (world-person w)) PDX)
                                                 (person-py (world-person w)))
                                    (world-sight w)
                                    (world-map w))]
        [else 
         w]))
