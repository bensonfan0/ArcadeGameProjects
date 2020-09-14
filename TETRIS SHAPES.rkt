;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |TETRIS SHAPES|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require spd/tags)
(require 2htdp/image)
(require 2htdp/universe)

;; TETRIS
;; Players control a falling block (one of six different shapes) and can rotate clockwise (except square) using "w" 
;; and can move in x-direction using "a" or "d". "s" will move it downwards.
;; Goal is to stack the block according to shapes and fill in all gaps. As soon as an entire row
;; is filled (no empty spaces) remove just that row. All blocks above will fall down one row.
;; If blocks fill up to top of screen -> game ends and score board fills screen
;; Objective:
;;  - when a row is removed add score
;;  - when multiple rows are removed simultaneously add score and multiply according to number of
;;    rows removed



;; CONSTANTS
(define SPEED-X 20)
(define SPEED-Y 20) 

(define S-SPEED 20) ;only multiples of 20 (blocks are 20 in width)

(define RUSH-SPEED 20)

(define WIDTH 180) ; multiples of 20
(define HEIGHT 400) ; multiples of 20
(define MTS (rectangle WIDTH HEIGHT "solid" "azure"))


(define CTR-X (/ WIDTH 2))
(define CTR-Y (/ HEIGHT 2))

(define TEXT-COLOR "black")
(define TEXT-SIZE 15)

(define b-block (overlay (square 18 "solid" "cyan") (square 20 "solid" "black")))

(define y-block (overlay (square 18 "solid" "medium yellow") (square 20 "solid" "black")))

(define o-block (overlay (square 18 "solid" "dark orange") (square 20 "solid" "black")))

(define p-block (overlay (square 18 "solid" "blue violet") (square 20 "solid" "black")))

(define r-block (overlay (square 18 "solid" "crimson") (square 20 "solid" "black")))

(define g-block (overlay (square 18 "solid" "lime green") (square 20 "solid" "black")))

(define db-block (overlay (square 18 "solid" "navy") (square 20 "solid" "black")))



;; =================
;; Data definitions:

(@htdd World)
(define-struct world (lob dy score))
;; World is (make-world ListOfBlock Number Number Integer)
;; interp. all moving parts of the world
(define start (make-world empty SPEED-Y 0)) ;no blocks, moving down 10, and no points

(@dd-template-rules compound)

(define (fn-for-world w)
  (... (world-lob w)
       (world-dy w)
       (world-score w)))

(@htdd Block)
(define-struct block (img x y))
;; Block is (make-block Image Number Number)
;; interp. position of a block
(define b1 (make-block b-block 0 0))

(@dd-template-rules compound)

(define (fn-for-block b)
  (... (block-color b)
       (block-x b)
       (block-y b)))

(@htdd ListOfBlock)
;; ListOfBlock is one of
;;    - empty
;;    - (cons Block ListOfBlock)
;; interp. a list of blocks in world
(define lob1 empty)
(define lob2 (cons (make-block b-block 0 0) (cons (make-block b-block 10 10) empty)))

(@dd-template-rules one-of
                    atomic-distinct
                    compound
                    ref
                    self-ref)

(define (fn-for-list-of-block lob)
  (cond [(empty? lob) (...)]
        [else (... (fn-for-block (first lob))
                   (fn-for-list-of-block (rest lob)))]))

                    
;;;;;;;; LINE SHAPE ;;;;;;;


;example of line block

(place-image b-block
             CTR-X
             70
             (place-image b-block
                          CTR-X
                          50
                          (place-image b-block
                                       CTR-X
                                       30
                                       (place-image b-block
                                                    CTR-X
                                                    10
                                                    MTS))))


;; spawn the shape at CTR-X
(cons (make-block b-block CTR-X -70)
      (cons (make-block b-block CTR-X -50)
            (cons (make-block b-block CTR-X -30)
                  (cons (make-block  b-block CTR-X -10) empty)))) ;; THIS LIST IS LINE -> spawns off screen

;;;;;;; SQUARE SHAPE ;;;;;;

(place-image y-block
             (- CTR-X 10)
             10
             (place-image y-block
                          (- CTR-X 10)
                          30
                          (place-image y-block
                                       (+ CTR-X 10)
                                       10
                                       (place-image y-block
                                                    (+ CTR-X 10)
                                                    30
                                                    MTS))))
;; spawn the square shape
(cons (make-block y-block CTR-X -10)
      (cons (make-block y-block CTR-X -30)
            (cons (make-block y-block (+ CTR-X 20) -10)
                  (cons (make-block y-block (+ CTR-X 20) -30) empty))))

;;;;;;;;; L SHAPE ;;;;;;;;;;
                  
(place-image o-block
             (- CTR-X 20)
             50
             (place-image o-block
                          CTR-X
                          50
                          (place-image o-block
                                       CTR-X
                                       30
                                       (place-image o-block
                                                    CTR-X 
                                                    10
                                                    MTS))))

(cons (make-block o-block (- CTR-X 20) -50)
      (cons (make-block o-block CTR-X -50)
            (cons (make-block o-block CTR-X -30)
                  (cons (make-block o-block CTR-X -10) empty))))

;;;; THE OTHER L SHAPE ;;;;;

(cons (make-block db-block (+ CTR-X 20) -50)
      (cons (make-block db-block CTR-X -50)
            (cons (make-block db-block CTR-X -30)
                  (cons (make-block db-block CTR-X -10) empty))))


                                                    
                          
;;;;;;;;; T BLOCK ;;;;;;;;;;;




(place-image p-block
             CTR-X
             30
             (place-image p-block
                          (- CTR-X 20)
                          30
                          (place-image p-block
                                       (+ CTR-X 20)
                                       30
                                       (place-image p-block
                                                    CTR-X
                                                    10
                                                    MTS))))

(cons (make-block p-block CTR-X 30)
      (cons (make-block p-block (- CTR-X 20) 30)
            (cons (make-block p-block (+ CTR-X 20) 30)
                  (cons (make-block p-block CTR-X 10) empty))))
                                       

;;;;;;;;; Z BLOCK ;;;;;;;;;;;;


(place-image r-block
             CTR-X
             10
             (place-image r-block
                          (- CTR-X 20)
                          10
                          (place-image r-block
                                       CTR-X
                                       30
                                       (place-image r-block
                                                    (+ CTR-X 20)
                                                    30
                                                    MTS))))

(cons (make-block r-block CTR-X 10)
      (cons (make-block r-block (- CTR-X 20) 10)
            (cons (make-block r-block CTR-X 30)
                  (cons (make-block r-block (+ CTR-X 20) 30) empty))))


;;;;;; THE OTHER Z BLOCK ;;;;;

(cons (make-block g-block CTR-X 10)
      (cons (make-block g-block (+ CTR-X 20) 10)
            (cons (make-block g-block CTR-X 30)
                  (cons (make-block g-block (- CTR-X 20) 30) empty))))



;; =================
;; Functions:

(@htdf main)
(@signature World -> World)
;; start the world with (main start)
;; 

(@template htdw-main)

(define (main w)
  (big-bang w                            ; World
    (on-tick   next-world (/ 1 2))      ; World -> World
    (to-draw   render)                   ; World -> Image
    (stop-when game-over)                ; World -> Boolean 
    (on-key    handle-key)))             ; World KeyEvent -> World


(@htdf render)
(@signature World -> Image)
;; render the world and all the blocks given x and y
(check-expect (render (make-world empty SPEED-Y 0)) (place-image (text (number->string 0) TEXT-SIZE TEXT-COLOR)
                                                                 (- WIDTH 20)
                                                                 20
                                                                 MTS))
(check-expect (render (make-world (cons (make-block b-block CTR-X 70)
                                        (cons (make-block b-block CTR-X 50)
                                              (cons (make-block b-block CTR-X 30)
                                                    (cons (make-block  b-block CTR-X 10) empty)))) SPEED-Y 0))
              (place-image (text (number->string 0) TEXT-SIZE TEXT-COLOR)
                           (- WIDTH 20)
                           20
                           (place-image b-block
                                        CTR-X
                                        70
                                        (place-image b-block
                                                     CTR-X
                                                     50
                                                     (place-image b-block
                                                                  CTR-X
                                                                  30
                                                                  (place-image b-block
                                                                               CTR-X
                                                                               10
                                                                               MTS))))))
                              

#;
(define (render w) empty-image)

(define (render w)
  (place-image (text (number->string (world-score w)) TEXT-SIZE TEXT-COLOR)
               (- WIDTH 20)
               20
               (insert-lob-image (world-lob w))))

(@htdf insert-lob-image)
(@signature ListOfBlock -> Image)
;; create image from listofblock
(check-expect (insert-lob-image empty) MTS)
(check-expect (insert-lob-image (cons (make-block b-block CTR-X 70)
                                      (cons (make-block b-block CTR-X 50)
                                            (cons (make-block b-block CTR-X 30)
                                                  (cons (make-block  b-block CTR-X 10) empty)))))
              (place-image b-block
                           CTR-X
                           70
                           (place-image b-block
                                        CTR-X
                                        50
                                        (place-image b-block
                                                     CTR-X
                                                     30
                                                     (place-image b-block
                                                                  CTR-X
                                                                  10
                                                                  MTS)))))
#;
(define (insert-lob-image lob) empty-scene) ;stub

(@template ListOfBlock)

(define (insert-lob-image lob)
  (cond [(empty? lob) MTS]
        [else (shape-image (first lob)
                           (insert-lob-image (rest lob)))]))

(@htdf shape-image)
(@signature Block Image -> Image)
;; place the images based on x and y on another image
(check-expect (shape-image (make-block b-block 30 30) MTS) (place-image b-block
                                                                        30
                                                                        30
                                                                        MTS))

#;
(define (shape-image b img) img) ;stub

(@template Block)

(define (shape-image b img)
  (place-image (block-img b)
               (block-x b)
               (block-y b)
               img))



               


(@htdf next-world)
(@signature World -> World)
;; produce the next world
;;   - when ListOfBlock is empty add a random shape
;;   - check every time a shape touches another shape add another shape (random shape)
;;   - check list to see if there's a full row of shapes with same y-coord
;;   - removes the row with the same y-coord and adds score to score-board
;(check-expect (next-world (make-world empty SPEED-Y 0)) (add-block (make-world empty SPEED-Y 0)))

(check-expect (next-world (make-world (cons (make-block r-block CTR-X 10)
                                            (cons (make-block r-block (- CTR-X 20) 50)
                                                  (cons (make-block r-block CTR-X 30)
                                                        (cons (make-block r-block (+ CTR-X 20) 30) empty)))) SPEED-Y 0))
              (descent (make-world (cons (make-block r-block CTR-X 10)
                                         (cons (make-block r-block (- CTR-X 20) 50)
                                               (cons (make-block r-block CTR-X 30)
                                                     (cons (make-block r-block (+ CTR-X 20) 30) empty)))) SPEED-Y 0)))

(check-expect (next-world (make-world (cons (make-block r-block CTR-X 10) 
                                            (cons (make-block r-block (- CTR-X 20) 10)
                                                  (cons (make-block r-block CTR-X 30)
                                                        (cons (make-block r-block (+ CTR-X 20) 30)
                                                              (cons (make-block r-block CTR-X 10)
                                                                    (cons (make-block r-block (- CTR-X 20) 10)
                                                                          (cons (make-block r-block CTR-X 30)
                                                                                (cons (make-block r-block (+ CTR-X 20) 30) empty)))))))) SPEED-Y 0)) ;touching another block
              (add-block (make-world (cons (make-block r-block CTR-X 10)
                                           (cons (make-block r-block (- CTR-X 20) 10)
                                                 (cons (make-block r-block CTR-X 30)
                                                       (cons (make-block r-block (+ CTR-X 20) 30)
                                                             (cons (make-block r-block CTR-X 10)
                                                                   (cons (make-block r-block (- CTR-X 20) 10)
                                                                         (cons (make-block r-block CTR-X 30)
                                                                               (cons (make-block r-block (+ CTR-X 20) 30) empty)))))))) SPEED-Y 0)))

#;
(define (next-world w) w) ;stub

(@template World)

(define (next-world w)
  (cond [(touch? (world-lob w)) (if (full-row? (world-lob w))
                                    (remove-which-row  w)
                                    (add-block w))]
        [else (descent w)]))

(@htdf touch?)
(@signature ListOfBlock -> Boolean)
;; produce true if any of first four cons are touching x and y (diff of 20 units) rest of list or touching bot of screen
;; CONSTRAINT: will always have empty or 4 blocks + empty
(check-expect (touch? empty) true)
(check-expect (touch? (cons (make-block b-block CTR-X CTR-Y)
                            (cons (make-block b-block CTR-X CTR-Y)
                                  (cons (make-block b-block CTR-X CTR-Y)
                                        (cons (make-block b-block CTR-X CTR-Y) empty))))) false)

(check-expect (touch? (cons (make-block b-block CTR-X (- HEIGHT 20))
                            (cons (make-block b-block CTR-X (- HEIGHT 20))
                                  (cons (make-block b-block CTR-X (- HEIGHT 20))
                                        (cons (make-block b-block CTR-X (- HEIGHT 10)) empty))))) true) ;touch bottom

(check-expect (touch? (cons (make-block r-block CTR-X 10) 
                            (cons (make-block r-block (- CTR-X 20) 10)
                                  (cons (make-block r-block CTR-X 30)
                                        (cons (make-block r-block (+ CTR-X 20) 30)
                                              (cons (make-block r-block (- CTR-X 20) (- HEIGHT 10)) empty)))))) false) ;first four blocks are not touching bottom

(check-expect (touch? (cons (make-block r-block CTR-X 10) 
                            (cons (make-block r-block (- CTR-X 20) 10)
                                  (cons (make-block r-block CTR-X 30)
                                        (cons (make-block r-block (+ CTR-X 20) 30)
                                              (cons (make-block r-block (- CTR-X 40) (- HEIGHT 20)) empty)))))) false)

(check-expect (touch? (cons (make-block r-block CTR-X 10) 
                            (cons (make-block r-block (- CTR-X 20) 10)
                                  (cons (make-block r-block CTR-X 30)
                                        (cons (make-block r-block (+ CTR-X 20) 30)
                                              (cons (make-block r-block (- CTR-X 40) (- HEIGHT 10)) empty)))))) false)


(check-expect (touch? (cons (make-block r-block (+ CTR-X 20) 30) 
                            (cons (make-block r-block CTR-X 10)
                                  (cons (make-block r-block (- CTR-X 20) (- HEIGHT 10))
                                        (cons (make-block r-block CTR-X 30)
                                              (cons (make-block r-block (+ CTR-X 20) (- HEIGHT 10)) empty)))))) true) ;one of the four blocks are touching bottom
 
#;
(define (touch? lob) true) ;stub

(@template Block)

;; only looks at first four in list 

(define (touch? lob)
  (cond [(empty? lob) true]
        [(>= (block-y (first lob)) (- HEIGHT 10)) true]
        [(>= (block-y (second lob)) (- HEIGHT 10)) true]
        [(>= (block-y (third lob)) (- HEIGHT 10)) true]
        [(>= (block-y (fourth lob)) (- HEIGHT 10)) true]

        [(touch-other? lob) true]
        [else false]))



(@htdf touch-other?)
(@signature ListOfBlock -> Boolean)
;; check if first four blocks touch any other blocks in list (same x and (y - 20))
;; CONSTRAINT: List will always have empty or atleast 4 blocks + empty
(check-expect (touch-other? empty) false)

(check-expect (touch-other? (cons (make-block r-block (+ CTR-X 20) 30)
                                  (cons (make-block r-block CTR-X 10)
                                        (cons (make-block r-block (- CTR-X 20) 10)
                                              (cons (make-block r-block CTR-X (- HEIGHT 30)) 
                                                    (cons (make-block r-block CTR-X (- HEIGHT 10)) empty)))))) true) ;one of the four blocks are touching another block in list

(check-expect (touch-other? (cons (make-block r-block (+ CTR-X 20) 30)
                                  (cons (make-block r-block CTR-X 10)
                                        (cons (make-block r-block (- CTR-X 20) 10)
                                              (cons (make-block r-block CTR-X (- HEIGHT 40))  
                                                    (cons (make-block r-block CTR-X (- HEIGHT 10)) empty)))))) false) ;no blocks are touching



#;
(define (touch-other? lob) true) ;stub

(@template Block)

(define (touch-other? lob)
  (cond [(empty? lob) false]
        [(any-touch?  (first lob)
                      (rest (rest (rest (rest lob))))) true]
        [(any-touch?  (second lob)
                      (rest (rest (rest (rest lob))))) true]
        [(any-touch?  (third lob)
                      (rest (rest (rest (rest lob))))) true]
        [(any-touch?  (fourth lob)
                      (rest (rest (rest (rest lob))))) true]
        [else false]))


(@htdf any-touch?)
(@signature Block ListOfBlock -> Boolean)
;; checks if any of the four blocks touch rest of list blocks (same x and (y - 20))
;; CONSTRAINT: will always receive four blocks
(check-expect (any-touch?  (make-block b-block CTR-X CTR-Y)
                           empty) false)
(check-expect (any-touch?  (make-block b-block (+ CTR-X 60) (+ CTR-Y 60))
                           (cons (make-block b-block CTR-X (+ CTR-Y 100)) empty)) false) 
(check-expect (any-touch?  (make-block b-block (+ CTR-X 60) (+ CTR-Y 51))
                           (cons (make-block b-block (+ CTR-X 60) (+ CTR-Y 70)) empty)) true)
(check-expect (any-touch?  (make-block b-block (+ CTR-X 40) (+ CTR-Y 55))
                           (cons (make-block b-block (+ CTR-X 40) (+ CTR-Y 70)) empty)) true)
(check-expect (any-touch?  (make-block b-block (+ CTR-X 20) (+ CTR-Y 80))
                           (cons (make-block b-block (+ CTR-X 20) (+ CTR-Y 80)) empty)) true)
(check-expect (any-touch?  (make-block b-block (+ CTR-X 20) (+ CTR-Y 120))
                           (cons (make-block b-block (+ CTR-X 20) (+ CTR-Y 80)) empty)) false)


#;
(define (any-touch? b lob) true) ;stub

(@template Block)

(define (any-touch? b lob)
  (cond [(empty? lob) false]
        [else (if (and (<= (block-y b) (+ (block-y (first lob)) 19))       ; so if below a block it will not register as touching

                       (>= (block-y b) (- (block-y (first lob)) 20))       ; (first lob) will be below block so if moving block is within 20 units then true
                          
                       (= (block-x b) (block-x (first lob))))
                  true
                  (any-touch? b (rest lob)))]))
 


(@htdf full-row?)
(@signature ListOfBlock -> Boolean)
;; if the list has (/ WIDTH 20) list of blocks in same y-coord ((every 20 pixels) + 10) produce true
(check-expect (full-row? empty) false)
(check-expect (full-row? (cons (make-block b-block 10 (- HEIGHT 10))
                               (cons (make-block b-block 30 (- HEIGHT 10))
                                     (cons (make-block b-block 50 (- HEIGHT 10))
                                           (cons (make-block b-block 70 (- HEIGHT 10))
                                                 (cons (make-block b-block 90 (- HEIGHT 10))
                                                       (cons (make-block b-block 110 (- HEIGHT 10))
                                                             (cons (make-block b-block 130 (- HEIGHT 10))
                                                                   (cons (make-block b-block 150 (- HEIGHT 10))
                                                                         (cons (make-block b-block 170 (- HEIGHT 10))
                                                                               (cons (make-block b-block 190 (- HEIGHT 10)) empty)))))))))))
              true)
(check-expect (full-row? (cons (make-block b-block 10 (- HEIGHT 30))
                               (cons (make-block b-block 30 (- HEIGHT 30))
                                     (cons (make-block b-block 50 (- HEIGHT 30))
                                           (cons (make-block b-block 70 (- HEIGHT 30))
                                                 (cons (make-block b-block 90 (- HEIGHT 30))
                                                       (cons (make-block b-block 110 (- HEIGHT 30))
                                                             (cons (make-block b-block 130 (- HEIGHT 30))
                                                                   (cons (make-block b-block 150 (- HEIGHT 30))
                                                                         (cons (make-block b-block 170 (- HEIGHT 30))
                                                                               (cons (make-block b-block 190 (- HEIGHT 30)) empty)))))))))))
              true)
(check-expect (full-row? (cons (make-block b-block 10 (- HEIGHT 30))
                               (cons (make-block b-block 30 (- HEIGHT 30))
                                     (cons (make-block b-block 50 (- HEIGHT 30))
                                           (cons (make-block b-block 70 (- HEIGHT 30))
                                                 (cons (make-block b-block 90 (- HEIGHT 30))
                                                       (cons (make-block b-block 170 (- HEIGHT 10))
                                                             (cons (make-block b-block 110 (- HEIGHT 30))
                                                                   (cons (make-block b-block 130 (- HEIGHT 30))
                                                                         (cons (make-block b-block 150 (- HEIGHT 30))
                                                                               (cons (make-block b-block 170 (- HEIGHT 30))
                                                                                     (cons (make-block b-block 190 (- HEIGHT 30)) empty))))))))))))
              true)
(check-expect (full-row? (cons (make-block b-block 10 (- HEIGHT 10))
                               (cons (make-block b-block 30 (- HEIGHT 10)) empty)))
              false)


#;                              
(define (full-row? lob) true) ;stub

(@template ListOfBlock)

(define (full-row? lob)
  (cond [(empty? lob) false]
        [else  (if (>= (equal-to? (first lob) (rest lob)) (- (/ WIDTH 20) 1))
                   true
                   (full-row? (rest lob)))]))


(@htdf equal-to?)
(@signature ListOfBlock -> Number)
;; if same y then add 1
(check-expect (equal-to? (make-block b-block 10 10) empty) 0)
(check-expect (equal-to? (make-block b-block 20 20) (cons (make-block b-block 20 20)
                                                          (cons (make-block b-block 40 20) empty))) 2)
(check-expect (equal-to? (make-block b-block 20 20) (cons (make-block b-block 20 20)
                                                          (cons (make-block b-block 40 30)
                                                                (cons (make-block b-block 60 20) empty)))) 2)
#;
(define (equal-to? lob) 0) ;stub

(@template ListOfBlock)
                

(define (equal-to? b lob)
  (cond [(empty? lob) 0]
        [else (if (= (block-y b) (block-y (first lob)))
                  (+ 1 (equal-to? b (rest lob)))
                  (equal-to? b (rest lob)))]))

(@htdf remove-which-row)
(@signature World -> World)
;; find which row is full and remove it and add score
(check-expect (remove-which-row (make-world empty SPEED-Y 0)) (make-world empty SPEED-Y 0))
(check-expect (remove-which-row (make-world (cons (make-block b-block 10 (- HEIGHT 30))
                                                  (cons (make-block b-block 30 (- HEIGHT 30))
                                                        (cons (make-block b-block 50 (- HEIGHT 30))
                                                              (cons (make-block b-block 70 (- HEIGHT 30))
                                                                    (cons (make-block b-block 90 (- HEIGHT 30))
                                                                          (cons (make-block b-block 170 (- HEIGHT 10))
                                                                                (cons (make-block b-block 110 (- HEIGHT 30))
                                                                                      (cons (make-block b-block 130 (- HEIGHT 30))
                                                                                            (cons (make-block b-block 150 (- HEIGHT 30))
                                                                                                  (cons (make-block b-block 170 (- HEIGHT 30))
                                                                                                        (cons (make-block b-block 190 (- HEIGHT 30)) empty))))))))))) SPEED-Y 0))
              (make-world (cons (make-block b-block 170 (- HEIGHT 10)) empty) SPEED-Y 100))

#;
(define (remove-which-row w) w) ;stub

(@template ListOfBlock)   ;; problem is that when it checks the first four and finds the y-value it doesn't pass the entire list along, only things after the block-y

(define (remove-which-row w)
  (cond [(empty? (world-lob w)) w]
        [else  (if (>= (equal-to? (first (world-lob w)) (rest (world-lob w))) (- (/ WIDTH 20) 1))
                   (make-world (remove-row-and-lower (first (world-lob w)) (world-lob w))  
                               (world-dy w)
                               (+ (world-score w) 100))
                       (if (>= (equal-to? (second (world-lob w)) (rest (world-lob w))) (- (/ WIDTH 20) 1))
                           (make-world (remove-row-and-lower (second (world-lob w)) (world-lob w))  
                               (world-dy w)
                               (+ (world-score w) 100))
                       (if (>= (equal-to? (third (world-lob w)) (rest (world-lob w))) (- (/ WIDTH 20) 1))
                           (make-world (remove-row-and-lower (third (world-lob w)) (world-lob w))  
                               (world-dy w)
                               (+ (world-score w) 100))
                       (if (>= (equal-to? (fourth (world-lob w)) (rest (world-lob w))) (- (/ WIDTH 20) 1))
                   (make-world (remove-row-and-lower (fourth (world-lob w)) (world-lob w))  
                               (world-dy w)
                               (+ (world-score w) 100))
                   w))))]))

 





(@htdf remove-row-and-lower)
(@signature Block ListOfBlock -> Boolean)
;; remove all blocks with same y and lower all blocks above y by 20 pixels
(check-expect (remove-row-and-lower (make-block b-block 40 50) empty) empty)
(check-expect (remove-row-and-lower (make-block b-block 30 (- HEIGHT 30))
                                    (cons (make-block b-block 10 (- HEIGHT 30))
                                          (cons (make-block b-block 30 (- HEIGHT 30))
                                                (cons (make-block b-block 50 (- HEIGHT 30))
                                                      (cons (make-block b-block 70 (- HEIGHT 30))
                                                            (cons (make-block b-block 90 (- HEIGHT 30))
                                                                  (cons (make-block b-block 170 (- HEIGHT 10))
                                                                        (cons (make-block b-block 110 (- HEIGHT 30))
                                                                              (cons (make-block b-block 130 (- HEIGHT 30))
                                                                                    (cons (make-block b-block 150 (- HEIGHT 30))
                                                                                          (cons (make-block b-block 170 (- HEIGHT 30))
                                                                                                (cons (make-block b-block 190 (- HEIGHT 30)) empty))))))))))))
                         
              (cons (make-block b-block 170 (- HEIGHT 10)) empty))
(check-expect (remove-row-and-lower (make-block b-block 30 (- HEIGHT 30))
                                    (cons (make-block b-block 10 (- HEIGHT 30))
                                          (cons (make-block b-block 30 (- HEIGHT 30))
                                                (cons (make-block b-block 50 (- HEIGHT 30))
                                                      (cons (make-block b-block 70 (- HEIGHT 30))
                                                            (cons (make-block b-block 90 (- HEIGHT 30))
                                                                  (cons (make-block b-block 170 (- HEIGHT 10))
                                                                        (cons (make-block b-block 110 (- HEIGHT 30))
                                                                              (cons (make-block b-block 130 (- HEIGHT 30))
                                                                                    (cons (make-block b-block 150 (- HEIGHT 30))
                                                                                          (cons (make-block b-block 170 (- HEIGHT 30))
                                                                                                (cons (make-block b-block 190 (- HEIGHT 30))
                                                                                                      (cons (make-block b-block 210 (- HEIGHT 60)) empty)))))))))))))
              (cons (make-block b-block 170 (- HEIGHT 10))
                    (cons (make-block b-block 210 (- HEIGHT 40)) empty)))
(check-expect (remove-row-and-lower (make-block b-block 30 (- HEIGHT 30)) 
                                    (cons (make-block b-block 10 (- HEIGHT 20))
                                          (cons (make-block b-block 30 (- HEIGHT 30))
                                                (cons (make-block b-block 50 (- HEIGHT 30))
                                                      (cons (make-block b-block 70 (- HEIGHT 30))
                                                            (cons (make-block b-block 90 (- HEIGHT 30))
                                                                  (cons (make-block b-block 170 (- HEIGHT 10))
                                                                        (cons (make-block b-block 110 (- HEIGHT 30))
                                                                              (cons (make-block b-block 130 (- HEIGHT 30))
                                                                                    (cons (make-block b-block 150 (- HEIGHT 30))
                                                                                          (cons (make-block b-block 170 (- HEIGHT 30))
                                                                                                (cons (make-block b-block 190 (- HEIGHT 30))
                                                                                                      (cons (make-block b-block 210 (- HEIGHT 30)) empty)))))))))))))
              (cons (make-block b-block 10 (- HEIGHT 20))
                    (cons (make-block b-block 170 (- HEIGHT 10)) empty)))

(check-expect (remove-row-and-lower (make-block b-block 20 20) empty) empty)

(check-expect (remove-row-and-lower (make-block b-block 20 20) (cons (make-block y-block CTR-X 100)
                                                                     (cons (make-block y-block CTR-X 20)
                                                                           (cons (make-block y-block (+ CTR-X 20) 100)
                                                                                 (cons (make-block y-block (+ CTR-X 20) 20) empty)))))
              (cons (make-block y-block CTR-X 100)
                    (cons (make-block y-block (+ CTR-X 20) 100) empty)))
(check-expect (remove-row-and-lower (make-block b-block 20 100) (cons (make-block y-block CTR-X 50)
                                                                      (cons (make-block y-block CTR-X 100)
                                                                            (cons (make-block y-block (+ CTR-X 20) 100)
                                                                                  (cons (make-block y-block (+ CTR-X 20) 50) empty)))))
              (cons (make-block y-block CTR-X 70)
                    (cons (make-block y-block (+ CTR-X 20) 70) empty)))


#;
(define (remove-row-and-lower b lob) lob) ;stub

(@template ListOfBlock)

(define (remove-row-and-lower b lob)
  (cond [(empty? lob) empty]
        [else (if (<= (block-y (first lob)) (block-y b))
                  (if (= (block-y (first lob)) (block-y b))
                      (remove-row-and-lower b (rest lob))
                      (cons (make-block (block-img (first lob))      ;; remove row first and then lower?
                                        (block-x (first lob))
                                        (+ (block-y (first lob)) 20))
                            (remove-row-and-lower b (rest lob))))
                  (cons (first lob) (remove-row-and-lower b (rest lob))))]))
                      
      


(@htdf descent)
(@signature World -> World)
;; lower the first 4 blocks by dy
;; CONSTRAINT: either 4 blocks in list or empty
(check-expect (descent (make-world empty SPEED-Y 0)) (make-world empty SPEED-Y 0))
(check-expect (descent (make-world (cons (make-block b-block CTR-X -70)
                                         (cons (make-block b-block CTR-X -50)
                                               (cons (make-block b-block CTR-X -30)
                                                     (cons (make-block  b-block CTR-X -10) empty)))) SPEED-Y 0))
              (make-world (cons (make-block b-block CTR-X (+ -70 SPEED-Y))
                                (cons (make-block b-block CTR-X (+ -50 SPEED-Y))
                                      (cons (make-block b-block CTR-X (+ -30 SPEED-Y))
                                            (cons (make-block  b-block CTR-X (+ -10 SPEED-Y)) empty)))) SPEED-Y 0))
(check-expect (descent (make-world (cons (make-block b-block CTR-X -70)
                                         (cons (make-block b-block CTR-X -50)
                                               (cons (make-block b-block CTR-X -30)
                                                     (cons (make-block  b-block CTR-X -10)
                                                           (cons (make-block b-block CTR-X (- HEIGHT 10)) empty))))) SPEED-Y 200))
              (make-world (cons (make-block b-block CTR-X (+ -70 SPEED-Y))
                                (cons (make-block b-block CTR-X (+ -50 SPEED-Y))
                                      (cons (make-block b-block CTR-X (+ -30 SPEED-Y))
                                            (cons (make-block  b-block CTR-X (+ -10 SPEED-Y))
                                                  (cons (make-block b-block CTR-X (- HEIGHT 10))empty))))) SPEED-Y 200))
#;
(define (descent w) w) ;stub

(@template Block)

(define (descent w)
  (if (empty? (world-lob w))
      w
      (make-world (cons (make-block (block-img (first (world-lob w)))
                                    (block-x (first (world-lob w)))
                                    (+ (block-y (first (world-lob w))) (world-dy w)))
                        (cons (make-block (block-img (second (world-lob w)))
                                          (block-x (second (world-lob w)))
                                          (+ (block-y (second (world-lob w))) (world-dy w)))
                              (cons (make-block (block-img (third (world-lob w)))
                                                (block-x (third (world-lob w)))
                                                (+ (block-y (third (world-lob w))) (world-dy w)))
                                    (cons (make-block (block-img (fourth (world-lob w)))
                                                      (block-x (fourth (world-lob w)))
                                                      (+ (block-y (fourth (world-lob w))) (world-dy w)))
                                          (rest (rest (rest (rest (world-lob w))))))))) (world-dy w) (world-score w))))
                    





(@htdf add-block)
(@signature World -> World)
;; adds one of the 5 shapes (each consisting of 4 blocks)
(check-random (add-block (make-world empty 10 0)) (make-world (shape-maker (random 8) empty) 10 0))
(check-random (add-block (make-world (cons (make-block b-block CTR-X -70)
                                           (cons (make-block b-block CTR-X -50)
                                                 (cons (make-block b-block CTR-X -30)
                                                       (cons (make-block  b-block CTR-X -10) empty)))) 10 0))
              (make-world (shape-maker (random 8) (cons (make-block b-block CTR-X -70)
                                                        (cons (make-block b-block CTR-X -50)
                                                              (cons (make-block b-block CTR-X -30)
                                                                    (cons (make-block  b-block CTR-X -10) empty))))) 10 0))

#;
(define (add-block w) w)


(define (add-block w)
  (make-world (shape-maker (random 8) (world-lob w))
              (world-dy w)
              (world-score w)))


(@htdf shape-maker)
(@signature Number ListOfBlock -> ListOfBlock)
;; append one of the 5 shapes onto list
;; 7 -> Z-g
;; 6 -> L-db
;; 5 -> line 
;; 4 -> square 
;; 3 -> L-o
;; 2 -> T
;; 1 -> Z-r

(check-expect (shape-maker 5 empty) (cons (make-block b-block CTR-X -70)
                                          (cons (make-block b-block CTR-X -50)
                                                (cons (make-block b-block CTR-X -30)
                                                      (cons (make-block  b-block CTR-X -10) empty)))))

(check-expect (shape-maker 4 empty) (cons (make-block y-block  CTR-X -10)
                                          (cons (make-block y-block CTR-X -30)
                                                (cons (make-block y-block (+ CTR-X 20) -10)
                                                      (cons (make-block y-block (+ CTR-X 20) -30) empty)))))

(check-expect (shape-maker 4 (cons (make-block r-block 400 400) empty))
              (cons (make-block y-block CTR-X -10)
                    (cons (make-block y-block CTR-X -30)
                          (cons (make-block y-block (+ CTR-X 20) -10)
                                (cons (make-block y-block (+ CTR-X 20) -30)
                                      (cons (make-block r-block 400 400) empty))))))

#;
(define (shape-maker n lob) lob) ;stub

(@template Number)

(define (shape-maker n lob)
  (cond [(= n 7) (add-Z-g lob)]
        [(= n 6) (add-L-db lob)]
        [(= n 5) (add-line-shape lob)]
        [(= n 4) (add-square lob)]
        [(= n 3) (add-L-o lob)]
        [(= n 2) (add-T lob)]
        [else (add-Z-r lob)]))

(@htdf add-Z-g)
(@signature ListOfBlock -> ListOfBlock)
;; add Z shape onto list
(check-expect (add-Z-g empty) (cons (make-block g-block CTR-X -10)
                                    (cons (make-block g-block (+ CTR-X 20) -10)
                                          (cons (make-block g-block CTR-X -30)
                                                (cons (make-block g-block (- CTR-X 20) -30) empty)))))
#; 
(define (add-Z-g lob) lob) ;stub

(define (add-Z-g lob)
  (cons (make-block g-block CTR-X -10)
        (cons (make-block g-block (+ CTR-X 20) -10)
              (cons (make-block g-block CTR-X -30)
                    (cons (make-block g-block (- CTR-X 20) -30) lob)))))
 



(@htdf add-L-db)
(@signature ListOfBlock -> ListOfBlock)
;; add L shape onto list
(check-expect (add-L-db empty) (cons (make-block db-block (+ CTR-X 20) -50)
                                     (cons (make-block db-block CTR-X -50)
                                           (cons (make-block db-block CTR-X -30)
                                                 (cons (make-block db-block CTR-X -10) empty)))))
                                                    
#;
(define (add-L-db lob) lob) ;stub

(define (add-L-db lob)
  (cons (make-block db-block (+ CTR-X 20) -50)
        (cons (make-block db-block CTR-X -50)
              (cons (make-block db-block CTR-X -30)
                    (cons (make-block db-block CTR-X -10) lob)))))

(@htdf add-line-shape)
(@signature ListOfBlock -> ListOfBlock)
;; add line shape onto list
(check-expect (add-line-shape empty) (cons (make-block b-block CTR-X -70)
                                           (cons (make-block b-block CTR-X -50)
                                                 (cons (make-block b-block CTR-X -30)
                                                       (cons (make-block  b-block CTR-X -10) empty)))))

#;
(define (add-line-shape lob) lob) ;stub

(define (add-line-shape lob)
  (cons (make-block b-block CTR-X -70)
        (cons (make-block b-block CTR-X -50)
              (cons (make-block b-block CTR-X -30)
                    (cons (make-block  b-block CTR-X -10) lob)))))
              




(@htdf add-square)
(@signature ListOfBlock -> ListOfBlock)
;; add square shape onto list
(check-expect (add-square empty) (cons (make-block y-block  CTR-X  -10)
                                       (cons (make-block y-block CTR-X  -30)
                                             (cons (make-block y-block (+ CTR-X 20) -10)
                                                   (cons (make-block y-block (+ CTR-X 20) -30) empty)))))
#;
(define (add-square lob) lob) ;stub

(define (add-square lob)
  (cons (make-block y-block CTR-X  -10)
        (cons (make-block y-block  CTR-X  -30)
              (cons (make-block y-block (+ CTR-X 20) -10)
                    (cons (make-block y-block (+ CTR-X 20) -30) lob)))))

(@htdf add-L-o)
(@signature ListOfBlock -> ListOfBlock)
;; add L shape onto list
(check-expect (add-L-o empty) (cons (make-block o-block (- CTR-X 20) -50)
                                    (cons (make-block o-block CTR-X -50)
                                          (cons (make-block o-block CTR-X -30)
                                                (cons (make-block o-block CTR-X -10) empty)))))
                                                    
#;
(define (add-L-o lob) lob) ;stub

(define (add-L-o lob)
  (cons (make-block o-block (- CTR-X 20) -50)
        (cons (make-block o-block CTR-X -50)
              (cons (make-block o-block CTR-X -30)
                    (cons (make-block o-block CTR-X -10) lob)))))
  

(@htdf add-T)
(@signature ListOfBlock -> ListOfBlock)
;; add T shape onto list
(check-expect (add-T empty) (cons (make-block p-block CTR-X -30)
                                  (cons (make-block p-block (- CTR-X 20) -30)
                                        (cons (make-block p-block (+ CTR-X 20) -30)
                                              (cons (make-block p-block CTR-X -10) empty))))) 
#;
(define (add-T lob) lob) ;stub

(define (add-T lob)
  (cons (make-block p-block CTR-X -30)
        (cons (make-block p-block (- CTR-X 20) -30)
              (cons (make-block p-block (+ CTR-X 20) -30)
                    (cons (make-block p-block CTR-X -10) lob)))))

(@htdf add-Z-r)
(@signature ListOfBlock -> ListOfBlock)
;; add Z shape onto list
(check-expect (add-Z-r empty) (cons (make-block r-block CTR-X -10)
                                    (cons (make-block r-block (- CTR-X 20) -10)
                                          (cons (make-block r-block CTR-X -30)
                                                (cons (make-block r-block (+ CTR-X 20) -30) empty)))))
#; 
(define (add-Z-r lob) lob) ;stub

(define (add-Z-r lob)
  (cons (make-block r-block CTR-X -10)
        (cons (make-block r-block (- CTR-X 20) -10)
              (cons (make-block r-block CTR-X -30)
                    (cons (make-block r-block (+ CTR-X 20) -30) lob)))))


(@htdf game-over)
(@signature World -> Boolean)
;; when a block touches the top of screen
(check-expect (game-over (make-world empty SPEED-Y 0)) false)
(check-expect (game-over (make-world (cons (make-block y-block CTR-X -10)
                                           (cons (make-block y-block CTR-X -30)
                                                 (cons (make-block y-block (+ CTR-X 20) -10)
                                                       (cons (make-block y-block (+ CTR-X 20) -30) empty)))) SPEED-Y 0)) false)
(check-expect (game-over (make-world (cons (make-block y-block CTR-X -10)
                                           (cons (make-block y-block CTR-X -30)
                                                 (cons (make-block y-block (+ CTR-X 20) -10)
                                                       (cons (make-block y-block (+ CTR-X 20) -30)
                                                             (cons (make-block y-block CTR-X -10)
                                                                   (cons (make-block y-block CTR-X -30)
                                                                         (cons (make-block y-block (+ CTR-X 20) -10)
                                                                               (cons (make-block y-block (+ CTR-X 20) -30) empty)))))))) SPEED-Y 0)) true)



#;
(define (game-over w) true) ;stub

(@template ListOfBlock)

(define (game-over w)
  (cond [(empty? (world-lob w)) false]
        [else (if (list-over-4? (world-lob w))
                  (next-four-above? (rest (rest (rest (rest (world-lob w))))))
                  false)]))

                  
                  

(@htdf list-over-4?)
(@signature ListOfBlock -> Boolean)
;; check if the fifth, sixth, seventh, or eighth blocks are above y=0
(check-expect (list-over-4? empty) false)
(check-expect (list-over-4? (cons (make-block y-block CTR-X -10)
                                  (cons (make-block y-block CTR-X -30)
                                        (cons (make-block y-block (+ CTR-X 20) -10)
                                              (cons (make-block y-block (+ CTR-X 20) -30) empty)))))
              false)
(check-expect (list-over-4? (cons (make-block y-block CTR-X -10)
                                  (cons (make-block y-block CTR-X -30)
                                        (cons (make-block y-block (+ CTR-X 20) -10)
                                              (cons (make-block y-block (+ CTR-X 20) -30)
                                                    (cons (make-block y-block CTR-X -10)
                                                          (cons (make-block y-block CTR-X -30)
                                                                (cons (make-block y-block (+ CTR-X 20) -10)
                                                                      (cons (make-block y-block (+ CTR-X 20) -30) empty)))))))))
              true)

#;
(define (list-over-4? lob) true);stub

(@template ListOfBlock)

(define (list-over-4? lob)
  (cond [(empty? lob) false]
        [else (if (not (empty? (rest lob)))
                  (if (not (empty? (rest (rest lob))))
                      (if (not (empty? (rest (rest (rest lob)))))
                          (if (not (empty? (rest (rest (rest (rest lob))))))
                              true
                              false)
                          false)
                      false)
                  false)]))
                          
              
(@htdf next-four-above?)
(@signature ListOfBlock -> Boolean)
;; next four in list above 0? produce true
(check-expect (next-four-above? (cons (make-block y-block CTR-X -10)
                                      (cons (make-block y-block CTR-X -30)
                                            (cons (make-block y-block (+ CTR-X 20) -10)
                                                  (cons (make-block y-block (+ CTR-X 20) -30)
                                                        (cons (make-block y-block CTR-X -10)
                                                              (cons (make-block y-block CTR-X -30)
                                                                    (cons (make-block y-block (+ CTR-X 20) -10)
                                                                          (cons (make-block y-block (+ CTR-X 20) -30) empty)))))))))
              true)
(check-expect (next-four-above? (cons (make-block y-block CTR-X 10)
                                      (cons (make-block y-block CTR-X 30)
                                            (cons (make-block y-block (+ CTR-X 20) 10)
                                                  (cons (make-block y-block (+ CTR-X 20) 30) empty)))))
              false)

(@template ListOfBlock)

(define (next-four-above? lob)
  (cond [(empty? lob) false]
        [else (if (< (block-y (first lob)) 0)
                  true
                  (next-four-above? (rest lob)))]))
                       
                             
                           

(@htdf handle-key)
(@signature World KeyEvent -> World)
;; when w a s d pressed check color of block and send to according function
;; w will rotate the shape
;; s will speed the descent
;; d will move right
;; a will move left
(check-expect (handle-key (make-world empty SPEED-Y 0) "d") (make-world empty SPEED-Y 0))
(check-expect (handle-key (make-world (cons (make-block y-block CTR-X 10)
                                            (cons (make-block y-block CTR-X 30)
                                                  (cons (make-block y-block (+ CTR-X 20) 10)
                                                        (cons (make-block y-block (+ CTR-X 20) 30) empty)))) SPEED-Y 0) "d")
              (make-world (cons (make-block y-block (+ CTR-X SPEED-X) 10)
                                (cons (make-block y-block (+ CTR-X SPEED-X) 30)
                                      (cons (make-block y-block (+ (+ CTR-X 20) SPEED-X) 10)
                                            (cons (make-block y-block (+ (+ CTR-X 20) SPEED-X) 30) empty)))) SPEED-Y 0))
(check-expect (handle-key (make-world (cons (make-block y-block CTR-X 10)
                                            (cons (make-block y-block CTR-X 30)
                                                  (cons (make-block y-block (+ CTR-X 20) 10)
                                                        (cons (make-block y-block (+ CTR-X 20) 30) empty)))) SPEED-Y 0) "a")
              (make-world (cons (make-block y-block (- CTR-X SPEED-X) 10)
                                (cons (make-block y-block (- CTR-X SPEED-X) 30)
                                      (cons (make-block y-block (- (+ CTR-X 20) SPEED-X) 10)
                                            (cons (make-block y-block (- (+ CTR-X 20) SPEED-X) 30) empty)))) SPEED-Y 0))
(check-expect (handle-key (make-world (cons (make-block y-block CTR-X 10)
                                            (cons (make-block y-block CTR-X 30)
                                                  (cons (make-block y-block (+ CTR-X 20) 10)
                                                        (cons (make-block y-block (+ CTR-X 20) 30) empty)))) SPEED-Y 0) "s")
              (make-world (cons (make-block y-block CTR-X  (+ 10 S-SPEED))
                                (cons (make-block y-block CTR-X  (+ 30 S-SPEED))
                                      (cons (make-block y-block (+ CTR-X 20) (+ 10 S-SPEED))
                                            (cons (make-block y-block  (+ CTR-X 20) (+ 30 S-SPEED)) empty)))) SPEED-Y 0))

                        

(@template KeyEvent)

(define (handle-key w ke)
  (cond [(empty? (world-lob w)) w]
        [(and (key=? ke "d") (moving-right-ok? (world-lob w)))  (make-world (add-first4-x (world-lob w))
                                                                            (world-dy w)
                                                                            (world-score w))]
        [(and (key=? ke "a") (moving-left-ok? (world-lob w)))      (make-world (sub-first4-x (world-lob w))
                                                                               (world-dy w)
                                                                               (world-score w))]
        [(and (key=? ke "s") (moving-down-ok? (world-lob w)))  (make-world (add-first4-y (world-lob w))
                                                                           (world-dy w)
                                                                           (world-score w))]
        [(and (key=? ke "w") (rotate-ok? (world-lob w)))       (make-world (rotate-shape (world-lob w))
                                                                           (world-dy w)
                                                                           (world-score w))]
        [else  w]))

(@htdf rotate-ok?)
(@signature ListOfBlock -> Boolean)
;; determine which shape first four blocks make up
;; determine is rotate the (first four blocks) shape will run into other blocks
(check-expect (rotate-ok? (cons (make-block b-block CTR-X -70)
                                (cons (make-block b-block CTR-X -50)
                                      (cons (make-block b-block CTR-X -30)
                                            (cons (make-block  b-block CTR-X -10) empty)))))
              true)
(check-expect (rotate-ok? (cons (make-block b-block CTR-X -70)
                                (cons (make-block b-block CTR-X -50)
                                      (cons (make-block b-block CTR-X -30)
                                            (cons (make-block  b-block CTR-X -10)
                                                  (cons (make-block b-block CTR-X -70)
                                                        (cons (make-block b-block CTR-X -50)
                                                              (cons (make-block b-block CTR-X -30)
                                                                    (cons (make-block  b-block CTR-X -10) empty)))))))))
              false)

#;
(define (rotate-ok? lob) true) ;stub

(@template ListOfBlock)


(define (rotate-ok? lob)
  (cond [(empty? lob) true]
        [else (if (or (touch-other? (rotate-shape lob))
                      (goes-out-edge? (rotate-shape lob)))
                  false
                  true)]))

(@htdf goes-out-edge?)
(@signature ListOfBlock -> Boolean)
;; if block < 0 or block > WIDTH produce true
;; if block > HEIGHT produce true
(check-expect (goes-out-edge? (cons (make-block b-block (+ WIDTH 10) 100) empty))
              true)
(check-expect (goes-out-edge? (cons (make-block b-block 100 (+ HEIGHT 10)) empty))
              true)

(define (goes-out-edge? lob)
  (cond [(empty? lob) false]
        [else (if (or (or (< (block-x (first lob)) 0)
                          (> (block-x (first lob)) WIDTH))
                      (> (block-y (first lob)) HEIGHT))
                  true
                  (goes-out-edge? (rest lob)))]))
                        


(@htdf rotate-shape)
(@signature ListOfBlock -> ListOfBlock)
;; determine which shape first four blocks make up
;; send to correct function to rotate clockwise -> no square

 
(define (rotate-shape lob)
  (cond [(empty? lob) lob]
        [(line-up? lob) (rotate-line-cw lob)]
        [(line-down? lob) (rotate-line-up lob)]
        [(L-db? lob) (rotate-L-db lob)]
        [(L-db-2? lob)   (rotate-L-db-2 lob)]
        [(L-db-3? lob)   (rotate-L-db-3 lob)]
        [(L-db-4? lob)   (rotate-L-db-4 lob)]
        [(L-o? lob)   (rotate-L-o lob)]
        [(L-o-2? lob)   (rotate-L-o-2 lob)]
        [(L-o-3? lob)   (rotate-L-o-3 lob)]
        [(L-o-4? lob)   (rotate-L-o-4 lob)]
        [(Z-g? lob)   (rotate-Z-g lob)]
        [(Z-g-2? lob) (rotate-Z-g-2 lob)]
        [(Z-r? lob)   (rotate-Z-r lob)]
        [(Z-r-2? lob) (rotate-Z-r-2 lob)]
        [(T? lob)       (rotate-T lob)]
        [(T-2? lob)       (rotate-T-2 lob)]
        [(T-3? lob)       (rotate-T-3 lob)]
        [(T-4? lob)       (rotate-T-4 lob)]
        [else                      lob]))

(@htdf rotate-T-4)
(@signature ListOfBlock -> ListOfBlock)
;; check orientation and rotate clockwise accordingly
(check-expect (rotate-T-4 (cons (make-block p-block CTR-X -30)                   ;; FIRST ONE DOESN'T CHANGE
                                (cons (make-block p-block (- (+ (+ (- CTR-X 20) 20) 20) 20) (+ (+ (- -30 20) 20) 20))
                                      (cons (make-block p-block (+ (- (- (+ CTR-X 20) 20) 20) 20) (- (- (+ -30 20) 20) 20))
                                            (cons (make-block p-block (+ (+ (- CTR-X 20) 20) 20) (+ (- (- -10 20) 20) 20)) empty)))))
              (cons (make-block p-block CTR-X -30)                   ;; FIRST ONE DOESN'T CHANGE
                    (cons (make-block p-block (- (- (+ (+ (- CTR-X 20) 20) 20) 20) 20) (- (+ (+ (- -30 20) 20) 20) 20))
                          (cons (make-block p-block (+ (+ (- (- (+ CTR-X 20) 20) 20) 20) 20) (+ (- (- (+ -30 20) 20) 20) 20))
                                (cons (make-block p-block (- (+ (+ (- CTR-X 20) 20) 20) 20) (+ (+ (- (- -10 20) 20) 20) 20)) empty)))))    

#;
(define (rotate-T-4 lob) lob) ;stub

(define (rotate-T-4 lob)
  (cons (make-block p-block (block-x (first lob)) (block-y (first lob)))
        (cons (make-block p-block (- (block-x (second lob)) 20)  (- (block-y (second lob)) 20) )
              (cons (make-block p-block  (+ (block-x (third lob)) 20)  (+ (block-y (third lob)) 20) )
                    (cons (make-block p-block (- (block-x (fourth lob)) 20) (+ (block-y (fourth lob)) 20)) (rest (rest (rest (rest lob)))))))))


(@htdf T-4?)
(@signature ListOfBlock -> Boolean)
;; if T shape produce true
(check-expect (T-4? (cons (make-block p-block CTR-X -30)                   ;; FIRST ONE DOESN'T CHANGE
                          (cons (make-block p-block (- (+ (+ (- CTR-X 20) 20) 20) 20) (+ (+ (- -30 20) 20) 20))
                                (cons (make-block p-block (+ (- (- (+ CTR-X 20) 20) 20) 20) (- (- (+ -30 20) 20) 20))
                                      (cons (make-block p-block (+ (+ (- CTR-X 20) 20) 20) (+ (- (- -10 20) 20) 20)) empty)))))true)
(check-expect (T-4?  (cons (make-block y-block  CTR-X  -10)
                           (cons (make-block y-block CTR-X  -30)
                                 (cons (make-block y-block (+ CTR-X 20) -10)
                                       (cons (make-block y-block (+ CTR-X 20) -30) empty))))) false)

#;
(define (T-4? b) true) ;stub

(define (T-4? lob)
  (and (= (- (block-x (fourth lob)) (block-x (third lob))) 20)
       (= (- (block-y (fourth lob)) (block-y (third lob))) 20)))


(@htdf rotate-T-3)
(@signature ListOfBlock -> ListOfBlock)
;; check orientation and rotate clockwise accordingly
(check-expect (rotate-T-3 (cons (make-block p-block CTR-X -30)                   ;; FIRST ONE DOESN'T CHANGE
                                (cons (make-block p-block (+ (+ (- CTR-X 20) 20) 20) (+ (- -30 20) 20))
                                      (cons (make-block p-block (- (- (+ CTR-X 20) 20) 20) (- (+ -30 20) 20))
                                            (cons (make-block p-block (+ (- CTR-X 20) 20) (- (- -10 20) 20)) empty)))))
              (cons (make-block p-block CTR-X -30)                   ;; FIRST ONE DOESN'T CHANGE
                    (cons (make-block p-block (- (+ (+ (- CTR-X 20) 20) 20) 20) (+ (+ (- -30 20) 20) 20))
                          (cons (make-block p-block (+ (- (- (+ CTR-X 20) 20) 20) 20) (- (- (+ -30 20) 20) 20))
                                (cons (make-block p-block (+ (+ (- CTR-X 20) 20) 20) (+ (- (- -10 20) 20) 20)) empty)))))    

#;
(define (rotate-T-3 lob) lob) ;stub

(define (rotate-T-3 lob)
  (cons (make-block p-block (block-x (first lob)) (block-y (first lob)))
        (cons (make-block p-block (- (block-x (second lob)) 20)  (+ (block-y (second lob)) 20) )
              (cons (make-block p-block  (+ (block-x (third lob)) 20)  (- (block-y (third lob)) 20) )
                    (cons (make-block p-block (+ (block-x (fourth lob)) 20) (+ (block-y (fourth lob)) 20)) (rest (rest (rest (rest lob)))))))))


(@htdf T-3?)
(@signature ListOfBlock -> Boolean)
;; if T shape produce true
(check-expect (T-3? (cons (make-block p-block CTR-X -30)                   ;; FIRST ONE DOESN'T CHANGE
                          (cons (make-block p-block (+ (+ (- CTR-X 20) 20) 20) (+ (- -30 20) 20))
                                (cons (make-block p-block (- (- (+ CTR-X 20) 20) 20) (- (+ -30 20) 20))
                                      (cons (make-block p-block (+ (- CTR-X 20) 20) (- (- -10 20) 20)) empty)))))true)
(check-expect (T-3?  (cons (make-block y-block  CTR-X  -10)
                           (cons (make-block y-block CTR-X  -30)
                                 (cons (make-block y-block (+ CTR-X 20) -10)
                                       (cons (make-block y-block (+ CTR-X 20) -30) empty))))) false)

#; 
(define (T-3? b) true) ;stub

(define (T-3? lob)
  (and (= (- (block-x (fourth lob)) (block-x (third lob))) 20)
       (= (- (block-y (fourth lob)) (block-y (third lob))) -20)))

(@htdf rotate-T-2)
(@signature ListOfBlock -> ListOfBlock)
;; check orientation and rotate clockwise accordingly
(check-expect (rotate-T-2 (cons (make-block p-block CTR-X -30)                   ;; FIRST ONE DOESN'T CHANGE
                                (cons (make-block p-block (+ (- CTR-X 20) 20) (- -30 20))
                                      (cons (make-block p-block (- (+ CTR-X 20) 20) (+ -30 20))
                                            (cons (make-block p-block  (- CTR-X 20) (- -10 20)) empty)))))
              (cons (make-block p-block CTR-X -30)                   ;; FIRST ONE DOESN'T CHANGE
                    (cons (make-block p-block (+ (+ (- CTR-X 20) 20) 20) (+ (- -30 20) 20))
                          (cons (make-block p-block (- (- (+ CTR-X 20) 20) 20) (- (+ -30 20) 20))
                                (cons (make-block p-block (+ (- CTR-X 20) 20) (- (- -10 20) 20)) empty)))))     

#;
(define (rotate-T-2 lob) lob) ;stub

(define (rotate-T-2 lob)
  (cons (make-block p-block (block-x (first lob)) (block-y (first lob)))
        (cons (make-block p-block (+ (block-x (second lob)) 20)  (+ (block-y (second lob)) 20) )
              (cons (make-block p-block  (- (block-x (third lob)) 20)  (- (block-y (third lob)) 20) )
                    (cons (make-block p-block (+ (block-x (fourth lob)) 20) (- (block-y (fourth lob)) 20)) (rest (rest (rest (rest lob)))))))))


(@htdf T-2?)
(@signature ListOfBlock -> Boolean)
;; if T shape produce true
(check-expect (T-2? (cons (make-block p-block CTR-X -30)                   ;; FIRST ONE DOESN'T CHANGE
                          (cons (make-block p-block (+ (- CTR-X 20) 20) (- -30 20))
                                (cons (make-block p-block (- (+ CTR-X 20) 20) (+ -30 20))
                                      (cons (make-block p-block  (- CTR-X 20) (- -10 20)) empty)))))true)
(check-expect (T-2?  (cons (make-block y-block  CTR-X  -10)
                           (cons (make-block y-block CTR-X  -30)
                                 (cons (make-block y-block (+ CTR-X 20) -10)
                                       (cons (make-block y-block (+ CTR-X 20) -30) empty))))) false)

#;
(define (T-2? b) true) ;stub

(define (T-2? lob)
  (and (= (- (block-x (fourth lob)) (block-x (third lob))) -20)
       (= (- (block-y (fourth lob)) (block-y (third lob))) -20)))

(@htdf rotate-T)
(@signature ListOfBlock -> ListOfBlock)
;; check orientation and rotate clockwise accordingly
(check-expect (rotate-T (cons (make-block p-block CTR-X -30)
                              (cons (make-block p-block (- CTR-X 20) -30)
                                    (cons (make-block p-block (+ CTR-X 20) -30)
                                          (cons (make-block p-block CTR-X -10) empty)))))
              (cons (make-block p-block CTR-X -30)                   ;; FIRST ONE DOESN'T CHANGE
                    (cons (make-block p-block (+ (- CTR-X 20) 20) (- -30 20))
                          (cons (make-block p-block (- (+ CTR-X 20) 20) (+ -30 20))
                                (cons (make-block p-block  (- CTR-X 20) (- -10 20)) empty)))))    
 
#;
(define (rotate-T lob) lob) ;stub

(define (rotate-T lob)
  (cons (make-block p-block (block-x (first lob)) (block-y (first lob)))
        (cons (make-block p-block (+ (block-x (second lob)) 20)  (- (block-y (second lob)) 20) )
              (cons (make-block p-block  (- (block-x (third lob)) 20)  (+ (block-y (third lob)) 20) )
                    (cons (make-block p-block (- (block-x (fourth lob)) 20) (- (block-y (fourth lob)) 20)) (rest (rest (rest (rest lob)))))))))



(@htdf T?)
(@signature ListOfBlock -> Boolean)
;; if T shape produce true
(check-expect (T? (cons (make-block p-block CTR-X -30)
                        (cons (make-block p-block (- CTR-X 20) -30)
                              (cons (make-block p-block (+ CTR-X 20) -30)
                                    (cons (make-block p-block CTR-X -10) empty)))))true)
(check-expect (T?  (cons (make-block y-block  CTR-X  -10)
                         (cons (make-block y-block CTR-X  -30)
                               (cons (make-block y-block (+ CTR-X 20) -10)
                                     (cons (make-block y-block (+ CTR-X 20) -30) empty))))) false)

#;
(define (T? b) true) ;stub

(define (T? lob)
  (and (= (- (block-x (fourth lob)) (block-x (third lob))) -20)
       (= (- (block-y (fourth lob)) (block-y (third lob))) 20)))


(@htdf rotate-Z-r-2)
(@signature ListOfBlock -> ListOfBlock)
;; rotate back to orignal


#;
(define (rotate-Z-r-2 lob) lob) ;stub


(define (rotate-Z-r-2 lob)
  (cons (make-block r-block (block-x (first lob)) (block-y (first lob)))
        (cons (make-block r-block (- (block-x (second lob)) 20) (- (block-y (second lob)) 20))
              (cons (make-block r-block (+ (block-x (third lob)) 20) (- (block-y (third lob)) 20))
                    (cons (make-block r-block (+ (block-x (fourth lob)) 40) (block-y (fourth lob))) (rest (rest (rest (rest lob)))))))))
           
(@htdf Z-r-2?)
(@signature ListOfBlock -> Boolean)
;; if Z-r shape produce true
(check-expect (Z-r-2?  (cons (make-block r-block CTR-X -10)
                             (cons (make-block r-block (+ (- CTR-X 20) 20) (+ -10 20))
                                   (cons (make-block r-block (- CTR-X 20) (+ -30 20))
                                         (cons (make-block r-block (- (+ CTR-X 20) 40) -30) empty))))) true)
(check-expect (Z-r-2? (cons (make-block g-block CTR-X -10)
                            (cons (make-block g-block (+ CTR-X 20) -10)
                                  (cons (make-block g-block CTR-X -30)
                                        (cons (make-block g-block (- CTR-X 20) -30) empty))))) false) 

#;
(define (Z-r-2? lob) true) ;stub


(define (Z-r-2? lob)
  (and (= (- (block-x (fourth lob)) (block-x (second lob))) -20)
       (= (- (block-y (fourth lob)) (block-y (second lob))) -40)))
  


(@htdf rotate-Z-r)
(@signature ListOfBlock -> ListOfBlock)
;; check orientation and rotate clockwise accordingly
(check-expect (rotate-Z-r 
               (cons (make-block r-block CTR-X -10)
                     (cons (make-block r-block (- CTR-X 20) -10)
                           (cons (make-block r-block CTR-X -30)
                                 (cons (make-block r-block (+ CTR-X 20) -30) empty)))))
              (cons (make-block r-block CTR-X -10)
                    (cons (make-block r-block (+ (- CTR-X 20) 20) (+ -10 20))
                          (cons (make-block r-block (- CTR-X 20) (+ -30 20))
                                (cons (make-block r-block (- (+ CTR-X 20) 40) -30) empty)))))
 
#;
(define (rotate-Z-r lob) lob) ;stub

(define (rotate-Z-r lob)
  (cons (make-block r-block (block-x (first lob)) (block-y (first lob)))
        (cons (make-block r-block (+ (block-x (second lob)) 20) (+ (block-y (second lob)) 20))
              (cons (make-block r-block (- (block-x (third lob)) 20) (+ (block-y (third lob)) 20))
                    (cons (make-block r-block (- (block-x (fourth lob)) 40) (block-y (fourth lob))) (rest (rest (rest (rest lob)))))))))
  


(@htdf Z-r?)
(@signature ListOfBlock -> Boolean)
;; if Z-r shape produce true
(check-expect (Z-r?  (cons (make-block r-block CTR-X -10)
                           (cons (make-block r-block (- CTR-X 20) -10)
                                 (cons (make-block r-block CTR-X -30)
                                       (cons (make-block r-block (+ CTR-X 20) -30) empty))))) true)
(check-expect (Z-r? (cons (make-block g-block CTR-X -10)
                          (cons (make-block g-block (+ CTR-X 20) -10)
                                (cons (make-block g-block CTR-X -30)
                                      (cons (make-block g-block (- CTR-X 20) -30) empty))))) false) 

#;
(define (Z-r? lob) true) ;stub


(define (Z-r? lob)
  (and (= (- (block-x (fourth lob)) (block-x (second lob))) 40)
       (= (- (block-y (fourth lob)) (block-y (second lob))) -20)))
  
  

(@htdf Z-g-2?)
(@signature ListOfBlock -> Boolean)
;; true if rotated Z-g
(check-expect (Z-g-2? (cons (make-block g-block CTR-X -10)
                            (cons (make-block g-block (- (+ CTR-X 20) 20) (+ -10 20))
                                  (cons (make-block g-block (+ CTR-X 20) (+ -30 20))
                                        (cons (make-block g-block (+ (- CTR-X 20) 40) -30) empty))))) true)

(define (Z-g-2? lob)
  (and (= (- (block-x (fourth lob)) (block-x (second lob))) 20)
       (= (- (block-y (fourth lob)) (block-y (second lob))) -40)))

(@htdf rotate-Z-g-2)
(@signature ListOfBlock -> ListOfBlock)
;; check orientation and rotate clockwise accordingly
(check-expect (rotate-Z-g-2 (cons (make-block g-block CTR-X -10)
                                  (cons (make-block g-block (- (+ CTR-X 20) 20) (+ -10 20))
                                        (cons (make-block g-block (+ CTR-X 20) (+ -30 20))
                                              (cons (make-block g-block (+ (- CTR-X 20) 40) -30) empty)))))
              (cons (make-block g-block CTR-X -10)
                    (cons (make-block g-block (+ CTR-X 20) -10)
                          (cons (make-block g-block CTR-X -30)
                                (cons (make-block g-block (- CTR-X 20) -30) empty)))))

#;
(define (rotate-Z-g-2 lob) lob) ;stub

(define (rotate-Z-g-2 lob)
  (cons (make-block g-block (block-x (first lob)) (block-y (first lob)))
        (cons (make-block g-block (+ (block-x (second lob)) 20) (- (block-y (second lob)) 20))
              (cons (make-block g-block (- (block-x (third lob)) 20) (- (block-y (third lob)) 20))
                    (cons (make-block g-block (- (block-x (fourth lob)) 40) (block-y (fourth lob))) (rest (rest (rest (rest lob)))))))))


(@htdf rotate-Z-g)
(@signature ListOfBlock -> ListOfBlock)
;; check orientation and rotate clockwise accordingly
(check-expect (rotate-Z-g (cons (make-block g-block CTR-X -10)
                                (cons (make-block g-block (+ CTR-X 20) -10)
                                      (cons (make-block g-block CTR-X -30)
                                            (cons (make-block g-block (- CTR-X 20) -30) empty)))))
              (cons (make-block g-block CTR-X -10)
                    (cons (make-block g-block (- (+ CTR-X 20) 20) (+ -10 20))
                          (cons (make-block g-block (+ CTR-X 20) (+ -30 20))
                                (cons (make-block g-block (+ (- CTR-X 20) 40) -30) empty)))))

#;
(define (rotate-Z-g lob) lob) ;stub

(define (rotate-Z-g lob)
  (cons (make-block g-block (block-x (first lob)) (block-y (first lob)))
        (cons (make-block g-block (- (block-x (second lob)) 20) (+ (block-y (second lob)) 20))
              (cons (make-block g-block (+ (block-x (third lob)) 20) (+ (block-y (third lob)) 20))
                    (cons (make-block g-block (+ (block-x (fourth lob)) 40) (block-y (fourth lob))) (rest (rest (rest (rest lob)))))))))
  


(@htdf Z-g?)
(@signature ListOfBlock -> Boolean)
;; if Z-g shape produce true
(check-expect (Z-g?  (cons (make-block r-block CTR-X -10)
                           (cons (make-block r-block (- CTR-X 20) -10)
                                 (cons (make-block r-block CTR-X -30)
                                       (cons (make-block r-block (+ CTR-X 20) -30) empty))))) false)
(check-expect (Z-g? (cons (make-block g-block CTR-X -10)
                          (cons (make-block g-block (+ CTR-X 20) -10)
                                (cons (make-block g-block CTR-X -30)
                                      (cons (make-block g-block (- CTR-X 20) -30) empty))))) true) 

#;
(define (Z-g? lob) true) ;stub


(define (Z-g? lob)
  (and (= (- (block-x (fourth lob)) (block-x (second lob))) -40)
       (= (- (block-y (fourth lob)) (block-y (second lob))) -20)))


(@htdf rotate-L-o-4)
(@signature ListOfBlock -> ListOfBlock)
;; check orientation and rotate clockwise accordingly

(check-expect (rotate-L-o-4 (cons (make-block o-block (- (+ (+ (- CTR-X 20) 20) 20) 20) (+ (+ (- -50 20) 20) 20))
                                  (cons (make-block o-block CTR-X -50)
                                        (cons (make-block o-block (+ (+ (- CTR-X 20) 20) 20) (+ (- (- -30 20) 20) 20))
                                              (cons (make-block o-block (+ (+ (- CTR-X 40) 40) 40) (+ (- (- -10 40) 40) 40)) empty)))))
              (cons (make-block o-block  (- (- (+ (+ (- CTR-X 20) 20) 20) 20) 20) (- (+ (+ (- -50 20) 20) 20) 20))
                    (cons (make-block o-block CTR-X -50)
                          (cons (make-block o-block (- (+ (+ (- CTR-X 20) 20) 20) 20) (+ (+ (- (- -30 20) 20) 20) 20))
                                (cons (make-block o-block (- (+ (+ (- CTR-X 40) 40) 40) 40) (+ (+ (- (- -10 40) 40) 40) 40)) empty)))))

#;
(define (rotate-L-o-4 lob) lob) ;stub
                

(define (rotate-L-o-4 lob)
  (cons (make-block o-block (- (block-x (first lob)) 20) (- (block-y (first lob)) 20))
        (cons (make-block o-block  (block-x (second lob))  (block-y (second lob)))
              (cons (make-block o-block (- (block-x (third lob)) 20) (+ (block-y (third lob)) 20))
                    (cons (make-block o-block (- (block-x (fourth lob)) 40) (+ (block-y (fourth lob)) 40)) (rest (rest (rest (rest lob)))))))))


(@htdf L-o-4?)
(@signature ListOfBlock -> Boolean)
;; if L-o shape produce true
(check-expect (L-o-4? (cons (make-block o-block (- (+ (+ (- CTR-X 20) 20) 20) 20) (+ (+ (- -50 20) 20) 20))
                            (cons (make-block o-block CTR-X -50)
                                  (cons (make-block o-block (+ (+ (- CTR-X 20) 20) 20) (+ (- (- -30 20) 20) 20))
                                        (cons (make-block o-block (+ (+ (- CTR-X 40) 40) 40) (+ (- (- -10 40) 40) 40)) empty))))) true)
(check-expect (L-o-4? (cons (make-block db-block (+ CTR-X 20) -50)
                            (cons (make-block db-block CTR-X -50)
                                  (cons (make-block db-block CTR-X -30)
                                        (cons (make-block db-block CTR-X -10)
                                              (cons (make-block o-block (- CTR-X 20) -50)
                                                    (cons (make-block o-block CTR-X -50)
                                                          (cons (make-block o-block CTR-X -30)
                                                                (cons (make-block o-block CTR-X -10) empty))))))))) false)

#;
(define (L-o-4? b) true) ;stub

(define (L-o-4? lob)
  (and (= (- (block-x (first lob)) (block-x (fourth lob))) -40)
       (= (- (block-y (first lob)) (block-y (fourth lob))) 20)))


(@htdf rotate-L-o-3)
(@signature ListOfBlock -> ListOfBlock)
;; check orientation and rotate clockwise accordingly

(check-expect (rotate-L-o-3 (cons (make-block o-block (+ (+ (- CTR-X 20) 20) 20) (+ (- -50 20) 20))
                                  (cons (make-block o-block CTR-X -50)
                                        (cons (make-block o-block (+ (- CTR-X 20) 20) (- (- -30 20) 20))
                                              (cons (make-block o-block (+ (- CTR-X 40) 40) (- (- -10 40) 40)) empty)))))
              (cons (make-block o-block (- (+ (+ (- CTR-X 20) 20) 20) 20) (+ (+ (- -50 20) 20) 20))
                    (cons (make-block o-block CTR-X -50)
                          (cons (make-block o-block (+ (+ (- CTR-X 20) 20) 20) (+ (- (- -30 20) 20) 20))
                                (cons (make-block o-block (+ (+ (- CTR-X 40) 40) 40) (+ (- (- -10 40) 40) 40)) empty)))))

#;
(define (rotate-L-o-3 lob) lob) ;stub
                

(define (rotate-L-o-3 lob)
  (cons (make-block o-block (- (block-x (first lob)) 20) (+ (block-y (first lob)) 20))
        (cons (make-block o-block  (block-x (second lob))  (block-y (second lob)) )
              (cons (make-block o-block (+ (block-x (third lob)) 20) (+ (block-y (third lob)) 20))
                    (cons (make-block o-block (+ (block-x (fourth lob)) 40) (+ (block-y (fourth lob)) 40)) (rest (rest (rest (rest lob)))))))))


(@htdf L-o-3?)
(@signature ListOfBlock -> Boolean)
;; if L-o shape produce true
(check-expect (L-o-3? (cons (make-block o-block (+ (+ (- CTR-X 20) 20) 20) (+ (- -50 20) 20))
                            (cons (make-block o-block CTR-X -50)
                                  (cons (make-block o-block (+ (- CTR-X 20) 20) (- (- -30 20) 20))
                                        (cons (make-block o-block (+ (- CTR-X 40) 40) (- (- -10 40) 40)) empty))))) true)
(check-expect (L-o-3? (cons (make-block db-block (+ CTR-X 20) -50)
                            (cons (make-block db-block CTR-X -50)
                                  (cons (make-block db-block CTR-X -30)
                                        (cons (make-block db-block CTR-X -10)
                                              (cons (make-block o-block (- CTR-X 20) -50)
                                                    (cons (make-block o-block CTR-X -50)
                                                          (cons (make-block o-block CTR-X -30)
                                                                (cons (make-block o-block CTR-X -10) empty))))))))) false)

#;
(define (L-o-3? b) true) ;stub

(define (L-o-3? lob)
  (and (= (- (block-x (first lob)) (block-x (fourth lob))) 20)
       (= (- (block-y (first lob)) (block-y (fourth lob))) 40)))
  
(@htdf rotate-L-o-2)
(@signature ListOfBlock -> ListOfBlock)
;; check orientation and rotate clockwise accordingly

(check-expect (rotate-L-o-2 (cons (make-block o-block (+ (- CTR-X 20) 20) (- -50 20))
                                  (cons (make-block o-block CTR-X -50)
                                        (cons (make-block o-block (- CTR-X 20)(- -30 20))
                                              (cons (make-block o-block (- CTR-X 40) (- -10 40)) empty)))))
              (cons (make-block o-block (+ (+ (- CTR-X 20) 20) 20) (+ (- -50 20) 20))
                    (cons (make-block o-block CTR-X -50)
                          (cons (make-block o-block (+ (- CTR-X 20) 20) (- (- -30 20) 20))
                                (cons (make-block o-block (+ (- CTR-X 40) 40) (- (- -10 40) 40)) empty)))))

#;
(define (rotate-L-o-2 lob) lob) ;stub
                

(define (rotate-L-o-2 lob)
  (cons (make-block o-block (+ (block-x (first lob)) 20) (+ (block-y (first lob)) 20))
        (cons (make-block o-block  (block-x (second lob))  (block-y (second lob)) )
              (cons (make-block o-block (+ (block-x (third lob)) 20) (- (block-y (third lob)) 20))
                    (cons (make-block o-block (+ (block-x (fourth lob)) 40) (- (block-y (fourth lob)) 40)) (rest (rest (rest (rest lob)))))))))

(@htdf L-o-2?)
(@signature ListOfBlock -> Boolean)
;; if L-o shape produce true
(check-expect (L-o-2? (cons (make-block o-block (+ (- CTR-X 20) 20) (- -50 20))
                            (cons (make-block o-block CTR-X -50)
                                  (cons (make-block o-block (- CTR-X 20)(- -30 20))
                                        (cons (make-block o-block (- CTR-X 40) (- -10 40)) empty))))) true)
(check-expect (L-o-2? (cons (make-block db-block (+ CTR-X 20) -50)
                            (cons (make-block db-block CTR-X -50)
                                  (cons (make-block db-block CTR-X -30)
                                        (cons (make-block db-block CTR-X -10)
                                              (cons (make-block o-block (- CTR-X 20) -50)
                                                    (cons (make-block o-block CTR-X -50)
                                                          (cons (make-block o-block CTR-X -30)
                                                                (cons (make-block o-block CTR-X -10) empty))))))))) false)

#;
(define (L-o-2? b) true) ;stub

(define (L-o-2? lob)
  (and (= (- (block-x (first lob)) (block-x (fourth lob))) 40)
       (= (- (block-y (first lob)) (block-y (fourth lob))) -20)))

(@htdf rotate-L-o)
(@signature ListOfBlock -> ListOfBlock)
;; check orientation and rotate clockwise accordingly
(check-expect (rotate-L-o (cons (make-block o-block (- CTR-X 20) -50)
                                (cons (make-block o-block CTR-X -50)
                                      (cons (make-block o-block CTR-X -30)
                                            (cons (make-block o-block CTR-X -10) empty)))))
              (cons (make-block o-block (+ (- CTR-X 20) 20) (- -50 20))
                    (cons (make-block o-block CTR-X -50)
                          (cons (make-block o-block (- CTR-X 20)(- -30 20))
                                (cons (make-block o-block (- CTR-X 40) (- -10 40)) empty)))))

#;
(define (rotate-L-o lob) lob) ;stub

(define (rotate-L-o lob)
  (cons (make-block o-block (+ (block-x (first lob)) 20) (- (block-y (first lob)) 20))
        (cons (make-block o-block  (block-x (second lob))  (block-y (second lob)) )
              (cons (make-block o-block (- (block-x (third lob)) 20) (- (block-y (third lob)) 20))
                    (cons (make-block o-block (- (block-x (fourth lob)) 40) (- (block-y (fourth lob)) 40)) (rest (rest (rest (rest lob)))))))))
  


(@htdf L-o?)
(@signature ListOfBlock -> Boolean)
;; if L-o shape produce true
(check-expect (L-o? (cons (make-block o-block (- CTR-X 20) -50)
                          (cons (make-block o-block CTR-X -50)
                                (cons (make-block o-block CTR-X -30)
                                      (cons (make-block o-block CTR-X -10) empty))))) true)
(check-expect (L-o? (cons (make-block db-block (+ CTR-X 20) -50)
                          (cons (make-block db-block CTR-X -50)
                                (cons (make-block db-block CTR-X -30)
                                      (cons (make-block db-block CTR-X -10)
                                            (cons (make-block o-block (- CTR-X 20) -50)
                                                  (cons (make-block o-block CTR-X -50)
                                                        (cons (make-block o-block CTR-X -30)
                                                              (cons (make-block o-block CTR-X -10) empty))))))))) false)

#;
(define (L-o? b) true) ;stub

(define (L-o? lob)
  (and (= (- (block-x (first lob)) (block-x (fourth lob))) -20)
       (= (- (block-y (first lob)) (block-y (fourth lob))) -40)))
        
(@htdf rotate-L-db-4)
(@signature ListOfBlock -> ListOfBlock)
;; check orientation and rotate clockwise accordingly

(check-expect (rotate-L-db-4 (cons (make-block db-block (+ (- (- (+ CTR-X 20) 20) 20) 20) (- (- (+ -50 20) 20) 20))
                                   (cons (make-block db-block CTR-X -50)
                                         (cons (make-block db-block (+ (+ (- CTR-X 20) 20) 20) (+ (- (- -30 20) 20) 20))
                                               (cons (make-block db-block (+ (+ (- CTR-X 40) 40) 40) (+ (- (- -10 40) 40) 40)) empty)))))
              (cons (make-block db-block  (+ (+ (- (- (+ CTR-X 20) 20) 20) 20) 20) (+ (- (- (+ -50 20) 20) 20) 20))
                    (cons (make-block db-block CTR-X -50)
                          (cons (make-block db-block (- (+ (+ (- CTR-X 20) 20) 20) 20) (+ (+ (- (- -30 20) 20) 20) 20))
                                (cons (make-block db-block (- (+ (+ (- CTR-X 40) 40) 40) 40) (+ (+ (- (- -10 40) 40) 40) 40)) empty)))))

#;
(define (rotate-L-db-4 lob) lob) ;stub
                

(define (rotate-L-db-4 lob)
  (cons (make-block db-block (+ (block-x (first lob)) 20) (+ (block-y (first lob)) 20))
        (cons (make-block db-block  (block-x (second lob))  (block-y (second lob)))
              (cons (make-block db-block (- (block-x (third lob)) 20) (+ (block-y (third lob)) 20))
                    (cons (make-block db-block (- (block-x (fourth lob)) 40) (+ (block-y (fourth lob)) 40)) (rest (rest (rest (rest lob)))))))))


(@htdf L-db-4?)
(@signature ListOfBlock -> Boolean)
;; if L-o shape produce true
(check-expect (L-db-4? (cons (make-block db-block (+ (- (- (+ CTR-X 20) 20) 20) 20) (- (- (+ -50 20) 20) 20))
                             (cons (make-block db-block CTR-X -50)
                                   (cons (make-block db-block (+ (+ (- CTR-X 20) 20) 20) (+ (- (- -30 20) 20) 20))
                                         (cons (make-block db-block (+ (+ (- CTR-X 40) 40) 40) (+ (- (- -10 40) 40) 40)) empty))))) true)
(check-expect (L-db-4? (cons (make-block db-block (+ CTR-X 20) -50)
                             (cons (make-block db-block CTR-X -50)
                                   (cons (make-block db-block CTR-X -30)
                                         (cons (make-block db-block CTR-X -10)
                                               (cons (make-block db-block (- CTR-X 20) -50)
                                                     (cons (make-block db-block CTR-X -50)
                                                           (cons (make-block db-block CTR-X -30)
                                                                 (cons (make-block db-block CTR-X -10) empty))))))))) false)

#;
(define (L-db-4? b) true) ;stub

(define (L-db-4? lob)
  (and (= (- (block-x (first lob)) (block-x (fourth lob))) -40)
       (= (- (block-y (first lob)) (block-y (fourth lob))) -20)))


(@htdf rotate-L-db-3)
(@signature ListOfBlock -> ListOfBlock)
;; check orientation and rotate clockwise accordingly

(check-expect (rotate-L-db-3 (cons (make-block db-block (- (- (+ CTR-X 20) 20) 20) (- (+ -50 20) 20))
                                   (cons (make-block db-block CTR-X -50)
                                         (cons (make-block db-block (+ (- CTR-X 20) 20) (- (- -30 20) 20))
                                               (cons (make-block db-block (+ (- CTR-X 40) 40) (- (- -10 40) 40)) empty)))))
              (cons (make-block db-block (+ (- (- (+ CTR-X 20) 20) 20) 20) (- (- (+ -50 20) 20) 20))
                    (cons (make-block db-block CTR-X -50)
                          (cons (make-block db-block (+ (+ (- CTR-X 20) 20) 20) (+ (- (- -30 20) 20) 20))
                                (cons (make-block db-block (+ (+ (- CTR-X 40) 40) 40) (+ (- (- -10 40) 40) 40)) empty)))))

#;
(define (rotate-L-db-3 lob) lob) ;stub
                

(define (rotate-L-db-3 lob)
  (cons (make-block db-block (+ (block-x (first lob)) 20) (- (block-y (first lob)) 20))
        (cons (make-block db-block  (block-x (second lob))  (block-y (second lob)) )
              (cons (make-block db-block (+ (block-x (third lob)) 20) (+ (block-y (third lob)) 20))
                    (cons (make-block db-block (+ (block-x (fourth lob)) 40) (+ (block-y (fourth lob)) 40)) (rest (rest (rest (rest lob)))))))))


(@htdf L-db-3?)
(@signature ListOfBlock -> Boolean)
;; if L-o shape produce true
(check-expect (L-db-3? (cons (make-block db-block (- (- (+ CTR-X 20) 20) 20) (- (+ -50 20) 20))
                             (cons (make-block db-block CTR-X -50)
                                   (cons (make-block db-block (+ (- CTR-X 20) 20) (- (- -30 20) 20))
                                         (cons (make-block db-block (+ (- CTR-X 40) 40) (- (- -10 40) 40)) empty))))) true)
(check-expect (L-db-3? (cons (make-block db-block (+ CTR-X 20) -50)
                             (cons (make-block db-block CTR-X -50)
                                   (cons (make-block db-block CTR-X -30)
                                         (cons (make-block db-block CTR-X -10)
                                               (cons (make-block db-block (- CTR-X 20) -50)
                                                     (cons (make-block db-block CTR-X -50)
                                                           (cons (make-block db-block CTR-X -30)
                                                                 (cons (make-block db-block CTR-X -10) empty))))))))) false)

#;
(define (L-db-3? b) true) ;stub

(define (L-db-3? lob)
  (and (= (- (block-x (first lob)) (block-x (fourth lob))) -20)
       (= (- (block-y (first lob)) (block-y (fourth lob))) 40)))
  
(@htdf rotate-L-db-2)
(@signature ListOfBlock -> ListOfBlock)
;; check orientation and rotate clockwise accordingly

(check-expect (rotate-L-db-2 (cons (make-block db-block (- (+ CTR-X 20) 20) (+ -50 20))
                                   (cons (make-block db-block CTR-X -50)
                                         (cons (make-block db-block (- CTR-X 20)(- -30 20))
                                               (cons (make-block db-block (- CTR-X 40) (- -10 40)) empty)))))
              (cons (make-block db-block (- (- (+ CTR-X 20) 20) 20) (- (+ -50 20) 20))
                    (cons (make-block db-block CTR-X -50)
                          (cons (make-block db-block (+ (- CTR-X 20) 20) (- (- -30 20) 20))
                                (cons (make-block db-block (+ (- CTR-X 40) 40) (- (- -10 40) 40)) empty)))))

#;
(define (rotate-L-db-2 lob) lob) ;stub
                

(define (rotate-L-db-2 lob)
  (cons (make-block db-block (- (block-x (first lob)) 20) (- (block-y (first lob)) 20))
        (cons (make-block db-block  (block-x (second lob))  (block-y (second lob)) )
              (cons (make-block db-block (+ (block-x (third lob)) 20) (- (block-y (third lob)) 20))
                    (cons (make-block db-block (+ (block-x (fourth lob)) 40) (- (block-y (fourth lob)) 40)) (rest (rest (rest (rest lob)))))))))

(@htdf L-db-2?)
(@signature ListOfBlock -> Boolean)
;; if L-o shape produce true
(check-expect (L-db-2? (cons (make-block db-block (- (+ CTR-X 20) 20) (+ -50 20))
                             (cons (make-block db-block CTR-X -50)
                                   (cons (make-block db-block (- CTR-X 20)(- -30 20))
                                         (cons (make-block db-block (- CTR-X 40) (- -10 40)) empty))))) true)
(check-expect (L-db-2? (cons (make-block db-block (+ CTR-X 20) -50)
                             (cons (make-block db-block CTR-X -50)
                                   (cons (make-block db-block CTR-X -30)
                                         (cons (make-block db-block CTR-X -10)
                                               (cons (make-block db-block (- CTR-X 20) -50)
                                                     (cons (make-block db-block CTR-X -50)
                                                           (cons (make-block db-block CTR-X -30)
                                                                 (cons (make-block db-block CTR-X -10) empty))))))))) false)

#;
(define (L-db-2? b) true) ;stub

(define (L-db-2? lob)
  (and (= (- (block-x (first lob)) (block-x (fourth lob))) 40)
       (= (- (block-y (first lob)) (block-y (fourth lob))) 20)))

(@htdf rotate-L-db)
(@signature ListOfBlock -> ListOfBlock)
;; check orientation and rotate clockwise accordingly
(check-expect (rotate-L-db (cons (make-block db-block (+ CTR-X 20) -50)
                                 (cons (make-block db-block CTR-X -50)
                                       (cons (make-block db-block CTR-X -30)
                                             (cons (make-block db-block CTR-X -10) empty)))))
              (cons (make-block db-block (- (+ CTR-X 20) 20) (+ -50 20))
                    (cons (make-block db-block CTR-X -50)
                          (cons (make-block db-block (- CTR-X 20)(- -30 20))
                                (cons (make-block db-block (- CTR-X 40) (- -10 40)) empty)))))

#;
(define (rotate-L-db lob) lob) ;stub

(define (rotate-L-db lob)
  (cons (make-block db-block (- (block-x (first lob)) 20) (+ (block-y (first lob)) 20))
        (cons (make-block db-block  (block-x (second lob))  (block-y (second lob)) )
              (cons (make-block db-block (- (block-x (third lob)) 20) (- (block-y (third lob)) 20))
                    (cons (make-block db-block (- (block-x (fourth lob)) 40) (- (block-y (fourth lob)) 40)) (rest (rest (rest (rest lob)))))))))
  


(@htdf L-db?)
(@signature ListOfBlock -> Boolean)
;; if L-db shape produce true
(check-expect (L-db? (cons (make-block db-block (- CTR-X 20) -50)
                           (cons (make-block db-block CTR-X -50)
                                 (cons (make-block db-block CTR-X -30)
                                       (cons (make-block db-block CTR-X -10) empty))))) false)
(check-expect (L-db? (cons (make-block db-block (+ CTR-X 20) -50)
                           (cons (make-block db-block CTR-X -50)
                                 (cons (make-block db-block CTR-X -30)
                                       (cons (make-block db-block CTR-X -10)
                                             (cons (make-block db-block (- CTR-X 20) -50)
                                                   (cons (make-block db-block CTR-X -50)
                                                         (cons (make-block db-block CTR-X -30)
                                                               (cons (make-block db-block CTR-X -10) empty))))))))) true)

#;
(define (L-db? b) true) ;stub

(define (L-db? lob)
  (and (= (- (block-x (first lob)) (block-x (fourth lob))) 20)
       (= (- (block-y (first lob)) (block-y (fourth lob))) -40)))


(@htdf line-down?)
(@signature ListOfBlock -> Boolean)
;; if line shape produce true
(check-expect (line-down? (cons (make-block b-block (- CTR-X 20) -30)
                                (cons (make-block b-block  CTR-X -30)
                                      (cons (make-block b-block  (+ CTR-X 20) -30)
                                            (cons (make-block  b-block (+ CTR-X 40) -30) empty))))) true)


#;
(define (line-down? lob) true) ;stub

(define (line-down? lob)
  (= (- (block-x (first lob)) (block-x (fourth lob))) -60))

(@htdf rotate-line-up)
(@signature ListOfBlock -> ListOfBlock)
;; rotate 90 degrees ccw


(define (rotate-line-up lob)
  (cons (make-block b-block (block-x (second lob))  (- (block-y (first lob)) 20))
        (cons (make-block b-block (block-x (second lob))  (block-y (second lob)))
              (cons (make-block b-block (block-x (second lob)) (+ (block-y (third lob)) 20))
                    (cons (make-block  b-block (block-x (second lob)) (+ (block-y (fourth lob)) 40)) (rest (rest (rest (rest lob)))))))))     

(@htdf rotate-line-cw)
(@signature ListOfBlock -> ListOfBlock)
;; rotate 90 degrees cw
(check-expect (rotate-line-cw (cons (make-block b-block CTR-X -70)
                                    (cons (make-block b-block CTR-X -50)
                                          (cons (make-block b-block CTR-X -30)
                                                (cons (make-block  b-block CTR-X -10) empty)))))
              (cons (make-block b-block (- CTR-X 20) -50)
                    (cons (make-block b-block  CTR-X -50)
                          (cons (make-block b-block  (+ CTR-X 20) -50)
                                (cons (make-block  b-block (+ CTR-X 40) -50) empty)))))

#;
(define (rotate-line-cw lob) lob) ;stub

(define (rotate-line-cw lob)
  (cons (make-block b-block (- (block-x (first lob)) 20) (block-y (second lob)))
        (cons (make-block b-block  (block-x (second lob)) (block-y (second lob)))
              (cons (make-block b-block  (+ (block-x (third lob)) 20) (block-y (second lob)))
                    (cons (make-block  b-block (+ (block-x (fourth lob)) 40) (block-y (second lob))) (rest (rest (rest (rest lob)))))))))


(@htdf line-up?)
(@signature ListOfBlock -> Boolean)
;; if line shape produce true
(check-expect (line-up? (cons (make-block b-block CTR-X -70)
                              (cons (make-block b-block CTR-X -50)
                                    (cons (make-block b-block CTR-X -30)
                                          (cons (make-block  b-block CTR-X -10) empty))))) true)

(check-expect (line-up? (cons (make-block y-block CTR-X -10)
                              (cons (make-block y-block CTR-X -30)
                                    (cons (make-block y-block (+ CTR-X 20) -10)
                                          (cons (make-block y-block (+ CTR-X 20) -30) (cons (make-block b-block CTR-X -70)
                                                                                            (cons (make-block b-block CTR-X -50)
                                                                                                  (cons (make-block b-block CTR-X -30)
                                                                                                        (cons (make-block  b-block CTR-X -10) empty))))))))) false)

#;
(define (line-up? lob) true) ;stub

(define (line-up? lob)
  (= (- (block-y (first lob)) (block-y (fourth lob))) -60))
       
 




(@htdf moving-down-ok?)
(@signature ListOfBlock -> Boolean)
;; check first 4 blocks if moving down won't pass bottom and run into others
;; CONSTRAINT: will receive atleast 4 blocks
(check-expect (moving-down-ok? (cons (make-block y-block CTR-X 10)
                                     (cons (make-block y-block CTR-X 30)
                                           (cons (make-block y-block (+ CTR-X 20) 10)
                                                 (cons (make-block y-block (+ CTR-X 20) 30) empty)))))
              true)
(check-expect (moving-down-ok? (cons (make-block y-block CTR-X (- HEIGHT 10))
                                     (cons (make-block y-block CTR-X (- HEIGHT 30))
                                           (cons (make-block y-block (+ CTR-X 20) (- HEIGHT 10))
                                                 (cons (make-block y-block (+ CTR-X 20) (- HEIGHT 30)) empty)))))
              false)
                                                 

(define (moving-down-ok? lob)
  (and (<= (+ (block-y (first lob)) S-SPEED) (- HEIGHT 19))
       (<= (+ (block-y (second lob)) S-SPEED) (- HEIGHT 19))
       (<= (+ (block-y (third lob)) S-SPEED) (- HEIGHT 19))
       (<= (+ (block-y (fourth lob)) S-SPEED) (- HEIGHT 19))
       (run-down-others? lob)))

(@htdf run-down-others?)
(@signature ListOfBlock -> Boolean)
;; checks if 4 blocks moving down will go through other blocks
;; CONSTRAINT: will receive atleast 4 blocks
 


(define (run-down-others? lob)
  (cond [(empty? lob) false]
        [(any-touch-s? (first lob) (rest (rest (rest (rest lob))))) false]
        [(any-touch-s? (second lob) (rest (rest (rest (rest lob))))) false]
        [(any-touch-s? (third lob) (rest (rest (rest (rest lob))))) false]
        [(any-touch-s? (fourth lob) (rest (rest (rest (rest lob))))) false]
        [else true]))


(@htdf any-touch-s?)
(@signature Block ListOfBlock -> Boolean)
;; checks if any of the four blocks touch rest of list blocks (same x and (y - 20))
;; CONSTRAINT: will always receive four blocks
(check-expect (any-touch-s?  (make-block b-block CTR-X CTR-Y)
                             empty) false)
(check-expect (any-touch-s?  (make-block b-block (+ CTR-X 60) (+ CTR-Y 60))
                             (cons (make-block b-block CTR-X (+ CTR-Y 100)) empty)) false) 
(check-expect (any-touch-s?  (make-block b-block (+ CTR-X 60) (+ CTR-Y 51))
                             (cons (make-block b-block (+ CTR-X 60) (+ CTR-Y 70)) empty)) true)
(check-expect (any-touch-s?  (make-block b-block (+ CTR-X 40) (+ CTR-Y 29))
                             (cons (make-block b-block (+ CTR-X 40) (+ CTR-Y 70)) empty)) false)
(check-expect (any-touch-s?  (make-block b-block (+ CTR-X 20) (+ CTR-Y 80))
                             (cons (make-block b-block (+ CTR-X 20) (+ CTR-Y 80)) empty)) true)


#;
(define (any-touch? b lob) true) ;stub

(@template Block)

(define (any-touch-s? b lob)
  (cond [(empty? lob) false]
        [else (if (and (>= (+ (block-y b) S-SPEED) (- (block-y (first lob)) 19))       ; (first lob) will be below block so if moving block is within 19 units then true
                       (= (block-x b) (block-x (first lob))))
                  true
                  (any-touch-s? b (rest lob)))]))
   

(@htdf moving-right-ok?)
(@signature ListOfBlock -> Boolean)
;; check first 4 blocks if moving right won't pass the edge
;; CONSTRAINT: will receive atleast 4 blocks 

(define (moving-right-ok? lob)
  (and (<= (+ (block-x (first lob)) SPEED-X) WIDTH)
       (<= (+ (block-x (second lob)) SPEED-X) WIDTH)
       (<= (+ (block-x (third lob)) SPEED-X) WIDTH)
       (<= (+ (block-x (fourth lob)) SPEED-X) WIDTH)
       (right-others? lob)))

(@htdf right-others?)
(@signature ListOfBlock -> Boolean)
;; any of first 4 run into other with same y and (- x 20) coord?
;; CONSTRAINT: will recieve 4 blocks


(define (right-others? lob)
  (cond [(empty? lob) false]
        [(any-touch-d? (first lob) (rest (rest (rest (rest lob))))) false]
        [(any-touch-d? (second lob) (rest (rest (rest (rest lob))))) false]
        [(any-touch-d? (third lob) (rest (rest (rest (rest lob))))) false]
        [(any-touch-d? (fourth lob) (rest (rest (rest (rest lob))))) false]
        [else true]))
        

(@htdf any-touch-d?)
(@signature Block ListOfBlock -> Boolean)
;; checks if any of the four blocks touch rest of list blocks (same x and (y - 20))
;; CONSTRAINT: will always receive four blocks
(check-expect (any-touch-d?  (make-block b-block CTR-X CTR-Y)
                             empty) false)
(check-expect (any-touch-d?  (make-block b-block (+ CTR-X 60) (+ CTR-Y 60))
                             (cons (make-block b-block CTR-X (+ CTR-Y 100)) empty)) false) 
(check-expect (any-touch-d?  (make-block b-block (+ CTR-X 60) (+ CTR-Y 61))
                             (cons (make-block b-block (+ CTR-X 60) (+ CTR-Y 70)) empty)) true)
(check-expect (any-touch-d?  (make-block b-block (+ CTR-X 20) (+ CTR-Y 79))
                             (cons (make-block b-block (+ CTR-X 40) (+ CTR-Y 70)) empty)) true)
(check-expect (any-touch-d?  (make-block b-block (+ CTR-X 10) (+ CTR-Y 80))
                             (cons (make-block b-block (+ CTR-X 20) (+ CTR-Y 80)) empty)) true)
(check-expect (any-touch-d?  (make-block b-block (+ CTR-X 10) (+ CTR-Y 80))
                             (cons (make-block b-block (+ CTR-X 60) (+ CTR-Y 80)) empty)) false)


#;
(define (any-touch-d? b lob) true) ;stub

(@template Block)

(define (any-touch-d? b lob)
  (cond [(empty? lob) false]
        [else (if (and (>= (block-y b) (- (block-y (first lob)) 19))    ; can still move right if right above a block
                       (<= (block-y b) (+ (block-y (first lob)) 19))            
                       (or (>= (block-x (first lob))
                               (block-x b)
                               (- (block-x (first lob)) 19))
                           (= (+ (block-x b) 20) (block-x (first lob)))))
                  true
                  (any-touch-d? b (rest lob)))]))

(@htdf moving-left-ok?)
(@signature ListOfBlock -> Boolean)
;; check first 4 blocks if moving left won't pass edge
;; CONSTRAINT: will receive atleast 4 blocks 

(define (moving-left-ok? lob)
  (and (>= (- (block-x (first lob)) SPEED-X) 0)
       (>= (- (block-x (second lob)) SPEED-X) 0)
       (>= (- (block-x (third lob)) SPEED-X) 0)
       (>= (- (block-x (fourth lob)) SPEED-X) 0)
       (left-others? lob)))


(@htdf left-others?)
(@signature ListOfBlock -> Boolean)
;; any of first 4 run into other with same y and (- x 20) coord?
;; CONSTRAINT: will recieve 4 blocks


(define (left-others? lob)
  (cond [(empty? lob) false]
        [(any-touch-l? (first lob) (rest (rest (rest (rest lob))))) false]
        [(any-touch-l? (second lob) (rest (rest (rest (rest lob))))) false]
        [(any-touch-l? (third lob) (rest (rest (rest (rest lob))))) false]
        [(any-touch-l? (fourth lob) (rest (rest (rest (rest lob))))) false]
        [else true]))
        

(@htdf any-touch-l?)
(@signature Block ListOfBlock -> Boolean)
;; checks if any of the four blocks touch rest of list blocks (same x and (y - 20))
;; CONSTRAINT: will always receive four blocks
(check-expect (any-touch-l?  (make-block b-block CTR-X CTR-Y)
                             empty) false)
(check-expect (any-touch-l?  (make-block b-block (+ CTR-X 60) (+ CTR-Y 60))
                             (cons (make-block b-block CTR-X (+ CTR-Y 100)) empty)) false) 
(check-expect (any-touch-l?  (make-block b-block (+ CTR-X 60) (+ CTR-Y 61))
                             (cons (make-block b-block (+ CTR-X 60) (+ CTR-Y 70)) empty)) true)
(check-expect (any-touch-l?  (make-block b-block (+ CTR-X 60) (+ CTR-Y 79))
                             (cons (make-block b-block (+ CTR-X 40) (+ CTR-Y 70)) empty)) true)
(check-expect (any-touch-l?  (make-block b-block (+ CTR-X 40) (+ CTR-Y 80))
                             (cons (make-block b-block (+ CTR-X 20) (+ CTR-Y 80)) empty)) true)
(check-expect (any-touch-l?  (make-block b-block (+ CTR-X 10) (+ CTR-Y 80))
                             (cons (make-block b-block (+ CTR-X 60) (+ CTR-Y 80)) empty)) false)


#;
(define (any-touch-l? b lob) true) ;stub

(@template Block)

(define (any-touch-l? b lob)
  (cond [(empty? lob) false]
        [else (if (and (>= (block-y b) (- (block-y (first lob)) 19))         ; can still move right if right above a block
                       (<= (block-y b) (+ (block-y (first lob)) 19))            
                       (or (<= (block-x (first lob))
                               (block-x b)
                               (+ (block-x (first lob)) 19))
                           (= (- (block-x b) 20) (block-x (first lob)))))
                  true
                  (any-touch-l? b (rest lob)))]))





(@htdf add-first4-x)
(@signature ListOfBlock -> ListOfBlock)
;; add in x direction
;; CONSTRAINT: will receive atleast 4 blocks 

(define (add-first4-x lob)
  (cons (make-block (block-img (first lob))
                    (+ (block-x (first lob))  SPEED-X) 
                    (block-y (first lob)))
        (cons (make-block (block-img (second lob))
                          (+ (block-x (second lob)) SPEED-X)
                          (block-y (second lob)))
              (cons (make-block (block-img (third lob))
                                (+ (block-x (third lob)) SPEED-X)
                                (block-y (third lob)))
                    (cons (make-block (block-img (fourth lob))
                                      (+ (block-x (fourth lob)) SPEED-X)
                                      (block-y (fourth lob))) (rest (rest (rest (rest lob)))))))))

(@htdf sub-first4-x)
(@signature ListOfBlock -> ListOfBlock)
;; sub in x direction 

(define (sub-first4-x lob)
  (cons (make-block (block-img (first lob))
                    (- (block-x (first lob)) SPEED-X)
                    (block-y (first lob)))
        (cons (make-block (block-img (second lob))
                          (- (block-x (second lob)) SPEED-X)
                          (block-y (second lob)))
              (cons (make-block (block-img (third lob))
                                (- (block-x (third lob)) SPEED-X)
                                (block-y (third lob)))
                    (cons (make-block (block-img (fourth lob))
                                      (- (block-x (fourth lob)) SPEED-X)
                                      (block-y (fourth lob))) (rest (rest (rest (rest lob)))))))))


(@htdf add-first4-y)
(@signature ListOfBlock -> ListOfBlock)
;; add in y direction

(define (add-first4-y lob)
  (cons (make-block (block-img (first lob))
                    (block-x (first lob))
                    (+ (block-y (first lob)) S-SPEED))
        (cons (make-block (block-img (second lob))
                          (block-x (second lob))
                          (+ (block-y (second lob)) S-SPEED))
              (cons (make-block (block-img (third lob))
                                (block-x (third lob))
                                (+ (block-y (third lob)) S-SPEED))
                    (cons (make-block (block-img (fourth lob))
                                      (block-x (fourth lob))
                                      (+ (block-y (fourth lob)) S-SPEED)) (rest (rest (rest (rest lob)))))))))














      



