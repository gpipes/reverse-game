#lang racket
(require racket/gui/base)
(require "board.rkt")

#|
should be able to show a window that has the board game on it
should have a function that takes a board and shows it
button to reset game with original board
way to configure clicks
button to start new game with new board

to draw to a canvas, on
|#

(define STANDARD-RECT-SIZE 50)
(define STANDARD-OFFSET STANDARD-RECT-SIZE)

(define game-frame%
  (class frame%
    (init-field [board-size 4]
                [clicks 4])
    
    (define/public (init)
      (new in-game-canvas%
           [clicks clicks]
           [board (init-board board-size clicks)]
           [parent this]))
    
    (define (one-rect-buff size length)
      (+ (* STANDARD-OFFSET 3)(* size length)))
    
    (super-new [label "Reverser"]
               [alignment '(center center)]
               [width (one-rect-buff STANDARD-RECT-SIZE
                                     board-size)]
               [height (one-rect-buff STANDARD-RECT-SIZE
                                      board-size)])))

(define in-game-canvas%
  (class canvas%
    (init-field clicks
                [board '(())]
                [initial-clicks clicks]
                [initial-board board]
                [rect-size STANDARD-RECT-SIZE]
                [offset STANDARD-OFFSET]) ;; zero board default

    (define/private (screen-coords->grid-coords x y)
      (values (add1 (quotient (- x offset) rect-size))
              (add1 (quotient (- y offset) rect-size)))) 
    
    (define (draw-board canvas dc)
      (for ([line board]
            [i (in-naturals)])
        (for ([color line]
              [j (in-naturals)])
          (send dc set-brush (get-color-string color) 'opaque)
          (send dc draw-rectangle
                (+ offset (* rect-size j)) (+ offset (* rect-size i))
                rect-size rect-size))))

    (define (draw-clicks canvas dc)
      (send dc draw-text
            (format "Clicks-Left: ~a" clicks)
            10 10))
    
    (define (draw-game-screen canvas dc)
      (draw-clicks canvas dc)
      (draw-board canvas dc))
    
    (init-field [paint-callback draw-game-screen])

    (define/public (set-board! new-board)
      (set! board new-board))

    (define/override (on-event event) ;; mouse event
      (when (send event button-up?)
        (define-values (x y) (screen-coords->grid-coords
                              (send event get-x) (send event get-y)))
        (set-board! (update-plusf board x y))
        (set! clicks (sub1 clicks))
        (send this refresh-now)))

    (define/public (board-length)
      (length board))

    (super-new [paint-callback paint-callback])))

(define(start-game)
  (define frame (new game-frame%))
  (send frame init)
  (send frame show #t))