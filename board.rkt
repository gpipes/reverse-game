#lang racket
(require rackunit)

(provide update-plusf
         update-plusp
         init-board)

(define color-list '(blue green red))
(define color-stream (sequence->stream (in-cycle '(blue green red))))
  
(define (empty-board size)
  (make-list size (make-list size 'blue)))

(define (get-next-color color)
  (stream-ref color-stream (+ (index-of color-list color) 1)))

(define (get-previous-color color)
  (stream-ref color-stream (+ (index-of color-list color)
                              (- (length color-list) 1)))) 

(define (update-single lst pos proc)
  (if (and (<= pos (length lst)) (> pos 0))
    (append (take lst (- pos 1))
            (list (proc (list-ref lst (- pos 1))))
            (drop lst pos))
    lst))

(define (update-with-neighbors lst pos proc)
  (update-single
   (update-single (update-single lst pos proc) (- pos 1) proc)
   (+ pos 1) proc))

(define (get-until-one-before lst place)
  (cond [(or (> place (length lst)) (<= place 0))
         (error "Out of bounds index")]
        [(<= place 2) (take lst 0)]
        [else (take lst (- place 2))]))

(define (get-from-one-after lst place)
  (define size (length lst))
  (cond [(or (> place size) (<= place 0))
         (error "Out of bounds index")]
        [(equal? place size) (drop lst place)]
        [else (drop lst (+ place 1))]))

(define (update-plus board x y proc)
  (define boundary (length board))
  (define real-y y);;(+ (- boundary y) 1)) ;; to reverse the y coordinates
  (define real-x x)
  (if (and (<= x boundary) (<= y boundary))
      (let* ([top-list (if (<= (- real-y 1) 0)
                           (get-until-one-before board real-y)
                           (append (get-until-one-before board real-y)
                                   (list (update-single (list-ref board (- real-y 2))
                                                        real-x proc))))]
             [middle-list (append top-list
                                  (list (update-with-neighbors (list-ref board (- real-y 1))
                                                               real-x proc)))]
             [rest-list (append middle-list
                                (if (> (+ real-y 1) boundary)
                                    (get-from-one-after board real-y)
                                    (append (list (update-single (list-ref board real-y)
                                                                 real-x proc))
                                            (get-from-one-after board real-y))))])
        rest-list)
      board))

(define (update-plusf board x y)
  (update-plus board x y get-next-color))

(define (update-plusp board x y)
  (update-plus board x y get-previous-color))

(define (init-board size clicks)
  (define (init-board-helper board size clicks)
    (cond [(> clicks 0)
           (init-board-helper (update-plusp board
                                            (+ (random size) 1)
                                            (+ (random size) 1))
                              size (- clicks 1))]
          [else board]))
  (init-board-helper (empty-board size) size clicks))

(module+ test
  (define test-board1 '((blue blue blue blue)
                        (blue blue blue blue)
                        (blue blue blue blue)
                        (blue blue blue blue)))
  
  (define test-board2 '((blue blue blue  blue )
                        (blue blue blue  blue )
                        (blue blue blue  green)
                        (blue blue green green)))
  
  (define test-board3 '((green green blue blue)
                        (green blue  blue blue)
                        (blue  blue  blue blue)
                        (blue  blue  blue blue)))

  (define test-board4 '((red   red  blue  green)
                        (blue  red  green blue )
                        (green blue blue  blue )
                        (blue  red  red   green)))
  
  (define test-board5 '((red   red  green green)
                        (blue  blue red   green)
                        (red   blue green blue )
                        (green blue red   green)))
  
  (check-equal? (update-plusf test-board1 1 1)
                test-board3)
  
  (check-equal? (update-plusf test-board1 4 4)
                test-board2)

  (check-equal? (update-plusf (update-plusf test-board4 3 2) 1 4)
                test-board5)

  (check-equal? (update-plusp test-board3 1 1)
                test-board1)

  (check-equal? (update-plusp test-board2 4 4)
                test-board1)

  (check-equal? (update-plusp (update-plusp test-board5 3 2) 1 4)
                test-board4)
  )
