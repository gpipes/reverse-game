#lang racket
(require "board.rkt")

;;TODO change to GUI

(define SIZE   4)
(define CLICKS 3)

(define color-strings (make-immutable-hash '((blue  . "blue  ")
                                             (red   . " red  ")
                                             (green . "green "))))
(define (print-board board)
  (for-each (lambda (line)
              (for-each
               (lambda (color) (display (hash-ref color-strings color)))
               line)
              (displayln ""))
            board))

(define (get-x-and-y-input)
  (let ([input (read-line)])
    (apply values (map (lambda (x) (string->number x))
                       (string-split input " ")))))

(define (prompt-and-update board)
  (print-board board)
  (display "Input x and y of click: ")
  (define-values (x y)
    (get-x-and-y-input))
  (update-plusf board x y))

(define (is-all-blue board)
  (equal? (apply + (map (lambda (line)
                          (count (lambda (val) (equal? val 'blue)) line))
                        board))
          (sqr (length board))))

(define (game-loop board clicks-left)
  (cond [(is-all-blue board) (displayln "Win!")]
        [(<=  clicks-left 0) (displayln "Lose :(")]
        [else (game-loop (prompt-and-update board) (- clicks-left 1))]))

(define game-board (init-board SIZE CLICKS))


(define (start-game)
  (printf "~a Clicks game.\n" CLICKS)
  (game-loop game-board CLICKS))
