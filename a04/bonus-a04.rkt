;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname bonus-a04) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Edison Ying (21119725)
;; CS 135 Fall 2024
;; Assignment 04, Bonus
;; ***************************************************
;;

;; Examples:
(check-expect
 (wordle-guess
  (cons #\h (cons #\e (cons #\l (cons #\l (cons #\o empty)))))
  (cons #\s (cons #\t (cons #\o (cons #\l (cons #\e empty))))))
 (cons 'gray (cons 'gray (cons 'yellow (cons 'green (cons 'yellow
                                                          empty))))))

(define (wordle-guess actual guess)
  (cond
    [(empty? guess) empty]
    [(<= (exists-index (first guess) actual 1) 0)
     (cons 'gray (wordle-guess actual (rest guess)))]
    [(= (- 6 (list-length guess)) ; compare index of guess with index of actual
        (exists-index (first guess) actual (- 6 (list-length guess))))
     (cons 'green (wordle-guess actual (rest guess)))]
    [else (cons 'yellow (wordle-guess actual (rest guess)))]
  ))

(define (exists-index val list starting-index)
  (cond
    [(empty? list) -6]
    [(< (- 6 (list-length list)) starting-index)
     (exists-index val (rest list) starting-index)]
    [(char=? (first list) val) starting-index]
    [else (+ 1 (exists-index val (rest list) starting-index))]))

(define (list-length list)
  (cond [(empty? list) 0]
        [else (+ 1 (list-length (rest list)))]))