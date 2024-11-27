;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname bonus-a01) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Edison Ying (21119725)
;; CS 135 Fall 2024
;; Assignment 01, Q7 Bonus
;; ***************************************************
;;

(define (final-cs135-grade participation-grade midterm-grade final-grade assessment-grade)
  (+ (* (f midterm-grade final-grade assessment-grade) ; if you passed everything
        (mark participation-grade midterm-grade final-grade assessment-grade)) 
     (* (- 1 (f midterm-grade final-grade assessment-grade)) ; if you fail one thing
        (min 46 (mark participation-grade midterm-grade final-grade assessment-grade)))
  ))

(define (mark participation-grade midterm-grade final-grade assessment-grade)
  ;; returns weighted average of everything
    (+ (* 0.05 participation-grade)
     (* 0.25 midterm-grade)
     (* 0.45 final-grade)
     (* 0.25 assessment-grade)))

(define (f midterm-grade final-grade assessment-grade)
  ;; returns 0 if there's a mark < 50 (conditions met)
  ;; returns 1 if no mark is lower than 50
  (floor (min (/ (+ (* 45/70 final-grade)
                    (* 25/70 midterm-grade)) 50)
              (/ assessment-grade 50) 1)))