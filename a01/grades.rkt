;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname grades) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Edison Ying (21119725)
;; CS 135 Fall 2024
;; Assignment 01, Q6 Grades
;; ***************************************************
;;

;; (a)
(define (final-cs135-grade participation-grade midterm-grade final-grade assessment-grade)
  (+ (* 0.05 participation-grade)
     (* 0.25 midterm-grade)
     (* 0.45 final-grade)
     (* 0.25 assessment-grade)))

;; (b)
(define (cs135-final-exam-grade-needed participation-grade midterm-grade assessment-grade)
  (/ (- 60 (+ (* 0.05 participation-grade)
              (* 0.25 midterm-grade)
              (* 0.25 assessment-grade))) 0.45))
