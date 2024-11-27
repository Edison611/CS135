;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname jeopardy) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Edison Ying (21119725)
;; CS 135 Fall 2024
;; Assignment 01, Q5 Jeopardy
;; ***************************************************
;;

;; (a)
(define (min-wager c1 c2 c3)
  (- (+ (* 2 (max c2 c3)) 1) c1))

;; (b)
(define (missed-question c1 c2 c3)
  (- c1 (min-wager c1 c2 c3)))