;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname median) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Edison Ying (21119725)
;; CS 135 Fall 2024
;; Assignment 02, Q2 Median
;; ***************************************************
;;

(define (median-of-3-simple a b c)
  ;; Think of this question as a number line where you place a and b first, then try to figure out where to place c\
  ;; 5 inequalities used
  (cond
    [(<= b a) (cond
                [(<= a c) a]
                [(<= c b) b]
                [else c])]
    [else (cond
            [(<= b c) b]
            [(<= c a) a]
            [else c])]))

(check-expect (median-of-3-simple 4 5 6) 5)
(check-expect (median-of-3-simple 100 50 1) 50)
(check-expect (median-of-3-simple 4 10 8) 8)
(check-expect (median-of-3-simple 10 2 5) 5)