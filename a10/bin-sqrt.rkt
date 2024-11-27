;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname bin-sqrt) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***************************************************
;; Edison Ying (21119725)
;; CS 135 Fall 2024
;; Assignment 10, Q1 Bin Sqrt
;; ***************************************************
;;

;; (bin-sqrt n tol) produces the square root of n with a tolerance of tol
;; Examples:
(check-within (bin-sqrt 2 0.001) (sqrt 2) 0.001)
(check-within (bin-sqrt 5 0.001) (sqrt 5) 0.001)
(check-within (bin-sqrt 1000000 0.001) (sqrt 1000000) 0.001)
(check-within (bin-sqrt 10000001000000100000010000001000000 0.001)
              (sqrt 10000001000000100000010000001000000) 0.001)
;; bin-sqrt: Nat Num -> Num
;; requires:
;; - tolerance > 0
;; - n >= 1

(define (bin-sqrt n tol)
  (local [(define (bin-sqrt/rec low high)
          (local [(define k (/ (+ low high) 2))]
            (cond [(< (abs (- (sqr k) n)) tol) k]
                  [(< (sqr k) n) (bin-sqrt/rec k high)]
                  [(> (sqr k) n) (bin-sqrt/rec low k)])))]
    (bin-sqrt/rec 0 n)))



    