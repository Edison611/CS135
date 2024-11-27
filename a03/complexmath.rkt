;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname complexmath) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Edison Ying (21119725)
;; CS 135 Fall 2024
;; Assignment 03, Q2 Complex Math
;; ***************************************************
;;


(define-struct point (x y))
;; A Point is a (make-point Num Num)

;; (a)
;; (point-mult p1 p2) produces the result of two points multiplication

;; Examples
(check-expect (point-mult (make-point 1 1) (make-point 1 1)) (make-point 0 2))

;; point-mult: Point Point -> Point
(define (point-mult p1 p2)
  (make-point (- (* (point-x p1) (point-x p2))
                 (* (point-y p1) (point-y p2)))
              (+ (* (point-x p1) (point-y p2))
                 (* (point-x p2) (point-y p1)))))

;; Test Cases
(check-expect (point-mult (make-point -2 0) (make-point 2 1)) (make-point -4 -2))

;; (b)
;; (point-div p1 p2) produces the result of two points division

;; Examples
(check-expect (point-div (make-point 1 1) (make-point 1 1)) (make-point 1 0))

;; point-div: point point -> point
;; Requires: point-x p2 (x2) and point-y p2 (y2) not both zero at the same time
(define (point-div p1 p2)
  (make-point (/ (+ (* (point-x p1) (point-x p2))
                    (* (point-y p1) (point-y p2)))
                 (+ (sqr (point-x p2)) (sqr (point-y p2))))
              (/ (- (* (point-y p1) (point-x p2))
                    (* (point-x p1) (point-y p2)))
                 (+ (sqr (point-x p2)) (sqr (point-y p2))))))

;; Test Cases
(check-expect (point-div (make-point -2 0) (make-point 2 1)) (make-point -4/5 2/5))