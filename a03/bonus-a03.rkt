;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname bonus-a03) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Edison Ying (21119725)
;; CS 135 Fall 2024
;; Assignment 03, Q5 Bonus
;; ***************************************************
;;

;; Sources:
;; https://www.geeksforgeeks.org/total-area-two-overlapping-rectangles/
;; Used the formula for area of intersecting rectangle

(define-struct box (xmin xmax ymin ymax))
;; A Box is a (make-box Num Num Num Num)

;; (overlap-area box1 box2) produces the overlapping area of two rectangles
;; Example:
(check-expect (overlap-area
               (make-box 1 5 1 4)
               (make-box 4 7 3 6))
              1)

;; overlap-area: Box Box -> Num              
(define (overlap-area box1 box2)
  (cond
    [(and (> (- (min (box-xmax box1) (box-xmax box2))
                (max (box-xmin box1) (box-xmin box2))) 0)
          (> (- (min (box-ymax box1) (box-ymax box2))
                (max (box-ymin box1) (box-ymin box2))) 0))
     (* (- (min (box-xmax box1) (box-xmax box2))
           (max (box-xmin box1) (box-xmin box2)))
        (- (min (box-ymax box1) (box-ymax box2))
           (max (box-ymin box1) (box-ymin box2))))]
    [else 0]))
