;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname bonus-a09) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***************************************************
;; Edison Ying (21119725)
;; CS 135 Fall 2024
;; Assignment 09, Bonus
;; ***************************************************
;;

;; (a)
(define (subsets lst)
  (foldr (lambda (x acc) (append (map (lambda (subset) (cons x subset)) acc) acc)) (list empty) lst))

;; (b)
(check-expect (subsets-hof (list 1 2)) (list (list 1 2) (list 1) (list 2) '()))

(define (subsets-hof lst)
  (foldr (lambda (x acc) (append (map (lambda (subset) (cons x subset)) acc) acc)) (list empty) lst))