;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname bonus-a08) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***************************************************
;; Edison Ying (21119725)
;; CS 135 Fall 2024
;; Assignment 08, Q5 Bonus
;; ***************************************************
;;

;; (a)
;; (my-compose f g) composes two functions
;; Examples:
(check-expect ((my-compose not symbol?) 1) true)
;; my-compose: (Any -> Any) (Any -> Any) -> (Any -> Any)
(define (my-compose f g)
  (lambda (x)
    (f (g x))))

;; (b)
;; (curry f) curries a function with two params
;; Examples:
(check-expect (((curry +) 1) 2) 3)
;; curry: (X Y -> Any) -> (X -> (Y -> Any))
(define (curry f)
  (lambda (x)
    (lambda (y)
      (f x y))))

;; (c)
;; (uncurry g) uncurries a function
;; Examples:
(check-expect ((uncurry (curry +)) 1 2) 3)
;; uncurry: (X -> (Y -> Any)) -> (X Y -> Any)
(define (uncurry g)
  (lambda (x y)
      ((g x) y)))

;; (d)
;; (eat-apples lst) removes all 'apple from the list
;; Examples:
(check-expect (eat-apples (list 'a 'b 'apple 'c)) (list 'a 'b 'c))
(check-expect (eat-apples (list 'apple 'apple 'c 'apple)) (list 'c))

;; eat-apples: (listof Sym) -> (listof Sym)
(define (eat-apples lst)
  (filter (my-compose not ((curry symbol=?) 'apple)) lst))

