;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname cond) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Edison Ying (21119725)
;; CS 135 Fall 2024
;; Assignment 02, Q3 Cond
;; ***************************************************
;;

;; (a)
(define (q3a-simplified n a?)
  (cond
    [(and a? (>= n 0)) (+ n 1)] 
    [a? (- n 1)]
    [else 0]))

(check-expect (q3a-simplified 5 true) 6)
(check-expect (q3a-simplified -1 true) -2)
(check-expect (q3a-simplified 5 false) 0)

;; (b)
(define (q3b-simplified a? b? c?)
  (cond
    [(and a? b?) 'elm]
    [(and a? (not c?)) 'birch]
    [a? 'cedar]
    [b? 'pine]
    [(not c?) 'birch]
    [else 'cherry]))

(check-expect (q3b-simplified true true true) 'elm)
(check-expect (q3b-simplified true true false) 'elm)
(check-expect (q3b-simplified true false true) 'cedar)
(check-expect (q3b-simplified true false false) 'birch)
(check-expect (q3b-simplified false true true) 'pine)
(check-expect (q3b-simplified false true false) 'pine)
(check-expect (q3b-simplified false false true) 'cherry)
(check-expect (q3b-simplified false false false) 'birch)

;; (c)
(define (q3c-simplified a? b? c?)
  (cond
    [(and c? b?) 'spruce]
    [(and (not c?) (not a?) b?) 'spruce]
    [(and (not c?) (not a?)) 'larch]
    [(and (not c?) (not a?) (not b?)) 'larch]
    [(or (and (not c?) a?) (and c? (not b?) a?)) 'hazel]
    [else 'hickory]))
    
(check-expect (q3c-simplified true true true) 'spruce)
(check-expect (q3c-simplified true true false) 'hazel)
(check-expect (q3c-simplified true false true) 'hazel)
(check-expect (q3c-simplified true false false) 'hazel)
(check-expect (q3c-simplified false true true) 'spruce)
(check-expect (q3c-simplified false true false) 'spruce)
(check-expect (q3c-simplified false false true) 'hickory)
(check-expect (q3c-simplified false false false) 'larch)



