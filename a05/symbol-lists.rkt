;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname symbol-lists) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Edison Ying (21119725)
;; CS 135 Fall 2024
;; Assignment 05, Q3 Symbols List
;; ***************************************************
;;

;; (make-symbol-list lon symbol) produces a nested list where inner list k
;; has length equal to the kth natural number from the consumed list
;; Examples
(check-expect (make-symbol-lists (list 2 1 3) 'X)
              (list (list 'X 'X) (list 'X) (list 'X 'X 'X)))
(check-expect (make-symbol-lists (list 1 2 4 3) 'AL)
              (list (list 'AL) (list 'AL 'AL) (list 'AL 'AL 'AL 'AL) (list 'AL 'AL 'AL)))
(check-expect (make-symbol-lists (list 0) 'A)
              (list (list)))
(check-expect (make-symbol-lists (list 0 1) 'A)
              (list (list) (list 'A)))
(check-expect (make-symbol-lists empty 'A)
              empty )

;; make-symbol-list: (listof Nat) Sym -> (listof (listof Sym))
(define (make-symbol-lists lon symbol)
  (cond [(empty? lon) empty]
        [else
         (cons (list-of-n-sym (first lon) symbol) (make-symbol-lists (rest lon) symbol))]))

;; (list-of-n-sym n sym) produces a list of n symbols
;; Examples
(check-expect (list-of-n-sym 4 'X)
              (list 'X 'X 'X 'X))
;; list-of-n-sym: Nat Sym -> (listof Sym)
(define (list-of-n-sym n sym)
  (cond [(= n 0) empty]
        [else (cons sym (list-of-n-sym (- n 1) sym))]))
  
  