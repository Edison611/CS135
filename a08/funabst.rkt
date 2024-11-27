;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname funabst) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***************************************************
;; Edison Ying (21119725)
;; CS 135 Fall 2024
;; Assignment 08, Q2 FunAbst
;; ***************************************************
;;

;; (a)
;; (or-pred pred lst) produces true if the application of the consumed predicate on any element
;; of the consumed list produces true, otherwise the function produces false
;; Examples:
(check-expect (or-pred even? empty) false)
(check-expect (or-pred odd? (list 6 10 4)) false)
(check-expect (or-pred string? (list 5 "wow")) true)
(check-expect (or-pred number? (list 'w 't 'f 1)) true)

;; or-pred: (Any -> Bool) (listof Any) -> Bool
(define (or-pred pred? lst)
  (cond [(empty? lst) false]
        [else (or (pred? (first lst))
                  (or-pred pred? (rest lst)))]))

;; ***************************************************
;; (b)
;; (map2argfn fns nums) Applies each function in fns to nums and outputs it as a single list
;; Examples:
(check-expect (map2argfn (list + - * / list) (list 3 2))
              (list 5 1 6 1.5 (list 3 2)))
(check-expect (map2argfn (list = - + * remainder) (list 5 2))
              (list false 3 7 10 1))
              
;; map2argfn: (listof (Num Num -> Any)) (list Num Num) -> (listof Any)
(define (map2argfn fns nums)
  (cond [(empty? fns) empty]
        [else (cons ((first fns) (first nums) (second nums))
                    (map2argfn (rest fns) nums))]))

;; ***************************************************
;; (c)
;; (arranged? fns loa) applies predicate function, then binary relational operator to each consecutive element,
;; if at any point anything is false, it returns false, otherwise returns true
;; Examples:
(check-expect (arranged? (list number? <) (list "wow")) false)
(check-expect (arranged? (list string? >) (list 'red "wow")) false)
(check-expect (arranged? (list string? >) (list "wow" 'red)) false)
(check-expect (arranged? (list string? string>?)
                         (list "wow" "cs135" "amazing")) true)
(check-expect (arranged? (list number? <) (list 1 5 3)) false)
(check-expect (arranged? (list string? string>?) empty) true)
(check-expect (arranged? (list string? >) (list "wow")) true)
(check-expect (arranged? (list string? >) (list 42)) false)

(check-expect (arranged? (list number? <) (list 1 4 5)) true)
;; arranged?: (list (Any -> Bool) (X X -> Bool)) (listof Any) -> Bool
;; requires: if binary-relational-operator is applied on any
;; elements, then predicate-function produces true on
;; elements of type X
(define (arranged? fns loa)
  (cond [(empty? loa) true]
        [(empty? (rest loa)) ((first fns) (first loa))]
        [(not (and ((first fns) (first loa))
                   ((first fns) (second loa)))) false]
        [((second fns) (first loa) (second loa))
         (arranged? fns (rest loa))]
        [else false]))