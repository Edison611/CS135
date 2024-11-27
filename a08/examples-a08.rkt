;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname examples-a08) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***************************************************
;; Edison Ying (21119725)
;; CS 135 Fall 2024
;; Assignment 08, Examples
;; ***************************************************
;;

;; Q2
;; (a)
(check-expect (or-pred even? empty) false)
(check-expect (or-pred odd? (list 6 10 4)) false)
(check-expect (or-pred string? (list 5 "wow")) true)
(check-expect (or-pred number? (list 'w 't 'f 1)) true)

;; (b)
(check-expect (map2argfn (list + - * / list) (list 3 2))
              (list 5 1 6 1.5 (list 3 2)))
(check-expect (map2argfn (list = - + * remainder) (list 5 2))
              (list false 3 7 10 1))

;; (c)
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

;; Q2
(check-expect (partition number? (list 1 "a" "b" 2 3 "c" 4 "d" 5))
              (list (list 1 2 3 4 5) (list "a" "b" "c" "d")))

;; Q3
(define t (make-node 5
                     (make-node 10 empty empty)
                     (make-node 15
                                (make-node 20 empty empty)
                                (make-node 33 empty empty))))
(define t2 (make-node 0
                     (make-node 10
                                (make-node 5 empty empty)
                                empty)
                     (make-node 6
                                (make-node 20 empty empty)
                                (make-node 33 empty empty))))
(check-expect ((tree-pred even?) t) false)
(check-expect ((tree-pred positive?) t) true)
(check-expect ((tree-pred number?) t2) true)
(check-expect ((tree-pred positive?) t2) false)

;; Q4
;; (b)
(define l1 (list (list 1 (list 1 4 'a) (list 2 3 6)
                       (list 'a 'b '1) (list (list 3 4) empty))
                 (list 2 3 (list 1 2) (list (list 'a 'b)))))

(check-expect (nested-filter number? l1)
              (list (list 1 (list 1 4) (list 2 3 6)
                       (list '1) (list (list 3 4)))
                 (list 2 3 (list 1 2) (list (list)))))

;; (c)
(check-expect
 (ruthless '(rabbit (apple pluto (ruth blue) ruth) hello))
 '(rabbit (apple pluto (blue)) hello))

(check-expect
 (ruthless (list (list 'ruth 'a 'b 'c 'ruth) (list (list 'd 'ruth) (list (list 'e) 'ruth))))
 (list (list 'a 'b 'c) (list (list 'd) (list (list 'e)))))

;; (d)
(check-expect (keep-between 5 10 '(1 3 5 (7 9 10) (8 (3 4)) 8 15))
              '(5 (7 9 10) (8 ()) 8))
(check-expect (keep-between 9 11 '(1 3 5 (7 9 10) (8 (3 4)) 8 15))
              '((9 10) (())))

;; (e)
(check-expect (nested-cleanup '(1 () 2 () () 3)) '(1 2 3))
(check-expect (nested-cleanup '(1 (()()) 2 ((3 () (()))) ))
              '(1 2 ((3))))
(check-expect (nested-cleanup '(()(()())(())())) false)
(check-expect (nested-cleanup '()) false)
(check-expect (nested-cleanup '(() () 3 5 ((() ()) () (() 1) 2) 'a))
              '(3 5 ((1) 2) 'a))

;; (f)
(check-expect (nested-apply (list abs floor) '(1.2 (-2 (3.5)) ()))
              (list '(1.2 (2 (3.5)) ()) '(1 (-2 (3)) ())))
(check-expect (nested-apply (list ceiling sqr abs) '(1.2 (-2.5 (3.5) 4 ()) () 6.2))
              (list '(2 (-2 (4) 4 ()) () 7)
                    '(1.44 (6.25 (12.25) 16 ()) () 38.44)
                    '(1.2 (2.5 (3.5) 4 ()) () 6.2)))
