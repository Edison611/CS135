;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname clique) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***************************************************
;; Edison Ying (21119725)
;; CS 135 Fall 2024
;; Assignment 10, Q3 Clique
;; ***************************************************
;;

;; A Node is a Sym
;; A Graph is one of:
;; * empty
;; * (cons (list v (list w_1 ... w_n)) g)
;; where g is a Graph
;; v, w_1, ... w_n are Nodes
;; v is the in-neighbour to w_1 ... w_n in the Graph
;; v does not appear as an in-neighbour in g

;; (is-clique? lon g) produces true if list of nodes form a clique in the graph
;; Examples:

(check-expect (is-clique? '(A B C) '((A (B C D)) (B (A C))
                                                 (C (A B)) (D ())))
              true)
(check-expect (is-clique? '(A B C D) '((A (B C D)) (B (A C D))
                                                 (C (A B D)) (D (A B C))))
              true)
(check-expect (is-clique? '(D C) '((A (B C D)) (B (A C))
                                                 (C (A B)) (D ())))
              false)

;; is-clique?: (listof Nodes) Graph -> Bool
(define (is-clique? lon g)
  (cond [(empty? lon) true]
        [(empty? (rest lon)) true]
        [else 
         (and (node-works? (first lon)
                           (neighbours (first lon) g)
                           (rest lon)
                           g)
              (is-clique? (rest lon) g))]))

;; (node-works? n nb lon g) checks if a singular node works for being a clique
;; Examples:
(check-expect (node-works? 'A '(B C D) '(B C) '((A (B C D)) (B (A C))
                                                           (C (A B)) (D ())))
              true)
;; node-works?: Node (listof Node) (listof Node) Graph -> Bool
(define (node-works? n nb lon g)
  (cond [(empty? lon) true]
        [(in? (first lon) nb)
         (and (in? n (neighbours (first lon) g))
              (node-works? n nb (rest lon) g))]
        [else false]))


;; Taken from prev assignment
(define (in? item lst)
  (foldr (lambda (x rror) (or (symbol=? x item) rror)) false lst))


  ;; (neighbours v g) produces list of neighbours of v in g
;; Examples:
(define g1
  '((A (C D E))
    (B (E J))
    (C ())
    (D (F J))
    (E (K))
    (F (K H))
    (H ())
    (J (H))
    (K ()))
  )
(check-expect (neighbours 'D g1) (list 'F 'J))
(check-expect (neighbours 'Z g1) false)
;; neighbours: Node Graph → (anyof (listof Node) false)
;; Requires: v is a node in g
(define (neighbours v g)
  (cond
    [(empty? g) false]
    [(symbol=? v (first (first g))) (second (first g))]
    [else (neighbours v (rest g))]))
  