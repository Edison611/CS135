;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname tree-pred) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***************************************************
;; Edison Ying (21119725)
;; CS 135 Fall 2024
;; Assignment 08, Q4 Tree Pred
;; ***************************************************
;;

(define-struct node (key left right))
;; A Node is a (make-node Nat BT BT)
;; A Binary Tree (BT) is one of:
;; * empty
;; * Node

;; (tree-pred pred) computes the predicate for a binary tree inputted to the predicate function
;; Examples:
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

;; tree-pred: (Any -> Bool) -> (BT -> Bool)
(define (tree-pred pred)
  (local [; f: BT -> Bool
          (define (f n)
            (cond [(empty? n) true]
                  [(pred (node-key n))
                   (and (f (node-left n))
                        (f (node-right n)))]
                  [else false]))]
    f))
