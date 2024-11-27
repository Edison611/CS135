;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname components) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***************************************************
;; Edison Ying (21119725)
;; CS 135 Fall 2024
;; Assignment 07, Q1 Components
;; ***************************************************
;;

(define-struct component (name num subcomponents))
;; A Component is a (make-component Str Nat (listof Component))

;; Variables:
(define bike (make-component "bike" 1 (list
                                       (make-component "frame" 1 empty)
                                       (make-component "wheel" 2 (list
                                                                  (make-component "tire" 1 empty)
                                                                  (make-component "rim" 1 empty)
                                                                  (make-component "spoke" 30 empty)
                                                                  (make-component "hub" 1 (list
                                                                                           (make-component "housing" 1 empty)
                                                                                           (make-component "axel" 1 empty)
                                                                                           (make-component "bearing" 20 empty)))))
                                       (make-component "seat" 1 empty)
                                       (make-component "handlebar" 1 empty))))
(define person (make-component "person" 1 (list
                                           (make-component "head" 1 (list
                                                                      (make-component "eyes" 1 empty)
                                                                      (make-component "nose" 1 empty)
                                                                      (make-component "ears" 1 empty)
                                                                      (make-component "mouth" 1 empty)))
                                           (make-component "body" 3 (list
                                                                      (make-component "arms" 1 (list
                                                                                                (make-component "left-arm" 1 empty)
                                                                                                (make-component "right-arm" 1 empty)))
                                                                      (make-component "stomach" 1 empty)))
                                           (make-component "legs" 2 (list
                                                                      (make-component "thighs" 1 empty)
                                                                      (make-component "left-leg" 1 empty)
                                                                      (make-component "right-leg" 1 empty)))
                                           (make-component "feet" 1 empty))))
                                                           
                                           

;; (contains-component? comp name) produces true if the name of the subcomponent is in the main component
;; Examples
(check-expect (contains-component? bike "hub") true)
(check-expect (contains-component? bike "brake") false)
(check-expect (contains-component? person "hamstring") false)
(check-expect (contains-component? person "left-arm") true)
(check-expect (contains-component? person "right-leg") true)
(check-expect (contains-component? person "legs") true)
(check-expect (contains-component? person "feet") true)

;; Note: it can also take (listof Component)
;; contains-component?: Component Str -> Bool
(define (contains-component? comp name)
  (cond [(empty? comp) false]
        [(cons? comp) (or (contains-component? (first comp) name)
                          (contains-component? (rest comp) name))]
        [(string=? (component-name comp) name) true]
        [else (contains-component? (component-subcomponents comp) name)]))

