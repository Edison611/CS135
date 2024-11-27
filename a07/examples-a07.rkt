;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname examples-a07) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***************************************************
;; Edison Ying (21119725)
;; CS 135 Fall 2024
;; Assignment 07, Examples
;; ***************************************************
;;

;; Q1
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

(check-expect (contains-component? bike "hub") true)
(check-expect (contains-component? bike "brake") false)
(check-expect (contains-component? person "hamstring") false)
(check-expect (contains-component? person "left-arm") true)
(check-expect (contains-component? person "right-leg") true)
(check-expect (contains-component? person "legs") true)
(check-expect (contains-component? person "feet") true)

;; Q2
;; (b)
(check-expect (eval (list 'AND (list 0 1 1))) 0)
(check-expect (eval (list 'AND (list 1 1 1))) 1)
(check-expect (eval (list 'OR (list 0 1 1))) 1)
(check-expect (eval (list 'XOR (list 0 1 1))) 0)
(check-expect (eval (list 'AND (list 1 (list 'XOR (list 0 1 (list 'OR
                                                                  (list 0 0 0)))) 1))) 1)
(check-expect (eval (list 'AND (list 1 1 (list 'XOR (list 0 (list 'AND (list 0 1)) 1))) 1)) 1)

;; (c)
(check-expect (bidexp->string (list 'AND (list 0 1 1))) "(f*t*t)")
(check-expect (bidexp->string (list 'OR (list 0 1 1))) "(f+t+t)")
(check-expect (bidexp->string (list 'XOR (list 0 1 1))) "(f.t.t)")
(check-expect (bidexp->string (list 'AND (list 1 (list 'XOR (list 0 1
                                                                  (list 'OR (list 0 0 0)))) 1))) "(t*(f.t.(f+f+f))*t)")
(check-expect (bidexp->string (list 'AND (list 1 (list 'AND (list 0
                                                                  1))))) "(t*(f*t))")
(check-expect (bidexp->string (list 'XOR (list 0 't 'u 1 'w)))
              "(f.'t.'u.t.'w)")
(check-expect (bidexp->string (list 'OR (list 0 1 'e (list 'XOR
                                                           (list 0 (list 'AND (list 0 1)) 1))))) "(f+t+'e+(f.(f*t).t))")
(check-expect (bidexp->string (list 'OR (list 0 (list 'AND (list 0 1)) 'e  1))) "(f+(f*t)+'e+t)")

;; (d)
(define identifier-table (list (list 'x 1) (list 'y 0)))
(check-expect (eval-id (list 'AND (list 0 'x 1)) identifier-table) 0)
(check-expect (eval-id (list 'OR (list 'x 'y 1)) identifier-table) 1)
(check-expect (eval-id (list 'XOR (list 0 'y 1)) identifier-table) 1)
(check-expect (eval-id (list 'OR (list 0 'y (list 'AND (list 'x 1)))) identifier-table) 1)

;; Q3
;; (b)
(check-expect (create-tree (list "PEN" "PENCIL" "PENDANT" "PENNY"))
              (make-node #\space false (list
                            (make-node #\P false (list
                                        (make-node #\E false (list
                                                    (make-node #\N true (list
                                                                (make-node #\C false (list
                                                                            (make-node #\I false (list
                                                                                        (make-node #\L true empty)))))
                                                                (make-node #\D false (list
                                                                            (make-node #\A false (list
                                                                                        (make-node #\N false (list
                                                                                                    (make-node #\T true empty)))))))
                                                                (make-node #\N false (list
                                                                            (make-node #\Y true empty))))))))))))



;; (c)
(define new-dict (create-tree (list "CAT" "CAR" "DOG" "CART")))
(check-expect (check "CAT" new-dict) true)       
(check-expect (check "car" new-dict) true)       
(check-expect (check "dog" new-dict) true)       
(check-expect (check "cart" new-dict) true)       
(check-expect (check "CATS" new-dict) false)    