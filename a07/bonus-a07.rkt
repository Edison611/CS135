;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname bonus-a07) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***************************************************
;; Edison Ying (21119725)
;; CS 135 Fall 2024
;; Assignment 07, Bonus
;; ***************************************************
;;

;; (add a b) calculates the sum of a and b.
;; Example:
(check-expect (add 13 5) 18)
(check-expect (add 135135135 424242) 135559377)
;; add: Nat Nat -> Nat
(define (add a b)
  (bin->dec (add-bin (dec->bin a) (dec->bin b) 0) 1))


;; (dec->bin n) converts a number to binary with the list reversed
;; Examples:
(check-expect (dec->bin 13) (list 1 0 1 1))
(check-expect (dec->bin 5) (list 1 0 1))
;; dec->bin: Nat -> (listof Boolean)
(define (dec->bin n)
  (cond [(= 0 n) empty]
        [else (cons (remainder n 2) (dec->bin (quotient n 2)))]))

;; (add-bin x y carry) adds two binary numbers together
;; Examples:
(check-expect (add-bin (list 1 0 1) (list 1 0 1 1) 0) (list 0 1 0 0 1))
(check-expect (add-bin (list 1 1 1 1 1 1) (list 1) 0) (list 0 0 0 0 0 0 1))

;; add-bin: (listof Bool) (listof Bool) Bool -> (listof Bool)
(define (add-bin x y carry)
  (cond [(empty? (stop x y)) (cons carry empty)]
        [(empty? x)
         (cons (first (full-adder 0 (first y) carry))
               (add-bin empty
                        (rest y)
                        (second (full-adder 0 (first y) carry))))]
        [(empty? y)
         (cons (first (full-adder (first x) 0 carry))
               (add-bin (rest x)
                        empty
                        (second (full-adder (first x) 0 carry))))]
        [else
         (cons (first (full-adder (first x) (first y) carry))
               (add-bin (rest x)
                        (rest y)
                        (second (full-adder (first x) (first y) carry))))]))

;; (bin->dec n multiplier) converts a binary number to base 10
;; Examples:
(check-expect (bin->dec (list 0 1 0 0 1) 1) 18)
;; bin->dec: (listof Boolean) Nat -> Nat
(define (bin->dec n multiplier)
  (cond [(empty? n) 0]
        [else (+ (* (first n) multiplier)
                 (bin->dec (rest n) (* 2 multiplier)))]))

;; ***************************************************
;; HELPER FUNCTIONS
;; ***************************************************

(define (half-adder x y)
  ;; 1st = SUM, 2nd = CARRY
  (list (my-xor (list x y)) (my-and (list x y))))

(define (full-adder x y carry)
  ;; 1st = SUM, 2nd = CARRY
  (list (my-xor (list carry (my-xor (list x y))))
        (my-or (list (my-and (list x y))
                     (my-and (list carry (my-xor (list x y))))))))

;; stop: (listof Bool) (listof Bool) -> (listof Bool)
(define (stop x y)
  (cond [(empty? x) y]
        [(empty? y) x]
        [else x]))


;; (my-and val bexp) applies the AND operator
;; my-and: Boolean BExp -> Boolean
(define (my-and lst)
  (cond [(empty? lst) 1]
        [(= (first lst) 0) 0]
        [else (my-and (rest lst))]))

;; (my-or val bexp) applies the OR operator
;; my-or: Boolean BExp -> Boolean
(define (my-or lst)
  (cond [(empty? lst) 0]
        [(= (first lst) 1) 1]
        [else (my-or (rest lst))]))

;; (my-xor val bexp acc) applies the XOR operator
;; my-xor: Boolean BExp Nat -> Boolean
(define (my-xor lst)
  (cond [(= 0 (first lst)) (second lst)]
        [(= 0 (second lst)) (first lst)]
        [else 0]))

               