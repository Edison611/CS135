;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname examples-a10) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***************************************************
;; Edison Ying (21119725)
;; CS 135 Fall 2024
;; Assignment 10, Examples
;; ***************************************************
;;

;; Q1
(check-within (bin-sqrt 2 0.001) (sqrt 2) 0.001)
(check-within (bin-sqrt 5 0.001) (sqrt 5) 0.001)
(check-within (bin-sqrt 1000000 0.001) (sqrt 1000000) 0.001)
(check-within (bin-sqrt 10000001000000100000010000001000000 0.001)
              (sqrt 10000001000000100000010000001000000) 0.001)

;; Q2
(check-expect (primes 10) (list 2 3 5 7))
(check-expect (primes 30) (list 2 3 5 7 11 13 17 19 23 29))
(check-expect (primes 50) (list 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47))

;; Q3
(check-expect (is-clique? '(A B C) '((A (B C D)) (B (A C))
                                                 (C (A B)) (D ())))
              true)
(check-expect (is-clique? '(A B C D) '((A (B C D)) (B (A C D))
                                                 (C (A B D)) (D (A B C))))
              true)
(check-expect (is-clique? '(D C) '((A (B C D)) (B (A C))
                                                 (C (A B)) (D ())))
              false)

;; Q4
(define left-soln '((3 2) (2 0) (1 3) (0 1)))
(define right-soln '((3 1) (2 3) (1 0) (0 2)))

;; (a)
(check-expect (attacking? (list 0 0) (list 1 1)) true)
(check-expect (attacking? (list 0 0) (list 1 2)) false)
(check-expect (attacking? (list 3 2) (list 0 5)) true)
(check-expect (attacking? (list 3 2) (list 7 7)) false)

;; (b)
(check-expect (valid-cand? left-soln 4) true)
(check-expect (valid-cand? right-soln 4) true)
(check-expect (valid-cand? left-soln 3) false)
(check-expect (valid-cand? (list (list 0 0) (list 1 1)) 4) false)
(check-expect (valid-cand? '((3 1) (2 3) (1 0)) 4) true)

;; (c)
(check-expect (neighbours-naive '((0 0)) 4)
              '(((1 2) (0 0)) ((1 3) (0 0)) ((2 1) (0 0))
                              ((2 3) (0 0)) ((3 1) (0 0)) ((3 2) (0 0))))
(check-expect (neighbours-naive '((1 1)) 4)
              '(((0 3) (1 1)) ((2 3) (1 1)) ((3 0) (1 1))
               ((3 2) (1 1))))
(check-expect (neighbours-naive '((1 2) (0 0)) 4)
              '(((3 1) (1 2) (0 0))))
(check-expect (neighbours-naive '((2 2)) 3)
              '(((0 1) (2 2)) ((1 0) (2 2))))

;; (d)
(check-expect (neighbours-row '((0 0)) 4)
              '(((1 2) (0 0)) ((1 3) (0 0))))
(check-expect (neighbours-row '((1 2) (0 0)) 4)
              '())
(check-expect (neighbours-row '((1 1)) 4)
              '(((2 3) (1 1))))
; (check-expect (neighbours-row '((1 1) (0 0)) 4)
;              '(((2 3) (1 1)) ((1 2) (0 0)) ((1 3) (0 0))))
(check-expect (neighbours-row '((3 3)) 4)
              '())

;; (e)
(check-expect (n-queens 5 neighbours-row)
              (list
               (list
                (list 4 3)
                (list 3 1)
                (list 2 4)
                (list 1 2)
                (list 0 0))
               (list
                (list 4 2)
                (list 3 4)
                (list 2 1)
                (list 1 3)
                (list 0 0))
               (list
                (list 4 4)
                (list 3 2)
                (list 2 0)
                (list 1 3)
                (list 0 1))
               (list
                (list 4 3)
                (list 3 0)
                (list 2 2)
                (list 1 4)
                (list 0 1))
               (list
                (list 4 4)
                (list 3 1)
                (list 2 3)
                (list 1 0)
                (list 0 2))
               (list
                (list 4 0)
                (list 3 3)
                (list 2 1)
                (list 1 4)
                (list 0 2))
               (list
                (list 4 1)
                (list 3 4)
                (list 2 2)
                (list 1 0)
                (list 0 3))
               (list
                (list 4 0)
                (list 3 2)
                (list 2 4)
                (list 1 1)
                (list 0 3))
               (list
                (list 4 2)
                (list 3 0)
                (list 2 3)
                (list 1 1)
                (list 0 4))
               (list
                (list 4 1)
                (list 3 3)
                (list 2 0)
                (list 1 2)
                (list 0 4))))
(check-expect (n-queens 4 neighbours-naive)
              (list (list (list 3 1) (list 2 3) (list 1 0) (list 0 2))
                    (list (list 3 2) (list 2 0) (list 1 3) (list 0 1))))
