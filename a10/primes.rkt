;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname primes) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***************************************************
;; Edison Ying (21119725)
;; CS 135 Fall 2024
;; Assignment 10, Q2 Bin Sqrt
;; ***************************************************
;;

;; (primes n) computes all primes up to n using Sieve of Eratosthenes
;; Examples
(check-expect (primes 10) (list 2 3 5 7))
(check-expect (primes 30) (list 2 3 5 7 11 13 17 19 23 29))
(check-expect (primes 50) (list 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47))

;; primes: Nat -> (listof Nat)
(define (primes n)
  (local [(define (primes/acc lon)
            (cond [(empty? lon) empty]
                  [(> (first lon) (sqrt n)) lon]
                  [else (cons (first lon)
                              (primes/acc (filter (lambda (x)
                                                    (not (= 0 (remainder x (first lon)))))
                                                  lon)))]))]
    (primes/acc (build-list (- n 1) (lambda (x) (+ x 2))))))
