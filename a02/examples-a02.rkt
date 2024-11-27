;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname examples-a02) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Edison Ying (21119725)
;; CS 135 Fall 2024
;; Assignment 02 Examples
;; ***************************************************
;;

;; Q2
(check-expect (median-of-3-simple 4 5 6) 5)
(check-expect (median-of-3-simple 100 50 1) 50)
(check-expect (median-of-3-simple 4 10 8) 8)
(check-expect (median-of-3-simple 10 2 5) 5)

;; Q4(a)
(check-expect (can-donate-to/cond? 'O- 'A+) true)
(check-expect (can-donate-to/cond? 'A+ 'AB+) true)
(check-expect (can-donate-to/cond? 'O+ 'O-) false)
(check-expect (can-donate-to/cond? 'O+ 'A+) true)

;; Q5
(check-expect (check-plan 'ThunderBay 'Regina 'Vancouver 'Vancouver) 'invalid) ; Regina -> Vancouver is invalid
(check-expect (check-plan 'ThunderBay 'Regina 'Regina 'Calgary) 'ready)
(check-expect (check-plan 'Waterloo 'SaultSteMarie 'ThunderBay 'Winnipeg) 'ready)
(check-expect (check-plan 'Waterloo 'SaultSteMarie 'SaultSteMarie 'Winnipeg) 'exhausted)