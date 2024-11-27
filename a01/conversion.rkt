;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname conversion) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Edison Ying (21119725)
;; CS 135 Fall 2024
;; Assignment 01, Q4 Speed
;; ***************************************************
;;

;; (a)
(define (m/s->mph speed)
  (* speed (/ 3600 1609.344)))

;; (b)
(define (mph->S/mfn speed)
  (* speed (/ (* 1609.344 1209.6) (* 3600 1.7018))))
