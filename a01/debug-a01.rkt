;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname debug-a01) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Edison Ying (21119725)
;; CS 135 Fall 2024
;; Assignment 01, Q1 Debugging
;; ***************************************************
;;

(define (luminosity red-pixel_value green-pixel_value blue-pixel_value)
  (+ (* 0.3 red-pixel_value)
     (* 0.59 green-pixel_value)
     (* 0.11 blue-pixel_value)))

(check-expect (luminosity 1 1 1) 1)
(check-expect (luminosity 2 2 2) 2)
(check-expect (luminosity 0 0 0) 0)