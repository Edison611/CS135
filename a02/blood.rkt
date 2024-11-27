;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname blood) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Edison Ying (21119725)
;; CS 135 Fall 2024
;; Assignment 02, Q4 Blood
;; ***************************************************
;;

;; (a)
(define (can-donate-to/cond? donator recipient)
  (cond
    ;; General Cases
    [(symbol=? donator 'O-) true]
    [(symbol=? recipient 'AB+) true]
    [(symbol=? donator recipient) true]
    ;; Special Cases
    [(symbol=? donator 'O+) (cond
                              [(symbol=? recipient 'A+) true]
                              [(symbol=? recipient 'B+) true]
                              [else false])]
    [(symbol=? donator 'A-) (cond
                              [(symbol=? recipient 'A+) true]
                              [(symbol=? recipient 'AB-) true]
                              [else false])]
    [(symbol=? donator 'B-) (cond
                              [(symbol=? recipient 'B+) true]
                              [(symbol=? recipient 'AB-) true]
                              [else false])]
    [else false]))

(check-expect (can-donate-to/cond? 'O- 'A+) true)
(check-expect (can-donate-to/cond? 'A+ 'AB+) true)
(check-expect (can-donate-to/cond? 'O+ 'O-) false)
(check-expect (can-donate-to/cond? 'O+ 'A+) true)

;; (b)
(define (can-donate-to/bool? donator recipient)
  (or (symbol=? donator 'O-)
      (symbol=? recipient 'AB+)
      (symbol=? donator recipient)
      (and (symbol=? donator 'O+) (or (symbol=? recipient 'A+)
                                      (symbol=? recipient 'B-)))
      (and (symbol=? donator 'A-) (or (symbol=? recipient 'A+)
                                      (symbol=? recipient 'AB-)))
      (and (symbol=? donator 'B-) (or (symbol=? recipient 'B+)
                                      (symbol=? recipient 'AB-)))))

(check-expect (can-donate-to/bool? 'O- 'A+) true)
(check-expect (can-donate-to/bool? 'A+ 'AB+) true)
(check-expect (can-donate-to/bool? 'O+ 'O-) false)
(check-expect (can-donate-to/cond? 'O+ 'A+) true)
