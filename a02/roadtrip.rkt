;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname roadtrip) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Edison Ying (21119725)
;; CS 135 Fall 2024
;; Assignment 02, Q5 Roadtrip
;; ***************************************************
;;

;; Main Function
(define (check-plan city1 city2 city3 city4)
  (cond
    [(< (check-between-two-cities city1 city2 2) 0) 'invalid]
    [(< (check-between-two-cities city2 city3 (check-between-two-cities city1 city2 2)) 0) 'invalid]
    [(< (check-between-two-cities city3 city4 (check-between-two-cities city2 city3 (check-between-two-cities city1 city2 2))) 0) 'invalid]
    [else (final-status (check-between-two-cities city3 city4 (check-between-two-cities city2 city3 (check-between-two-cities city1 city2 2))))]
    ))

;; Gives the city number given the name
(define (city-number city)
  (cond
    [(symbol=? city 'StJohns) 1]
    [(symbol=? city 'Charlottetown) 2]
    [(symbol=? city 'Halifax) 3]
    [(symbol=? city 'Fredericton) 4]
    [(symbol=? city 'QuebecCity) 5]
    [(symbol=? city 'Toronto) 6]
    [(symbol=? city 'Waterloo) 7]
    [(symbol=? city 'SaultSteMarie) 8]
    [(symbol=? city 'ThunderBay) 9]
    [(symbol=? city 'Winnipeg) 10]
    [(symbol=? city 'Regina) 11]
    [(symbol=? city 'Calgary) 12]
    [(symbol=? city 'Vancouver) 13]))

;; Gives the final status of the driver given the number of days they can still drive
(define (final-status status)
  (cond
    [(>= status 2) 'rested]
    [(= status 1) 'ready]
    [(= status 0) 'exhausted]
    [else 'invalid]))

;; gives the number of days the driver is able to travel after travelling between two cities
(define (check-between-two-cities city1 city2 cur-status)
  (cond
    [(< (- (city-number city2)
           (city-number city1)) 0) -1]
    [(> (- (city-number city2)
           (city-number city1)) 2) -1]
    [(= (city-number city2)
        (city-number city1)) (+ 1 cur-status)]
    [(= (- (city-number city2)
           (city-number city1)) 1) 1]
    [(< (- cur-status (- (city-number city2)
                         (city-number city1))) 0) -1]
    [else 0])) 
  
;; Custom Test-Cases:
(check-expect (check-plan 'ThunderBay 'Regina 'Vancouver 'Vancouver) 'invalid) ; Regina -> Vancouver is invalid
(check-expect (check-plan 'ThunderBay 'Regina 'Regina 'Calgary) 'ready)
(check-expect (check-plan 'Waterloo 'SaultSteMarie 'ThunderBay 'Winnipeg) 'ready)
(check-expect (check-plan 'Waterloo 'SaultSteMarie 'SaultSteMarie 'Winnipeg) 'exhausted)
(check-expect (check-plan 'Charlottetown 'Winnipeg 'Toronto 'Vancouver) 'invalid)
(check-expect (check-plan 'Toronto 'Waterloo 'Toronto 'Waterloo) 'invalid)
(check-expect (check-plan 'Charlottetown 'Fredericton 'Fredericton 'Fredericton) 'rested)
(check-expect (check-plan 'Winnipeg 'Calgary 'Calgary 'Vancouver) 'ready)
(check-expect (check-plan 'Fredericton 'Toronto 'Halifax 'QuebecCity)
'invalid)
(check-expect (check-plan 'Toronto 'Waterloo 'Waterloo 'Waterloo)
'rested)
(check-expect (check-plan 'Calgary 'Calgary 'Calgary 'Vancouver)
'ready)
(check-expect (check-plan 'Toronto 'Waterloo 'Waterloo 'Regina)
'invalid)
(check-expect (check-plan 'Toronto 'Waterloo 'Waterloo 'SaultSteMarie)
'ready)
(check-expect (check-plan 'StJohns 'Charlottetown 'Halifax 'Fredericton) 'ready)
(check-expect (check-plan 'Toronto 'Waterloo 'SaultSteMarie 'ThunderBay) 'ready)

;; Question Check-Expects:
(check-expect (check-plan 'Halifax 'Fredericton 'Halifax 'Fredericton)
'invalid)
(check-expect (check-plan 'Waterloo 'Waterloo 'Waterloo 'Waterloo)
'rested)
(check-expect (check-plan 'Halifax 'QuebecCity 'QuebecCity 'Toronto)
'ready)
(check-expect (check-plan 'Halifax 'QuebecCity 'QuebecCity 'Waterloo)
'invalid)
