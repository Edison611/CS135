;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname bonus-a02) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Edison Ying (21119725)
;; CS 135 Fall 2024
;; Assignment 02, Q6 Bonus
;; ***************************************************
;;


;; 1753 Jan 1st is a Monday
;; Main Function
(define (date->day-of-week date)
  (cond
    [(= 1 (remainder (total-days date) 7)) 'Monday]
    [(= 2 (remainder (total-days date) 7)) 'Tuesday]
    [(= 3 (remainder (total-days date) 7)) 'Wednesday]
    [(= 4 (remainder (total-days date) 7)) 'Thursday]
    [(= 5 (remainder (total-days date) 7)) 'Friday]
    [(= 6 (remainder (total-days date) 7)) 'Saturday]
    [(= 0 (remainder (total-days date) 7)) 'Sunday]
    [else 'error]
    ))

;; Calculates total number of days that have passed since Jan 1st, 1753
(define (total-days date)
  (+ (* (- (year date) 1753) 365)
     (num-leap-years-before (year date))
     (days-before-month (month date))
     (day date)
     (leap-year (year date) (month date))
     ))

;; Calculates the number of leap years before 
(define (num-leap-years-before year)
  (+ (- (quotient (- year 1 1752) 4)
        (quotient (- year 1 1700) 100))
     (quotient (- year 1 1600) 400)
     ))

;; returns 1 if its a leap year and the current date is after 
;; returns 
(define (leap-year year month)
  (cond
    [(>= month 3) (cond
                    [(= 0 (remainder year 400)) 1]
                    [(= 0 (remainder year 100)) 0]
                    [(= 0 (remainder year 4)) 1]
                    [else 0]
                    )]
    [else 0]
    ))

;; Calculates number of days before the current month
(define (days-before-month month)
  (cond
    [(= 1 month) 0]
    [(= 2 month) 31]
    [(= 3 month) 59]
    [(= 4 month) 90]
    [(= 5 month) 120]
    [(= 6 month) 151]
    [(= 7 month) 181]
    [(= 8 month) 212]
    [(= 9 month) 243]
    [(= 10 month) 273]
    [(= 11 month) 304]
    [(= 12 month) 334]
    [else 0]
    ))

;; calculates the current year
(define (year date)
  (quotient date 10000))
  
  ;; Really Stupid Method and Long Method Below:
;  (+ (* (quotient date 10000000) 1000) ; thousands digit
;     (* (- (quotient date 1000000) ; hundreds digit
;           (* (quotient date 10000000) 10)) 100)
;     (* (- (quotient date 100000) ; tens digit
;           (* (quotient date 10000000) 100)
;           (* (- (quotient date 1000000) 
;                 (* (quotient date 10000000) 10)) 10)) 10)
;    (* (- (quotient date 10000) ; ones digit
;        (* (quotient date 10000000) 1000)
;        (* (- (quotient date 1000000) 
;                 (* (quotient date 10000000) 10)) 100)
;        (* (- (quotient date 100000) ;
;           (* (quotient date 10000000) 100)
;           (* (- (quotient date 1000000) 
;                 (* (quotient date 10000000) 10)) 10)) 10)
;        ) 1)
;    )) 

;; calculates the current month
(define (month date)
  (+ (* (- (quotient date 1000) ; tens digit
        (* (year date) 10)) 10)
     (* (- (quotient date 100)
           (* (year date) 100)
           (* (- (quotient date 1000) ; tens digit
                 (* (year date) 10)) 10)
           ) 1)
  ))


;; calculates the current day
(define (day date)
  (- date (* (quotient date 100) 100)))
