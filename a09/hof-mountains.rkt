;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hof-mountains) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***************************************************
;; Edison Ying (21119725)
;; CS 135 Fall 2024
;; Assignment 09, Q4 Hof-Mountains
;; ***************************************************
;;

;; (a)

(check-expect (min-max < (list 1 5 7 6 0 3 4 2)) (list 0 7))
(check-expect (min-max
               (lambda (str-a str-b) (< (string-length str-a)
                                        (string-length str-b)))
               (list "CS135" "Is" "Lots" "Of" "Fun"))
              (list "Is" "CS135"))

(define (min-max f lst)
  (foldl
   (lambda (cur acc)
     (cond [(empty? (first acc)) (list cur cur)]
           [(f cur (first acc)) (list cur (second acc))]
           [(f (second acc) cur) (list (first acc) cur)]
           [else acc]))
   (list empty empty)
   lst))

;; (b)

(check-expect (in-order? < (list 0 1 2 3 5 135)) true)
(check-expect (in-order? < (list 0 1 2 3 5 135 -1)) false)
(check-expect (in-order? < (list 0 1 -1 2 3 5 135)) false)
(check-expect (in-order?
               (lambda (str-a str-b) (< (string-length str-a)
                                        (string-length str-b)))
               (list "A" "short" "wordlist" "for y'all"))
              true)

;(define (in-order? f lst)
;  (foldr (lambda (x y rror)
;           (cond [(empty? y) true]
;                 [else (and (f x y) rror)]))
;         true
;         lst
;         (foldr cons '(()) (rest lst))))


(define (in-order? f lst)
  (cond [(= (length lst) (length (foldl
                 (lambda (cur acc)
                   (cond [(empty? acc) (cons cur acc)]
                         [(not (f (first acc) cur)) acc]
                         [else (cons cur acc)]))
                 empty
                 lst))) true]
        [else false]))

;; (c)

(check-expect (slice 1 4 (list 1 2 3 4 5 6 7)) (list 2 3 4 5))
(check-expect (slice 0 0 (list "CS135" "Is" "Fun")) (list "CS135"))

(define (slice a b lst)
  (foldr
   (lambda (x y rror)
     (cond [(and (<= a y) (<= y b)) (cons x rror)]
           [else rror]))
   empty
   lst
   (build-list (length lst) (lambda (x) x))))

;; (d)
(check-expect (split-n 3 (list 1 2 3 4 5 6 7 8 9 10 11 12))
              (list (list 1 4 7 10) (list 2 5 8 11) (list 3 6 9 12)))
(check-expect (split-n 5 (list 1 2 3 4 5 6 7 8 9 10 11 12))
              (list (list 1 6 11) (list 2 7 12) (list 3 8) (list 4 9)
                    (list 5 10)))
(define my-lst (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
(check-expect (split-n 3 my-lst)
              (list (list 1 4 7 10 13)
                    (list 2 5 8 11 14)
                    (list 3 6 9 12 15)))
(check-expect (split-n 4 my-lst)
              (list (list 1 5 9 13) (list 2 6 10 14)
                    (list 3 7 11 15) (list 4 8 12)))
(check-expect (split-n 5 my-lst)
              (list (list 1 6 11) (list 2 7 12) (list 3 8 13)
                    (list 4 9 14) (list 5 10 15)))
(check-expect (split-n 6 my-lst)
              (list (list 1 7 13) (list 2 8 14) (list 3 9 15)
                    (list 4 10) (list 5 11) (list 6 12)))
              
(define (split-n n lst)
  (foldr (lambda (k rror)
           (cons (foldl cons empty k) rror))
         empty
         (foldl
          (lambda (x i acc)
            (foldr (lambda (y j rror)
                     (cond [(= (remainder i n) j) (cons (cons x y) rror)]
                           [else (cons y rror)]))
                   empty
                   acc
                   (build-list n (lambda (x) x))))
          (build-list n (lambda (x) empty))
          lst
          (build-list (length lst) (lambda (x) x)))))