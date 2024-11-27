;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hof-flats) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***************************************************
;; Edison Ying (21119725)
;; CS 135 Fall 2024
;; Assignment 09, Q2 Hof-Flats
;; ***************************************************
;;

;; (a)
(check-expect (absolutely-odd (list 1 -5 4 6 5)) 11)
(check-expect (absolutely-odd empty) 0)

(define (absolutely-odd lst)
  (foldr (lambda (v rror) (cond [(odd? v) (+ (abs v) rror)]
                                [else rror])) 0 lst))

;; (b)
(check-expect (count-n 1 (list 1 0 1 1 -1 0 1 0 0 1)) 5)
(check-expect (count-n 1 empty) 0)


(define (count-n n lst)
  (foldr (lambda (v rror) (cond [(= n v) (+ 1 rror)]
                                [else rror]))
         0
         lst))

;; (c)
(check-expect (unzip (list (list 1 'a) (list 2 'b) (list 3 'c)))
              (list (list 1 2 3) (list 'a 'b 'c)))
(check-expect (unzip empty) (list empty empty))

(define (unzip lst)
  (list (foldr (lambda (x rror) (cons (first x) rror)) empty lst)
        (foldr (lambda (x rror) (cons (second x) rror)) empty lst)))

;; (d)
(check-expect (dedup (list 1 2 1 3 3 2 4)) (list 1 2 3 4))

(define (dedup lst)
  (foldr (lambda (x rror)
           (cons x (filter (lambda (y) (not (= x y))) rror))) empty lst))

;; (e)
(check-expect (zero-fill "abcdefghijklmn") "000000abcdefghijklmn")
(check-expect (zero-fill "he00llo") "0000000000000he00llo")

(define (zero-fill s)
  (cond [(>= (length (string->list s)) 20) s]
        [else (list->string (foldr cons
                                   (string->list s)
                                   (build-list (- 20 (length (string->list s)))
                                               (lambda (x) #\0))))]))


  

