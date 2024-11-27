;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hof-hills) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***************************************************
;; Edison Ying (21119725)
;; CS 135 Fall 2024
;; Assignment 09, Q3 Hof-Hills
;; ***************************************************
;;

;; (a)
(check-expect (pocket-change (list 'loonie 'watcard 'button 'nickel
                                   'dime 'gum)) 1.15)
(define (pocket-change lst)
  (foldr (lambda (x rror) (+ ((lambda (y)
                                (cond [(symbol=? 'nickel y) 0.05]
                                      [(symbol=? 'dime y) 0.10]
                                      [(symbol=? 'quarter y) 0.25]
                                      [(symbol=? 'loonie y) 1.00]
                                      [(symbol=? 'toonie y) 2.00]
                                      [else 0])) x) rror))
         0 lst))

;; (b)
(check-expect ((make-validator (list '+ '- '*)) '*) true)
(check-expect ((make-validator (list 'This 'is 'Sparta)) 'Athens)
              false)

(define (make-validator lst)
  (lambda (x)
    (foldr (lambda (y rror) (cond [(symbol=? y x) true]
                             [else rror]))
           false lst)))

;; (c)
(check-expect (remove-outliers (list 1 2 3 1 2 1 3 10 2 -5)) (list 1
                                                                   2 3 1 2 1 3 2))
(define (remove-outliers lst)
  (local [(define n (length lst))
          (define mean (/ (foldr + 0 lst) n))
          (define sd (sqrt (/ (foldr (lambda (x rror)
                                       (+ (sqr (- x mean)) rror))
                                     0 lst) n)))]
    (foldr (lambda (x rror)
             (cond [(or (> x (+ mean (* 2 sd)))
                        (< x (- mean (* 2 sd))))
                    rror]
                   [else (cons x rror)]))
           empty lst)))

;; (d)
(check-expect (primes 10) (list 2 3 5 7))
(check-expect (primes 30) (list 2 3 5 7 11 13 17 19 23 29))

(define (primes n)
  (foldr (lambda (x r)
           (cond [(foldr (lambda (y rror)
                             (cond [(= 0 (remainder x y)) false]
                                   [else rror]))
                         true (build-list (- x 2) (lambda (a) (+ a 2)))) ; Builds a list from [2...)]
                  (cons x r)]
                 [else r]))
         empty (build-list (- n 1) (lambda (x) (+ x 2)))))

