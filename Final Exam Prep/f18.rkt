;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname f18) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; Q 
(define (destruction-sort lon)
  (foldr (lambda (x rror)
           (cond [(empty? rror) (cons x empty)]
                 [(< x (first rror)) (cons x rror)]
                 [else rror]))
         empty lon))

(destruction-sort '(1 3 5 9 4 10 16))