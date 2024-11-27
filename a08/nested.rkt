;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname nested) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***************************************************
;; Edison Ying (21119725)
;; CS 135 Fall 2024
;; Assignment 08, Q5 Nested
;; ***************************************************
;;

;; A (nested-listof X) is one of:
;; * empty
;; * (cons (nested-listof X) (nested-listof X))
;; * (cons X (nested-listof X))
;; Requires: X itself is not a list type

;; (a)

;; nested-listof-X-template: (nested-listof X) -> Any
(define (nested-listof-X-template nlox)
  (cond [(empty? nlox) ...]
        [(cons? (first nlox)) (... (nested-listof-X-template (first nlox))
                                   (nested-listof-X-template (rest nlox)))]
        [else (... (first nlox)
                   (nested-listof-X-template (rest nlox)))]))

;; ***************************************************
;; (b)

(define l1 (list (list 1 (list 1 4 'a) (list 2 3 6)
                       (list 'a 'b '1) (list (list 3 4) empty))
                 (list 2 3 (list 1 2) (list (list 'a 'b)))))


;; (nested-filter pred nlox) converts everything that does not follow predicate into false
;; Examples:
(check-expect (nested-filter number? l1)
              (list (list 1 (list 1 4) (list 2 3 6)
                       (list '1) (list (list 3 4)))
                 (list 2 3 (list 1 2) (list (list)))))

;; nested-filter: (Any -> Bool) (nested-listof Any) -> (nested-listof Any)
(define (nested-filter pred nlox)
  (cond [(empty? nlox) empty]
        [(cons? (first nlox)) (cons (nested-filter pred (first nlox))
                                    (nested-filter pred (rest nlox)))]
        [(pred (first nlox)) (cons (first nlox)
                                   (nested-filter pred (rest nlox)))]
        [else (nested-filter pred (rest nlox))]))

;; ***************************************************
;; (c)

;; (ruthless nlox) removes all instances of 'ruth in the nested list
;; Examples:
(check-expect
 (ruthless '(rabbit (apple pluto (ruth blue) ruth) hello))
 '(rabbit (apple pluto (blue)) hello))

(check-expect
 (ruthless (list (list 'ruth 'a 'b 'c 'ruth) (list (list 'd 'ruth) (list (list 'e) 'ruth))))
 (list (list 'a 'b 'c) (list (list 'd) (list (list 'e)))))


;; ruthless (nested-listof Sym) -> (nested-listof Sym)
(define (ruthless nlox)
  (local [(define (rem-ruth x)
          (not (symbol=? 'ruth x)))]
    (nested-filter rem-ruth nlox)))

;; ***************************************************
;; (d)
;; (keep-between a b nlox) keeps only numbers between a and b in the nested list
;; Examples:
(check-expect (keep-between 5 10 '(1 3 5 (7 9 10) (8 (3 4)) 8 15))
              '(5 (7 9 10) (8 ()) 8))
(check-expect (keep-between 9 11 '(1 3 5 (7 9 10) (8 (3 4)) 8 15))
              '((9 10) (())))


;; keep-between Num Num (nested-listof Num) -> (nested-listof Num)
(define (keep-between a b nlox)
  (local [; (between x) checks if the value is between a and b
          ; between: Num -> ((
          (define (between x)
            (and (<= a x) (<= x b)))]
    (nested-filter between nlox)))

;; ***************************************************
;; (e)
;; (nested-cleanup nlox) removes all empty lists in a nested list
;; Examples:
(check-expect (nested-cleanup '(1 () 2 () () 3)) '(1 2 3))
(check-expect (nested-cleanup '(1 (()()) 2 ((3 () (()))) ))
              '(1 2 ((3))))
(check-expect (nested-cleanup '(()(()())(())())) false)
(check-expect (nested-cleanup '()) false)
(check-expect (nested-cleanup '(() () 3 5 ((() ()) () (() 1) 2) 'a))
              '(3 5 ((1) 2) 'a))

;; nested-cleanup: (nested-listof Any) -> (anyof false (nested-listof Any))
(define (nested-cleanup nlox)
  (cond [(empty? nlox) false]
        [(empty? (first nlox)) (nested-cleanup (rest nlox))]
        [(cons? (first nlox))
         (local [(define empty-sublst (nested-cleanup (first nlox)))]
           (cond [(boolean? empty-sublst) (nested-cleanup (rest nlox))]
                 [(boolean? (nested-cleanup (rest nlox)))
                  (cons empty-sublst empty)]
                 [else
                  (cons empty-sublst (nested-cleanup (rest nlox)))]))]
        [else
         (cond [(boolean? (nested-cleanup (rest nlox)))
                (cons (first nlox) empty)]
               [else (cons (first nlox) (nested-cleanup (rest nlox)))])]))

;; ***************************************************
;; (f)
;; (nested-apply lof nlon) produces lists of the functions when applied to the nested list
;; Examples:
(check-expect (nested-apply (list abs floor) '(1.2 (-2 (3.5)) ()))
              (list '(1.2 (2 (3.5)) ()) '(1 (-2 (3)) ())))
(check-expect (nested-apply (list ceiling sqr abs) '(1.2 (-2.5 (3.5) 4 ()) () 6.2))
              (list '(2 (-2 (4) 4 ()) () 7)
                    '(1.44 (6.25 (12.25) 16 ()) () 38.44)
                    '(1.2 (2.5 (3.5) 4 ()) () 6.2)))

;; nested-apply (listof (Num -> (anyof Num Int Nat))) (nested-listof Num) ->  (listof (nested-listof Num))
(define (nested-apply lof nlon)
  (local [(define (my-apply f nlon)
          (cond [(empty? nlon) empty]
                [(empty? (first nlon)) (cons (first nlon) (my-apply f (rest nlon)))]
                [(cons? (first nlon)) (cons (my-apply f (first nlon))
                                            (my-apply f (rest nlon)))]
                [else (cons (f (first nlon))
                            (my-apply f (rest nlon)))]))]
    (cond [(empty? lof) empty]
          [else (cons (my-apply (first lof) nlon)
                      (nested-apply (rest lof) nlon))])))
          