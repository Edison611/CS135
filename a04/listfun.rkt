;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname listfun) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Edison Ying (21119725)
;; CS 135 Fall 2024
;; Assignment 04, Q2 Listfun
;; ***************************************************
;;

;; (a)
;; (list-has-exactly-4-symbols list) produces true if there are exactly 4 symbols and false otherwise

;; Examples:
(check-expect
 (list-has-exactly-4-symbols (cons 'Prof (cons "Roh" (cons "is"
                                                     (cons 'number (cons 1 empty)))))) false)
(check-expect
 (list-has-exactly-4-symbols (cons 'I (cons 'really (cons 'love
                                                    (cons 'CS135 (cons "!" empty)))))) true)
(check-expect
 (list-has-exactly-4-symbols (cons 'a
                     (cons 'b
                           (cons 'b
                                      (cons "!"
                                            (cons "GG" empty)))))) false)
(check-expect
 (list-has-exactly-4-symbols (cons 'a
                     (cons 'b
                           (cons true
                                      (cons 'false
                                            (cons 'true empty)))))) true)

;; list-has-exactly-4-symbols: (listof (anyof Num Str Bool Sym)) -> Bool
(define (list-has-exactly-4-symbols list)
  (= 4 (list-num-sym list)))


;; (list-num-sym list) produces the number of symbols in the list
;; Examples
(check-expect
 (list-num-sym (cons 'Prof (cons "Roh" (cons "is"
                                       (cons 'number (cons 1 empty)))))) 2)
(check-expect
 (list-num-sym (cons 'I (cons 'really (cons 'love
                                      (cons 'CS135 (cons "!" empty)))))) 4)
(check-expect
 (list-num-sym (cons 'a
                     (cons 'b
                           (cons 'b
                                      (cons "!"
                                            (cons "GG" empty)))))) 3)
(check-expect
 (list-num-sym (cons 'a
                     (cons 'b
                           (cons true
                                      (cons 'false
                                            (cons 'true empty)))))) 4)
;; list-num-sym: (listof (anyof Num Str Bool Sym)) -> Nat 
(define (list-num-sym list)
  (cond
    [(empty? list) 0]
    [(symbol? (first list))
     (+ 1
        (list-num-sym (rest list)))]
    [else (list-num-sym (rest list))]))

;; ***************************************************
;; (b)
;; (add-only-numbers list) Adds only numbers together and produces the result
;; Examples:
(check-expect
 (add-only-numbers (cons "CS" (cons 135 (cons 'is
                                              (cons "number" (cons 1 empty)))))) 136)
(check-expect
 (add-only-numbers (cons (cons 1 (cons 3 empty)) (cons "Hi"
                                                       (cons 5 empty)))) 5)
(check-expect
 (add-only-numbers (cons true (cons 11 (cons 2
                                              (cons 4 (cons 0.5 empty)))))) 17.5)
(check-expect
 (add-only-numbers empty) 0)
(check-expect
 (add-only-numbers (cons "CS" (cons false (cons 'is
                                              (cons "number" (cons true empty)))))) 0)
(check-expect
 (add-only-numbers (cons "CS" (cons -3 (cons 'is
                                              (cons -5 (cons true empty)))))) -8)
;; add-only-numbers: (listof Any) -> Num
(define (add-only-numbers list)
  (cond
    [(empty? list) 0]
    [(number? (first list))
     (+ (first list)
        (add-only-numbers (rest list)))]
    [else (add-only-numbers (rest list))]))

;; ***************************************************
;; (c)
;; (before-after s1 s2 s3 list) produces a new list where whenever the first consumed string occurs, the second
;; consumed string will occur before it and the third string will occur after it. If the
;; first string does not appear in the list, nothing happens.

;; Examples
(check-expect
 (before-after "student" "hello" "welcome"
               (cons "smart" (cons "student" (cons "here" empty))))
 (cons "smart" (cons "hello" (cons "student"
                                   (cons "welcome" (cons "here" empty))))))
(check-expect
 (before-after "student" "hello" "welcome"
               (cons "smart" (cons "ah" (cons "here" empty))))
 (cons "smart" (cons "ah" (cons "here" empty))))
(check-expect
 (before-after "student" "hello" "welcome"
               (cons "smart" (cons "a" (cons "student" empty))))
 (cons "smart" (cons "a" (cons "hello"
                                   (cons "student" (cons "welcome" empty))))))

;; before-after: Str Str Str (listof Str) -> (listof Str)
(define (before-after s1 s2 s3 list)
  (cond
    [(empty? list) empty]
    [(string=? (first list) s1)
     (cons s2
           (cons (first list)
                 (cons s3 (rest list))))]
    [else (cons (first list) (before-after s1 s2 s3 (rest list)))]))

;; ***************************************************
;; (d)
;; (exists? val list) checks whether val is in the list or not
;; Examples:
(check-expect (exists? 135 (cons 'CS (cons 135 (cons "rules"
                                                     empty)))) true)
(check-expect (exists? 135 (cons 'CS (cons "135" (cons "then" (cons
                                                               136 empty))))) false)
(check-expect (exists? "pi" (cons 'CS (cons 'AJADJA (cons "pi" (cons
                                                               pi empty))))) true)
(check-expect (exists? "" (cons 'a (cons 'AJADJA (cons "pi" (cons
                                                               pi empty))))) false)

;; exists?: (anyof Num Str Sym) (listof (anyof Num Str Sym)) -> Bool
(define (exists? val list)
  (cond
    [(empty? list) false]
    [(and (number? val) (number? (first list)) (= (first list) val)) true]
    [(and (string? val) (string? (first list)) (string=? (first list) val)) true]
    [(and (symbol? val) (symbol? (first list)) (symbol=? (first list) val)) true]
    [else (exists? val (rest list))]))

;; ***************************************************
;; (e)
;; (remove-duplicates list) removes duplicate elements, keeping the first occuring one in the list
;; Examples
(check-expect (remove-duplicates
               (cons 'happy (cons 3 (cons 'happy (cons 2
                                                       (cons 'cs (cons 2 (cons "CS" (cons 2
                                                                      (cons 5 empty))))))))))
              (cons 3 (cons 'happy (cons 'cs (cons "CS" (cons 2 (cons 5
                                                                      empty)))))))
(check-expect (remove-duplicates
               (cons 3 (cons 3 (cons 3 (cons 3 (cons 3 (cons 3 empty)))))))
              (cons 3 empty))
(check-expect (remove-duplicates
               (cons 3 (cons 4 (cons 3 (cons 5 (cons 3 (cons 3 empty)))))))
              (cons 4 (cons 5 (cons 3 empty))))
;; remove-duplicates: (listof (anyof Num Str Sym)) -> (listof (anyof Num Str Sym))
(define (remove-duplicates list)
  (cond [(empty? list) empty]
        [(exists? (first list) (rest list))
         (remove-duplicates (rest list))]
        [else (cons (first list) (remove-duplicates (rest list)))]))



