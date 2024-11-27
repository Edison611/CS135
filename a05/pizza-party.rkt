;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname pizza-party) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;; Edison Ying (21119725)
;; CS 135 Fall 2024
;; Assignment 05, Q4 Pizza Party
;; ***************************************************
;;

;; (a)
;; A StudentChoice is a (list Str Sym Nat)
;; Requires: (second StudentChoice) to be one of 'Hawaiian, 'meaty, or 'veggie

;; studentchoice-template: StudentChoice -> Any
(define (studentchoice-template sc)
  (... (first sc)
       (second sc)
       (third sc)))

;; A Section is a (list Str Nat (listof StudentChoice))

;; section-template: Section -> Any
(define (section-template section)
  (... (first section)
       (second section)
       (stc-list-template (third section))))

;; stc-list-template: (listof StudentChoice) -> Any
(define (stc-list-template losc)
  (cond [(empty? losc) ...]
        [else (... (first (first sc))
                   (second (first sc))
                   (third (first sc))
                   (stc-rest-template (rest losc)))]))

;; A Course is a (listof Section)
        
;; course-template: Course -> Any
(define (course-template course)
  (cond [(empty? course) ...]
        [else (... (section-template (first course))
                   (course-template (rest course)))]))

;; ***************************************************

;; Sample Constants (Used for example/test cases)
(define stc1 (list "Edison" 'meaty 3))
(define stc2 (list "A" 'meaty 2))
(define stc3 (list "B" 'veggie 100))
(define stc4 (list "C" 'Hawaiian 1))

(define stc5 (list "D" 'meaty 1))
(define stc6 (list "E" 'veggie 2))
(define stc7 (list "F" 'veggie 1))
(define stc8 (list "G" 'veggie 1))

(define stc9 (list "Hi" 'veggie 1))
(define stc10 (list "Ian" 'Hawaiian 2))
(define stc11 (list "J" 'meaty 1))
(define stc12 (list "Iaa" 'Hawaiian 1))

(define sec1 (list "Becker" 1 (list stc1 stc2 stc3 stc4)))
(define sec2 (list "Ying" 2 (list stc5 stc6 stc7 stc8)))
(define sec3 (list "Byron" 3 (list stc9 stc10 stc11 stc12)))

(define CS135 (list sec1 sec2 sec3))

;; ***************************************************
;; (b)
;; (popular-pizza class-section) Produces the most popular type of pizza given a section
;; Examples
(check-expect (popular-pizza sec1) 'meaty)
(check-expect (popular-pizza sec2) 'veggie)
;; popular-pizza: Section -> 'Sym
;; requires: The most popular-pizza should be a unique value
(define (popular-pizza class-section)
  (cond [(and (> (count-fav-type 'meaty (third class-section))
                 (count-fav-type 'veggie (third class-section)))
              (> (count-fav-type 'meaty (third class-section))
                 (count-fav-type 'Hawaiian (third class-section))))
         'meaty]
        [(and (> (count-fav-type 'veggie (third class-section))
                 (count-fav-type 'meaty (third class-section)))
              (> (count-fav-type 'veggie (third class-section))
                 (count-fav-type 'Hawaiian (third class-section))))
         'veggie]
        [else 'Hawaiian]))

;; (count-fav-type) counts the number of students that like a certain type of pizza
;; Examples
(check-expect (count-fav-type 'meaty (third sec1)) 2)
(check-expect (count-fav-type 'veggie (third sec1)) 1)
(check-expect (count-fav-type 'Hawaiian (third sec2)) 0)

;; count-fav-type: Sym (listof StudentChoice) -> Nat
(define (count-fav-type type student-list)
  (cond [(empty? student-list) 0]
        [(symbol=? type (second (first student-list)))
         (+ 1 (count-fav-type type (rest student-list)))]
        [else (count-fav-type type (rest student-list))]))

;; ***************************************************
;; (c)
;; (sort-choices class-section) produces a sorted section that goes by pizza type 'Hawaiian -> 'meaty -> 'veggie
;;                              then sorted alphabetically by students' name
;; Examples
(check-expect (sort-choices sec1)
              (list "Becker" 1 (list stc4 stc2 stc1 stc3)))
(check-expect (sort-choices sec2)
              (list "Ying" 2 (list stc5 stc6 stc7 stc8)))
(check-expect (sort-choices sec3)
              (list "Byron" 3 (list stc12 stc10 stc11 stc9)))
;; sort-choices: Section -> Section
(define (sort-choices class-section)
  (list (first class-section)
        (second class-section)
        (sorted-class-section (third class-section))))

;; (sorted-class-section) produces a sorted list by pizza type 'Hawaiian -> 'meaty -> 'veggie
;;                        then sorted alphabetically by students' name
;; Examples
(check-expect (sorted-class-section (list stc1 stc2 stc3 stc4))
              (list stc4 stc2 stc1 stc3))
(check-expect (sorted-class-section (list stc5 stc6 stc7 stc8))
              (list stc5 stc6 stc7 stc8))
(check-expect (sorted-class-section (list stc9 stc10 stc11 stc12))
              (list stc12 stc10 stc11 stc9))

;; sorted-class-section: (listof StudentChoice) -> (listof StudentChoice)
(define (sorted-class-section class-list)
  (cond [(empty? class-list) empty]
        [else (insert (first class-list)
                      (sorted-class-section (rest class-list)))]))

;; (choices<= choice1 choice2) produces true if the first students' choice should come before the second, false otherwise.
;; It should come before (true) in the following pizza type order 'Hawaiian -> 'meaty -> 'veggie , and the name is alphabetically less than the second
;; Examples
(check-expect (choices<= stc1 stc2) false)
(check-expect (choices<= stc4 stc1) true)
(check-expect (choices<= stc6 stc7) true)
(check-expect (choices<= stc12 stc10) true)

;; choices<=: StudentChoice StudentChoice -> Bool
(define (choices<= choice1 choice2)
  (cond [(and (symbol=? (second choice1) (second choice2))
              (string<? (first choice1) (first choice2)))
         true]
        [(and (symbol=? (second choice1) 'Hawaiian)
              (not (symbol=? (second choice2) 'Hawaiian))) true]
        [(and (symbol=? (second choice1) 'meaty)
              (symbol=? (second choice2) 'veggie))
         true]
        [else false]))

;; (insert) inserts an item into a list at a specific position if choices<= is true compared to the current item
;; Examples
(check-expect (insert stc3 (list stc2 stc4))
              (list stc2 stc4 stc3))
(check-expect (insert stc1 empty) (list stc1))
(check-expect (insert stc12 (list stc10 stc11 stc9)) (list stc12 stc10 stc11 stc9))

;; insert: StudentChoice (listof StudentChoice) -> (listof StudentChoice)
;; Requires: list to be already sorted in the correct order
(define (insert item list)
  (cond [(empty? list) (cons item empty)]
        [(choices<= item (first list))
         (cons item list)]
        [else (cons (first list) (insert item (rest list)))]))
        
;; ***************************************************
;; (d)
;; (pizza-lookup course-input sec-num name) produces the fixed-length list containing the pizza type and slice
;; count for the given student
;; Examples
(check-expect (pizza-lookup CS135 1 "Edison") (list 'meaty 3))
(check-expect (pizza-lookup CS135 3 "Ian") (list 'Hawaiian 2))
;; pizza-lookup: Course Nat Str -> (list Sym Nat)
(define (pizza-lookup course-input sec-num name)
  (list
   (second (student-lookup name
                           (third (section-lookup sec-num course-input))))
   (third (student-lookup name
                          (third (section-lookup sec-num course-input))))))

;; (section-lookup sec-num sections) produces the desired section given a section list based off the section number
;; Examples
(check-expect (section-lookup 3 (list sec1 sec2 sec3)) sec3)

;; section-lookup: Nat (listof Section) -> Section
(define (section-lookup sec-num sections)
  (cond [(= (second (first sections)) sec-num)
         (first sections)]
        [else (section-lookup sec-num (rest sections))]))

;; (student-lookup) produces the desired studentchoice based off a students' name in a class list
;; Examples
(check-expect (student-lookup "F" (list stc5 stc6 stc7 stc8)) stc7)
;; student-lookup: Str (listof StudentChoice) -> (Anyof StudentChoice Bool)
;; Requires: name to be in class-list
(define (student-lookup name class-list)
  (cond [(empty? class-list) false] ; this case is theoretically not possible
        [(string=? name (first (first class-list)))
         (first class-list)]
        [else (student-lookup name (rest class-list))]))

;; ***************************************************
;; (e)
;; (count-slices course-input) produces a list of the slice count for each type of of the 3 pizzas
;; Examples
(check-expect (count-slices CS135) (list 4 7 105))
;; count-slices: Course -> (list Nat Nat Nat)
(define (count-slices course-input)
  (list (slice-type-count 'Hawaiian course-input)
        (slice-type-count 'meaty course-input)
        (slice-type-count 'veggie course-input)))


;; (slice-type-count type class-list) produces the count of a certain type of pizza for a list of sections
;; Examples
(check-expect (slice-type-count 'meaty (list sec1 sec2 sec3)) 7)
;; slice-type-count: Sym (listof Section) -> Nat
(define (slice-type-count type sections)
  (cond [(empty? sections) 0]
        [else (+ (class-slice-type-count type (third (first sections)))
                 (slice-type-count type (rest sections)))]))

;; (class-slice-type-count type class-list) produces the count of a certain type of pizza for a list of students
;; Examples
(check-expect (class-slice-type-count 'meaty (list stc1 stc2 stc3 stc4)) 5)
;; class-slice-type-count: Sym (listof StudentChoice) -> Nat
(define (class-slice-type-count type class-list)
  (cond [(empty? class-list) 0]
        [(symbol=? type (second (first class-list)))
         (+ (third (first class-list)) (class-slice-type-count type (rest class-list)))]
        [else (class-slice-type-count type (rest class-list))]))







           
