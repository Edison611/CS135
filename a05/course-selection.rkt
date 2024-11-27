;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname course-selection) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Edison Ying (21119725)
;; CS 135 Fall 2024
;; Assignment 05, Q2 Course Selection
;; ***************************************************
;;

;; A DesiredCourses is one of:
;; * empty
;; * (cons (list Str (listof Sym)) DesiredCourses)

(define selections
  (list
   (list "mpines" (list 'CS135 'MATH135 'MATH137 'ENGL109 'FINE100))
   (list "w46dles" (list 'ARBUS101 'ECON101 'ECON102 'ECON206 'LS101))
   (list "d32pines" (list 'CS115 'MATH135 'MATH137 'ENGL109 'FINE100))
   (list "gnclstan" (list 'ANTH241 'LS201 'AMATH231 'PMATH347))))
(define selections2
  (list
   (list "e3ying" (list 'CS135 'MATH135 'MATH137 'ENGL109 'AFM101))
   (list "abcd" (list 'ARBUS101 'ECON101 'ECON102 'ECON206 'LS101))
   (list "ke" (list 'CS115 'MATH135 'MATH137 'ENGL109 'FINE100))))

;; ***************************************************
;; (a)
;; (missed-deadline-add dcs id) adds a new key-value pair to the end of DesiredCourses of an empty list if
;; it is not already in the list. Otherwise the list remains unchanged

;; Examples
(check-expect (missed-deadline-add selections "w2cordur")
              (list
               (list "mpines" (list 'CS135 'MATH135 'MATH137 'ENGL109 'FINE100))
               (list "w46dles" (list 'ARBUS101 'ECON101 'ECON102 'ECON206 'LS101))
               (list "d32pines" (list 'CS115 'MATH135 'MATH137 'ENGL109 'FINE100))
               (list "gnclstan" (list 'ANTH241 'LS201 'AMATH231 'PMATH347))
               (list "w2cordur" empty)))

(check-expect (missed-deadline-add selections2 "abcd")
              selections2)

;; (missed-deadline-add): DesiredCourses Str -> DesiredCourses
(define (missed-deadline-add dcs id)
  (cond [(empty? dcs) (cons (list id empty) empty)]
        [(string=? (first (first dcs)) id) dcs]
        [else (cons (first dcs) (missed-deadline-add (rest dcs) id))]))

;; Test Cases
(check-expect (missed-deadline-add empty "a")
              (list (list "a" empty)))
;; ***************************************************
;; (b)
;; (taking-course? dcs id cc) produces true of the student has selected the course false otherwise
;; Examples
(check-expect (taking-course? selections "d32pines" 'CS115) true)
(check-expect (taking-course? selections2 "ke" 'MATH135) true)
(check-expect (taking-course? selections2 "ababaab" 'MATH135) false)
(check-expect (taking-course? selections2 "e3ying" 'CS145) false)

;; taking-course?: DesiredCourses Str Sym -> Bool
(define (taking-course? dcs id cc)
  (cond [(empty? dcs) false]
        [(string=? (first (first dcs)) id)
         (cond [(item-in? (first (rest (first dcs))) cc) true]
               [else false])]
        [else (taking-course? (rest dcs) id cc)]))
        
;; (item-in? list item) Produces true if the item is in the list, false otherwise
;; Examples
(check-expect (item-in? (list 'CS135 'MATH135 'MATH137 'ENGL109 'AFM101) 'CS135) true)
(check-expect (item-in? (list 'CS135 'MATH135 'MATH137 'ENGL109 'AFM101) 'CS145) false) 

;; item-in? (listof Sym) Sym -> Bool
(define (item-in? list item)
  (cond [(empty? list) false]
        [(symbol=? (first list) item) true]
        [else (item-in? (rest list) item)]))

;; ***************************************************
;; (c)
;; (add-course dcs id cc) Adds the course to the end of a students' course-list
;; Examples
(check-expect (add-course selections "gnclstan" 'CS246)
              (list
               (list "mpines" (list 'CS135 'MATH135 'MATH137 'ENGL109 'FINE100))
               (list "w46dles" (list 'ARBUS101 'ECON101 'ECON102 'ECON206 'LS101))
               (list "d32pines" (list 'CS115 'MATH135 'MATH137 'ENGL109 'FINE100))
               (list "gnclstan" (list 'ANTH241 'LS201 'AMATH231 'PMATH347 'CS246))))
(check-expect (add-course empty "mdluffy" 'CS246)
              (list (list "mdluffy" (list 'CS246))))
(check-expect (add-course selections2 "e3ying" 'CS135) selections2)

;; add-course: DesiredCourses Str Sym -> DesiredCourses
(define (add-course dcs id cc)
  (cond [(empty? dcs) (cons (list id (list cc)) empty)]
        [(string=? (first (first dcs)) id)
         (cond [(item-in? (second (first dcs)) cc) dcs]
               [else  (cons (list id (add-to-end (second (first dcs)) cc)) (rest dcs))])]
        [else (cons (first dcs) (add-course (rest dcs) id cc))]))

;; Test Cases
(check-expect (add-course selections2 "abaa" 'CS246)
              (list
               (list "e3ying" (list 'CS135 'MATH135 'MATH137 'ENGL109 'AFM101))
               (list "abcd" (list 'ARBUS101 'ECON101 'ECON102 'ECON206 'LS101))
               (list "ke" (list 'CS115 'MATH135 'MATH137 'ENGL109 'FINE100))
               (list "abaa" (list 'CS246))))

;; (add-to-end list item) adds the item to the end of the list
;; Examples
(check-expect (add-to-end (list "a" "b") "c")
              (list "a" "b" "c"))

;; add-to-end: (listof Any) Any -> (listof Any)
(define (add-to-end list item)
  (cond [(empty? list) (cons item empty)]
        [else (cons (first list) (add-to-end (rest list) item))]))

;; ***************************************************
;; (d)
;; (create-classlist dcs cc) produces a list of all the students that want to take the course
;; Examples
(check-expect (create-classlist selections 'MATH135)
              (list "mpines" "d32pines"))

(check-expect (create-classlist selections2 'ABC191) empty)

;; create-classlist: DesiredCourses Sym -> (listof Str)
(define (create-classlist dcs cc)
  (cond [(empty? dcs) empty]
        [(item-in? (first (rest (first dcs))) cc)
         (cons (first (first dcs)) (create-classlist (rest dcs) cc))]
        [else (create-classlist (rest dcs) cc)]))

;; Test Cases
(check-expect (create-classlist selections2 'ENGL109)
              (list "e3ying" "ke"))
  

