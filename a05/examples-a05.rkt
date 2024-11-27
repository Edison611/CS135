;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname examples-a05) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Edison Ying (21119725)
;; CS 135 Fall 2024
;; Assignment 05, Examples
;; ***************************************************
;;

;; Q2
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

;; (a)
(check-expect (missed-deadline-add selections "w2cordur")
              (list
               (list "mpines" (list 'CS135 'MATH135 'MATH137 'ENGL109 'FINE100))
               (list "w46dles" (list 'ARBUS101 'ECON101 'ECON102 'ECON206 'LS101))
               (list "d32pines" (list 'CS115 'MATH135 'MATH137 'ENGL109 'FINE100))
               (list "gnclstan" (list 'ANTH241 'LS201 'AMATH231 'PMATH347))
               (list "w2cordur" empty)))

(check-expect (missed-deadline-add selections2 "abcd")
              selections2)

;; (b)
(check-expect (taking-course? selections "d32pines" 'CS115) true)
(check-expect (taking-course? selections2 "ke" 'MATH135) true)
(check-expect (taking-course? selections2 "ababaab" 'MATH135) false)
(check-expect (taking-course? selections2 "e3ying" 'CS145) false)

;; (c)
(check-expect (add-course selections "gnclstan" 'CS246)
              (list
               (list "mpines" (list 'CS135 'MATH135 'MATH137 'ENGL109 'FINE100))
               (list "w46dles" (list 'ARBUS101 'ECON101 'ECON102 'ECON206 'LS101))
               (list "d32pines" (list 'CS115 'MATH135 'MATH137 'ENGL109 'FINE100))
               (list "gnclstan" (list 'ANTH241 'LS201 'AMATH231 'PMATH347 'CS246))))
(check-expect (add-course empty "mdluffy" 'CS246)
              (list (list "mdluffy" (list 'CS246))))
(check-expect (add-course selections2 "abaa" 'CS246)
              (list
               (list "e3ying" (list 'CS135 'MATH135 'MATH137 'ENGL109 'AFM101))
               (list "abcd" (list 'ARBUS101 'ECON101 'ECON102 'ECON206 'LS101))
               (list "ke" (list 'CS115 'MATH135 'MATH137 'ENGL109 'FINE100))
               (list "abaa" (list 'CS246))))

;; (d)
(check-expect (create-classlist selections 'MATH135)
              (list "mpines" "d32pines"))
(check-expect (create-classlist selections2 'ABC191) empty)
(check-expect (create-classlist selections2 'ENGL109)
              (list "e3ying" "ke"))

;; Q3
(check-expect (make-symbol-lists (list 2 1 3) 'X)
              (list (list 'X 'X) (list 'X) (list 'X 'X 'X)))
(check-expect (make-symbol-lists (list 1 2 4 3) 'AL)
              (list (list 'AL) (list 'AL 'AL) (list 'AL 'AL 'AL 'AL) (list 'AL 'AL 'AL)))
(check-expect (make-symbol-lists (list 0) 'A)
              (list (list)))
(check-expect (make-symbol-lists (list 0 1) 'A)
              (list (list) (list 'A)))

;; Q4
(define-struct studentchoice (name type slices))
(define-struct section (instructor sec-num losc))
(define-struct course (sections))
;; Sample Variables (Used for example/test cases)

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

;; (b)
(check-expect (popular-pizza sec1) 'meaty)
(check-expect (popular-pizza sec2) 'veggie)

;; (c)
(check-expect (sort-choices sec1)
              (list "Becker" 1 (list stc4 stc2 stc1 stc3)))
(check-expect (sort-choices sec2)
              (list "Ying" 2 (list stc5 stc6 stc7 stc8)))
(check-expect (sort-choices sec3)
              (list "Byron" 3 (list stc12 stc10 stc11 stc9)))

;; (d)
(check-expect (pizza-lookup CS135 1 "Edison") (list 'meaty 3))
(check-expect (pizza-lookup CS135 3 "Ian") (list 'Hawaiian 2))

;; (e)
(check-expect (count-slices CS135) (list 4 7 105))


