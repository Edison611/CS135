;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname matrix) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***************************************************
;; Edison Ying (21119725)
;; CS 135 Fall 2024
;; Assignment 06, Q2 Matrix
;; ***************************************************
;;

;; (a)
;; A Matrix is a (listof (listof Any))

;; ***************************************************
;; Sample Variables
(define M (list (list 1 2 3 4) (list 5 6 7 8)))
(define M1 (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
(define M2 (list (list 1 0) (list 0 1)))
(define M3 (list (list 2 1) (list 1 3)))


;; ***************************************************
;; (b)
;; (matrix-item matrix row column) produces the item in the matrix in the specified row and column
;; Examples
(check-expect (matrix-item M 1 2) 7)
(check-expect (matrix-item M1 1 1) 5)
(check-expect (matrix-item M2 0 0) 1)
;; matrix-item: Matrix Nat Nat -> Any
;; Requires:
;; * row < (length matrix)
;; * column < (length (first matrix))
(define (matrix-item matrix row column)
  (cond [(and (= 0 row) (= 0 column))
         (first (first matrix))]
        [(and (= 0 row) (> column 0))
         (matrix-item (list (rest (first matrix))) row (- column 1))]
        [(> row 0)
         (matrix-item (rest matrix) (- row 1) column)]))

;; ***************************************************
;; (c)
;; (matrix-col matrix col) produces the specified column of the matrix
;; Examples
(check-expect (matrix-col M 2) (list 3 7))
(check-expect (matrix-col M1 0) (list 1 4 7))

;; matrix-col: Matrix Nat -> (listof Any)
;; Requires: col < # of columns in the list
(define (matrix-col matrix col)
  (cond [(empty? matrix) empty]
        [else (cons (lst-col (first matrix) col)
                    (matrix-col (rest matrix) col))]))

;; (lst-col lst col) produces the item of a list at the specified index/column
;; Examples
(check-expect (lst-col (list 1 2 3 4) 2) 3)
;; lst-col: (listof Any) Nat -> Any
;; Requires: col < (length lst)
(define (lst-col lst col)
  (cond [(= col 0) (first lst)]
        [else (lst-col (rest lst) (- col 1))]))

;; ***************************************************
;; (d)
;; (matrix-transpose matrix) produces the transpose of the matrix (rows and columns swap)
;; Examples
(check-expect (matrix-transpose M) (list (list 1 5) (list 2 6) (list 3 7) (list 4 8)))
(check-expect (matrix-transpose M1) (list (list 1 4 7) (list 2 5 8) (list 3 6 9)))
(check-expect (matrix-transpose empty) empty)

;; matrix-transpose: Matrix -> Matrix
(define (matrix-transpose matrix)
  (cond [(empty? matrix) empty]
        [else (matrix-transpose-helper matrix 0 (length (first matrix)))]))

;; (matrix-transpose-helper matrx cur-col total-col) returns the transpose of the matrix
;; Examples
(check-expect (matrix-transpose-helper M1 0 3) (list (list 1 4 7) (list 2 5 8) (list 3 6 9)))
;; matrix-tranpose-helper: Matrix Nat Nat -> Matrix
(define (matrix-transpose-helper matrix cur-col total-col)
  (cond [(= cur-col total-col) empty]
        [else (cons (matrix-col matrix cur-col)
                    (matrix-transpose-helper matrix (+ cur-col 1) total-col))]))

;; ***************************************************
;; (e)
;; (matrix-multiply m1 m2) produces the product of two matrices
;; Examples:
(check-expect (matrix-multiply M3 M2) M3)
(check-expect (matrix-multiply M2 M3) M3)
(check-expect (matrix-multiply (list (list -1 4) (list 2 3))
                               (list (list 9 -3) (list 6 1)))
              (list (list 15 7) (list 36 -3)))
(check-expect (matrix-multiply (list (list 2 6 4) (list 1 9 7))
                               (list (list 3 2) (list 4 10) (list 11 8)))
              (list (list 74 96) (list 116 148)))
(check-expect (matrix-multiply (list (list 3 2) (list 4 10) (list 11 8))
                               (list (list 2 6 4) (list 1 9 7)))
              (list (list 8 36 26) (list 18 114 86) (list 30 138 100)))
                               
;; matrix-multiply: Matrix Matrix -> Matrix
;; Requires:
;; * Initial # of rows in m1 = # of columns in m2
;; * All items to be numbers
(define (matrix-multiply m1 m2)
  (cond [(empty? m1) empty]
        [else (cons (matrix-mul-row (first m1) m2 0 (length (first m2)))
                    (matrix-multiply (rest m1) m2))]))

;; (matrix-mul-row row matrix cur-col) produces the matrix multiplication of a row and a matrix
;; Examples
(check-expect (matrix-mul-row (list 2 1) M2 0 2) (list 2 1))
;; matrix-mul-row: (ne-listof Num) Matrix Nat -> (ne-listof Num)
(define (matrix-mul-row row matrix cur-col total-col)
  (cond [(= total-col cur-col) empty]
        [else (cons (dot-product row (matrix-col matrix cur-col))
                    (matrix-mul-row row matrix (+ cur-col 1) total-col))]))
                    
;; (dot-product lst1 lst2) produces the dot product of two 1D lists
;; Examples:
(check-expect (dot-product (list 1 3 2) (list 3 2 2)) 13)

;; dot-product: (listof Num) (listof Num) -> Num
;; Requires: (length lst1) = (length lst2)
(define (dot-product lst1 lst2)
  (cond [(empty? lst1) 0]
        [else (+ (* (first lst1) (first lst2))
                 (dot-product (rest lst1) (rest lst2)))]))
  
