;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname n-queens) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***************************************************
;; Edison Ying (21119725)
;; CS 135 Fall 2024
;; Assignment 10, Q4 N-Queens
;; ***************************************************
;;

;; A Position is a (list Nat Nat)
;;
;; A Candidate is a (listof Position)
;; Requires: No two Positions are the same

;; Variables:
(define left-soln '((3 2) (2 0) (1 3) (0 1)))
(define right-soln '((3 1) (2 3) (1 0) (0 2)))

;; ***************************************************
;; (a)
;; (attacking? p1 p2) t produces true if a queen at Position
;; p1 is attacking a queen at Position p2, and false otherwise.
;; Examples:
(check-expect (attacking? (list 0 0) (list 1 1)) true)
(check-expect (attacking? (list 0 0) (list 1 2)) false)
(check-expect (attacking? (list 3 2) (list 0 5)) true)
(check-expect (attacking? (list 3 2) (list 7 7)) false)
;; attacking?: Position Position -> Bool
(define (attacking? p1 p2)
  (or (= (first p1) (first p2))
      (= (second p1) (second p2))
      (= (abs (- (first p1) (first p2)))
         (abs (- (second p1) (second p2))))))

;; ***************************************************
;; (b)
;; (valid-cand? cand n) produces false if any positions are attacking each other
;; or outside the n grid bounds
;; Examples:
(check-expect (valid-cand? left-soln 4) true)
(check-expect (valid-cand? right-soln 4) true)
(check-expect (valid-cand? left-soln 3) false)
(check-expect (valid-cand? (list (list 0 0) (list 1 1)) 4) false)
(check-expect (valid-cand? '((3 1) (2 3) (1 0)) 4) true)
;; valid-cand?: Candidate Nat -> Bool
(define (valid-cand? cand n)
  (cond [(empty? cand) true]
        [(and ((check-between? 0 (- n 1)) (first (first cand)))
              ((check-between? 0 (- n 1)) (second (first cand)))
              (not (foldr (lambda (x rror)
                       (or (attacking? (first cand) x) rror))
                     false
                     (rest cand))))
         (valid-cand? (rest cand) n)]
        [else false]))

;; From Prev Assignment
(define (check-between? a b)
  (lambda (x)
    (and (<= a x) (<= x b))))

;; ***************************************************
;; (c)
;; (neighbours-naive cand n) generates the unoccupied neighbors of the Candidate list of grid n
;; Examples:
(check-expect (neighbours-naive '((0 0)) 4)
              '(((1 2) (0 0)) ((1 3) (0 0)) ((2 1) (0 0))
                              ((2 3) (0 0)) ((3 1) (0 0)) ((3 2) (0 0))))
(check-expect (neighbours-naive '((1 1)) 4)
              '(((0 3) (1 1)) ((2 3) (1 1)) ((3 0) (1 1))
               ((3 2) (1 1))))
;(check-expect (neighbours-naive '((1 2) (0 0)) 4)
;              '(((3 1) (1 2) (0 0))))

;; neighbours-naive: Candidate Nat -> (listof Candidate)
;; requires: no two positions in candidate are attacking each other
(define (neighbours-naive cand n)
  (local [(define grid (build-list n (lambda (j) (build-list n (lambda (i) (list j i))))))
          (define coords (foldr append empty grid))
          ;; neighbours-naive/help: (listof Position)
          (define (neighbours-naive/help candi lop)
            (cond [(empty? lop) empty]
                  [(empty? candi) (cons (append (list (first lop)) cand)
                                        (neighbours-naive/help cand (rest lop)))]
                  [(equal? (first cand) (first lop))
                   (neighbours-naive/help candi (rest lop))]
                  [(attacking? (first candi) (first lop))
                   (neighbours-naive/help cand (rest lop))]
                  [else (neighbours-naive/help (rest candi) lop)]))]
    (cond [(empty? cand) empty]
          [else (neighbours-naive/help cand coords)])))

;; ***************************************************
;; (d)
;; (neighbours-row cand n) produces possible solutions based off a row by row algorithm
;; Examples:
(check-expect (neighbours-row '((0 0)) 4)
              '(((1 2) (0 0)) ((1 3) (0 0))))
(check-expect (neighbours-row '((1 1)) 4)
              '(((2 3) (1 1))))
(check-expect (neighbours-row '((1 2) (0 0)) 4)
              '())
(check-expect (neighbours-row '((3 3)) 4)
              '())
(check-expect (neighbours-row '((1 3) (0 1)) 4)
              '(((2 0) (1 3) (0 1))))

;; neighbours-row: Candidate Nat -> (listof Candidate)
(define (neighbours-row cand n)
  (local [(define (neighbours-row/help candi lop)
            (cond [(empty? lop) empty]
                  [(empty? candi) (cons (append (list (first lop)) cand)
                                        (neighbours-row/help cand (rest lop)))]
                  [(attacking? (first candi) (first lop))
                   (neighbours-row/help cand (rest lop))]
                  [else (neighbours-row/help (rest candi) lop)]))]
  (cond [(empty? cand) empty]
        [(>= (first (first cand)) (- n 1)) empty]
        [else
          (neighbours-row/help cand
                               (build-list n (lambda (x)
                                               (list (+ (first (first cand)) 1) x))))])))

;; ***************************************************
;; (e)
;; (n-queens n nbr-fun) produces the full solution to the n-queens problem
;; Examples:
(check-expect (n-queens 5 neighbours-row)
              (list
               (list
                (list 4 3)
                (list 3 1)
                (list 2 4)
                (list 1 2)
                (list 0 0))
               (list
                (list 4 2)
                (list 3 4)
                (list 2 1)
                (list 1 3)
                (list 0 0))
               (list
                (list 4 4)
                (list 3 2)
                (list 2 0)
                (list 1 3)
                (list 0 1))
               (list
                (list 4 3)
                (list 3 0)
                (list 2 2)
                (list 1 4)
                (list 0 1))
               (list
                (list 4 4)
                (list 3 1)
                (list 2 3)
                (list 1 0)
                (list 0 2))
               (list
                (list 4 0)
                (list 3 3)
                (list 2 1)
                (list 1 4)
                (list 0 2))
               (list
                (list 4 1)
                (list 3 4)
                (list 2 2)
                (list 1 0)
                (list 0 3))
               (list
                (list 4 0)
                (list 3 2)
                (list 2 4)
                (list 1 1)
                (list 0 3))
               (list
                (list 4 2)
                (list 3 0)
                (list 2 3)
                (list 1 1)
                (list 0 4))
               (list
                (list 4 1)
                (list 3 3)
                (list 2 0)
                (list 1 2)
                (list 0 4))))
(check-expect (n-queens 4 neighbours-naive)
              (list (list (list 3 2) (list 2 0) (list 1 3) (list 0 1))
                    (list (list 3 1) (list 2 3) (list 1 0) (list 0 2))))

;; n-queens: Nat (Candidate Nat -> (listof Candidate)) -> (listof Candidate)
(define (n-queens n nbr-fun)
  (local [(define (find-path cand)
            (cond [(= (length cand) n) cand]
                  [else (local [(define nbrs (nbr-fun cand n))
                                (define paths (foldr
                                               (lambda (x rror)
                                                 (cond [(= (+ 1 (first (first cand)))
                                                           (first (first x)))
                                                        (cons x rror)]
                                                       [else rror])) empty nbrs))]
                          (foldr (lambda (x rror)
                                   (cond [(valid-cand? x n)
                                          (cond [(empty? (find-path x)) rror]
                                                [else (append (find-path x) rror)])]
                                         [else rror]))
                                 empty
                                 paths))]))]
  (foldr (lambda (x rror)
           (local [(define paths (find-path (list x)))]
           (cond [(empty? paths) rror]
                 [(= (length paths) n) (cons paths rror)]
                 [else (append (foldr (lambda (i rror_1)
                                (cons (slice (* n i) (- (* n (+ i 1)) 1) paths) rror_1))
                              empty
                              (build-list (quotient (length paths) n) (lambda (x) x))) rror)])))
         empty
         (build-list n (lambda (x) (list 0 x))))))

;; From Prev Assignment
(define (slice a b lst)
  (foldr
   (lambda (x y rror)
     (cond [(and (<= a y) (<= y b)) (cons x rror)]
           [else rror]))
   empty
   lst
   (build-list (length lst) (lambda (x) x))))

;; (find-path orig dest g) finds path from orig to dest in g if it exists
;; find-path: Node Node Graph → (anyof (ne-listof Node) false)
;(define (find-path cand n)
;  (cond [(= (length cand) n) cand]
;        [else (local [(define nbrs (neighbours-row (list (first cand)) n))
;                      (define paths (map (lambda (x) (cons (first x) cand)) nbrs))]
;                (foldr (lambda (x rror)
;                         (cond [(valid-cand? x n)
;                                (cond [(empty? (find-path x n)) rror]
;                                      [else (append (find-path x n) rror)])]
;                               [else rror]))
;                       empty
;                       paths))]))

;; (find-path/list nbrs dest g) produces path from
;; an element of nbrs to dest in g, if one exists
;; find-path/list: (listof Node) Node Graph → (anyof (ne-listof Node) false)
;(define (find-path/list nbrs dest g)
;  (cond [(empty? nbrs) false]
;        [else (local [(define ?path (find-path (first nbrs) dest g))]
;                (cond [(false? ?path)
;                       (find-path/list (rest nbrs) dest g)]
;                      [else ?path]))]))

    
    
    
