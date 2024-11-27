;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname spelling) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***************************************************
;; Edison Ying (21119725)
;; CS 135 Fall 2024
;; Assignment 07, Q3 SpellBound
;; ***************************************************
;;

;; (a)

(define-struct node (letter term next))
;; A Node is a (make-node Char Bool Next)

;; A Next is a (listof Node)

;; node-template bexp: Node -> Any
(define (node-template n)
  (cond [(empty? (node-next n)) ...]
        [(cons? (node-next n)) (... (next-template (node-next n)))]))

;; next-template: (listof Node) -> Any
(define (next-template lon)
  (cond [(empty? lon) ...]
        [else (... (node-template (first lon))
                   (next-template (rest lon)))]))

;; ***************************************************
;; (b)
;; (create-tree words) produces a tree of the characters to form words in the words list
;; Examples:
(check-expect (create-tree (list "PEN" "PENCIL" "PENDANT" "PENNY"))
              (make-node #\space false (list
                            (make-node #\P false (list
                                        (make-node #\E false (list
                                                    (make-node #\N true (list
                                                                (make-node #\C false (list
                                                                            (make-node #\I false (list
                                                                                        (make-node #\L true empty)))))
                                                                (make-node #\D false (list
                                                                            (make-node #\A false (list
                                                                                        (make-node #\N false (list
                                                                                                    (make-node #\T true empty)))))))
                                                                (make-node #\N false (list
                                                                            (make-node #\Y true empty))))))))))))
(check-expect (create-tree (list "FEW")) (make-node #\space false (list
                                        (make-node #\F false (list
                                                              (make-node #\E false (list
                                                                                    (make-node #\W true empty))))))))
(check-expect (create-tree (list "FEW" "FUN"))
              (make-node #\space false (list
                                        (make-node #\F false (list
                                                              (make-node #\E false (list
                                                                                    (make-node #\W true empty)))
                                                              (make-node #\U false (list
                                                                                    (make-node #\N true empty))))))))
(check-expect (create-tree (list "FUN" "FAR"))
              (make-node #\space false (list
                                        (make-node #\F false (list
                                                              (make-node #\A false (list
                                                                                    (make-node #\R true empty)))
                                                              (make-node #\U false (list
                                                                                    (make-node #\N true empty))))))))
(check-expect (create-tree (list "FUN" "fun" "fUn" "F"))
              (make-node #\space false (list
                                        (make-node #\F true (list
                                                             (make-node #\U false (list
                                                                                   (make-node #\N true empty))))))))

;; create-tree: (listof Str) -> Node
(define (create-tree words)
  (build-tree (make-node #\space false empty) words))

;; (build-tree cur-tree words) builds the tree recursively
;; build-tree: Node (listof Str) -> Node
(define (build-tree cur-tree words)
  (cond [(empty? words) cur-tree]
        [else (build-tree (tree-insert cur-tree (to-upper (string->list (first words)))) (rest words))]))

;; (to-upper wlst) converts a list of characters to a all upper case
;; Examples
(check-expect (to-upper (list #\f #\u #\n)) (list #\F #\U #\N))

;; to-upper: (listof Char) -> (listof Char)
(define (to-upper wlst)
  (cond [(empty? wlst) empty]
        [else (cons (char-upcase (first wlst)) (to-upper (rest wlst)))]))

;; (tree-insert n word) inserts a node onto a tree
;; tree-insert: Node (listof Char) -> Node
(define (tree-insert n word)
  (cond [(empty? word) (make-node (node-letter n) true (node-next n))]
        [else (make-node (node-letter n) (node-term n)
                         (next-insert (node-next n) word))]))

;; (next-insert lon word) processes a list of nodes with mutual recursion with tree-insert
;; next-insert: (listof Node) (listof Char) -> Node
(define (next-insert lon word)
  (cond [(empty? lon) (cons (tree-insert (make-node (first word) false empty)
                                         (rest word)) lon)]
        [(char=? (node-letter (first lon)) (first word))
         (cons (tree-insert (first lon) (rest word)) (rest lon))]
        [(char<? (first word) (node-letter (first lon)))
         (cons (tree-insert (make-node (first word) false empty)
                            (rest word)) lon)]
        [else (cons (first lon) (next-insert (rest lon) word))]))

;; ***************************************************
;; (c)
;; (check word tree) checks if the word is in the word tree
;; Examples
(define dict (create-tree (list "FEW" "FUN")))
(check-expect (check "FEW" dict) true)
(check-expect (check "few" dict) true)
(check-expect (check "fun" dict) true)
(check-expect (check "" dict) false)
(check-expect (check "fu" dict) false)
(check-expect (check "potatoes" dict) false)
;; check: Str Node -> Bool
(define (check word tree)
  (check-node (to-upper (string->list word)) tree))

;; (check-node word n) checks if the word is empty, if not recurses through the list of next
;; check-node: (listof Char) Node -> Bool
(define (check-node word n)
  (cond [(empty? word) (node-term n)]
        [else (check-list word (node-next n))]))
        
;; (check-node word lon) checks if the first of the word corresponds to the first in the list and recurses
;; check-node: (listof Char) (listof Node) -> Bool
(define (check-list word lon)
  (cond [(empty? lon) false]
        [(char=? (first word) (node-letter (first lon)))
         (check-node (rest word) (first lon))]
        [else (check-list word (rest lon))]))
  
       




