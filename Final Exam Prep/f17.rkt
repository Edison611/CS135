;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname f17) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; Q3 (c)

(define t '(8 (7 (4) (2)) (3) (5 (1) (9 (0))) (6)))

;; MY num-nodes/list
(define (num-nodes/list alot)
  (cond
    [(empty? alot) 0]
    [else (+ (num-nodes6 (first alot)) (num-nodes/list (rest alot)))]))

(define (num-nodes alot) 1)

;; 1
(define (num-nodes1 nest)
  (cond
    [(empty? (rest nest)) 0]
    [else (+ 1 (num-nodes/list (rest nest)))]))

;; 2
(define (num-nodes2 nest)
  (add1 (num-nodes/list (rest nest))))

;; 3
(define (num-nodes3 nest)
  (foldr + 1 (map num-nodes3 (rest nest))))

;; 4
(define (num-nodes4 nest)
  (cond
    [(empty? (rest nest)) 1]
    [else (+ 1 (num-nodes/list (rest nest)))]))

;; 5
(define (num-nodes5 nest)
  (add1 (map + (num-nodes5 (rest nest)))))

;; 6
(define (num-nodes6 nest)
  (cond
    [(empty? (rest nest)) 1]
    [else (+ (num-nodes6 (first nest))
             (num-nodes/list (rest nest)))]))

;; (num-nodes1 t) -> 4
;; (num-nodes2 t) -> 10
;; (num-nodes3 t) -> 10
;; (num-nodes4 t) -> 10
;; (num-nodes5 t) -> error
;; (num-nodes6 t) -> error



;; Q7

(define (neighbours v g)
  (cond
    [(empty? g) empty]
    [(symbol=? v (first (first g))) (second (first g))]
    [else (neighbours v (rest g))]))

(define g
  '((A (C D E))
    (B (E J))
    (C ())
    (D (F J))
    (E (K))
    (F (K H))
    (H ())
    (J (H))
    (K ()))
  )


;; (find-path orig dest g) finds path from orig to dest in g if it exists
;; find-path: Node Node Graph → (anyof (ne-listof Node) false)
(define (find-path orig dest g)
  (cond [(symbol=? orig dest) (list (list dest))]
        [else (local [(define nbrs (neighbours orig g))
                      (define ?path (find-path/list nbrs dest g))]
                (cond [(empty? ?path) ?path]
                      [else (map (lambda (x) (cons orig x)) ?path)]))]))

;; (find-path/list nbrs dest g) produces path from
;; an element of nbrs to dest in g, if one exists
;; find-path/list: (listof Node) Node Graph → (anyof (ne-listof Node) false)
(define (find-path/list nbrs dest g)
  (cond [(empty? nbrs) empty]
        [else (local [(define ?path (find-path (first nbrs) dest g))]
                (cond [(empty? ?path)
                       (find-path/list (rest nbrs) dest g)]
                      [else (append ?path (find-path/list (rest nbrs) dest g))]))]))