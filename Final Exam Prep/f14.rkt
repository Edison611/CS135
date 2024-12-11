;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname f14) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; neighbours: Node Graph -> (anyof (listof Node) false)
(define (neighbours v g)
  (cond [(empty? g) false]
        [(symbol=? v (first (first g))) (second (first g))]
        [else (neighbours v (rest g))]))
;; (find-path orig dest g) finds path from orig to dest in g if it exists
;; find-path: Node Node Graph -> (anyof (ne-listof Node) false)
(define (find-path orig dest g)
  (cond [(symbol=? orig dest) (list dest)]
        [else (local [(define nbrs (neighbours orig g))
                      (define ?path (find-path/list nbrs dest g))]
                (cond [(false? ?path) false]
                      [else (cons orig ?path)]))]))
;; (find-path/list nbrs dest g) produces a path from an element of nbrs to dest in g,
;; if one exists
;; find-path/list: (listof Node) Node Graph -> (anyof (ne-listof Node) false)
(define (find-path/list nbrs dest g)
  (cond [(empty? nbrs) false]
        [else (local [(define ?path (find-path (first nbrs) dest g))]
                (cond [(false? ?path)
                       (find-path/list (rest nbrs) dest g)]
                      [else ?path]))]))

;; 10
(define graph '((A (C D E))
                (B (E J))
                (C ())
                (D (F J))
                (E (K))
                (F (K H))
                (H ())
                (J (H))
                (K ())))

;; (a)
(check-expect (sinks graph) '(C H K))
               
(define (sinks G)
  (foldr (lambda (x rror) (cond [(empty? (second x))
                            (cons (first x) rror)]
                           [else rror]))
         empty
         G))

;; (b)
(check-expect (in-neighbours 'E graph) '(A B))
(check-expect (in-neighbours 'A graph) '())
(check-expect (in-neighbours 'D graph) '(A))

(define (in-neighbours n G)
  (foldr (lambda (x rror)
           (cond [(foldr (lambda (y r)
                           (or (symbol=? n y) r))
                         false
                         (second x)) (cons (first x) rror)]
                 [else rror]))
         empty
         G))

;; (c)
(check-expect (sources graph) '(A B))
(define (sources G)
  (foldr (lambda (x rror)
           (cond [(empty? (in-neighbours (first x) G))
                  (cons (first x) rror)]
                 [else rror]))
         empty
         G))

;; (d)
(check-expect (reverse-graph graph)
              (list
               (list 'A '())
               (list 'B '())
               (list 'C (list 'A))
               (list 'D (list 'A))
               (list 'E (list 'A 'B))
               (list 'F (list 'D))
               (list 'H (list 'F 'J))
               (list 'J (list 'B 'D))
               (list 'K (list 'E 'F))))
(define (reverse-graph G)
  (foldr (lambda (x rror) (cons (list (first x) (in-neighbours (first x) G))
                                rror))
         empty
         G))

;; 11
;; (a)
(define (quick-select lon n)
  (local [(define pivot (first lon))
          (define less (filter (lambda (x) (< x pivot)) (rest lon)))
          (define greater (filter (lambda (x) (>= x pivot)) (rest lon)))]
    (cond
      [(= (+ (length less) 1) n)
       pivot]
      [(>= (length less) n)
       (quick-select less n)]
      [else
       (quick-select greater (- n (add1 (length less))))])))

;; (b)
;; Yes, everytime you recurse on the rest of lon, so you get closer to base case

;; 12
;; (a)
(define (make-add1-gen stored)
  (lambda (x)
    (cond [(symbol=? x 'value) stored]
          [(symbol=? x 'next) (make-add1-gen (add1 stored))])))

(define g (make-add1-gen 0))
(g 'next) ; Produces a function (a generator)
(g 'value) ; Produces 0
((g 'next) 'value) ; Produces 1
(((g 'next) 'next) 'value) ; Produces 2        

;; (b)
(define (all-values gen-list) (map (lambda (f) (f 'value)) gen-list))
