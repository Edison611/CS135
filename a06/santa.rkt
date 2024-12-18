;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname santa) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***************************************************
;; Edison Ying (21119725)
;; CS 135 Fall 2024
;; Assignment 06, Q3 Santa
;; ***************************************************
;;

;; A Name is a Str

;; A Desc is a Str

;; A NiceScore n is an Int
;; Requires: -100 <= n <= 100 and n not= 0

(define-struct action (niceness desc))
;; An Action is a (make-action NiceScore Desc)

;; An ActionList is a (listof (list Name (listof Action)))
;; Requires: The list is sorted alphabetically by child name.
;;           Each list of Actions is non-empty.
;; Note: the order of Actions for the same child is arbitrary.

;; An ActionUpdate is a (listof (list Name Action))
;; Requires: The list is sorted alphabetically by child name.
;; A name will only appear once in the list.

(define-struct wish (score gift))
;; A Wish is a (make-wish NiceScore Desc)
;; Requires: score is further restricted to be > 0

;; A WishList is a (listof Wish)
;; Requires: Wishes are sorted in non-decreasing order by score.

;; A ChildrenList is a (listof (list Name Wishlist))
;; Requires: The list is sorted alphabetically by child name.

;; A GiftList is a (listof (list Name (listof Desc)))
;; Requires: The GiftList is sorted alphabetically by child name.
;;           The gifts (listof Desc) are sorted in non-increasing
;;           order of score; i.e., gift with highest score is first.


;; Definitions used for testing
(define action1 (make-action 3 "Prepared assignment question"))
(define action2 (make-action -7 "Questions are too hard"))
(define actlst (list (list "Zaphod" (list action1 action2))))
(define action3 (make-action 42 "Told a good joke about recursion."))
(define newactlst (list (list "Zaphod" (list action3 action1
                                             action2))))
(define super-actlst  
  (list (list "Barry" (list (make-action 10 "Won the running event")
                            (make-action -1 "Stayed up too late on a schoolnight")
                            (make-action 6 "Helped some friends get to school on time")))
        (list "Bruce" (list (make-action -4 "Pretended to be a bat and scared children in the park")))
        (list "Clark" (list (make-action -5 "Skipped his chores on the farm")
                            (make-action 5 "Saved Earth from ...")))
        (list "Hal" (list (make-action 3 "Shared his toys with the other children")))
        (list "Harley" (list (make-action -10 "Joined a gang")))
        (list "Zatanna" (list (make-action 15 "Show friends magic tricks")
                            (make-action 3 "Studied for their exam")
                            (make-action -7 "Made a mess when magic went wrong")
                            (make-action -4 "Procrastinated on CS 135 assignment")))))
(define wish1 (make-wish 32 "Amigurumi Bee Plushie"))
(define wish2 (make-wish 99 "Wayne Gretzky Rookie Card"))
(define chldlst (list (list "Zaphod" (list wish1 wish2))))

(define super-chldlst
  (list (list "Barry" (list (make-wish 3 "New boots") (make-wish 4 "Red suit") (make-wish 20 "Fancy hat")))
        (list "Clark" (list (make-wish 1 "Red cape")))
        (list "Zatanna" (list (make-wish 3 "Tophat") (make-wish 5 "New wand")))))

;; ***************************************************
;; (a)
;; (extreme-actions name alst) produces:
;; • empty if the child’s name does not appear in the ActionList.
;; • a list of two strings where the first string is the description of the child’s action
;; with the lowest niceness score and the second string is the description of the child’s
;; action with the highest niceness score.
;; Examples:
(check-expect (extreme-actions "Zatanna" super-actlst)
              (list "Made a mess when magic went wrong" "Show friends magic tricks"))
(check-expect (extreme-actions "Clark" super-actlst)
              (list "Skipped his chores on the farm"
                    "Saved Earth from ..."))
(check-expect (extreme-actions "Edison" super-actlst) empty)
(check-expect
 (extreme-actions "Zaphod" actlst)
 (list "Questions are too hard" "Prepared assignment question"))
                    
;; extreme-actions: Str ActionList -> (Anyof empty (list Str Str))
(define (extreme-actions name alst)
  (cond [(empty? alst) empty]
        [(string=? name (first (first alst)))
         (low-high-action (second (first alst)) (make-action 101 "") (make-action -101 ""))]
        [else (extreme-actions name (rest alst))]))

;; (low-high-action loa lowest highest) produces a list of the lowest and highest scores with their action description
;; Examples:
(check-expect (low-high-action (list (make-action 10 "Won the running event")
                                     (make-action -1 "Stayed up too late on a schoolnight")
                                     (make-action 6 "Helped some friends get to school on time"))
                               (make-action 101 "") (make-action -101 ""))
              (list "Stayed up too late on a schoolnight" "Won the running event"))
;; low-high-action: ActionList Int Int -> (list Action Action)
;; Requires: ActionList is non-empty
(define (low-high-action loa lowest highest)
  (cond [(empty? loa) (list (action-desc lowest) (action-desc highest))]
        [(and (> (action-niceness (first loa)) (action-niceness highest))
              (< (action-niceness (first loa)) (action-niceness lowest)))
         (low-high-action (rest loa) (first loa) (first loa))]
        [(> (action-niceness (first loa)) (action-niceness highest))
         (low-high-action (rest loa) lowest (first loa))]
        [(< (action-niceness (first loa)) (action-niceness lowest))
         (low-high-action (rest loa) (first loa) highest)]
        [else (low-high-action (rest loa) lowest highest)]))


;; ***************************************************
;; (b)
;; (merge-actions alst aulist) produces a new ActionList with any new children and actions included
;; Examples:
(check-expect (merge-actions actlst (list (list "Zaphod" action3)))
              newactlst)
(check-expect (merge-actions (list (list "Zaphod" (list action3))) (list (list "A" action1)))
              (list (list "A" (list action1)) (list "Zaphod" (list action3))))
(check-expect (merge-actions super-actlst (list
                                           (list "Alpha" action1)
                                           (list "Edison" action1)
                                           (list "Hal" action2)
                                           (list "Zz" action2)))
              (list (list "Alpha" (list action1))
                    (list "Barry" (list (make-action 10 "Won the running event")
                                        (make-action -1 "Stayed up too late on a schoolnight")
                                        (make-action 6 "Helped some friends get to school on time")))
                    (list "Bruce" (list (make-action -4 "Pretended to be a bat and scared children in the park")))
                    (list "Clark" (list (make-action -5 "Skipped his chores on the farm")
                                        (make-action 5 "Saved Earth from ...")))
                    (list "Edison" (list action1))
                    (list "Hal" (list action2 (make-action 3 "Shared his toys with the other children")))
                    (list "Harley" (list (make-action -10 "Joined a gang")))
                    (list "Zatanna" (list (make-action 15 "Show friends magic tricks")
                                          (make-action 3 "Studied for their exam")
                                          (make-action -7 "Made a mess when magic went wrong")
                                          (make-action -4 "Procrastinated on CS 135 assignment")))
                    (list "Zz" (list action2))))

(check-expect (merge-actions super-actlst (list
                                           (list "Alpha" action1)
                                           (list "Barry" action1)
                                           (list "Edison" action1)
                                           (list "Hal" action2)
                                           (list "Zatanna" action1)
                                           (list "Zz" action2)))
              (list (list "Alpha" (list action1))
                    (list "Barry" (list action1
                                        (make-action 10 "Won the running event")
                                        (make-action -1 "Stayed up too late on a schoolnight")
                                        (make-action 6 "Helped some friends get to school on time")))
                    (list "Bruce" (list (make-action -4 "Pretended to be a bat and scared children in the park")))
                    (list "Clark" (list (make-action -5 "Skipped his chores on the farm")
                                        (make-action 5 "Saved Earth from ...")))
                    (list "Edison" (list action1))
                    (list "Hal" (list action2 (make-action 3 "Shared his toys with the other children")))
                    (list "Harley" (list (make-action -10 "Joined a gang")))
                    (list "Zatanna" (list action1
                                          (make-action 15 "Show friends magic tricks")
                                          (make-action 3 "Studied for their exam")
                                          (make-action -7 "Made a mess when magic went wrong")
                                          (make-action -4 "Procrastinated on CS 135 assignment")))
                    (list "Zz" (list action2))))
;; merge-actions: ActionList ActionUpdate -> ActionList
(define (merge-actions alst aulst)
  (cond [(and (empty? aulst) (empty? alst)) empty]
        [(empty? aulst) alst] 
        [(empty? alst) (cons (list (first (first aulst)) (list (second (first aulst))))
                             (merge-actions alst (rest aulst)))]
        [(string=? (first (first alst)) (first (first aulst)))
         (cons (list (first (first alst)) (cons (second (first aulst)) (second (first alst))))
               (merge-actions (rest alst) (rest aulst)))]
        [(string>? (first (first alst)) (first (first aulst)))
         (cons (list (first (first aulst)) (list (second (first aulst))))
               (merge-actions alst (rest aulst)))]
        [else (cons (first alst) (merge-actions (rest alst) aulst))]))

;; ***************************************************
;; (c)
;; (choose-gifts N low)  determines the gifts a child will receive based
;; on their overall niceness score, and the niceness scores Santa assigned to the gifts
;; Examples
(check-expect (choose-gifts -1 empty) (list "coal"))
(check-expect (choose-gifts -1 (list (make-wish 1 "Red cape"))) (list "coal"))
(check-expect (choose-gifts 0 empty) (list "socks"))
(check-expect (choose-gifts 0 (list (make-wish 1 "Red cape"))) (list "socks"))
(check-expect (choose-gifts 42 empty) (list "socks"))
(check-expect (choose-gifts 2 (list (make-wish 3 "Tophat") (make-wish 5 "New wand"))) (list "socks"))
(check-expect (choose-gifts 4 (list (make-wish 3 "Tophat") (make-wish 5 "New wand"))) (list "Tophat"))
(check-expect (choose-gifts 10 (list (make-wish 3 "Tophat") (make-wish 5 "New wand"))) (list "New wand" "Tophat"))
;; choose-gifts: Int (listof Wish) -> (listof Desc)
(define (choose-gifts N low)
  (cond [(< N 0) (list "coal")]
        [(= N 0) (list "socks")]
        [(empty? low) (list "socks")]
        [(< N (wish-score (first low))) (list "socks")]
        [else (wish->gift (get-gifts<N N low empty))]))

;; (wish->gift wlst) converts a wishlist to just the gifts
;; Examples:
(check-expect (wish->gift (list (make-wish 1 "Tie"))) (list "Tie"))
;; wish->gift: WishList -> (listof Str)
(define (wish->gift wlst)
  (cond [(empty? wlst) empty]
        [else (cons (wish-gift (first wlst))
                    (wish->gift (rest wlst)))]))

;; (get-gifts<N N low) produces a wishlist of all wishes with niceness scores < N in sorted order (descending)
;; Examples:
(check-expect (get-gifts<N 10 (list (make-wish 3 "Tophat") (make-wish 5 "New wand")) empty)
              (list (make-wish 5 "New wand") (make-wish 3 "Tophat")))
;; get-gifts<N: Nat WishList -> WishList
(define (get-gifts<N N low acc)
  (cond [(empty? low) acc]
        [(<= (wish-score (first low)) N)
         (get-gifts<N N (rest low) (cons (first low) acc))]
        [else (get-gifts<N N (rest low) acc)]))

;; NOT IN USE ANYMORE:

;;; (insert wish sorted-wlst) inserts a Wish to an already sorted WishList
;;; Examples:
;(check-expect (insert (make-wish 5 "New wand") (list (make-wish 3 "Tophat")))
;              (list (make-wish 5 "New wand") (make-wish 3 "Tophat")))
;;; insert: Wish WishList -> WishList
;;; Requires: The WishList to already be sorted
;(define (insert wish sorted-wlst)
;  (cond [(empty? sorted-wlst) (cons wish empty)]
;        [(> (wish-score wish) (wish-score (first sorted-wlst)))
;         (cons wish sorted-wlst)]
;        [else (cons (first sorted-wlst)
;                    (insert wish (rest sorted-wlst)))]))

(define not-found-nscore 0)
;; (assign-gifts alst clst) assigns the gift based off the choose-gifts function from above and their niceness scores
;; Examples:
(check-expect (assign-gifts actlst chldlst)
              (list (list "Zaphod" (list "coal"))))
(check-expect (assign-gifts newactlst chldlst)
              (list (list "Zaphod" (list "Amigurumi Bee Plushie"))))
(check-expect (assign-gifts
               (list (list "Clark" (list (make-action -5 "Skipped his chores on the farm")
                                         (make-action 4 "Saved Earth from ...")))
                     (list "Zatanna" (list (make-action 15 "Show friends magic tricks")
                                           (make-action 3 "Studied for their exam")
                                           (make-action -7 "Made a mess when magic went wrong")
                                           (make-action -4 "Procrastinated on CS 135 assignment"))))
               super-chldlst)
              (list (list "Barry" (list "socks"))
                    (list "Clark" (list "coal"))
                    (list "Zatanna" (list "New wand" "Tophat"))))
              
              
;; assign-gifts: ActionList ChildrenList -> GiftList
(define (assign-gifts alst clst)
  (cond [(empty? clst) empty]
        [(string=? (first (first alst)) (first (first clst)))
         (cons (list (first (first alst))
                     (choose-gifts (niceness-score (second (first alst)))
                                   (second (first clst))))
               (assign-gifts (rest alst) (rest clst)))]
        [(string>? (first (first alst)) (first (first clst)))
         (cons (list (first (first clst))
                     (choose-gifts not-found-nscore
                                   (second (first clst))))
               (assign-gifts alst (rest clst)))]))
        
;; (niceness-score wlst) produces the niceness score of a WishList
;; Examples:
(check-expect (niceness-score (list (make-action 3 "Test") (make-action 5 "Test 2"))) 8)
;; niceness-score: WishList -> Int
(define (niceness-score wlst)
  (cond [(empty? wlst) 0]
        [else (+ (action-niceness (first wlst))
                 (niceness-score (rest wlst)))]))
  







