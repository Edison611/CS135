;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname examples-a06) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***************************************************
;; Edison Ying (21119725)
;; CS 135 Fall 2024
;; Assignment 06, Examples
;; ***************************************************
;;

;; Q1
;; (a)
(check-expect (my-list-ref (list 1 2 3 4) 0) 1)
(check-expect (my-list-ref (list 5 4 3) 2) 3)
(check-expect (my-list-ref (list 2) 20) false)
(check-expect (my-list-ref (list 3 6.2 9 12) 3) 12)

;; (b)
(check-expect (zip (list 1 2 3 4) (list "a" "b" "c" "d"))
              (list (list 1 "a") (list 2 "b") (list 3 "c") (list 4 "d")))
(check-expect (zip (list "a" "b" "c" ) (list 1 2 3))
              (list (list "a" 1) (list "b" 2) (list "c" 3)))
(check-expect (zip empty empty) empty)

;; (c)
(check-expect (list-xor (list 1 3 5) (list 2 3 4)) (list 1 2 4 5))
(check-expect (list-xor (list 1 3 5) (list 1 3 5)) (list))
(check-expect (list-xor (list 1 3 5 7) (list 2 4 6)) (list 1 2 3 4 5 6 7))
(check-expect (list-xor (list 1 3 5) (list 2 4 6 8)) (list 1 2 3 4 5 6 8))

;; Q2
(define M (list (list 1 2 3 4) (list 5 6 7 8)))
(define M1 (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
(define M2 (list (list 1 0) (list 0 1)))
(define M3 (list (list 2 1) (list 1 3)))

;; (b)
(check-expect (matrix-item M 1 2) 7)
(check-expect (matrix-item M1 1 1) 5)
(check-expect (matrix-item M2 0 0) 1)

;; (c)
(check-expect (matrix-col M 2) (list 3 7))
(check-expect (matrix-col M1 0) (list 1 4 7))

;; (d)
(check-expect (matrix-transpose M) (list (list 1 5) (list 2 6) (list 3 7) (list 4 8)))
(check-expect (matrix-transpose M1) (list (list 1 4 7) (list 2 5 8) (list 3 6 9)))
(check-expect (matrix-transpose empty) empty)

;; (e)
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

;; Q3
;; Variables
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

;; (a)
(check-expect (extreme-actions "Zatanna" super-actlst)
              (list "Made a mess when magic went wrong" "Show friends magic tricks"))
(check-expect (extreme-actions "Clark" super-actlst)
              (list "Skipped his chores on the farm"
                    "Saved Earth from ..."))
(check-expect (extreme-actions "Edison" super-actlst) empty)
(check-expect
 (extreme-actions "Zaphod" actlst)
 (list "Questions are too hard" "Prepared assignment question"))

;; (b)
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


;; (c)
(check-expect (choose-gifts -1 empty) (list "coal"))
(check-expect (choose-gifts -1 (list (make-wish 1 "Red cape"))) (list "coal"))
(check-expect (choose-gifts 0 empty) (list "socks"))
(check-expect (choose-gifts 0 (list (make-wish 1 "Red cape"))) (list "socks"))
(check-expect (choose-gifts 42 empty) (list "socks"))
(check-expect (choose-gifts 2 (list (make-wish 3 "Tophat") (make-wish 5 "New wand"))) (list "socks"))
(check-expect (choose-gifts 4 (list (make-wish 3 "Tophat") (make-wish 5 "New wand"))) (list "Tophat"))
(check-expect (choose-gifts 10 (list (make-wish 3 "Tophat") (make-wish 5 "New wand"))) (list "New wand" "Tophat"))

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