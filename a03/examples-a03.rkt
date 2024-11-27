;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname examples-a03) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Edison Ying (21119725)
;; CS 135 Fall 2024
;; Assignment 03, Examples
;; ***************************************************
;;

;; P2
;; (a)
(check-expect (point-mult (make-point 1 1) (make-point 1 1)) (make-point 0 2))
(check-expect (point-mult (make-point -2 0) (make-point 2 1)) (make-point -4 -2))
;; (b)
(check-expect (point-div (make-point 1 1) (make-point 1 1)) (make-point 1 0))
(check-expect (point-div (make-point -2 0) (make-point 2 1)) (make-point -4/5 2/5))

;; P3
;; (b)
(check-expect (robot-ctl (make-state 2 2 'North) 'forward) (make-state 2 3 'North))
(check-expect (robot-ctl (make-state 4 7 'East) 'forward) (make-state 5 7 'East))
(check-expect (robot-ctl (make-state 2 2 'North) 'turn-left) (make-state 2 2 'West))
(check-expect (robot-ctl (make-state 4 7 'East) 'turn-right) (make-state 4 7 'South))
(check-expect (robot-ctl (make-state 0 10 'North) 'forward) (make-state 0 10 'North))

;; P4
;; (b)
(check-expect (mod-weight (make-mod "Main" 10 (make-sub-mod "A" 3) (make-sub-mod "B" 7))) 20)
(check-expect (mod-weight (make-mod "A" 50 (make-sub-mod "B" 7) (make-sub-mod "C" 12))) 69)

;; (c)
(check-expect (predict-winner (make-robot "Atom"
                                          (make-mod "Head-A" 10
                                                    (make-sub-mod "Eye-A" 5)
                                                    (make-sub-mod "Eye-A" 5))
                                          (make-mod "Arms-A" 100
                                                    (make-sub-mod "Fist-A-Big" 50)
                                                    (make-sub-mod "Fist-A-Small" 2))
                                          (make-mod "Legs-A" 20
                                                    (make-sub-mod "Foot-A" 10)
                                                    (make-sub-mod "Foot-A" 10)))
                              (make-robot "Smasher"
                                          (make-mod "Head-S" 30
                                                    (make-sub-mod "Eye-S" 10)
                                                    (make-sub-mod "Eye-S" 10))
                                          (make-mod "Arms-S" 50
                                                    (make-sub-mod "Fist-S" 15)
                                                    (make-sub-mod "Fist-S" 15))
                                          (make-mod "Legs-S" 30
                                                    (make-sub-mod "Foot-S-Big" 8)
                                                    (make-sub-mod "Foot-S-Small" 5)))) "Atom")
;; (d)
(check-expect (combine (make-robot "Atom"
                                   (make-mod "Head-A" 10
                                             (make-sub-mod "Eye-A" 5)
                                             (make-sub-mod "Eye-A" 5))
                                   (make-mod "Arms-A" 100
                                             (make-sub-mod "Fist-A-Big" 50)
                                             (make-sub-mod "Fist-A-Small" 2))
                                   (make-mod "Legs-A" 20
                                             (make-sub-mod "Foot-A" 10)
                                             (make-sub-mod "Foot-A" 10)))
                       (make-robot "Smasher"
                                   (make-mod "Head-S" 30
                                             (make-sub-mod "Eye-S" 10)
                                             (make-sub-mod "Eye-S" 10))
                                   (make-mod "Arms-S" 50
                                             (make-sub-mod "Fist-S" 15)
                                             (make-sub-mod "Fist-S" 15))
                                   (make-mod "Legs-S" 30
                                             (make-sub-mod "Foot-S-Big" 8)
                                             (make-sub-mod "Foot-S-Small" 5))))
              (make-robot "Atom-Smasher"
                          (make-mod "Head-S" 30
                                    (make-sub-mod "Eye-S" 10)
                                    (make-sub-mod "Eye-S" 10))
                          (make-mod "Arms-A" 100
                                    (make-sub-mod "Fist-A-Big" 50)
                                    (make-sub-mod "Fist-S" 15))
                          (make-mod "Legs-S" 30
                                    (make-sub-mod "Foot-A" 10)
                                    (make-sub-mod "Foot-A" 10))))

(check-expect (combine 
               (make-robot "A"
                           (make-mod "Head-A" 10
                                     (make-sub-mod "Eye-A1" 15)
                                     (make-sub-mod "Eye-A2" 5))
                           (make-mod "Arms-A" 25
                                     (make-sub-mod "Fist-A1" 50)
                                     (make-sub-mod "Fist-A2" 2))
                           (make-mod "Legs-A" 4
                                     (make-sub-mod "Foot-A1" 5)
                                     (make-sub-mod "Foot-A2" 4)))

               (make-robot "B"
                           (make-mod "Head-B" 30
                                     (make-sub-mod "Eye-B1" 10)
                                     (make-sub-mod "Eye-B2" 10))
                           (make-mod "Arms-B" 40
                                     (make-sub-mod "Fist-B1" 30)
                                     (make-sub-mod "Fist-B2" 29))
                           (make-mod "Legs-B" 3
                                     (make-sub-mod "Foot-B1" 8)
                                     (make-sub-mod "Foot-B2" 3))))
              (make-robot "A-B"
                           (make-mod "Head-B" 30
                                     (make-sub-mod "Eye-A1" 15)
                                     (make-sub-mod "Eye-B1" 10))
                           (make-mod "Arms-B" 40
                                     (make-sub-mod "Fist-A1" 50)
                                     (make-sub-mod "Fist-B1" 30))
                           (make-mod "Legs-A" 4
                                     (make-sub-mod "Foot-B1" 8)
                                     (make-sub-mod "Foot-A1" 5))))