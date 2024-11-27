;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname rfl) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Edison Ying (21119725)
;; CS 135 Fall 2024
;; Assignment 03, Q4 RFL
;; ***************************************************
;;

;; (a)
(define-struct sub-mod (name weight))
;; A Sub-Mod is a (make-sub-mod Str Num)
;; Requires: weight > 0

(define-struct mod (name frame-wt primary secondary))
;; A Sub-Mod is a (make-mod Num sub-mod sub-mod)
;; Requires: frame-wt > 0

(define-struct robot (name head arms legs))
;; A Sub-Mod is a (make-robot Str Mod Mod Mod)

;; Sample Robots
(define robot-atom
  (make-robot "Atom"
              (make-mod "Head-A" 10
                        (make-sub-mod "Eye-A" 5)
                        (make-sub-mod "Eye-A" 5))
              (make-mod "Arms-A" 100
                        (make-sub-mod "Fist-A-Big" 50)
                        (make-sub-mod "Fist-A-Small" 2))
              (make-mod "Legs-A" 20
                        (make-sub-mod "Foot-A" 10)
                        (make-sub-mod "Foot-A" 10))))
(define robot-smasher
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
(define robot-atom-smasher
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

;; ***************************************************
;; (b)
;; (mod-weight module) produces the total weight of a module

;; Examples:
(check-expect (mod-weight (make-mod "Main" 10 (make-sub-mod "A" 3) (make-sub-mod "B" 7))) 20)

;; mod-weight: Mod -> Num
(define (mod-weight module)
  (+ (mod-frame-wt module)
     (sub-mod-weight (mod-primary module))
     (sub-mod-weight (mod-secondary module))))

;; Test Cases
(check-expect (mod-weight (make-mod "A" 50 (make-sub-mod "B" 7) (make-sub-mod "C" 12))) 69)

;; ***************************************************
;; (c)
;; (predict-winner r1 r2) gives the name of the robot with the greater total weight

;; Examples
(check-expect (predict-winner robot-atom robot-smasher) "Atom")

;; predict-winner: Robot Robot -> Str
(define (predict-winner r1 r2)
  (cond [(>= (weight-robot r1) (weight-robot r2)) (robot-name r1)]
        [else (robot-name r2)]))

;; (weight-robot r) gives the weight of an entire robot

;; Examples
(check-expect (weight-robot robot-atom) 212)
(check-expect (weight-robot robot-smasher) 173)

;; weight-robot: Robot -> Num
(define (weight-robot r)
  (+ (mod-weight (robot-head r))
     (mod-weight (robot-arms r))
     (mod-weight (robot-legs r))))

;; ***************************************************
;; (d)
;; (combine r1 r2) combines two robots together with whichever
;;                 module/sub-modules has the greater weight

;; Examples
(check-expect (combine robot-atom robot-smasher) robot-atom-smasher)

;; combine: Robot Robot -> Robot
(define (combine r1 r2)
  (make-robot (string-append (robot-name r1) "-" (robot-name r2))
              (bigger-mod (robot-head r1) (robot-head r2))
              (bigger-mod (robot-arms r1) (robot-arms r2))
              (bigger-mod (robot-legs r1) (robot-legs r2))
   ))
;; Test Cases
(check-expect (combine 
               (make-robot "A"
                           (make-mod "Head-A" 10
                                     (make-sub-mod "Eye-A1" 15)
                                     (make-sub-mod "Eye-A2" 5))
                           (make-mod "Arms-A" 25
                                     (make-sub-mod "Fist-A1" 50)
                                     (make-sub-mod "Fist-A2" 2))
                           (make-mod "Legs-A" 4
                                     (make-sub-mod "Foot-A1" 4)
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
                                     (make-sub-mod "Foot-B2" 5))))
              (make-robot "A-B"
                           (make-mod "Head-B" 30
                                     (make-sub-mod "Eye-A1" 15)
                                     (make-sub-mod "Eye-B1" 10))
                           (make-mod "Arms-B" 40
                                     (make-sub-mod "Fist-A1" 50)
                                     (make-sub-mod "Fist-B1" 30))
                           (make-mod "Legs-A" 4
                                     (make-sub-mod "Foot-B1" 8)
                                     (make-sub-mod "Foot-B2" 5))))

;; (bigger-mod m1 m2) returns the module with the greater frame-wt
;;                    and the sub-modules that are greater

;; Examples
(check-expect (bigger-mod
               (make-mod "Head-S" 30
                         (make-sub-mod "Eye-S" 10)
                         (make-sub-mod "Eye-S" 10))
               (make-mod "Head-A" 10
                          (make-sub-mod "Eye-A" 5)
                          (make-sub-mod "Eye-A" 5)))
              (make-mod "Head-S" 30
                        (make-sub-mod "Eye-S" 10)
                        (make-sub-mod "Eye-S" 10)))
               
;; combine: Mod Mod -> Mod
(define (bigger-mod m1 m2)
  (cond [(> (mod-frame-wt m1) (mod-frame-wt m2))
         (make-mod (mod-name m1)
                   (mod-frame-wt m1)
                   ;; returns the two heaviest sub-modules
                   (bigger-sub-mod (mod-primary m1)
                                   (mod-primary m2))
                   (bigger-sub-mod
                     (smaller-sub-mod (mod-primary m1)
                                      (mod-primary m2))
                     (bigger-sub-mod (mod-secondary m1)
                                     (mod-secondary m2))))]
        [else
          (make-mod (mod-name m2)
                    (mod-frame-wt m2)
                    ;; returns the two heaviest sub-modules
                    (bigger-sub-mod (mod-primary m1)
                                    (mod-primary m2))
                    (bigger-sub-mod
                     (smaller-sub-mod (mod-primary m1)
                                      (mod-primary m2))
                     (bigger-sub-mod (mod-secondary m1)
                                     (mod-secondary m2))))]))

;; (bigger-sub-mod sm1 sm2) returns the sub-module with the greater weight

;; Examples
(check-expect (bigger-sub-mod (make-sub-mod "Fist-A-Big" 50)
                              (make-sub-mod "Fist-S" 15))
              (make-sub-mod "Fist-A-Big" 50))

;; bigger-sub-mod Sub-Mod Sub-Mod -> Sub-Mod
(define (bigger-sub-mod sm1 sm2)
  (cond [(>= (sub-mod-weight sm1) (sub-mod-weight sm2))
         sm1]
        [else sm2]))

;; (smaller-sub-mod sm1 sm2) returns the sub-module with the smaller weight

;; Examples
(check-expect (smaller-sub-mod (make-sub-mod "Fist-A-Big" 50)
                              (make-sub-mod "Fist-S" 15))
              (make-sub-mod "Fist-S" 15))

;; smaller-sub-mod Sub-Mod Sub-Mod -> Sub-Mod
(define (smaller-sub-mod sm1 sm2)
  (cond [(> (sub-mod-weight sm1) (sub-mod-weight sm2)) sm2]
        [else sm1]))
