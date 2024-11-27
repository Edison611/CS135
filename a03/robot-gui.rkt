#lang racket/gui
(require racket/gui/base
         lang/prim)

(provide robot-gui)

(define-higher-order-primitive 
  robot-gui       ; exported name
  robot-gui/proc  ; internal name
  (_ robot-ctl state-x state-y state-dir))

(define orientation-x first)
(define orientation-y second)
(define orientation-dir third)

; These will be set with functions provided by the
; student via robot-gui
(define robot '(0 0 North))
(define robot-ctl 0)

; Constants related to the robot's grid
(define max-grid-x 10)
(define max-grid-y 10)

; Make a frame by instantiating the frame% class
(define frame (new frame% 
                   [label "Robots"]
                   [width 300]
                   [height 300]))

(define button-panel 
  (new horizontal-panel% 
       [parent frame]
       [alignment '(center center)]
       [stretchable-height #f]))


; Make a button in the frame
(define turn-left-button 
  (new button% [parent button-panel]
       [label "Turn Left"]
       ; Callback procedure for a button click:
       (callback (lambda (button event)
                   (set! robot (robot-ctl robot 'turn-left))  
                   (send canvas on-paint)         ; repaint
                   ))))


; Make a button in the frame
(define forward-button 
  (new button% [parent button-panel]
       [label "Forward"]
       ; Callback procedure for a button click:
       (callback (lambda (button event)
                   ; move the robot
                   (set! robot (robot-ctl robot 'forward))  
                   (send canvas on-paint)         ; repaint
                   ))))

; Make a button in the frame
(define turn-right-button 
  (new button% [parent button-panel]
       [label "Turn Right"]
       ; Callback procedure for a button click:
       (callback (lambda (button event)
                   (set! robot (robot-ctl robot 'turn-right))  
                   (send canvas on-paint)         ; repaint
                   ))))


; Make some pens and brushes
(define blue-brush (make-object brush% "BLUE" 'solid))


; Draw the robot scene.  It's a 10x10 grid with a border around it
; that just happens to be the same width/height as one of the grid cells
; Draw the robot on the grid.
; Remember that the robot's origin is lower left while the canvas'
; origin is upper left.
(define (draw-scene dc)
  (local [(define w (send canvas get-width))
          (define h (send canvas get-height))
          (define h-spacing (/ w (+ max-grid-x 2)))
          (define v-spacing (/ h (+ max-grid-y 2)))
          (define (vert-lines x n)
            (cond [(= x n) empty]
                  [else (send dc draw-line (* x h-spacing) 0 (* x h-spacing) h)
                        (vert-lines (add1 x) n)]))
          (define (hort-lines y n)
            (cond [(= y n) empty]
                  [else (send dc draw-line 0 (* y v-spacing) w (* y v-spacing) )
                        (hort-lines (add1 y) n)]))
          (define (draw-robot)
            (local [(define centre-x (* h-spacing (+ 1 (orientation-x robot))))
                    (define centre-y (- h (* v-spacing (+ 1 (orientation-y robot)))))
                    (define dir (orientation-dir robot))
                    (define diameter (/ (max h-spacing v-spacing) 1.25))
                    (define radius (/ diameter 2))
                    (define sensor-diameter radius)
                    (define sensor-radius (/ sensor-diameter 2))
                    (define sensor-x-offset (cond [(symbol=? dir 'North) 0]
                                                  [(symbol=? dir 'South) 0]
                                                  [(symbol=? dir 'East) radius]
                                                  [(symbol=? dir 'West) (- radius)]))
                    (define sensor-y-offset (cond [(symbol=? dir 'North) (- radius)]
                                                  [(symbol=? dir 'South) radius]
                                                  [(symbol=? dir 'East) 0]
                                                  [(symbol=? dir 'West) 0]))
                    ]
              
              (send dc set-brush blue-brush)
              
              ; draw the robot's "body"
              (send dc draw-ellipse  
                    (- centre-x radius) 
                    (- centre-y radius)
                    diameter
                    diameter)
              
              ; draw a "sensor"
              (send dc draw-ellipse
                    (- (+ centre-x sensor-x-offset) sensor-radius)
                    (- (+ centre-y sensor-y-offset) sensor-radius)
                    sensor-diameter
                    sensor-diameter)))
          ]
    (send dc clear)
    (vert-lines 1 (+ max-grid-x 2))
    (hort-lines 1 (+ max-grid-y 2))
    (draw-robot)
    ))


(define canvas (new canvas%
                    [parent frame]
                    [paint-callback (lambda (canvas dc)
                                      (draw-scene dc))]))

(define (robot-gui/proc 
         initial-orientation 
         controller
         x-selector
         y-selector
         dir-selector)
         
  ; save local version of student-defined robot-ctl
  (set! robot initial-orientation)
  (set! robot-ctl controller)
  (set! orientation-x x-selector)
  (set! orientation-y y-selector)
  (set! orientation-dir dir-selector)
  
  ; Show the frame
  (send frame show #t)
  )