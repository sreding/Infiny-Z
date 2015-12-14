#lang racket

; shoot with left mousebutton
; add projectiles

(require "datadefinitions.rkt")


; Projectiles Player Number Number -> Projectiles
; Adds a projectile in the direction of the mouse courser
(define (add-projectile Projectiles Player x y)
  (local [(define direction (normalise (make-posn (- x (posn-x (Player-position Player)))
                                                  (- y (posn-y (Player-position Player))))))
          (define dx (posn-x direction))
          (define dy (posn-y direction))]
                  (cons (make-Projectile 1
                                         (make-posn (+ (* 20 dx) (posn-x (Player-position Player)))
                                                    (+ (* 20 dy) (posn-y (Player-position Player))))
                                         direction
                                         10)
                        Projectiles)))
                        

; GameState -> GameState
; handels all the mouse events (adding projectiles and navigating the menues)
(define (mouse-handle state x y event)
  (cond [(= (GameState-Menue state) 5)
  (make-GameState (make-Player
                   (Player-img (GameState-player state))
                   (Player-health (GameState-player state)) 
                   (Player-position (GameState-player state))
                   (Player-direction (GameState-player state))
                   (make-Weapon (Weapon-img (Player-Weapon (GameState-player state)))
                                x
                                y
                                (Weapon-projectilespeed (Player-Weapon (GameState-player state)))))
                  (GameState-Zombies state)
                  (if (string=? "button-down" event) (add-projectile (GameState-Projectiles state) (GameState-player state) x y) (GameState-Projectiles state))
                  (GameState-PowerUps state)
                  (GameState-Score state)
                  (GameState-Menue state))]
        [(and (= (GameState-Menue state) 1) (and (< 455 x 830) (< 350 y 495)) (string=? "button-down" event)) Level1]
        [(and (= (GameState-Menue state) 1) (and (< 510 x 770) (< 545 y 650)) (string=? "button-down" event)) HowTo-state]
        [(and (= (GameState-Menue state) 2) (and (< 60 x 285) (< 85 y 170)) (string=? "button-down" event)) InitState]
        [(and (= (GameState-Menue state) 2) (not (and (< 60 x 285) (< 85 y 170))) (string=? "button-down" event)) HowTo2-state]
        [(and (= (GameState-Menue state) 3) (and (< 60 x 285) (< 85 y 170)) (string=? "button-down" event)) InitState]
        [(and (= (GameState-Menue state) 4) (and (< 460 x 815) (< 450 y 560)) (string=? "button-down" event)) Level1]
        [(and (= (GameState-Menue state) 4) (and (< 500 x 775) (< 680 y 770)) (string=? "button-down" event)) InitState]
        [(= (GameState-Menue state) 10) state]
        [else state]))

(provide (all-defined-out))