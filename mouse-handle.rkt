#lang racket

; shoot with left mousebutton
; add projectiles

(require "datadefinitions.rkt")


; Projectiles Player Number Number -> Projectiles
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
                  (GameState-Score state)
                  (GameState-Menue state))]
        [(and (= (GameState-Menue state) 1) (string=? "button-down" event)) Level1]
        [else state]))

(provide (all-defined-out))