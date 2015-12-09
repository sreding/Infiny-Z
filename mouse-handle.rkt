#lang racket

; shoot with left mousebutton
; add projectiles

(require "datadefinitions.rkt")


; Projectiles Player Number Number -> Projectiles
(define (add-projectile Projectiles Player x y)
                  (cons (make-Projectile 1
                                         (make-posn (posn-x (Player-position Player))
                                                    (posn-y (Player-position Player)))
                                         (normalise (make-posn (- x (posn-x (Player-position Player)))
                                                               (- y (posn-y (Player-position Player)))))
                                         10)
                        Projectiles))
                        


(define (mouse-handle state x y event)
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
                  (GameState-Score state)))

(provide (all-defined-out))