#lang racket

; shoot with left mousebutton
; add projectiles

(require "datadefinitions.rkt")

(define (mouse-handle state x y key)
  (make-GameState (Player
                   (Player-img (GameState-player state))
                   (Player-health (GameState-player state)) 
                   (Player-position (GameState-player state))
                   (make-Weapon (Weapon-img (Player-Weapon (GameState-player state)))
                                x
                                y
                                (Weapon-projectilespeed (Player-Weapon (GameState-player state)))))
                  (GameState-Zombies state)
                  (GameState-Projectiles state)
                  (GameState-Score state)))

(provide (all-defined-out))