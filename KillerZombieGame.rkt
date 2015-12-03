#lang racket


(require 2htdp/image)
(require 2htdp/universe)
(require "datadefinitions.rkt")
(require "render.rkt")
(require "mouse-handle.rkt")
(require "key-handle.rkt")
(require "stop-conditions.rkt")
(require "update.rkt")


; constants

;(GameState (GameState-player state) (GameState-Zombies state) (GameState-Projectiles state) (GameState-Score state)

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


(define (main _)
  (big-bang Example
            [to-draw render]
            [on-mouse mouse-handle]))