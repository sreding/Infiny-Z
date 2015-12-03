#lang racket

; Top Down 2-D Zombie Survival Shooter (birds eye view)


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




(define (main _)
  (big-bang Example
            [to-draw render]
            [on-mouse mouse-handle]))


