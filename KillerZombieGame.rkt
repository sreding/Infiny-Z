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
(require "release-key.rkt")
 

; constants

;(GameState (GameState-player state) (GameState-Zombies state) (GameState-Projectiles state) (GameState-Score state)
;(define (render2 state)
;  (overlay (circle 20 "solid" "pink") BACKGROUND)
;  )
;(define (key-handler2 state key)
;  (cond [(string=? key "h") (big-bang Example
;            [to-draw render]
;            [on-key keyhandler]
;            [on-tick update (/ 1 60)]
;            [on-mouse mouse-handle])]
;        ))
;  
;
;(define (main2 _)
;  (big-bang 2
;            [to-draw render2]
;            [on-key key-handler2]))

(define (main _)
  (big-bang Example
            [to-draw render]
            [on-key keyhandler]
            [on-tick update]
            [on-mouse mouse-handle]))


