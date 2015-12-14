#lang racket

; Top Down 2-D Zombie Survival Shooter (birds eye view)


(require 2htdp/image)
(require 2htdp/universe)
(require "datadefinitions.rkt")
(require "render.rkt")
(require "mouse-handle.rkt")
(require "key-handle.rkt")
(require "update.rkt")
(require "release-key.rkt")
 


(define (main _)
  (big-bang InitState
            [to-draw render]
            [on-key keyhandler]
            [on-tick update]
            [on-release release]
            [on-mouse mouse-handle]
            ))


