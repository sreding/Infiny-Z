#lang racket

; Top Down 2-D Zombie Survival Shooter (birds eye view)


(require 2htdp/image)
(require 2htdp/universe)
(require "programs/datadefinitions.rkt")
(require "programs/render.rkt")
(require "programs/mouse-handle.rkt")
(require "programs/key-handle.rkt")
(require "programs/update.rkt")
(require "programs/release-key.rkt")
 


(define (main _)
  (big-bang InitState
            [to-draw render]
            [on-key keyhandler]
            [on-tick update]
            [on-release release]
            [on-mouse mouse-handle]
            ))


