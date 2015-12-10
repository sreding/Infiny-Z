#lang racket

;Player Health <= 0 function

; GameState -> Boolean
; Retruns true if Player-health < 0
(define (sop state)
  (< (Player-health (GameState-player state)) 0))

(require "datadefinitions.rkt")

(provide (all-defined-out))