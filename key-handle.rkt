#lang racket

(define SPEED 10)



(require "datadefinitions.rkt")

; wasd to walk + diagonal
; evtl. space to jump
; Gamestate + key input -> Gamestate

(define (keyhandler state key)
  (cond [(string=? key "a")
         (make-GameState (make-Player (Player-img (GameState-player state))
                                      (Player-health (GameState-player state))
                                      (Player-position (GameState-player
                                       (make-posn (- (posn-x GameState) SPEED) (posn-y state))))
                                      (Player-Weapon (GameState-player state))
                                       )
                         (GameState-Zombies state)
                         (GameState-Projectiles state)
                         (GameState-Score state))]
        
        [(string=? key "d")
         (make-GameState (make-Player (Player-img (GameState-player state))
                                      (Player-health (GameState-player state))
                                      (Player-position (GameState-player
                                       (make-posn (+ (posn-x GameState) SPEED) (posn-y state))))
                                      (Player-Weapon (GameState-player state))
                                       )
                         (GameState-Zombies state)
                         (GameState-Projectiles state)
                         (GameState-Score state))]

        [(string=? key "w")
         (make-GameState (make-Player (Player-img (GameState-player state))
                                      (Player-health (GameState-player state))
                                      (Player-position (GameState-player
                                       (make-posn (posn-x state) (- (posn-y state) SPEED))))
                                      (Player-Weapon (GameState-player state))
                                       )
                         (GameState-Zombies state)
                         (GameState-Projectiles state)
                         (GameState-Score state))]
        [(string=? key "s")
         (make-GameState (make-Player (Player-img (GameState-player state))
                                      (Player-health (GameState-player state))
                                      (Player-position (GameState-player
                                       (make-posn (posn-x state) (+ (posn-y GameState) SPEED))))
                                      (Player-Weapon (GameState-player state))
                                       )
                         (GameState-Zombies state)
                         (GameState-Projectiles state)
                         (GameState-Score state))]
        [else state]))

(provide (all-defined-out))