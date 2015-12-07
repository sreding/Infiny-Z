#lang racket

(define SPEED 20)



(require "datadefinitions.rkt")

; wasd to walk + diagonal
; evtl. space to jump
; Gamestate + key input -> Gamestate

(define (keyhandler state key)
  (cond [(string=? key "a")
         (make-GameState (make-Player (Player-img (GameState-player state))
                                      (Player-health (GameState-player state))
                                      (make-posn (- (posn-x (Player-position (GameState-player state))) SPEED)
                                                 (posn-y (Player-position (GameState-player state))))
                                      (Player-Weapon (GameState-player state))
                                       )
                         (GameState-Zombies state)
                         (GameState-Projectiles state)
                         (GameState-Score state))]
        
        [(string=? key "d")
         (make-GameState (make-Player (Player-img (GameState-player state))
                                      (Player-health (GameState-player state))
                                      (make-posn (+ (posn-x (Player-position (GameState-player state))) SPEED)
                                                 (posn-y (Player-position (GameState-player state))))
                                      (Player-Weapon (GameState-player state))
                                       )
                         (GameState-Zombies state)
                         (GameState-Projectiles state)
                         (GameState-Score state))]

        [(string=? key "w")
         (make-GameState (make-Player (Player-img (GameState-player state))
                                      (Player-health (GameState-player state))
                                      
                                        (make-posn (posn-x (Player-position (GameState-player state)))
                                                 (- (posn-y (Player-position (GameState-player state))) SPEED))
                                        (Player-Weapon (GameState-player state)))
                         (GameState-Zombies state)
                         (GameState-Projectiles state)
                         (GameState-Score state))]
        [(string=? key "s")
         (make-GameState (make-Player (Player-img (GameState-player state))
                                      (Player-health (GameState-player state))
                                      (make-posn (posn-x (Player-position (GameState-player state)))
                                                 (+ (posn-y (Player-position (GameState-player state))) SPEED))
                                      (Player-Weapon (GameState-player state)))
                         (GameState-Zombies state)
                         (GameState-Projectiles state)
                         (GameState-Score state))]
        [else state]))

(provide (all-defined-out))