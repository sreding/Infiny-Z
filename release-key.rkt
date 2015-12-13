#lang racket
(require "datadefinitions.rkt")


; Player key -> Player
; 
(define (release-update-player Player key)
  (cond [(string=? key "a")  (make-Player (Player-img Player)
                                          (Player-health Player)
                                          (Player-position Player)
                                          (make-posn 0
                                                     (posn-y (Player-direction Player)))
                                          (Player-Weapon Player))]
        [(string=? key "d") (make-Player (Player-img Player)
                                          (Player-health Player)
                                          (Player-position Player)
                                          (make-posn 0
                                                     (posn-y (Player-direction Player)))
                                          (Player-Weapon Player))]
        [(string=? key "w")  (make-Player (Player-img Player)
                                                                 (Player-health Player)
                                                                 (Player-position Player)
                                                                 (make-posn (posn-x (Player-direction Player))
                                                                            0)
                                                                 (Player-Weapon Player))]
        [(string=? key "s") (make-Player (Player-img Player)
                                                                 (Player-health Player)
                                                                 (Player-position Player)
                                                                 (make-posn (posn-x (Player-direction Player))
                                                                            0)
                                                                 (Player-Weapon Player))]
        [else Player]))
        
                                          
(define (release state key)
  (cond [(= (GameState-Menue state) 5)
  (make-GameState (release-update-player (GameState-player state) key)
                  (GameState-Zombies state)
                  (GameState-Projectiles state)
                  (GameState-Score state)
                  (GameState-Menue state))]
  [else state]))


(provide (all-defined-out))

