#lang racket
(require "datadefinitions.rkt")


; Player key -> Player
; Stops the player from walking in a direction if the key bound to this direction is released
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
        
; GameState String -> GameState
; handles the button releases (on-release)
(define (release state key)
  (cond [(or (= (GameState-Menue state) 5)  (= (GameState-Menue state) 10))
  (make-GameState (release-update-player (GameState-player state) key)
                  (GameState-Zombies state)
                  (GameState-Projectiles state)
                  (GameState-PowerUps state)
                  (GameState-Score state)
                  (GameState-Menue state))]
        [(= (GameState-Menue state)10) state]
  [else state]))


(provide (all-defined-out))

