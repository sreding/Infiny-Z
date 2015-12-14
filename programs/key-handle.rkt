#lang racket

(require "datadefinitions.rkt")


; GameState String -> GameState
; handles the player movement changes and the pause functionality
(define (keyhandler-game state key)
  (local [(define x (posn-x (Player-position (GameState-player state))))
          (define y (posn-y (Player-position (GameState-player state))))]
    
    (cond 
      [(string=? key "d")
       (make-GameState (make-Player (Player-img (GameState-player state))
                                    (Player-health (GameState-player state))
                                    (make-posn x
                                               y)
                                    (make-posn 1
                                               (posn-y (Player-direction (GameState-player state))))
                                    (Player-Weapon (GameState-player state))
                                    )
                       (GameState-Zombies state)
                       (GameState-Projectiles state)
                       (GameState-PowerUps state)
                       (GameState-Score state)
                       (GameState-Menue state))]
      
      [(string=? key "a")
       (make-GameState (make-Player (Player-img (GameState-player state))
                                    (Player-health (GameState-player state))
                                    (make-posn x
                                               y)
                                    (make-posn -1
                                               (posn-y (Player-direction (GameState-player state))))
                                    (Player-Weapon (GameState-player state))
                                    )
                       (GameState-Zombies state)
                       (GameState-Projectiles state)
                       (GameState-PowerUps state)
                       (GameState-Score state)
                       (GameState-Menue state))]
      
      
      
      [(string=? key "w")
       (make-GameState (make-Player (Player-img (GameState-player state))
                                    (Player-health (GameState-player state))
                                    
                                    (make-posn x
                                               y)
                                    (make-posn (posn-x (Player-direction (GameState-player state)))
                                               -1)
                                    (Player-Weapon (GameState-player state)))
                       (GameState-Zombies state)
                       (GameState-Projectiles state)
                       (GameState-PowerUps state)
                       (GameState-Score state)
                       (GameState-Menue state))]
      
      
      [(string=? key "s")
       (make-GameState (make-Player (Player-img (GameState-player state))
                                    (Player-health (GameState-player state))
                                    (make-posn x
                                               y)
                                    (make-posn (posn-x (Player-direction (GameState-player state)))
                                               1)
                                    (Player-Weapon (GameState-player state)))
                       (GameState-Zombies state)
                       (GameState-Projectiles state)
                       (GameState-PowerUps state)
                       (GameState-Score state)
                       (GameState-Menue state))]
      [(string=? key "p")
       (make-GameState (GameState-player state)
                       (GameState-Zombies state)
                       (GameState-Projectiles state)
                       (GameState-PowerUps state)
                       (GameState-Score state)
                       (if (= (GameState-Menue state) 10) 5 10))]
      [else state])))

; GameState String -> GameState
; handles the key-input (on-key)
(define (keyhandler state key)
  (cond [(or  (= (GameState-Menue state) 10)(= (GameState-Menue state) 5))
         (keyhandler-game state key)]
        [else state]))


(provide (all-defined-out))