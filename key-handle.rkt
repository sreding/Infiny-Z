#lang racket





(require "datadefinitions.rkt")

; wasd to walk + diagonal
; evtl. space to jump
; Gamestate + key input -> Gamestate


(define (keyhandler state key)
  (local [(define x (posn-x (Player-position (GameState-player state))))
          (define y (posn-y (Player-position (GameState-player state))))]
  
  (cond 
        [(and (string=? key "d") (not (obstacle-hit (+ x (* 3 SPEED)) y 1))  )
         (make-GameState (make-Player (Player-img (GameState-player state))
                                      (Player-health (GameState-player state))
                                      (make-posn (+ x SPEED)
                                                 y)
                                      (make-posn 1
                                                 (posn-y (Player-direction (GameState-player state))))
                                      (Player-Weapon (GameState-player state))
                                       )
                         (GameState-Zombies state)
                         (GameState-Projectiles state)
                         (GameState-Score state))]
        
        [(and (string=? key "d") (obstacle-hit (+ x (* 3 SPEED)) y 1))
         (make-GameState (make-Player (Player-img (GameState-player state))
                                      (Player-health (GameState-player state))
                                      (Player-position (GameState-player state))
                                      (make-posn 0
                                                 (posn-y (Player-direction (GameState-player state))))
                                      (Player-Weapon (GameState-player state))
                                       )
                         (GameState-Zombies state)
                         (GameState-Projectiles state)
                         (GameState-Score state))]
         
        [(and (string=? key "a") (not (obstacle-hit (- x (* 3 SPEED)) y 1)))
         (make-GameState (make-Player (Player-img (GameState-player state))
                                      (Player-health (GameState-player state))
                                      (make-posn (- x SPEED)
                                                 y)
                                      (make-posn -1
                                                 (posn-y (Player-direction (GameState-player state))))
                                      (Player-Weapon (GameState-player state))
                                       )
                         (GameState-Zombies state)
                         (GameState-Projectiles state)
                         (GameState-Score state))]
         [(and (string=? key "a") (obstacle-hit (- x (* 3 SPEED)) y 1))
         (make-GameState (make-Player (Player-img (GameState-player state))
                                      (Player-health (GameState-player state))
                                      (Player-position (GameState-player state))
                                      (make-posn 0
                                                 (posn-y (Player-direction (GameState-player state))))
                                      (Player-Weapon (GameState-player state))
                                       )
                         (GameState-Zombies state)
                         (GameState-Projectiles state)
                         (GameState-Score state))]
        

        [(and (string=? key "w") (not (obstacle-hit x (- y (* 3 SPEED)) 1)))
         (make-GameState (make-Player (Player-img (GameState-player state))
                                      (Player-health (GameState-player state))
                                      
                                        (make-posn x
                                                 (- y SPEED))
                                        (make-posn (posn-x (Player-direction (GameState-player state)))
                                                   -1)
                                        (Player-Weapon (GameState-player state)))
                         (GameState-Zombies state)
                         (GameState-Projectiles state)
                         (GameState-Score state))]
        [(and (string=? key "w") (obstacle-hit x (- y (* 3 SPEED)) 1))
         (make-GameState (make-Player (Player-img (GameState-player state))
                                      (Player-health (GameState-player state))
                                        (Player-position (GameState-player state))
                                        (make-posn (posn-x (Player-direction (GameState-player state)))
                                                   0)
                                        (Player-Weapon (GameState-player state)))
                         (GameState-Zombies state)
                         (GameState-Projectiles state)
                         (GameState-Score state))]
        
        [(and (string=? key "s") (not (obstacle-hit x (+ y (* 3 SPEED)) 1)))
         (make-GameState (make-Player (Player-img (GameState-player state))
                                      (Player-health (GameState-player state))
                                      (make-posn x
                                                 (+ y SPEED))
                                      (make-posn (posn-x (Player-direction (GameState-player state)))
                                                 1)
                                      (Player-Weapon (GameState-player state)))
                         (GameState-Zombies state)
                         (GameState-Projectiles state)
                         (GameState-Score state))]
         [(and (string=? key "s") (obstacle-hit x (+ y (* 3 SPEED)) 1))
         (make-GameState (make-Player (Player-img (GameState-player state))
                                      (Player-health (GameState-player state))
                                      (Player-position (GameState-player state))
                                      (make-posn (posn-x (Player-direction (GameState-player state)))
                                                 0)
                                      (Player-Weapon (GameState-player state)))
                         (GameState-Zombies state)
                         (GameState-Projectiles state)
                         (GameState-Score state))]
        
        [else state])))

(provide (all-defined-out))