#lang racket

(require "datadefinitions.rkt")

(define PROJECTILE-SPEED 1)
; update zombies
; update projectiles
; hit detection
; update score
; spawn zombies
; 



; List<Projectile> -> List<Projectile>
; updates the projectiles position accortingto their direction
; position = position +speed*direction
(define (update-projectiles list)
  (map (lambda (x) (make-Projectile (Projectile-img x)
                                    (make-posn (+ (posn-x (Projectile-position x)) (* PROJECTILE-SPEED (posn-x (Projectile-direction x))))
                                               (+ (posn-y (Projectile-position x)) (* PROJECTILE-SPEED (posn-y (Projectile-direction x)))))
                                    (Projectile-direction x)
                                    (Projectile-damage x)))
       list))






; GameState -> GameState
(define (update state)
  (make-GameState (GameState-player state)
                  (GameState-Zombies state)
                  (update-projectiles (GameState-Projectiles state))
                  (GameState-Score state)))


(provide (all-defined-out))

