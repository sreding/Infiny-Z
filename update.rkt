#lang racket

(require "datadefinitions.rkt")
(define ZSPEED #i1)

(define PROJECTILE-SPEED 10)
; update zombies
; update projectiles
; hit detection
; update score
; spawn zombies
; 



; List<Projectile> -> List<Projectile>
; updates the projectiles position accortingto their direction
; position = position +speed*direction
(define (update-position-projectiles list)
  (map (lambda (x) (make-Projectile (Projectile-img x)
                                    (make-posn (+ (posn-x (Projectile-position x)) (* PROJECTILE-SPEED (posn-x (Projectile-direction x))))
                                               (+ (posn-y (Projectile-position x)) (* PROJECTILE-SPEED (posn-y (Projectile-direction x)))))
                                    (Projectile-direction x)
                                    (Projectile-damage x)))
       list))


; Projectiles -> Projectiles
(define (delete-projectiles Projectiles)
  (filter (lambda (Projectile)
            (and (< 0 (posn-x (Projectile-position Projectile)) WIDTH)
                 (< 0  (posn-y (Projectile-position Projectile)) HEIGHT)))
          Projectiles))

; Projectiles -> Projectiles
(define (update-projectiles Projectiles)
  (delete-projectiles (update-position-projectiles Projectiles)))

; Zombie, Player -> Posn
; Returns the vector between Zombie and Player 
(define (zombie-direction Zombie Player)
 (normalise
  (make-posn (- (posn-x (Player-position Player)) (posn-x (Zombie-position Zombie)))
             (- (posn-y (Player-position Player)) (posn-y (Zombie-position Zombie))))))

; Zombies, Player -> Zombies
; Moves Zombies in the players direction
(define (update-zombies Zombies Player)
  (map (lambda (Zombie) (local [(define direction (zombie-direction Zombie Player))]
                          (make-Zombie
                           (Zombie-img Zombie)
                           (Zombie-health Zombie)
                          (make-posn (+ (posn-x (Zombie-position Zombie)) (* ZSPEED (posn-x direction)))
                                     (+ (posn-y (Zombie-position Zombie)) (* ZSPEED (posn-y direction))))
                          (Zombie-damage Zombie))))
       Zombies))

; GameState -> GameState
; Tests if a projetile hits a zombie deletes the projectile and calculates damage
(define (Z-hit-detection state)
  (local [; retrns true if zombie is hit
          (define (check-collision Zombie list)
            (cond [(empty? list) #false]
                  [else (local [ (define x (posn-x (Zombie-position Zombie)))
                                 (define y (posn-y (Zombie-position Zombie)))]
                   (or (and (< (- x 30) (posn-x (Projectile-position (first list))) (+ x 30))
                            (< (- y 30) (posn-y (Projectile-position (first list))) (+ y 30)))
                       (check-collision Zombie (rest list))))]))
          (define (check-collision-1-arg Zombie)
            (not (check-collision Zombie (GameState-Projectiles state))))]
                    (filter check-collision-1-arg (GameState-Zombies state))))

(define (Projectile-hit-detection state)
  (local [; retrns true if zombie is hit
          (define (check-collision Projectile list)
            (cond [(empty? list) #false]
                  [else (local [ (define x (posn-x (Zombie-position (first list))))
                                 (define y (posn-y (Zombie-position (first list))))]
                   (or (and (< (- x 30) (posn-x (Projectile-position Projectile)) (+ x 30))
                            (< (- y 30) (posn-y (Projectile-position Projectile)) (+ y 30)))
                       (check-collision Projectile (rest list))))]))
          (define (check-collision-1-arg Projectile)
            (not (check-collision Projectile (GameState-Zombies state))))]
                    (filter check-collision-1-arg (GameState-Projectiles state))))


; updates Player in direction
; Player -> Player
(define (update-player Player)
  (make-Player (Player-img Player)
               (Player-health Player)
               (make-posn (+ (* SPEED (posn-x (Player-direction Player))) (posn-x (Player-position Player)))
                          (+ (* SPEED (posn-y (Player-direction Player))) (posn-y (Player-position Player))))
               (Player-direction Player)
               (Player-Weapon Player)))


; GameState -> GameState
(define (update state)
  
  (make-GameState (update-player (GameState-player state))
                  (update-zombies (Z-hit-detection state) (GameState-player state))
                  (update-projectiles (Projectile-hit-detection state))
                  (GameState-Score state)))


(provide (all-defined-out))

