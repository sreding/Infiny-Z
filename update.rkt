#lang racket

(require "datadefinitions.rkt")
(define ZSPEED #i5)
(define WEAPONDAMAGE 10)
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
            (not (obstacle-hit-proj (posn-x (Projectile-position Projectile)) (posn-y (Projectile-position Projectile)) 1))) 
            ;(and (< 0 (posn-x (Projectile-position Projectile)) WIDTH)
            ;     (< 0  (posn-y (Projectile-position Projectile)) HEIGHT)))
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
  (map (lambda (Zombie) (local [(define direction (zombie-direction Zombie Player))
                                (define x-plus-dx (+ (posn-x (Zombie-position Zombie)) (* ZSPEED (posn-x direction))))
                                (define y-plus-dy (+ (posn-y (Zombie-position Zombie)) (* ZSPEED (posn-y direction))))
                                (define x (posn-x (Zombie-position Zombie)))
                                (define y (posn-y (Zombie-position Zombie)))]
                          (make-Zombie
                           (Zombie-img Zombie)
                           (Zombie-health Zombie)
                           (make-posn (if (obstacle-hit-z x-plus-dx y 1) x x-plus-dx)
                                      (if (obstacle-hit-z x y-plus-dy 1) y y-plus-dy))
                           (Zombie-damage Zombie))))
       Zombies))

; GameState -> Zombies
; Tests if a projetile hits a zombie and reduces the Zombies health
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
                    (map (lambda (Zombie) (if (check-collision-1-arg Zombie)  Zombie
                                              (make-Zombie (Zombie-img Zombie)
                                                           (- (Zombie-health Zombie) WEAPONDAMAGE)
                                                           (Zombie-position Zombie)
                                                           (Zombie-damage Zombie))))
                                              (GameState-Zombies state))))

; Zombies -> Zombies
; Deletes all Zombies with Zombie-health < 0
(define (zombie-dead Zombies)
  (filter (lambda (Zombie) (> (Zombie-health Zombie) 0)) Zombies))

; Zombies -> Number
; counts how many zombies died that frame
(define (nr-dead-zombies Zombies)
  (cond [(empty? Zombies) 0]
        [else (if (<= (Zombie-health (first Zombies)) 0)
                  (add1 (nr-dead-zombies (rest Zombies) ))
                  (nr-dead-zombies (rest Zombies)))]))
                  

; Zombies -> Zombies
; add random zombies
(define (add-random-zombies Zombies)
  (local [(define rand-nr (random 150))
          (define rand-zombie (random 10))
          (define rand-nr2 (random 4))]
    (cond [(= 0 rand-nr) (cons (make-Zombie (if (= rand-zombie 0) ZOMBIE2 ZOMBIE1)
                                      (if (= rand-zombie 0) 160 40)
                                      (make-posn 1210 590)
                                      1) Zombies)]
          [(= 1 rand-nr) (cons (make-Zombie (if (= rand-zombie 0) ZOMBIE2 ZOMBIE1)
                                      (if (= rand-zombie 0) 160 40)
                                      (make-posn 440 320)
                                      1) Zombies)]
          [(= 2 rand-nr) (cons (make-Zombie (if (= rand-zombie 0) ZOMBIE2 ZOMBIE1)
                                      (if (= rand-zombie 0) 160 40)
                                      (cond [(= rand-nr2 0) (make-posn (random WIDTH) -30)]
                                            [(= rand-nr2 1) (make-posn (random WIDTH) (+ 30 HEIGHT))]
                                            [(= rand-nr2 2) (make-posn -30 (random HEIGHT))]
                                            [else (make-posn (+ 30 WIDTH) (random HEIGHT))])
                                      1) Zombies)]
          [else Zombies])))

; GameState -> Projectiles
; 
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
(define (update-player-position Player)
  (local [ (define x-plus-dx (+ (* SPEED (posn-x (Player-direction Player))) (posn-x (Player-position Player))))
           (define y-plus-dy (+ (* SPEED (posn-y (Player-direction Player))) (posn-y (Player-position Player))))
           (define x (posn-x (Player-position Player)))
           (define y (posn-y (Player-position Player)))]
  (make-Player (Player-img Player)
               (Player-health Player)
               (make-posn  (if (obstacle-hit x-plus-dx y 1) x x-plus-dx)
                           (if (obstacle-hit x y-plus-dy 1) y y-plus-dy))
               (Player-direction Player)
               (Player-Weapon Player))))

; Player Zombie -> Boolean
; returns #true if player is hit
(define (player-hit Player Zombie)
  (local [(define x (posn-x (Player-position Player)))
          (define y (posn-y (Player-position Player)))
          (define Zx (posn-x (Zombie-position Zombie)))
          (define Zy (posn-y (Zombie-position Zombie)))]
  (and (< (- x 30) Zx (+ x 30))
                (< (- y 30) Zy (+ y 30)))))

; Player Zombies -> Player
; Calculates the damage a player takes from a zombie
(define (damage-calculation Player Zombies)
  (cond [(empty? Zombies) Player]
        [else (damage-calculation (make-Player (Player-img Player)
                      (if (player-hit Player (first Zombies)) (- (Player-health Player) (Zombie-damage (first Zombies))) (Player-health Player))
                      (Player-position Player)
                      (Player-direction Player)
                      (Player-Weapon Player)) (rest Zombies))]))
; Player PowerUp -> boolean
; returns #true if player hits a healt pack
(define (health-pack? Player PowerUp)
  (and (and (< (- (posn-x (PowerUp-position PowerUp)) 30) (posn-x (Player-position Player)) (+ (posn-x (PowerUp-position PowerUp)) 30))
                                (< (- (posn-y (PowerUp-position PowerUp)) 30) (posn-y (Player-position Player)) (+ (posn-y (PowerUp-position PowerUp)) 30)))
        (= 0 (PowerUp-nr PowerUp))))

; Player PowerUps -> Player
; Sets Player health to 100 if the player picks up a health pack
(define (update-health? Player PowerUps)
  (cond [(empty? PowerUps) #false]
        [else (or (health-pack? Player (first PowerUps))
                  (update-health? Player (rest PowerUps)))]))
; Player PowerUps -> Player
(define (update-health Player PowerUps)
  (if (update-health? Player PowerUps)
      (make-Player (Player-img Player)
               100
               (Player-position Player)
               (Player-direction Player)
               (Player-Weapon Player))
      Player))
      
 
; Player -> Player
(define (update-player Player Zombies PowerUps)
  (update-player-position (update-health (damage-calculation Player Zombies) PowerUps)))

; GameState -> Boolean
; Returns #true if Player-health < 0
(define (game-over? state)
  (< (Player-health (GameState-player state)) 0))

; PowerUps -> PowerUps
(define (spawn-random-power-up PowerUps)
  (local [(define rand-nr (random 100))
          (define rand-nr2 (random 2))
          (define rand-x (random WIDTH))
          (define rand-y (random HEIGHT))]
    (cond [(and (not (obstacle-hit rand-x rand-y 1)) (= rand-nr 0))
           (cons (make-PowerUp (make-posn rand-x rand-y)
                               rand-nr2) PowerUps)]
          [else PowerUps])))

; PowerUps Player -> PowerUps
(define (delete-powerups PowerUps Player)
  (filter (lambda (x) (not (and (< (- (posn-x (PowerUp-position x)) 30) (posn-x (Player-position Player)) (+ (posn-x (PowerUp-position x)) 30))
                                (< (- (posn-y (PowerUp-position x)) 30) (posn-y (Player-position Player)) (+ (posn-y (PowerUp-position x)) 30)))))
            PowerUps))

; Player PowerUp -> boolean
; returns #true if player hits a powerup
(define (player-overlaps-power-up Player PowerUp)
  (and (and (< (- (posn-x (PowerUp-position PowerUp)) 30) (posn-x (Player-position Player)) (+ (posn-x (PowerUp-position PowerUp)) 30))
                                (< (- (posn-y (PowerUp-position PowerUp)) 30) (posn-y (Player-position Player)) (+ (posn-y (PowerUp-position PowerUp)) 30)))
        (= 1 (PowerUp-nr PowerUp))))

; Player PowerUps -> Player
; Sets Player health to 100 if the player picks up a health pack
(define (nuke? Player PowerUps)
  (cond [(empty? PowerUps) #false]
        [else (or (player-overlaps-power-up Player (first PowerUps))
                  (nuke? Player (rest PowerUps)))]))
; Player PowerUps -> Player
(define (nuke Zombies Player PowerUps)
  (if (nuke? Player PowerUps)
      (map (lambda (x) (make-Zombie (Zombie-img x)
                                    0
                                    (Zombie-position x)
                                    (Zombie-damage x)))
           Zombies)
      Zombies))

               

                                             

; GameState -> GameState
(define (update state)
  (cond [(game-over? state) Game-Over]
        [(= (GameState-Menue state) 10) state]
    [(= (GameState-Menue state) 5)
  (make-GameState (update-player (GameState-player state) (GameState-Zombies state) (GameState-PowerUps state))
                  (add-random-zombies (zombie-dead (update-zombies (nuke (Z-hit-detection state) (GameState-player state) (GameState-PowerUps state)) (GameState-player state)) ))
                  (update-projectiles (Projectile-hit-detection state))
                  (delete-powerups (spawn-random-power-up (GameState-PowerUps state) ) (GameState-player state))
                  (+ (nr-dead-zombies (nuke (Z-hit-detection state) (GameState-player state) (GameState-PowerUps state))) (GameState-Score state))
                  (GameState-Menue state))]
  [else state]))


(provide (all-defined-out))

