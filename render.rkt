#lang racket

; Draw weapon fix

(require 2htdp/image)
(require "DataDefinitions.rkt")

(define HEIGHT 720)
(define WIDTH 1280)
(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define GUN (scale (/ 1 15)(bitmap/file "Gun.png")))
(define ZOMBIE1 (bitmap/file "Zombie.png"))
(define PLAYER1 (bitmap/file "Player.png"))

;Example colapsed for visibility
(define Example (make-GameState (make-Player 1
                                             100
                                             (make-posn (/ WIDTH 2) (/ (- HEIGHT 30)2))
                                             (make-posn 0 0)
                                             (make-Weapon 1
                                                          0
                                                          0
                                                          9000))
                                (list
                                 (make-Zombie ZOMBIE1
                                              100
                                              (make-posn (random WIDTH) (- HEIGHT 30))
                                              9000)
                                 (make-Zombie ZOMBIE1
                                              100
                                              (make-posn (random WIDTH) (- HEIGHT 30))
                                              9000)
                                 (make-Zombie ZOMBIE1
                                              100
                                              (make-posn (random WIDTH) (- HEIGHT 30))
                                              9000)
                                 (make-Zombie ZOMBIE1
                                              100
                                              (make-posn (random WIDTH) (- HEIGHT 30))
                                              9000)
                                 (make-Zombie ZOMBIE1
                                              100
                                              (make-posn (random WIDTH) (- HEIGHT 30))
                                              9000)
                                 (make-Zombie ZOMBIE1
                                              100
                                              (make-posn (random WIDTH) (- HEIGHT 30))
                                              9000)
                                 (make-Zombie ZOMBIE1
                                              100
                                              (make-posn (random WIDTH) (- HEIGHT 30))
                                              9000)
                                 (make-Zombie ZOMBIE1
                                              100
                                              (make-posn (random WIDTH) (- HEIGHT 30))
                                              9000))
                                '()
                                0))


; Zombies, img -> image
; Takes a List of Zombie and places it on the image
(define (draw-zombies Zombies Image)
  (cond [(empty? Zombies) Image]
        [else (place-image  (Zombie-img (first Zombies))
                            (posn-x (Zombie-position (first Zombies)))
                            (posn-y (Zombie-position (first Zombies)))
                            (draw-zombies (rest Zombies) Image))]))

; Projectiles --> image
(define (draw-projectiles Projectiles Image)
  (cond [(empty? Projectiles) Image]
        [else (place-image (cond [(= 1 (Projectile-img (first Projectiles))) (circle 10 "solid" "green")]) 
                            (posn-x (Projectile-position (first Projectiles)))
                            (posn-y (Projectile-position (first Projectiles)))
                            (draw-projectiles (rest Projectiles) Image))]))




  


; Player + background-> Img
; Places player into background
(define (draw-player Player Background)
 (place-image
 (cond [(= 1 (Player-img Player)) PLAYER1])
 (posn-x (Player-position Player))
 (posn-y (Player-position Player))
 Background))

; Player -> Number
(define (WeaponAngle Player)
  (cond [(and (= (posn-x (Player-position Player)) (Weapon-x (Player-Weapon Player))) (> (posn-y (Player-position Player)) (Weapon-y (Player-Weapon Player))))  90]
        [(= (posn-x (Player-position Player)) (Weapon-x (Player-Weapon Player))) 180]
        [(not (= (posn-x (Player-position Player)) (Weapon-x (Player-Weapon Player))))
         (local ((define angle (+ 270 (* (/ 360 (* 2 pi))
            (atan
             (/
              (- (Weapon-y (Player-Weapon Player)) (posn-y (Player-position Player)))
              (-  (posn-x (Player-position Player)) (Weapon-x (Player-Weapon Player))))))))
                 )
           (if (< (Weapon-x (Player-Weapon Player)) (posn-x (Player-position Player))) (+ angle 180) angle))] ))


        

; Player, Image -> Image
(define (draw-gun Player img)
  (place-image (rotate (WeaponAngle Player) (cond [(= 1 (Weapon-img (Player-Weapon Player))) GUN] ))
               (+ (posn-x (Player-position Player)) 0)
               (+ (posn-y (Player-position Player)) 0)
               img))


;to-draw
; GameStat -> Image
(define (render state)
  (draw-gun (GameState-player state)
(draw-projectiles
 (GameState-Projectiles state)
 (draw-zombies
  (GameState-Zombies state)
  (draw-player
   (GameState-player state)
   BACKGROUND)))))




(provide (all-defined-out))