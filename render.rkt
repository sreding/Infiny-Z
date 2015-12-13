#lang racket

; Draw weapon fix

(require 2htdp/image)
(require "DataDefinitions.rkt")

;Menue
(define Menue (bitmap/file "Menu.png"))
(define HowTo (bitmap/file "HowTo.png"))
(define HowToTwo (bitmap/file "HowToTwo.png"))
(define GameOver (bitmap/file "GameOver.png"))


(define GUN (rotate 90 (bitmap/file "Gun.png")))

;(define GUN (scale (/ 1 15)(bitmap/file "Gun.png")))


(define PLAYER1 (circle 0 "solid" "white"))




; Zombies, img -> image
; Takes a List of Zombie and places it on the image
(define (draw-zombies Zombies Player Image)
  (cond [(empty? Zombies) Image]
        [else (place-image  (rotate-towards (Zombie-img (first Zombies)) (first Zombies) Player)
                            (posn-x (Zombie-position (first Zombies)))
                            (posn-y (Zombie-position (first Zombies)))
                            (draw-zombies (rest Zombies) Player Image))]))
; Projectiles --> image
(define (draw-projectiles Projectiles Image)
  (cond [(empty? Projectiles) Image]
        [else (place-image (cond [(= 1 (Projectile-img (first Projectiles))) (circle 3 "solid" "red")]) 
                            (posn-x (Projectile-position (first Projectiles)))
                            (posn-y (Projectile-position (first Projectiles)))
                            (draw-projectiles (rest Projectiles) Image))]))

; rotate-towards : Image -> Image
; Takes a Zombie and rotates it towards the player

(define (rotate-towards img Zombie Player)
  (local [(define angle (+ 90 (* (/ 360 (* 2 pi))
            (atan
             (/
              (- (posn-y (Zombie-position Zombie)) (posn-y (Player-position Player)))
              (if (= 0 (-  (posn-x (Player-position Player)) (posn-x (Zombie-position Zombie)))) 1 (-  (posn-x (Player-position Player)) (posn-x (Zombie-position Zombie))))))))
                 )]
    (cond
      [(> (posn-x (Player-position Player)) (posn-x (Zombie-position Zombie))) (rotate (+ 180 angle) img)]
      [(< (posn-x (Player-position Player)) (posn-x (Zombie-position Zombie))) (rotate angle img)]
       [(and (> (posn-y (Player-position Player)) (posn-y (Zombie-position Zombie)))
            (= (posn-x (Player-position Player)) (posn-x (Zombie-position Zombie))))
       (rotate 180 img)]
      [(= (posn-x (Player-position Player)) (posn-x (Zombie-position Zombie)))
       img]))) 




  


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

; Score, Image -> Image
(define (draw-score score img)
  (place-image
   (text (string-append "Score: " (number->string score)) 24 "red")
   (- WIDTH 90) 20
  img))


;to-draw
; GameStat -> Image
(define (render state)
  (cond [(= (GameState-Menue state) 5)
         (draw-score (GameState-Score state)
         (draw-gun (GameState-player state)
                   (draw-projectiles
                    (GameState-Projectiles state)
                    (draw-zombies
                     (GameState-Zombies state) (GameState-player state)
                     (draw-player
                      (GameState-player state)
                      BACKGROUND)))))]
        [(= (GameState-Menue state) 1) Menue]
        [(= (GameState-Menue state) 2) HowTo]
        [(= (GameState-Menue state) 3) HowToTwo]
        [(= (GameState-Menue state) 4) GameOver]))
        





(provide (all-defined-out))