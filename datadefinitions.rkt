#lang racket
;(require "render.rkt")
(require 2htdp/image)



(define-struct posn [x y] #:prefab)
; Player:
; img - Image
; health - integer
; position - posn
; direction is one of (1,0), (0,1), (-1,0), (0,-1), (0,0)
(define-struct Player [img health position direction Weapon] #:prefab)

; Weapon
; img - Image
; x - Number
; y - Number
; projectilespeed - Number
(define-struct Weapon [img x y projectilespeed]  #:prefab)

; Projectile
; img - Image
; position - posn
; direction - posn
(define-struct Projectile [img position direction damage]  #:prefab)

; Zombie
(define-struct Zombie [img health position damage]  #:prefab)

; Game State
; Player- player
; Zombies - List<Zombie>
; Projectiles - List<Projectile>
; Score - Integer
; 1 -> Menu
; 2 -> HowTo
; 3 -> HowTo2
; 4 -> Game Over
; 5 -> Level1 
(define-struct GameState [player Zombies Projectiles Score Menue] #:prefab)





; posn -> posn
; Normalize vector
(define (normalise vec)
  (cond [(= 0 (posn-x vec) (posn-y vec)) (make-posn 0 0)]
        [else
         (make-posn  (/ (posn-x vec) (sqrt (+ (sqr (posn-x vec)) (sqr (posn-y vec)))))
                     (/ (posn-y vec) (sqrt (+ (sqr (posn-x vec)) (sqr (posn-y vec))))))]))

; global constants
(define BACKGROUND (bitmap/file "Map.png"))
(define SPEED 10)
(define HEIGHT (image-height BACKGROUND))
(define WIDTH (image-width BACKGROUND))
(define ZOMBIE1 (rotate 90 (bitmap/file "Zombie.png")))
(define ZOMBIE2 (rotate 90 (bitmap/file "Super-Zombie.png")))

;Initial State Menue
(define InitState (make-GameState (make-Player 1
                                             100
                                             ;(make-posn (/ WIDTH 2) (/ (- HEIGHT 30)2))
                                             (make-posn 100 100)
                                             (make-posn 0 0)
                                             (make-Weapon 1
                                                          0
                                                          0
                                                          500))
                                (list)
                                '()
                                0
                                1))

;Initial State
(define Level1 (make-GameState (make-Player 1
                                             100
                                             ;(make-posn (/ WIDTH 2) (/ (- HEIGHT 30)2))
                                             (make-posn 100 100)
                                             (make-posn 0 0)
                                             (make-Weapon 1
                                                          0
                                                          0
                                                          500))
                                (list)
                                '()
                                0
                                5))

;Initial State GameOver
(define Game-Over (make-GameState (make-Player 1
                                             100
                                             ;(make-posn (/ WIDTH 2) (/ (- HEIGHT 30)2))
                                             (make-posn 100 100)
                                             (make-posn 0 0)
                                             (make-Weapon 1
                                                          0
                                                          0
                                                          500))
                                (list)
                                '()
                                0
                                4))


; global functions
; Zombie/Player Number -> Boolean
;
(define (obstacle-hit x y level)
  (cond [(= level 1) (or
                      (and (< 535 x 845)
                           (< 395 y 675))
                     (and (< 40 x 330)
                           (< 400 y 676))
                     (and (< 375 x 773)
                           (< 0 y 261))
                     (and (< 1173 x 1285)
                           (< 30 y 300))
                     (and (< 263 x 400) ;Blue Car
                           (< 672 y 737))
                     (and (< 420 x 632) ;Truck
                           (< 760 y 850))
                     (and (< 860 x 946) ;Red Car
                           (< 0 y 109))
                     (and (< 1070 x 1175) ;5-O
                           (< 385 y 560))
                      (< x 30)
                      (< (- WIDTH 30) x)
                      (< y 30)
                      (< (- HEIGHT 40) y))]))

; Number Number Number -> Boolean
; same as obsatcle-hit but changed hitboxes for projectiles
(define (obstacle-hit-proj x y level)
  (cond [(= level 1) (or
                      (and (< 555 x 815) ; middle house
                           (< 420 y 645))
                     (and (< 55 x 305)
                           (< 420 y 640))
                     (and (< 410 x 750) ; top house 
                           (< 0 y 240))
                     (and (< 1200 x 1280) ; right house
                           (< 45 y 275))
                     
                      (< x 0)
                      (< (- WIDTH 00) x)
                      (< y 0)
                      (< (- HEIGHT 0) y))]))

(provide (all-defined-out))

