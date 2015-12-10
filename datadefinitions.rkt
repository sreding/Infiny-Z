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
(define-struct GameState [player Zombies Projectiles Score] #:prefab)





; posn -> posn
; Normalize vector
(define (normalise vec)
  (cond [(= 0 (posn-x vec) (posn-y vec)) (make-posn 0 0)]
        [else
         (make-posn  (/ (posn-x vec) (sqrt (+ (sqr (posn-x vec)) (sqr (posn-y vec)))))
                     (/ (posn-y vec) (sqrt (+ (sqr (posn-x vec)) (sqr (posn-y vec))))))]))

; global constants
(define SPEED 5)
(define HEIGHT 961)
(define WIDTH 1280)
(define ZOMBIE1 (rotate 90 (bitmap/file "Zombie.png")))
(define ZOMBIE2 (rotate 90 (bitmap/file "Super-Zombie.png")))

; global functions
; Zombie/Player Number -> Boolean
;
(define (obstacle-hit x y level)
  (cond [(= level 1) (or(and (< 535 x 845)
                           (< 395 y 675))
                     (and (< 40 x 330)
                           (< 400 y 676))
                     (and (< 40 x 330)
                           (< 400 y 676))
                     (and (< 375 x 773)
                           (< 0 y 261))
                     (and (< 1173 x 1285)
                           (< 30 y 300)))]))

(provide (all-defined-out))

