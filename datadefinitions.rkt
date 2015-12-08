#lang racket
;(require "render.rkt")
(require 2htdp/image)



(define-struct posn [x y] #:prefab)
; Player:
; img - Image
; health - integer
; position - posn
(define-struct Player [img health position Weapon] #:prefab)

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


; Usefulf functions for GameState

; update-player-position: Gamestate Number Number -> GameState
; Takes a GamState and updates Player-position
(define (update-player-position state x y)
  (local  [(define PrevPlayer (GameState-player state))]
  (make-GameState (make-Player (Player-img PrevPlayer)
                               (Player-health PrevPlayer)
                               (make-posn x y)
                               (Player-Weapon PrevPlayer))
                  (GameState-Zombies state)
                  (GameState-Projectiles state)
                  (GameState-Score state))))






(provide (all-defined-out))

;This is a test to see how commits work