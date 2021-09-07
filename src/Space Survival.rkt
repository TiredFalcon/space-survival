#lang racket
;###############################################################################
;#################### PROGRAMMING FUNDAMENTALS I - PROJECT #####################

; Authors: Jacopo Fidacaro & Aron Fiechter
; Version.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Libraries: 
(require 2htdp/image)
(require 2htdp/universe)
(require racket/math)
(require lang/posn)
(require test-engine/racket-tests)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Constants:
(require "constants.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Data definitions:

; A Ship is a structure with:
; - Posn : position of the ship
; - Number : angle of rotation of the ship
; - Posn : velocity of the ship
; - NonNegNumber : current life left  
; - NonNegNumber : current energy level
; - NonNegNumber : magic number used for ad-hoc animations
; Interpretation:
; The spaceship's position, angle, velocity, health and energy level.
(define-struct ship
  [posn ang vel life energy jacopo] #:transparent)
; Template
;(define (fn-for-ship ship)
;  (make-ship (... (ship-posn ship))
;             (... (ship-ang ship))
;             (... (ship-vel ship))
;             (... (ship-life ship))
;             (... (ship-energy ship))
;             (... (ship-jacopo ship))

; A Shot is a structure with:
; - Posn : position of the shot
; - Number : angle of the shot in degrees
; Interpretation: 
; A spaceship's shot position and direction.
(define-struct shot
  [posn ang speed] #:transparent)
; Template
;(define (fn-for-shot shot)
;  (make-shot (... (shot-posn shot))
;             (... (shot-ang shot))
;             (... (shot-speed shot))))

; An Enemy is a structure with:
; - Posn : position of the enemy
; - Number : angle of rotation of movement of the enemy
; - Image : the enemy's sprite
; - NonNegNumber : current life left
; - Number : magic number used for ad-hoc animations
; Interpretation:
; The enemy's position, velocity vector, sprite and life left.
(define-struct enemy
  [name posn ang life aron] #:transparent)
;Template
;(define (fn-for-enemy enemy)
;  (make-enemy (... (enemy-name enemy))
;              (... (enemy-posn enemy))
;              (... (enemy-ang enemy))
;              (... (enemy-life enemy))
;              (... (enemy-aron enemy))))

; An HPPD is one of:
; - "home"
; - "playing"
; - "pause"
; - "dead"
; - "ins"
; - "exit"
; Interpretation:
; the current state of the game screen stated with a string.

; A Game is a structure with:
; - Ship          : the ship controlled by the player
; - Shots         : list of shots shooted by the spaceship
; - Number[0,3]   : life number or dead (0)
; - Enemies       : list of all enemy objects
; - HPPD          : state of the game screen
; - NonNegNumber  : the score
; - NonNegNumber  : the current level
; - Number[0,359] : ticker for oscillating animations.
; (and some alignment fetish)
; Interpretation : the game with all the stuff defined above (TODO : complete
; when finished)
(define-struct game
  [ship shots lives enemies HPPD score level ticker] #:transparent)
; Template
;(define (fn-for-game game)
;  (make-game (... (game-ship game))
;             (... (game-shots game))
;             (... (game-lives game))
;             (... (game-enemies game))
;             (... (game-HPPD game))
;             (... (game-score game))
;             (... (game-level game))
;             (... (game-ticker game))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Functions:

;_______________________________________________________________________________
; debug and utility

; Number -> Number
; Rationalize a number to avoid crash.
(define (pls-dont-crash n)
  (* (round (/ n TOLERANCE)) TOLERANCE))

; Image -> Number
; Return the radius of a circular image.
(define (r image)
  (/ (image-width image) 2))

;_______________________________________________________________________________
; to-draw

; Game -> Image
; Render the current state of the game (each frame).
(define (render game)
  (cond [(string=? (game-HPPD game) "home") (homescreen game)]
        [(string=? (game-HPPD game) "playing") (playscreen game)]
        [(string=? (game-HPPD game) "pause") (pausescreen game)]
        [(string=? (game-HPPD game) "dead") (gameoverscreen game)]
        [(string=? (game-HPPD game) "ins") (instructionscreen game)]
        [else SOMEONE-REALLY-CLICKED-EXIT-INSTEAD-OF-JUST-CLOSING]))

;_____________________
; Game -> Image
; Render the image for the home screen.
(define (homescreen game)
  (if (= 0 (game-level game))
      INITIAL-HOME-SCREEN
      (place-images
       (list (score-display-2 (game-score game))
             (level-display-2 (game-level game))
             (lives-display-2 (game-lives game)))
       (list (make-posn 120 680)
             (make-posn 360 680)
             (make-posn 600 680))
       HOME-SCREEN)))

;_____________________
; Game -> Image
; Render the image for the game scene.
(define (playscreen game)
  (overlay (place-images/align
            
            ; Game info (life, energy, level)
            (playscreen/stats game)
            (list (make-posn 0 0)
                  (make-posn 0 10)
                  (make-posn 340 690)
                  (make-posn 20 690)
                  (make-posn 640 690))
            "left"
            "top"
            ; Actual game scenes
            (overlay
             ; Radiation
             (cropped (ship-posn (game-ship game))
                      (choose-frame RAD (game-ticker game)))
             ; Glorbs
             (draw-glorbs game)
             ; Ship
             (rotate (ship-ang (game-ship game))
                     (overlay (draw-ship (ship-jacopo (game-ship game)))
                              (circle 50 0 "white")))
             ; Sprites and asteroids
             (draw-junk game)
             ; Shots
             (draw-shots game)
             ; Background
             (cropped (ship-posn (game-ship game))
                      (place-image
                       (rotate (* -40
                                  (modulo (game-ticker game) 360))
                               (overlay PORTAL
                                        PORTAL-BASE))
                       WINSIZE
                       (- GAMESIZE WINSIZE)
                       (place-image
                        (rotate (* -40
                                   (modulo (game-ticker game) 360))
                                (overlay PORTALO
                                         PORTAL-BASE))
                        (- GAMESIZE WINSIZE)
                        WINSIZE
                        SPACE)))))
           WINDOW))

; Num -> Image
; Chooses frame for ship according to if it is being damaged or not.
(define (draw-ship j)
  (if (= j 0)
      SHIP
      SHIP-DMG))

; Game -> List<Image>
; Creates list with background, life and energy bar.
(define (playscreen/stats game)
  (list (rectangle (* WINSIZE (/ (if (> 0 (ship-life (game-ship game)))
                                     0
                                     (ship-life (game-ship game)))
                                     ENERGY))
                       10
                       255
                       "green")
        (local (; color of energy bar dependant on energy left:
                (define e (ship-energy (game-ship game)))
                (define en-col (cond [(> e (* ENERGY 0.7)) "yellow"]
                                     [(< e (* ENERGY 0.1)) "red"]
                                     [else "orange"])))
          (rectangle (* WINSIZE (/ (ship-energy (game-ship game))
                                   ENERGY))
                     10
                     255
                     en-col))
        (level-display (game-level game))
        (score-display (game-score game))
        (lives-display (game-lives game))))

; List<ShotOrEnemy> Function Game -> Boolean
; Checks if a posn of an object (shot or enemy) in a list of A is inside the
; window. Used to filter list of shots and list enemies before rendering.
(define (filter-objects list fn game)
  (filter (lambda (a)
            (and (<= (- (posn-x (ship-posn (game-ship game)))
                        (* WINSIZE 0.7))
                     (posn-x (fn a))
                     (+ (posn-x (ship-posn (game-ship game)))
                        (* WINSIZE 0.7)))
                 (<= (- (posn-y (ship-posn (game-ship game)))
                        (* WINSIZE 0.7))
                     (posn-y (fn a))
                     (+ (posn-y (ship-posn (game-ship game)))
                        (* WINSIZE 0.7)))))
          list))

; List<ShotOrEnemy> Function Posn -> List<Posn>
; Reduces every position to a position relative to WINSIZE (relative to the
; position of the ship).
(define (relative-posns list fn posn)
  (map (lambda (a)
         (make-posn (- (posn-x (fn a))
                       (posn-x posn)
                       (- (/ WINSIZE 2)))
                    (- (posn-y (fn a))
                       (posn-y posn)
                       (- (/ WINSIZE 2)))))
       list))

; Game -> Image
; Returns an image with all the shots contained in the window.
(define (draw-shots game)
  (local (; filter list of shots to keep only shot to be rendered:
          (define render-shots
            (filter-objects (game-shots game) shot-posn game)))
    (place-images (rotate-shots render-shots)
                  (relative-posns render-shots
                                  shot-posn
                                  (ship-posn (game-ship game)))
                  BLANKWINDOW)))

; List<Shot> -> List<Image>
; Given a list of shots, returns a list of images of shots, each one rotated
; according to its angle.
(define (rotate-shots shots)
  (map (lambda (shot)
         (rotate (shot-ang shot) SHOT))
       shots))

; Image Number[0,3] -> Image
; Given an image and a number n from 0 to 3, returns the image rotated n * 90
; degrees.
(define (choose-frame image ticker)
  (rotate (* 90 (modulo ticker 4)) image))

; Posn Image -> Image
(define (cropped posn image)
  (crop (- (posn-x posn) (/ WINSIZE 2))
        (- (posn-y posn) (/ WINSIZE 2))
        WINSIZE
        WINSIZE
        image))

; Game -> Image
; Given a list of enemies, return a list of images of glorbs rotated at their
; respective angle, and chooses a frame.
(define (draw-glorbs game)
  (local (; filter the list of enemies to keep only the ones seen in the window:
          (define render-enemies
            (filter (lambda (enemy)
                      (string=? (enemy-name enemy) GLORB))
                    (filter-objects (game-enemies game) enemy-posn game))))
    (place-images (map (lambda (enemy)
                         (rotate (enemy-ang enemy)
                                 (choose-enemy enemy (game-ticker game))))
                       render-enemies)
                  (relative-posns render-enemies
                                  enemy-posn
                                  (ship-posn (game-ship game)))
                  BLANKWINDOW)))

; Game -> Image
; Given a list of enemies, returns a list of images of enemies rotated at their
; respective angle, and chooses frame for animated enemies.
(define (draw-junk game)
  (local (; filter the list of enemies to keep only the ones seen in the window:
          (define render-enemies
            (filter (lambda (enemy)
                      (not (string=? (enemy-name enemy) GLORB)))
                    (filter-objects (game-enemies game) enemy-posn game))))
    (place-images (map (lambda (enemy)
                         (rotate (enemy-ang enemy)
                                 (choose-enemy enemy (game-ticker game))))
                       render-enemies)
                  (relative-posns render-enemies
                                  enemy-posn
                                  (ship-posn (game-ship game)))
                  BLANKWINDOW)))

; Enemy Number[0,100] -> Image
; Given an enemy, returns its image at the frame determined by ticker.
(define (choose-enemy enemy ticker)
  (local (; rotation for shake:
          (define shake (* HIT-SHAKE (sub1 (modulo (enemy-aron enemy) 3)))))
    (cond [(string=? (enemy-name enemy) GLORB)
           (if (zero? (enemy-aron enemy))
               (if (< (modulo ticker 4) 2)
                   IMM-GLORB-1
                   IMM-GLORB-2)
               IMM-GLORB-D)]
          [(string=? (enemy-name enemy) SPRITE)
           (rotate (* 45 ticker) IMM-SPRITE)]
          [(string=? (enemy-name enemy) ASTEROID-A)
           (if (zero? (enemy-aron enemy))
               (rotate (* 3 ticker) IMM-ASTEROID-A)
               (rotate (+ shake (* 3 ticker)) IMM-ASTEROID-A))]
          [(string=? (enemy-name enemy) ASTEROID-B)
           (if (zero? (enemy-aron enemy))
               (rotate (- ticker) IMM-ASTEROID-B)
               (rotate (+ shake (- ticker)) IMM-ASTEROID-B))]
          [(string=? (enemy-name enemy) ASTEROID-C)
           (if (zero? (enemy-aron enemy))
               (rotate ticker IMM-ASTEROID-C)
               (rotate (+ shake ticker) IMM-ASTEROID-C))]
          [(string=? (enemy-name enemy) ASTEROID-X)
           (if (zero? (enemy-aron enemy))
               (rotate (/ ticker -2) IMM-ASTEROID-X)
               (rotate (+ shake (/ ticker -2)) IMM-ASTEROID-X))])))

;_____________________
; Game -> Image
; Render the image for the pause screen.
(define (pausescreen game)
  (place-images
   (list (score-display-2 (game-score game))
         (level-display-2 (game-level game))
         (lives-display-2 (game-lives game)))
   (list (make-posn 120 680)
         (make-posn 360 680)
         (make-posn 600 680))
   PAUSE-SCREEN)) ; TODO

;_____________________
; Game -> Image
; Renders the image for the game over screen.
(define (gameoverscreen game)
  (local (; countdown:
          (define countdown
            (number->string (inexact->exact (round (* (game-ticker game)
                                                      GAMESPEED))))))
  (if (< 0 (game-lives game))
      (place-image/align
       (overlay/offset
        (text/font "CONTINUE? " (* TEXT-SIZE 2) "white" "Gill Sans"
                   'modern 'normal 'normal #f)
        0 100
        (text/font countdown (* TEXT-SIZE 2) "white" "Gill Sans"
                   'modern 'normal 'normal #f))
       MIDDLE 100
       "middle"
       "top"
       GAMEOVER-SCREEN-1)
      (place-image/align
       (above (text/font "GAME OVER"
                  35 "white" "Gill Sans" 'modern 'normal 'normal #f)
              (text/font "________________________"
                  35 "white" "Gill Sans" 'modern 'normal 'normal #f)
              (text/font (string-append "Score: "
                                        (number->string (game-score game)))
                  35 "white" "Gill Sans" 'modern 'normal 'normal #f))
       MIDDLE 50
       "middle"
       "top"
       GAMEOVER-SCREEN-2))))

;_____________________
; Game -> Image
; Renders the image for the instructions screen.
(define (instructionscreen game)
  INSTRUCTION-SCREEN)

;_______________________________________________________________________________
; on-tick

; Game -> Game
; Updates positions and states of all elements of the game.
(define (advance game)
  (cond [(string=? (game-HPPD game) "home") (homeadvance game)]
        [(string=? (game-HPPD game) "playing") (playadvance game)]
        [(string=? (game-HPPD game) "pause") (pauseadvance game)]
        [(string=? (game-HPPD game) "dead") (gameoveradvance game)]
        [else game]))

; Number[0,99] -> String
; Given a random number between 0 and 99, return an enemy.
(define (choose-opponents n)
  (cond [(< n 5) SPRITE]      ; 5%
        [(< n 6) ASTEROID-X]  ; 1%
        [(< n 16) ASTEROID-C] ; 10%
        [(< n 38) ASTEROID-B] ; 22%
        [(< n 70) ASTEROID-A] ; 32%
        [(< n 100) GLORB]))   ; 30%

; List<Enemy> PosNumber -> List<Enemy>
; Glorb-generator:
(define (enemy-gen xs n)
  (local (; generate new random enemy
          (define new-enemy
            (make-enemy (choose-opponents (random 100))
                        (make-posn (+ 500 (random (- GAMESIZE 1000)))
                                   (+ 500 (random (- GAMESIZE 1000))))
                        (random 360)
                        ELIFE
                        0)))
    (cond [(zero? n) xs]
          [else
           (if
            ; if the enemy is not too close to the start point of the ship
            (far-enough? new-enemy)
            ; then keep it and go on generating enemies
            (cons new-enemy (enemy-gen xs (- n 1)))
            ; else re generate it
            (enemy-gen xs n))])))

; Enemy -> Boolean
; Check if the enemy is far enough from the ship.
(define (far-enough? enemy)
  (or (> (posn-x (enemy-posn enemy)) (* WINSIZE 3/2))
      (< (posn-y (enemy-posn enemy)) (- GAMESIZE (* WINSIZE 3/2)))))

;_____________________
; Game -> Game
; Waits for the player to click "Start game".
(define (homeadvance game)
  (make-game (game-ship game)
             (game-shots game)
             (game-lives game)
             (game-enemies game)
             (game-HPPD game)
             (game-score game)
             (game-level game)
             (modulo (add1 (game-ticker game)) 4)))

;_____________________
; Game -> Game
; Updates positions and states of all elements of the game.
(define (playadvance game)
  (cond [(<= (ship-life (game-ship game)) 0)
         (gameover game)]
        [(EmanP (ship-posn (game-ship game))
                (make-posn (- GAMESIZE WINSIZE)
                           WINSIZE)
                (r PORTALO))
         (generate-random-level game)]
        [else
         (make-game (update-ship game)
                    (update-shots game)
                    (game-lives game)
                    (update-enemies game)
                    (game-HPPD game)
                    ; let's add1 for every tick the player survives this thing.
                    (game-score game)
                    (game-level game)
                    (modulo (add1 (game-ticker game)) TICKER))]))

; Game -> Game
; Goes to gameover with current stats, decreases lives by one, sets ticker to
; be countdown for gameoverscreen.
(define (gameover game)
  (make-game (game-ship game)
             (game-shots game)
             (sub1 (game-lives game))
             (game-enemies game)
             "dead"
             (game-score game)
             (game-level game)
             (round (* (/ 1 GAMESPEED) 10))))

; update ship:
; Game -> Ship
; Update positions and states of all elements of the ship.
(define (update-ship game)
  (local (; Ship
          (define ship (game-ship game)))
    (make-ship (new-posn game)
               (ship-ang ship)
               (update-vel game)
               (damage game)
               (if (> ENERGY (ship-energy ship))
                   (+ (ship-energy ship) RECHARGE)
                   (ship-energy ship))
               (trigger-damage game))))

; Game -> Posn
; Slow ship if on limits.
(define (update-vel game)
  (local (; position and velocity of ship:
          (define posn (ship-posn (game-ship game)))
          (define vel (ship-vel (game-ship game)))
          ; posn-x and posn-y of ship:
          (define x (posn-x posn))
          (define y (posn-y posn))
          ; just keep asteroids:
          (define asteroids
            (filter
             (lambda (enemy)
               (not (or (string=? (enemy-name enemy) GLORB)
                        (string=? (enemy-name enemy) SPRITE))))
             (game-enemies game)))
          ; next posn of ship:
          (define next-posn
            (make-posn
             (pls-dont-crash (+ (posn-x posn) (* BOOST (posn-x vel))))
             (pls-dont-crash (+ (posn-y posn) (* BOOST (posn-y vel)))))))
    (cond [(or (not (<= MIDDLE (posn-x next-posn) (- GAMESIZE MIDDLE)))
               (not (<= MIDDLE (posn-y next-posn) (- GAMESIZE MIDDLE))))
           (bounce-ship next-posn vel)]
          [(or (not (<= MIN-LIM x MAX-LIM))
               (not (<= MIN-LIM y MAX-LIM)))
           (make-posn (* (posn-x vel) 1/2) (* (posn-y vel) 1/2))]
          [(object-on-any-enemy? next-posn asteroids)
           (make-posn (- (posn-x vel)) (- (posn-y vel)))]
          [else vel])))

; Posn Posn -> Posn
; According to the position and velocity of the ship, bounce it (reverse x or y
; component of velocity).
(define (bounce-ship posn vel)
  (local (; components of posn and vel:
          (define x (posn-x posn))
          (define y (posn-y posn))
          (define dx (posn-x vel))
          (define dy (posn-y vel)))
    (cond [(and (< x (/ GAMESIZE 2))
                (< y (/ GAMESIZE 2)))
           (which-bounce x y dx dy)]
          [(and (> x (/ GAMESIZE 2))
                (< y (/ GAMESIZE 2)))
           (which-bounce (- GAMESIZE x) y dx dy)]
          [(and (< x (/ GAMESIZE 2))
                (> y (/ GAMESIZE 2)))
           (which-bounce x (- GAMESIZE y) dx dy)]
          [(and (> x (/ GAMESIZE 2))
                (> y (/ GAMESIZE 2)))
           (which-bounce (- GAMESIZE x) (- GAMESIZE y) dx dy)])))

; Number Number -> Posn
; Checks which velocity component is to be bounced and returns the new velocity.
(define (which-bounce x y dx dy)
  (if (< x y)
      (make-posn (- dx) dy)
      (make-posn dx (- dy))))

; Game -> Posn
; Move the ship.
(define (new-posn game)
  (local (; position and velocity of ship:
          (define posn (ship-posn (game-ship game)))
          (define vel (ship-vel (game-ship game))))
    (make-posn
     (pls-dont-crash (+ (posn-x posn) (* BOOST (posn-x vel))))
     (pls-dont-crash (+ (posn-y posn) (* BOOST (posn-y vel)))))))

; Game -> Number
; Decrease life if radiation zone or collision.
(define (damage game)
  (local (; life and posn
          (define life (ship-life (game-ship game)))
          (define posn (ship-posn (game-ship game)))
          (define enemies (game-enemies game)))
    (cond [(or (not (<= MIN-LIM (posn-x posn) MAX-LIM))
               (not (<= MIN-LIM (posn-y posn) MAX-LIM)))
           (- life RADDAMMIT)]
          [(object-on-any-enemy? posn enemies)
           (- life (enemy-damages-ship game))]
          [else life])))

; Game -> Number
; Decreases life according to which one is the closest enemy.
(define (enemy-damages-ship game)
  (local (; ship posn:
          (define posn (ship-posn (game-ship game)))
          ; closest enemy:
          (define closest
            (enemy-name
             (first
              (filter (lambda (enemy)
                        (EmanP
                        (enemy-posn enemy)
                        posn
                        (select-enemy-radius enemy)))
                      (game-enemies game))))))
    (cond [(string=? closest GLORB)
           GLORB-DMG]
          [(string=? closest SPRITE)
           SPRITE-DMG]
          [else ASTEROID-DMG])))

; Game -> Number
; Check for game conditions that trigger ship damage animation
(define (trigger-damage game)
  (local (; posn and enemies
          (define posn (ship-posn (game-ship game)))
          (define enemies (game-enemies game)))
    (if (or (not (<= MIN-LIM (posn-x posn) MAX-LIM))
            (not (<= MIN-LIM (posn-y posn) MAX-LIM))
            (object-on-any-enemy? posn enemies))
        1
        0)))

; Enemy -> Number
; Given an enemy, returns the radius of its sprite.
(define (select-enemy-radius enemy)
  (local (; enemy name:
          (define n (enemy-name enemy)))
    (cond [(string=? n GLORB)
           (r IMM-GLORB-1)]
          [(string=? n SPRITE)
           (r IMM-SPRITE)]
          [(string=? n ASTEROID-A)
           (r IMM-ASTEROID-B)]
          [(string=? n ASTEROID-B)
           (r IMM-ASTEROID-B)]
          [(string=? n ASTEROID-C)
           (r IMM-ASTEROID-C)]
          [(string=? n ASTEROID-X)
           (r IMM-ASTEROID-X)])))

; update shots:
; Game -> List<Shots>
; Update shots and remove those that are too far.
(define (update-shots game)
  (local (; Shots
          (define shots (game-shots game))
          ; Check if a shot is in the range of an enemy
          (define (shot-penetration? shot)
            (local (; posn of shot
                    (define s-posn (shot-posn shot)))
              (not (object-on-any-enemy? s-posn (game-enemies game))))))
     (filter (lambda (shot)
               (and (<= 0 (posn-x (shot-posn shot)) GAMESIZE)
                    (<= 0 (posn-y (shot-posn shot)) GAMESIZE)))
             (map update-single-shot
                  (filter shot-penetration? shots)))))

; Shot -> Shot
; Update position of single shot according to angle.
(define (update-single-shot shot)
  (make-shot
   (make-posn (+ (posn-x (shot-posn shot))
                 (* (+ SHOTSPEED (shot-speed shot))
                    SHOTSPEED
                    (pls-dont-crash
                     (sin (degrees->radians (+ (shot-ang shot) 180))))))
              (+ (posn-y (shot-posn shot))
                 (* (+ SHOTSPEED (shot-speed shot))
                    SHOTSPEED
                    (pls-dont-crash
                     (cos (degrees->radians (+ (shot-ang shot) 180)))))))
   (shot-ang shot)
   (shot-speed shot)))

; update enemies:
; Game -> List<Enemy>
; Given a list of enemies, moves them according to their nature.
(define (update-enemies game)
  (local (; Enemies:
          (define enemies (game-enemies game))
          ; Ship:
          (define ship (game-ship game))
          ; Shots
          (define shots (game-shots game))
          ; Remove dead enemies:
          (define (remove-dead-enemies enemies)
            (filter (lambda (enemy)
                      (< 0 (enemy-life enemy)))
                    enemies)))
    (remove-dead-enemies
     (map (lambda (enemy)
            (cond [(string=? (enemy-name enemy) GLORB)
                   (update-glorb enemy
                                 (ship-posn ship)
                                 shots
                                 (game-level game))]
                  [(string=? (enemy-name enemy) SPRITE)
                   (random-move enemy SPRITE-VEL)]
                  [(string=? (enemy-name enemy) ASTEROID-A)
                   (hit-asteroid enemy shots RES-ASTEROID-A)]
                  [(string=? (enemy-name enemy) ASTEROID-B)
                   (hit-asteroid enemy shots RES-ASTEROID-B)]
                  [(string=? (enemy-name enemy) ASTEROID-C)
                   (hit-asteroid enemy shots RES-ASTEROID-C)]
                  [(string=? (enemy-name enemy) ASTEROID-X)
                   (hit-asteroid enemy shots RES-ASTEROID-X)]
                  [else enemy]))
          enemies))))

; Enemy Posn List<Shot> Number -> Enemy
; Move glorb randomly if not inside window, else towards ship, and rotate it
; towards moving direction.
(define (update-glorb enemy s-posn shots level)
  (cond [(ormap (lambda (shot)
                  (EmanP (enemy-posn enemy)
                         (shot-posn shot)
                         (select-enemy-radius enemy)))
                shots)
         (make-enemy (enemy-name enemy)
                     (enemy-posn enemy)
                     (enemy-ang enemy)
                     (- (enemy-life enemy) (/ SHOT-DMG RES-GLORB))
                     DAMAGE-FLASH)]
        [(and (<= (- (posn-x s-posn)
                     (* WINSIZE GLORB-RANGE))
                  (posn-x (enemy-posn enemy))
                  (+ (posn-x s-posn)
                     (* WINSIZE GLORB-RANGE)))
              (<= (- (posn-y s-posn)
                     (* WINSIZE GLORB-RANGE))
                  (posn-y (enemy-posn enemy))
                  (+ (posn-y s-posn)
                     (* WINSIZE GLORB-RANGE))))
         (move-glorb-towards-ship enemy s-posn level)]
        [else (random-move enemy GLORB-VEL)]))

; Enemy Posn Number -> Enemy
; Update posn of glorb making the enemy move towards the ship
; (faster every level).
(define (move-glorb-towards-ship enemy s-posn level)
  (local (; rotation of the enemy:
          (define ang (enemy-ang enemy))
          ; x and y coordinates of glorb relative to ship posn:
          (define rel-x (- (posn-x (enemy-posn enemy)) (posn-x s-posn)))
          (define rel-y (- (posn-y (enemy-posn enemy)) (posn-y s-posn))))
    (make-enemy "glorb"
                (make-posn (+ (posn-x (enemy-posn enemy))
                              (pls-dont-crash
                               (* (- (sin (degrees->radians ang)))
                                  (+ GLORB-VEL (round (sqrt level))))))
                           (+ (posn-y (enemy-posn enemy))
                              (pls-dont-crash
                               (* (- (cos (degrees->radians ang)))
                                  (+ GLORB-VEL (round (sqrt level)))))))
                (adjust-rotation rel-x rel-y)
                (enemy-life enemy)
                (if (zero? (enemy-aron enemy))
                    0
                    (sub1 (enemy-aron enemy))))))

; Posn Posn Num Num -> Num
; Rotate glorb towards ship.
(define (adjust-rotation x y)
  (radians->degrees
    (cond [(zero? y)
           (/ pi 2)]
          [(> y 0)
           (atan (/ x y))]
          [else (+ pi (atan (/ x y)))])))

; Enemy Number -> Enemy
; Move enemy in ang direction by velocity, rotating it and making it reenter the
; gamescene at a random angle when it goes outside the game borders.
(define (random-move enemy velocity)
  (make-enemy (enemy-name enemy)
              (just-move-it enemy velocity)
              (random-bounce-from-deep-space enemy)
              (enemy-life enemy)
              (if (zero? (enemy-aron enemy))
                  0
                  (sub1 (enemy-aron enemy)))))

; Enemy Number -> Posn
; Move the enemy in direction ang by SPRITE-VEL.
(define (just-move-it enemy velocity)
  (make-posn (+ (posn-x (enemy-posn enemy))
                (pls-dont-crash
                 (* (- (sin (degrees->radians (enemy-ang enemy))))
                    velocity)))
             (+ (posn-y (enemy-posn enemy))
                (pls-dont-crash
                 (* (- (cos (degrees->radians (enemy-ang enemy))))
                    velocity)))))

; Enemy -> Num
; Bounce the enemy back into the game at a random angle if it goes outside.
(define (random-bounce-from-deep-space enemy)
  (local (; x and y of enemy:
          (define x (posn-x (enemy-posn enemy)))
          (define y (posn-y (enemy-posn enemy)))
          (define min (- DEEPSPACE))
          (define max (+ DEEPSPACE GAMESIZE)))
  (cond [(< x min)
         (+ (random 180) 180)]
        [(> x max)
         (random 180)]
        [(< y min)
         (+ (random 180) 90)]
        [(> y max)
         (modulo (- (random 180) 90) 360)]
        [else (enemy-ang enemy)])))

; Enemy List<Shot> Number -> Enemy
; Decrease life of asteroid according to resistance if hit by a shot.
(define (hit-asteroid enemy shots resistance)
  (if (ormap (lambda (shot)
                  (EmanP (enemy-posn enemy)
                         (shot-posn shot)
                         (select-enemy-radius enemy)))
                shots)
      (make-enemy (enemy-name enemy)
                  (enemy-posn enemy)
                  (enemy-ang enemy)
                  (- (enemy-life enemy) (/ SHOT-DMG resistance))
                  DAMAGE-FLASH)
      (make-enemy (enemy-name enemy)
                  (enemy-posn enemy)
                  (enemy-ang enemy)
                  (enemy-life enemy)
                  (if (zero? (enemy-aron enemy))
                      0
                      (sub1 (enemy-aron enemy))))))



; Posn List<Enemy> -> Boolean
; Return #true if the object at s-posn is on one of the enemies.
(define (object-on-any-enemy? s-posn enemies)
  (local (; Enemy name:
          (define (fn enemy) (enemy-name enemy)))
    (ormap (lambda (enemy)
             (local (; name n of precise enemy:
                     (define n (fn enemy)))
               (cond [(string=? n ASTEROID-A)
                      (EmanP s-posn
                             (enemy-posn enemy)
                             (r IMM-ASTEROID-A))]
                     [(string=? n ASTEROID-B)
                      (EmanP s-posn
                             (enemy-posn enemy)
                             (r IMM-ASTEROID-B))]
                     [(string=? n ASTEROID-C)
                      (EmanP s-posn
                             (enemy-posn enemy)
                             (r IMM-ASTEROID-C))]
                     [(string=? n ASTEROID-X)
                      (EmanP s-posn
                             (enemy-posn enemy)
                             (r IMM-ASTEROID-X))]
                     [(string=? n GLORB)
                      (EmanP s-posn
                             (enemy-posn enemy)
                             (r IMM-GLORB-1))]
                     [(string=? n SPRITE)
                      (EmanP s-posn
                             (enemy-posn enemy)
                             (r IMM-SPRITE))]
                     [else #false])))
             enemies)))

; Posn Posn Number -> Boolean
; Given two posns, return true if the distance between them is less than r.
; Thanks to EmanP.
(define (EmanP s-posn e-posn r)
  (<= (sqrt (+ (sqr (abs (- (posn-x s-posn) (posn-x e-posn))))
               (sqr (abs (- (posn-y s-posn) (posn-y e-posn))))))
      r))

;_____________________
; Game -> Game
; Do nothing and wait for the player to click a button
; (handled by on-mouse and on-key).
(define (pauseadvance game)
  game)

;_____________________
; Game -> Game
; Wait for the player to click a button (handled by on-mouse and on-key) and
; return to home if the player fails to choose in 10 seconds.
(define (gameoveradvance game)
  (if (zero? (game-ticker game))
      START
      (make-game (game-ship game)
             (game-shots game)
             (game-lives game)
             (game-enemies game)
             (game-HPPD game)
             (game-score game)
             (game-level game)
             (sub1 (game-ticker game)))))

;_______________________________________________________________________________
; on-key

; Game KeyEvent -> Game
; Handles keyboard input for the four different states of the game.
(define (keyboard game key)
  (cond [(string=? (game-HPPD game) "home") (home-keyboard game key)]
        [(string=? (game-HPPD game) "playing") (playing-keyboard game key)]
        [(string=? (game-HPPD game) "pause") (pause-keyboard game key)]
        [(string=? (game-HPPD game) "dead") (gameover-keyboard game key)]
        [else game]))

;_____________________
; Game KeyEvent -> Game
; Does nothing.
(define (home-keyboard game key)
  game)

;_____________________
; Game KeyEvent -> Game
; Handles key input while playing. " " to shoot and "escape" to pause game.
(define (playing-keyboard game key)
  (cond [(string=? key " ")
         (shoot game)]
        [(string=? key "escape")
         (pause game)]
        [else game]))

; Game -> Game
; Checks if there's enough energy, if yes calls function to add a shoot and
; removes energy from ship.
(define (shoot game)
  (if (>= (ship-energy (game-ship game)) SHOOT)
             (make-game
              (make-ship
               (ship-posn (game-ship game))
               (ship-ang (game-ship game))
               (ship-vel (game-ship game))
               (ship-life (game-ship game))
               (- (ship-energy (game-ship game)) SHOOT)
               (ship-jacopo (game-ship game)))
              (add-shot game)
              (game-lives game)
              (game-enemies game)
              (game-HPPD game)
              (game-score game)
              (game-level game)
              (game-ticker game))
             game))

; Game -> Shots
; Adds a shot to the game.
(define (add-shot game)
  (local (; left or right shot:
          (define lr (modulo (game-ticker game) 2)))
    (if (zero? lr)
        (cons (make-shot
               (make-posn (+ (posn-x (ship-posn (game-ship game)))
                             (* (- (pls-dont-crash
                                    (sin (degrees->radians
                                          (+ (ship-ang (game-ship game))
                                             45)))))
                                13))
                          (+ (posn-y (ship-posn (game-ship game)))
                             (* (- (pls-dont-crash
                                    (cos (degrees->radians
                                          (+ (ship-ang (game-ship game))
                                             45)))))
                                13)))
               (ship-ang (game-ship game))
               (sqrt (+ (sqr (posn-x (ship-vel (game-ship game))))
                        (sqr (posn-y (ship-vel (game-ship game)))))))
              (game-shots game))
        (cons (make-shot
               (make-posn (+ (posn-x (ship-posn (game-ship game)))
                             (* (- (pls-dont-crash
                                    (sin (degrees->radians
                                          (- (ship-ang (game-ship game))
                                             45)))))
                                13))
                          (+ (posn-y (ship-posn (game-ship game)))
                             (* (- (pls-dont-crash
                                    (cos (degrees->radians
                                          (- (ship-ang (game-ship game))
                                             45)))))
                                13)))
               (ship-ang (game-ship game))
               (sqrt (+ (sqr (posn-x (ship-vel (game-ship game))))
                        (sqr (posn-y (ship-vel (game-ship game)))))))
              (game-shots game)))))

; Game -> Game
; Changes HPPD from "playing" to "pause".
(define (pause game)
  (make-game (game-ship game)
             (game-shots game)
             (game-lives game)
             (game-enemies game)
             (if (string=? (game-HPPD game) "playing")
                 "pause"
                 "playing")
             (game-score game)
             (game-level game)
             (game-ticker game)))

;_____________________
; Game KeyEvent -> Game
; Handles kay input during pause. "escape" to unpause game.
(define (pause-keyboard game key)
  (cond [(string=? key "escape")
         (pause game)]
        [else game]))

;_____________________
; Game KeyEvent -> Game
; Do nothing.
(define (gameover-keyboard game key)
  game)

;_______________________________________________________________________________
; on-mouse

; Game Num Num MouseEvent -> Game
; Handles mouse input for the four different states of the game.
(define (mouse game mx my event)
  (cond [(string=? (game-HPPD game) "home") (home-mouse game mx my event)]
        [(string=? (game-HPPD game) "playing") (playing-mouse game mx my event)]
        [(string=? (game-HPPD game) "pause") (pause-mouse game mx my event)]
        [(string=? (game-HPPD game) "dead") (gameover-mouse game mx my event)]
        [(string=? (game-HPPD game) "ins") (instruction-mouse game mx my event)]
        [else game]))

; generalised button clicks

; Num[1,3] Num Num MousEvent -> Boolean
; Returns #true if button-down on button n.
(define (button-click n mx my event)
  (if (string=? event "button-down")
      (cond [(and (= n 1)
                  (<= BUTTON-X mx (+ BUTTON-X BUTTON-WIDTH))
                  (<= BUTTON-Y-1 my (+ BUTTON-Y-1 BUTTON-HEIGHT)))
             #true]
            [(and (= n 2)
                  (<= BUTTON-X mx (+ BUTTON-X BUTTON-WIDTH))
                  (<= BUTTON-Y-2 my (+ BUTTON-Y-2 BUTTON-HEIGHT)))
             #true]
            [(and (= n 3)
                  (<= BUTTON-X mx (+ BUTTON-X BUTTON-WIDTH))
                  (<= BUTTON-Y-3 my (+ BUTTON-Y-3 BUTTON-HEIGHT)))
             #true]
            [(and (= n 4)
                  (<= BUTTON-X-? mx (+ BUTTON-X-? BUTTON-HEIGHT))
                  (<= BUTTON-Y-? my (+ BUTTON-Y-? BUTTON-HEIGHT)))
             #true]
            [(and (= n 5)
                  (<= BUTTON-X-CLOSE mx (+ BUTTON-X-CLOSE BUTTON-HEIGHT))
                  (<= BUTTON-Y-CLOSE my (+ BUTTON-Y-CLOSE BUTTON-HEIGHT)))
             #true]
            [else false])
      #false))

; Going home
; Game -> Game
(define (goto-home game)
  (make-game (game-ship game)
             (game-shots game)
             (game-lives game)
             (game-enemies game)
             "home"
             (game-score game)
             (game-level game)
             (game-ticker game)))

;_____________________
; Game Num Num MouseEvent -> Game
; Determines event and calls functions on "button-down" for clicks in two/three
; different areas on the homescreen (continue game if not dead, new game, exit).
(define (home-mouse game mx my event)
  (if (and (< 0 (game-lives game)) (< 0 (game-level game)))
      (cond [(button-click 1 mx my event)
             (pause game)]
            [(button-click 2 mx my event)
             (generate-random-level START)]
            [(button-click 3 mx my event)
             (exit-from-everything game)]
            [(button-click 4 mx my event)
             (goto-ins game)]
            [else game])
      (cond [(button-click 2 mx my event)
             (generate-random-level START)]
            [(button-click 3 mx my event)
             (exit-from-everything game)]
            [(button-click 4 mx my event)
             (goto-ins game)]
            [else game])))

; Game -> Game
; Returns game that triggers stop-when condition.
(define (exit-from-everything game)
  (make-game (game-ship game)
             (game-shots game)
             (game-lives game)
             (game-enemies game)
             "exit"
             (game-score game)
             (game-level game)
             (game-ticker game)))

; Game -> Game
(define (goto-ins game)
  (make-game (game-ship game)
             (game-shots game)
             (game-lives game)
             (game-enemies game)
             "ins"
             (game-score game)
             (game-level game)
             (game-ticker game)))

;_____________________
; Game Num Num MouseEvent -> Game
; Determines event and calls functions on "move" and on "button-down".
(define (playing-mouse game mx my event)
  (make-game
   (cond [(string=? event "move")
          (angle-of-ship (game-ship game) mx my)]
         [(string=? event "button-down")
          (if (>= (ship-energy (game-ship game)) MOVE)
              (add-velocity-ship (game-ship game) mx my)
              (game-ship game))]
         [else (game-ship game)])
   (game-shots game)
   (game-lives game)
   (game-enemies game)
   (game-HPPD game)
   (game-score game)
   (game-level game)
   (game-ticker game)))

; move
; Ship Number Number -> Ship
; Return new ship with updated angle according to position of mouse.
(define (angle-of-ship ship mx my)
  (local (; a and b coordinates of mouse direction vector
          (define a (- mx (/ WINSIZE 2)))
          (define b (- (- my (/ WINSIZE 2)))))
    (make-ship
     (ship-posn ship)
     (angle-from-vector a b)
     (ship-vel ship)
     (ship-life ship)
     (ship-energy ship)
     (ship-jacopo ship))))

; Number Number -> Number
; Given two coordinates of a vector, returns the vector's angle.
(define (angle-from-vector a b)
  (local (; angle using good 'ol trigonometry:
          (define angle
            (if (and (zero? a) (zero? b))
                0
                (pls-dont-crash
                 (radians->degrees (acos (/ b (sqrt (+ (sqr a)
                                                       (sqr b))))))))))
    (if (= a 0)
        (if (positive? b) 0 180)
        (if (positive? a) (- angle) angle))))

; button-down
; Ship Number Number -> Ship
; Returns new ship with added velocity.
(define (add-velocity-ship ship mx my)
  (local (; a and b coordinates of mouse direction vector:
          (define a (+ (- mx (/ WINSIZE 2)) (posn-x (ship-vel ship))))
          (define b (+ (- my (/ WINSIZE 2)) (posn-y (ship-vel ship))))
          ; normalize velocity and create velocity vector new-vel:
          (define tot (+ (abs a) (abs b)))
          (define new-vel (make-posn (/ a tot) (/ b tot))))
    (make-ship
     (ship-posn ship)
     (ship-ang ship)
     (make-posn (pls-dont-crash (+ (posn-x new-vel)
                                   (posn-x (ship-vel ship))))
                (pls-dont-crash (+ (posn-y new-vel)
                                   (posn-y (ship-vel ship)))))
     (ship-life ship)
     (- (ship-energy ship) MOVE)
     (ship-jacopo ship))))

;_____________________
; Game Num Num MouseEvent -> Game
; Determines event and calls functions on "button-down" for clicks in two
; different areas in the pausescreen (resume, exit to main menu).
(define (pause-mouse game mx my event)
  (cond [(button-click 1 mx my event)
         (pause game)]
        [(button-click 2 mx my event)
         (goto-home game)]
        [else game]))

;_____________________
; Game Num Num MouseEvent -> Game
; Determines event and calls functions on "button-down" for clicks in one/two
; different areas in the gameoverscreen (continue from last level if available,
; exit to main menu).
(define (gameover-mouse game mx my event)
  (if (< 0 (game-lives game))
      (cond [(button-click 1 mx my event)
             (generate-random-level (last game))]
            [(button-click 2 mx my event)
             START]
            [else game])
      (cond [(button-click 2 mx my event)
             START]
            [else game])))

; Game -> Game
; Returns a game with a lower level so that the new level will have the same
; level as the last played.
(define (last game)
  (make-game (make-ship PORTAL-OUT
                        -45
                        (make-posn 0 0)
                        (ship-life (game-ship game))
                        (ship-energy (game-ship game))
                        (ship-jacopo (game-ship game)))
             (game-shots game)
             (game-lives game)
             (make-list
              (+ BASE-ENEMIES (round (/ (sqr (game-level game)) 2))) 0)
             (game-HPPD game)
             (game-score game)
             (sub1 (game-level game))
             (game-ticker game)))

;_____________________
; Game Num Num MouseEvent -> Game
; Determines event and calls functions on "button-down" for clicks in one
; area in the instructionscreen (close window).
(define (instruction-mouse game mx my event)
  (cond [(button-click 5 mx my event)
         (goto-home game)]
        [else game]))

;_______________________________________________________________________________
; stop-when
; Game -> Boolean
(define (exit-game game)
  (cond [(string=? (game-HPPD game) "home") (home-exit game)]
        [(string=? (game-HPPD game) "playing") (playing-exit game)]
        [(string=? (game-HPPD game) "pause") (pause-exit game)]
        [(string=? (game-HPPD game) "dead") (gameover-exit game)]
        [else #false]))

;_____________________
; Game -> Boolean
; Exit game when clicking the exit button.
(define (home-exit game)
  (string=? (game-HPPD game) "exit"))

;_____________________
; Game -> Boolean
; Exit game when clicking the exit button.
(define (playing-exit game)
  #false)

;_____________________
; Game -> Boolean
; Exit game when clicking the exit button.
(define (pause-exit game)
  #false)

;_____________________
; Game -> Boolean
; Exit game when clicking the exit button.
(define (gameover-exit game)
  #false)
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Big bang:

;_______________________________________________________________________________
; Level generator:
(define (generate-random-level game)
  (local (; number of enemies killed in the last game and number of new enemies:
          (define killed ; number of enemies generated minus remaining enemies
            (- (+ BASE-ENEMIES (round (expt (game-level game) 1.7)))
               (length (game-enemies game))))
          (define new
            (+ BASE-ENEMIES (round (expt (add1 (game-level game)) 1.7))))
          ; flag: #true if generating after death:
          (define flag (<= (ship-life (game-ship game)) 0)))
    (make-game (make-ship PORTAL-OUT
                          (ship-ang (game-ship game))
                          (ship-vel (game-ship game))
                          (if flag
                           LIFE
                           (ship-life (game-ship game)))
                          ENERGY
                          (ship-jacopo (game-ship game)))
               '()
               (game-lives game)
               (enemy-gen '() new)
               "playing"
               (if flag
                   (game-score game)
                   (inexact->exact
                    (+ (game-score game)
                       (* 100 killed) ; a hundred points per kill
                       (* (game-level game) 1000)))) ; because level
                   (add1 (game-level game))
                   0)))

; Initial state
(define START
  (make-game (make-ship PORTAL-OUT
                        -45
                        (make-posn 0 0)
                        LIFE
                        ENERGY
                        0)
             '()
             3
             '()
             "home"
             -2500
             0
             0))

;_______________________________________________________________________________
; main
(define (main _)
  (big-bang START
            [to-draw render]
            [on-tick advance GAMESPEED]
            [on-key keyboard]
            [on-mouse mouse]
            [stop-when exit-game]))

; Run the game
(main 0)
(test)

; TODO list:
; 
; X Place radiation image above spaceship.
; X Include Portal (start) sprite.
; X Place Portal (start) sprite at spaceship location below spaceship.
; X Include Portal (destination) sprite.
; X Place Portal (destination) at the other end of game scene.
; X Include Shot sprite.
; X Make the spaceship soot adding a shot to "shots" list:
;   X Place the shot sprite in front of the spaceship with the correct rotation;
;   X Make the shot move at every tick in a straight line after being shot;
;   X Make both the sprite and the shot elment of the list disappear after
;      some ticks.
;   X Determine active hitbox and power of shot.
; X Add enemies:
;  X Include glorg sprite.
;  X Include sprito sprite.
;  X Include asteroid sprite.
;  X Make function that adds some random enemies at random places according to
;    the level of "game".
;  X Glorb:
;   X Animate glorb (sprite sheet cropping).
;   X Make glorb move randomly when not inside window.
;   X Make glorb follow the ship when inside window.
;   X Determine strength, passive and active hitbox.
;  X Sprito:
;   X Animate sprito (rotation).
;   X Make sprito move in one direction.
;   X Make sprito reenter the game-scene.
;   X Determine strength, passive and active hitbox.
;  X Astroid:
;   X Animate astrid (rotation).
;   X Determine strength, passive and active hitbox.
;  X Generate a new level when Portal (destination) is reached by ship.
;  - Home:
;   ...
;  - Pause:
;   ...
;  - Gameover:
;   ...