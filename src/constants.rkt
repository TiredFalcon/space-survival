#lang racket
;###############################################################################
;#################### PROGRAMMING FUNDAMENTALS I - PROJECT #####################

; Authors: Jacopo Fidacaro & Aron Fiechter
; Version.


; Libraries: 
(require 2htdp/image)
(require lang/posn)

; - Screen:
(define WINSIZE 720)
(define WINDOW (empty-scene WINSIZE WINSIZE))
(define BLANKWINDOW (rectangle WINSIZE WINSIZE 0 "black"))

; - Game speed:
(define GAMESPEED 0.06)

; - Path of executable:
(define PATH
  (substring (path->string (find-system-path 'run-file))
             0
             (- (string-length (path->string (find-system-path 'run-file)))
                14)))

;_______________________________________________________________________________
; BUTTONS

; - Button aspect:
(define BUTTON-COLOR "black")
(define BUTTON-BORDER "firebrick")
(define TEXT-COLOR "white")
(define DIMMED "DimGray")
(define BUTTON-WIDTH 170)
(define BUTTON-HEIGHT 50)
(define TEXT-SIZE 30)
(define BUTTON
  (overlay
   (rectangle (- BUTTON-WIDTH 3) (- BUTTON-HEIGHT 3) 255 BUTTON-COLOR)
   (rectangle BUTTON-WIDTH BUTTON-HEIGHT 255 BUTTON-BORDER)))
(define ?-BUTTON
  (overlay
   (rectangle (- BUTTON-HEIGHT 3) (- BUTTON-HEIGHT 3) 255 BUTTON-COLOR)
   (rectangle BUTTON-HEIGHT BUTTON-HEIGHT 255 BUTTON-BORDER)))

; - Coordinates of buttons:
(define MIDDLE (/ WINSIZE 2))
(define SPACING 5)
(define BUTTON-X (- MIDDLE (/ BUTTON-WIDTH 2)))
(define BUTTON-X-? (- MIDDLE (/ BUTTON-HEIGHT 2)))
(define BUTTON-X-CLOSE (- WINSIZE BUTTON-HEIGHT SPACING))
(define BUTTON-Y-1 MIDDLE)
(define BUTTON-Y-2 (+ BUTTON-Y-1 BUTTON-HEIGHT SPACING))
(define BUTTON-Y-3 (+ BUTTON-Y-2 BUTTON-HEIGHT SPACING))
(define BUTTON-Y-? (+ BUTTON-Y-3 BUTTON-HEIGHT SPACING))
(define BUTTON-Y-CLOSE SPACING)
(define BUTTON-1 (make-posn BUTTON-X BUTTON-Y-1))
(define BUTTON-2 (make-posn BUTTON-X BUTTON-Y-2))
(define BUTTON-3 (make-posn BUTTON-X BUTTON-Y-3))
(define BUTTON-? (make-posn BUTTON-X-? BUTTON-Y-?))
                                  
; - Functions to create and place buttons
(define (create-button text color)
  (overlay
   (text/font text TEXT-SIZE color
              #f 'modern 'normal 'bold #f)
   (if (= (string-length text) 1)
       ?-BUTTON
       BUTTON)))

; Examples:
(define CONTINUE-BUTTON (create-button "CONTINUE" TEXT-COLOR))
(define DIMMED-CONTINUE-BUTTON (create-button "CONTINUE" DIMMED))
(define NEW-GAME-BUTTON (create-button "NEW GAME" TEXT-COLOR))
(define EXIT-BUTTON (create-button "EXIT" TEXT-COLOR))
(define RESUME-BUTTON (create-button "RESUME" TEXT-COLOR))
(define MENU-BUTTON (create-button "MENU" TEXT-COLOR))
(define RETRY-BUTTON (create-button "RETRY" TEXT-COLOR))
(define DIMMED-RETRY-BUTTON (create-button "RETRY" DIMMED))
(define INSTUCTIONS-BUTTON (create-button "?" TEXT-COLOR))
(define CLOSE-BUTTON (create-button "X" TEXT-COLOR))

;_______________________________________________________________________________
; HOME

; - Background:
(define HOME-BACKGROUND
  (bitmap/file (string-append PATH "sprites/background_2800.png")))

; - Logo and position:
(define LOGO (bitmap/file (string-append PATH "sprites/logo.png")))
(define LOGO-POSN (make-posn (- MIDDLE (/ (image-width LOGO) 2)) 70))

; - Homescreeen:
(define HOME-SCREEN
  (place-images/align
   (list CONTINUE-BUTTON NEW-GAME-BUTTON EXIT-BUTTON INSTUCTIONS-BUTTON LOGO)
   (list BUTTON-1 BUTTON-2 BUTTON-3 BUTTON-? LOGO-POSN)
   "left"
   "top"
   HOME-BACKGROUND))

(define INITIAL-HOME-SCREEN
  (place-images/align
   (list DIMMED-CONTINUE-BUTTON NEW-GAME-BUTTON EXIT-BUTTON INSTUCTIONS-BUTTON
         LOGO)
   (list BUTTON-1 BUTTON-2 BUTTON-3 BUTTON-? LOGO-POSN)
   "left"
   "top"
   HOME-BACKGROUND))

;_______________________________________________________________________________
; PLAYING

; - Portal sprite:
(define PORTAL (bitmap/file (string-append PATH "sprites/portal_64.png")))
(define PORTALO (bitmap/file (string-append PATH "sprites/portalo_64.png")))
(define PORTAL-BASE (circle 80 0 "black"))

; - Space sprite:
(define SPACE (bitmap/file (string-append PATH "sprites/background_2800_2.jpg")))
(define GAMESIZE (image-height SPACE))

; - Ratiation sprite
(define RAD (bitmap/file (string-append PATH "sprites/rad_2800_2.png")))

; - Background sprite (with added portals):
(define SCENE (underlay/xy SPACE
                           2236 500
                           PORTAL))

; - Scene limits:
(define DEEPSPACE 64)
(define MIN-LIM (* WINSIZE 3/5))
(define MAX-LIM (- (image-height SCENE) (* WINSIZE 3/5)))

; - Ship sprite:
(define SHIP
  (crop 0 0 32 32 (bitmap/file (string-append PATH "sprites/n-8_32_ss.png"))))
(define SHIP-DMG
  (crop 32 0 32 32 (bitmap/file (string-append PATH "sprites/n-8_32_ss.png"))))

; - Shot sprite:
(define SHOT (bitmap/file (string-append PATH "sprites/lasershot_16.png")))

; - Enemies sprites:
(define GLORB "glorb")
(define IMM-GLORB-1
  (crop 0 0 64 64 (bitmap/file (string-append PATH "sprites/glorb_64_ss.png"))))
(define IMM-GLORB-2
  (crop 64 0 64 64 (bitmap/file (string-append PATH "sprites/glorb_64_ss.png"))))
(define IMM-GLORB-D
  (crop 128 0 192 64 (bitmap/file (string-append PATH "sprites/glorb_64_ss.png"))))
(define SPRITE "sprite")
(define IMM-SPRITE (bitmap/file (string-append PATH "sprites/sprito_32.png")))
(define ASTEROID-A "asteroid-a")
(define IMM-ASTEROID-A (bitmap/file (string-append PATH "sprites/astrid_32.png")))
(define ASTEROID-B "asteroid-b")
(define IMM-ASTEROID-B (scale 2 IMM-ASTEROID-A))
(define ASTEROID-C "asteroid-c")
(define IMM-ASTEROID-C (scale 4 IMM-ASTEROID-A))
(define ASTEROID-X "asteroid-x")
(define IMM-ASTEROID-X (scale 10 IMM-ASTEROID-A))

; - Technical constants:
(define TOLERANCE 1/100)
(define TICKER 360)
(define PORTAL-OUT (make-posn WINSIZE (- GAMESIZE WINSIZE)))
(define BASE-ENEMIES 25)

; - Spaceship specs:
(define BOOST 4) ; FIXME
(define LIFE 100)
(define ENERGY 100)
(define MOVE (* 0.01 ENERGY))
(define SHOOT (* 0.01 ENERGY))
(define RECHARGE (* 0.001 ENERGY))
(define SHOTSPEED 5.0)
(define SHOT-DMG 50)
(define RADDAMMIT 1)

; - Enemies specs:
(define ELIFE 100)
(define GLORB-VEL 5)
(define GLORB-RANGE 0.45)
(define SPRITE-VEL 40)
(define ASTEROID-DMG 10)
(define SPRITE-DMG 50)
(define GLORB-DMG 2)
(define RES-GLORB 2)
(define RES-ASTEROID-A 1)
(define RES-ASTEROID-B 2)
(define RES-ASTEROID-C 4)
(define RES-ASTEROID-X 10)
(define DAMAGE-FLASH 7)
(define HIT-SHAKE 7)

; - In-game HUD
(define (level-display n)
  (text/font (string-append "Level - " (number->string n)) 15 "white"
             "Gill Sans" 'modern 'normal 'normal #f))
(define (score-display n)
  (text/font (string-append "Score - " (number->string n)) 15 "white"
             "Gill Sans" 'modern 'normal 'normal #f))
(define (lives-display n)
  (text/font (string-append "Lives left - " (number->string n)) 15 "white"
             "Gill Sans" 'modern 'normal 'normal #f))

; - Out-game HUD
(define (level-display-2 n)
  (text/font (string-append "Level - " (number->string n)) 25 "white"
             "Gill Sans" 'modern 'normal 'normal #f))
(define (score-display-2 n)
  (text/font (string-append "Score - " (number->string n)) 25 "white"
             "Gill Sans" 'modern 'normal 'normal #f))
(define (lives-display-2 n)
  (text/font (string-append "Lives left - " (number->string n)) 25 "white"
             "Gill Sans" 'modern 'normal 'normal #f))

;_______________________________________________________________________________
; PAUSE

; - Pausescreeen:
(define PAUSE-SCREEN
  (place-images/align
   (list RESUME-BUTTON MENU-BUTTON)
   (list BUTTON-1 BUTTON-2)
   "left"
   "top"
   HOME-BACKGROUND))

;_______________________________________________________________________________
; DEAD

; - Gameoverscreens:
(define GAMEOVER-SCREEN-1
  (place-images/align
   (list RETRY-BUTTON MENU-BUTTON)
   (list BUTTON-1 BUTTON-2)
   "left"
   "top"
   HOME-BACKGROUND))
(define GAMEOVER-SCREEN-2
  (place-images/align
   (list DIMMED-RETRY-BUTTON MENU-BUTTON)
   (list BUTTON-1 BUTTON-2)
   "left"
   "top"
   HOME-BACKGROUND))

;_______________________________________________________________________________
; INSTRUCTIONS

; - Background:
(define INS-BACKGROUND
  (bitmap/file (string-append PATH "sprites/command_screen.jpg")))

; - Instructionscreen:
(define INSTRUCTION-SCREEN
  (place-image/align
   CLOSE-BUTTON
   BUTTON-X-CLOSE
   BUTTON-Y-CLOSE
   "left"
   "top"
   INS-BACKGROUND))

;_______________________________________________________________________________
; EXIT

; - SOMEONE-REALLY-CLICKED-EXIT-INSTEAD-OF-JUST-CLOSING:
(define SOMEONE-REALLY-CLICKED-EXIT-INSTEAD-OF-JUST-CLOSING
  (bitmap/file (string-append PATH "sprites/exit.png")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide (all-defined-out))