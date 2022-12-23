;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname behaviour-trees) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(define-struct cnode (type id children))

(define npc1-through-window
  (make-cnode 'Sequence 1 (list "Walk to Window"
                                "Open Window"
                                "Climb through Window"
                                "Turn Around"
                                "Close Window")))

(define npc1-through-steal
  (make-cnode 'Sequence 1 (list "Walk to Window"
                                "Throw Brick"
                                "Take Items"
                                "Run"
                                "No Face No Case")))


;;Exmaples a
(check-expect (action-exists? npc1-through-window "Open Door") false)
(check-expect (action-exists? npc1-through-window "Open Window") true)
(check-expect (action-exists? npc1-through-steal "Ski Mask") false)
(check-expect (action-exists? npc1-through-steal "No Face No Case") true)

;;Examples b
(check-expect (summarize-bt npc1-through-window)
"(Walk to Window and Open Window and Climb through Window and \
Turn Around and Close Window)")

(check-expect (summarize-bt npc1-through-steal)
"(Walk to Window and Throw Brick and Take Items and \
Run and No Face No Case)")


;;Examples c
(check-expect
 (add-action npc1-through-window 1 "Look through Window" 2)
 (make-cnode 'Sequence 1 (list "Walk to Window"
                               "Look through Window"
                               "Open Window"
                               "Climb through Window"
                               "Turn Around"
                               "Close Window")))
(check-expect
 (add-action npc1-through-steal 1 "Look through Window" 2)
 (make-cnode 'Sequence 1 (list "Walk to Window"
                               "Look through Window"
                                "Throw Brick"
                                "Take Items"
                                "Run"
                                "No Face No Case")))

(check-expect
 (add-action npc1-through-steal 1 "GG15" 3)
 (make-cnode 'Sequence 1 (list "Walk to Window"
                                "Throw Brick"
                                "GG15"
                                "Take Items"               
                                "Run"   
                                "No Face No Case")))

(check-expect
 (add-action
  (make-cnode 'Selector 10 (list "Walk Left" "Walk Right"))
  10
  npc1-through-steal
  1)
 (make-cnode 'Selector 10
             (list (make-cnode 'Sequence 1
                               (list "Walk to Window"
                                "Throw Brick"
                                "Take Items"
                                "Run"
                                "No Face No Case"))
                   "Walk Left"
                   "Walk Right")))

;;Examples d
(check-expect (rewind npc1-through-window)
              (list "Close Window" "Turn Around"
                    "Climb through Window"
                    "Open Window" "Walk to Window"))

(check-expect (rewind npc1-through-steal)
              (list "No Face No Case" "Run"
                    "Take Items"
                    "Throw Brick" "Walk to Window"))




