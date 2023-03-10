;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname behaviour-trees) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;;
;; ***************************************************
;; CS 135 Fall 2022
;; Assignment 07, Problem 3
;; ***************************************************
;;

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

;; action-exists?: BT Str -> Bool
(define (action-exists? bt action)
  (cond
    [(cnode? bt) (search-children (cnode-children bt) action)]
    [else (string=? action bt)]))

;; search children: (listof BT) Str -> Bool
(define (search-children lobt action)
  (cond [(empty? lobt) false]
        [else (or (action-exists? (first lobt) action)
                  (search-children (rest lobt) action))]))

;;Examples b
(check-expect (summarize-bt npc1-through-window)
"(Walk to Window and Open Window and Climb through Window and \
Turn Around and Close Window)")

(check-expect (summarize-bt npc1-through-steal)
"(Walk to Window and Throw Brick and Take Items and \
Run and No Face No Case)")

;; summarize-bt: BT -> Str
(define (summarize-bt bt)
  (cond
    [(and (cnode? bt) (symbol=? (cnode-type bt) 'Sequence))
     (process-node (cnode-children bt) "(" " and ")]
    [(and (cnode? bt) (symbol=? (cnode-type bt) 'Selector))
     (process-node (cnode-children bt) "( " " or ")]
    [else bt]))

;; process-node: (ne-listof BT) Str Str -> Str
(define (process-node lobt output-string connector)
  (cond
    [(empty? (rest lobt)) (string-append output-string (summarize-bt (first lobt)) ")")]
    [else (process-node (rest lobt)
                        (string-append output-string
                                       (summarize-bt (first lobt)) connector)
                        connector)]))


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

;; add-action: CNode Nat Str Nat -> CNode
(define (add-action a-cnode id action n)
  (local [(define (index loa action n)
            (cond
              [(empty? loa) empty]
              [(= 1 n) (cons action loa)]   
              [else (cons (first loa)
                          (index (rest loa) action (sub1 n)))]))
          (define (add-act-bt bt id)
            (cond
              [(string? bt) bt]
              [(= id (cnode-id bt))
               (make-cnode (cnode-type bt) id
                           (index (cnode-children bt) action n))]
              [else (make-cnode (cnode-type bt)
                                (cnode-id bt)
                                (add-act-bt (cnode-children bt) id))]))

          (define (add-act-lbt lbt id)
            (cond
              [(empty? lbt) empty]
              [else (cons (add-act-bt (first lbt) id)
                          (add-act-lbt (rest lbt) id))]))]
    (add-act-bt a-cnode id)))
              



;;Examples d
(check-expect (rewind npc1-through-window)
              (list "Close Window" "Turn Around"
                    "Climb through Window"
                    "Open Window" "Walk to Window"))

(check-expect (rewind npc1-through-steal)
              (list "No Face No Case" "Run"
                    "Take Items"
                    "Throw Brick" "Walk to Window"))

;; rewind: BT -> (ne-listof Str)
(define (rewind a-bt)
  (rewind/acc (cnode-children a-bt) empty))

;; rewind/acc: (listof BT) (listof Str) -> (listof Str)
(define (rewind/acc lobt acc)
  (cond
    [(empty? lobt) acc]
    [(string? (first lobt)) (rewind/acc (rest lobt) (cons (first lobt) acc))]
    [(cnode? (first lobt)) (append (rewind/acc (rest lobt) empty)
                                   (rewind/acc (cnode-children (first lobt)) acc))]))