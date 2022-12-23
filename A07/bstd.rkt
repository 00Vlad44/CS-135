;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname bstd) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(define-struct node (key val left right))

(define sal-top-songs
(list (list 1 "Respect")
(list 3 "A Change is Gonna Come")
(list 7 "Strawberry Fields Forever")
(list 9 "Dreams")
(list 12 "Superstition")
(list 17 "Bohemian Rhapsody")
(list 19 "Imagine")
(list 21 "Strange Fruit")
(list 25 "Runaway")
(list 33 "Johnny B. Goode")))

(define bstd-top-songs
(make-node 12 "Superstition"
(make-node 3 "A Change is Gonna Come"
(make-node 1 "Respect" empty empty)
(make-node 7 "Strawberry Fields Forever"
empty
(make-node 9 "Dreams" empty empty)))
(make-node 21 "Strange Fruit"
(make-node 17 "Bohemian Rhapsody"
empty
(make-node 19 "Imagine" empty empty))
(make-node 25 "Runaway"
empty
(make-node 33 "Johnny B. Goode" empty empty)))))


(define sal1
  (list (list 1 "Jeez")
        (list 2 "HIMM")
        (list 3 "Back of my mind")))

(define sal1-btsd
  (make-node 2 "HIMM"
             (make-node 1 "Jeez" empty empty)
             (make-node 3 "Back of my mind" empty empty)))

;;Examples 2a
(check-expect (build-bstd sal-top-songs)  bstd-top-songs)
(check-expect (build-bstd sal1)  sal1-btsd)



;; Examples 2b
(check-expect (range-query bstd-top-songs 3 18)
'("A Change is Gonna Come" "Strawberry Fields Forever" "Dreams"
"Superstition" "Bohemian Rhapsody"))

(check-expect (range-query bstd-top-songs 3 10)
'("A Change is Gonna Come" "Strawberry Fields Forever" "Dreams"))

(check-expect (range-query sal1-btsd 1 2)
'("Jeez" "HIMM"))

(check-expect (range-query sal1-btsd 1 1)
'("Jeez"))




