;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hot-dog) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;; Vlad Comsa (21027115)
;; CS 135 Fall 2020
;; Assignment 04, Problem 3
;; ***************************************************
;;


;;PURPOSE;;
;; (contains-hot-dog? list) determines if the
;; list contains the symbol 'hot-dog

;; Examples:
(check-expect (contains-hot-dog? (list 'Hot-dog)) false)
(check-expect (contains-hot-dog? (list 'hot-gdog 32 3 3 4 'hot '-dog)) false)
(check-expect (contains-hot-dog? (list 'Hot--dog)) false)
(check-expect (contains-hot-dog? (list 3 3 2 'hot-dog)) true)
(check-expect (contains-hot-dog? (list 'hot-dog 3 2)) true)

;; Helper function that iterates through the list and finds
;; the keyword 'hot-dog and return true, else false
(define (contains list value)
 (cond
  [(empty? list) false]
  [(and (symbol? (first list)) (symbol=? (first list) value)) true]
  [else (contains (rest list) value)]))

;; contains-hot-dog?: List -> Boolean
(define (contains-hot-dog? list)
  (contains list 'hot-dog))


;; Test Cases:
(contains-hot-dog? (list 3243432 'hot-dog 2))
(contains-hot-dog? (list 'hot-dog))
(contains-hot-dog? (list 3 'hot-dog 3))
(contains-hot-dog? (list 'zoon 344 'hot-dog))
(contains-hot-dog? (list 'pizza 'hot-dog 'hamburger))



;;PURPOSE;;
;; (spells-hot-dog? string) determines if the
;; list has a chars to form "hot dog"

;; Examples:
(check-expect (spells-hot-dog? "abcdefgh too") true)
(check-expect (spells-hot-dog? "Hot Dog!") true)
(check-expect (spells-hot-dog? "hot dg") false)
(check-expect (spells-hot-dog? "GoDThoo") false)


;; Helper function that counts the occurences of ch in loc
;; count-char/list: Char (listof Char) -> Nat
(define (count-char/list ch loc)
  (cond
    [(empty? loc) 0]
    [else (+ (cond
               [(char=? ch (first loc)) 1]
               [else 0])
             (count-char/list ch (rest loc)))]))

;; Wraper function that helps format the input
;; count-char: Char str -> Nat
(define (count-char ch s)
(count-char/list ch (string->list s)))

;; spells-hot-dog?: String -> Boolean
(define (spells-hot-dog? string)
  (cond
    [(and (>= (count-char #\ (string-downcase string))1)
          (>= (count-char #\h (string-downcase string))1)
          (>= (count-char #\o (string-downcase string))2)
          (>= (count-char #\t (string-downcase string))1)
          (>= (count-char #\d (string-downcase string))1)
          (>= (count-char #\g (string-downcase string))1)) true]
    [else false]))

;; Test Cases:
(check-expect (spells-hot-dog? "abcdefgh to") false)
(check-expect (spells-hot-dog? "HotDog!") false)
(check-expect (spells-hot-dog? "hot dgdogsdsddt") true)
(check-expect (spells-hot-dog? "Gadasdhtasd whodoasdgeoto") true)
