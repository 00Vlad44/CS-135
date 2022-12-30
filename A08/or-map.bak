;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname or-map) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;; Vlad Comsa (21027115)
;; CS 135 Fall 2022
;; Assignment 08, Problem 1
;; ***************************************************
;;

;;PURPOSE;;
;; (my-ormap pred? listOfX) produces true if
;; the pred? is true for any elements in the list
;; otherwise produce false

;; Examples:
(check-expect (my-ormap zero? '(8 6 7 5 3 0 9)) true)
(check-expect (my-ormap zero? '(8 3 0 9)) true)
(check-expect (my-ormap zero? '(2 4 34 340)) false)


;; Note: that the contract for pred? is, pred?: X -> Bool
;; my-ormap: pred? listOfX -> Bool
(define (my-ormap pred? listOfX)
  (local
    [(define rest-listOfX
       (cond
         [(empty? listOfX) empty]
         [(empty? (rest listOfX))  listOfX]
         [(pred? (first listOfX)) listOfX]
         [else (rest listOfX)]))]
    (cond
      [(empty? rest-listOfX) false]
      [(and (empty? (rest rest-listOfX)) (not (pred? (first rest-listOfX)))) false]
      [(pred? (first rest-listOfX)) true]
      [else (my-ormap pred? rest-listOfX)])))

;; Test Cases:
(check-expect (my-ormap zero? '(8 3 94 4 0)) true)
(check-expect (my-ormap zero? '(0 3 94 4)) true)
(check-expect (my-ormap zero? '(2)) false)
(check-expect (my-ormap zero? '()) false)


;;PURPOSE;;
;; (pred?-ormap num listOfPred) produces true if
;; there is a pred in the list that returns true wtih the
;; give num as input

;; Examples:
(check-expect (pred?-ormap 5 (list zero? even? negative? posn? inexact?)) false)
(check-expect (pred?-ormap 0 (list even? zero? posn? inexact?)) true)
(check-expect (pred?-ormap 10 (list even? positive?)) true)
(check-expect (pred?-ormap 0 (list odd? posn?)) false)

;; pred?-ormap: num listOfPred -> Bool
(define (pred?-ormap num listOfPred)
  (local
    [(define rest-listOfPred
       (cond
         [(empty? listOfPred) empty]
         [(empty? (rest listOfPred))  listOfPred]
         [((first listOfPred) num) listOfPred]
         [else (rest listOfPred)]))]
    (cond
      [(empty? rest-listOfPred) false]
      [(and (empty? (rest rest-listOfPred)) (not ((first rest-listOfPred) num))) false]
      [((first listOfPred) num) true]
      [else (pred?-ormap num rest-listOfPred)])))

;; Test Cases:
(check-expect (pred?-ormap 5 (list)) false)
(check-expect (pred?-ormap 0 (list zero?)) true)
(check-expect (pred?-ormap 5 (list zero? even? negative? posn? odd?)) true)
