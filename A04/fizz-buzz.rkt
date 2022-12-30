;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname fizz-buzz) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;; CS 135 Fall 2020
;; Assignment 04, Problem 5
;; ***************************************************
;;

;;PURPOSE;;
;; (fizz-buzz start end fizz buzz) cases the string
;; to be sarcastic cased

;; Examples:
(check-expect (fizz-buzz 8 15 3 5) (list 8 'fizz 'buzz 11 'fizz 13 14 'honk))
(check-expect (fizz-buzz 1 4 2 2) (list 1 'honk 3 'honk))
(check-expect (fizz-buzz 1 1 1 1) (list 'honk))

;; Creates a list from x - y given two values
(define (interval-list x y)
    (cond
    [(> x y) empty]
    [else  (cons x (interval-list (+ x 1) y))]))

;; Checks the divisibility of numbers in the interval
;; to see if it is divisble by the value of fizz and/or buzz
(define (divisibility num fizz buzz)
    (cond
    [(and (= (remainder num buzz) 0) (= (remainder num fizz) 0)) 'honk]
    [(= (remainder num fizz) 0) 'fizz]
    [(= (remainder num buzz) 0) 'buzz]
    [else num]))

;; Checks the divisibility of the given list interval
(define (fizz-buzz-helper lst fizz buzz)
    (cond
    [(empty? lst) empty]
    [else (cons (divisibility (first lst) fizz buzz) (fizz-buzz-helper (rest lst) fizz buzz))]))

;; fizz-buzz: num num num num -> listOfFizzBuzz
(define (fizz-buzz start end fizz buzz)
    (fizz-buzz-helper (interval-list start end) fizz buzz))

;; Tests Cases:
(check-expect (fizz-buzz 8 15 3 5) (list 8 'fizz 'buzz 11 'fizz 13 14 'honk))
(check-expect (fizz-buzz 0 8 3 5) (list 'honk 1 2 'fizz 4 'buzz 'fizz 7 8))
(check-expect (fizz-buzz 0 1 1 1) (list 'honk 'honk))