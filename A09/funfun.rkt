;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname funfun) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;; CS 135 Fall 2022
;; Assignment 09, Problem 4
;; ***************************************************
;;

;;PURPOSE;;
;; (multi-apply list v) produces a number base off
;; a list of function with a given starting value
;; of v by adding the elemtents in the same order 
;; as foldr

;; Examples:
(check-expect (multi-apply (list sub1 sqr add1) 3) 5)
(check-expect (multi-apply (list add1 sqrt sub1) 3) 1)
(check-expect (multi-apply (list add1 sqr sqr) 3) 256)

;; multi-apply: (listOf-f) number -> number
(define (multi-apply list v)
  ((foldr (lambda (f x)
            (lambda (y) (x (f y)))) (lambda (z) z) list) v))

;; Test Cases:
(check-expect (multi-apply (list) 3) 3)
(check-expect (multi-apply (list add1) 3) 4)
(check-expect (multi-apply (list add1 sqr sqr sqrt sqr add1 add1) 3) 258)

;;PURPOSE;;
;; (aop n (v)) produces a nat number
;; based off the value of nth degree polynomial
;; and the given value to sub into the polynomial

;; Examples:
(check-expect ((aop 4) 2) 31)
(check-expect ((aop 0) 5) 1)
(check-expect ((aop 16) 1) 17)
(check-expect ((aop 1) 5) 6)

;; aop: (all-one-poly-n-degree) nat -> nat
(define (aop n)
    (foldr (lambda (f x) (lambda (value) (+ (f value) (x value)))) 
        (lambda (x) 0) (map (lambda (exponent)
                              (lambda (x) (expt x exponent))) 
        (build-list (+ 1 n) (lambda (x) x)))))

;; Test Cases:
(check-expect ((aop 10) 5) 12207031)
(check-expect ((aop 2) 0) 1)
(check-expect ((aop 20) 4) 1466015503701)

;;PURPOSE;;
;; (multi-compose list v) produces a number base off
;; a list of function with a given value
;; of v by adding the elemtents in the same order
;; as foldl

;; Examples:
(check-expect ((multi-compose (list sub1 sqr add1)) 3) 15)
(check-expect ((multi-compose (list sub1 add1 add1)) 2) 3)
(check-expect ((multi-compose (list add1 sqr add1)) 5) 37)

;; multi-compose: (listOf-f) number -> number
(define (multi-compose list)
  (foldl (lambda (f x)
           (lambda (y) (x (f y)))) (lambda (z) z) list))

;; Test Cases:
(check-expect ((multi-compose (list)) 3) 3)
(check-expect ((multi-compose (list sqr)) 2) 4)
(check-expect ((multi-compose (list add1 sqr add1 sqr add1)) 5) 1370)