;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname median) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;; Vlad Comsa (21027115)
;; CS 135 Fall 2020
;; Assignment 02, Problem 2
;; ***************************************************
;;

;;PURPOSE;;
;; (median-of-3-simple a b c) produces the median of
;; the numbers a b c

;; Examples:
(check-expect (median-of-3-simple 3 4 5) 4)
(check-expect (median-of-3-simple 8 0 109) 8)


;; median-of-3-simple: Num Num Num -> Num
(define (median-of-3-simple a b c)
  (cond
    [(or (<= b a c) (<= c a b)) a]
    [(or (<= a b c) (<= c b a)) b]
    [(or (<= b c a) (<= a c b)) c]))

;; Tests:
(check-expect (median-of-3-simple 0 0 0) 0)
(check-expect (median-of-3-simple 1 2 3) 2)
(check-expect (median-of-3-simple 2 3 1) 2)
(check-expect (median-of-3-simple 3 2 1) 2)
(check-expect (median-of-3-simple 3 3 2) 3)
(check-expect (median-of-3-simple 2 3 3) 3)
(check-expect (median-of-3-simple 1 3 2) 2)