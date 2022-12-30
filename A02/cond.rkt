;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname cond) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;; CS 135 Fall 2020
;; Assignment 02, Problem 3
;; ***************************************************
;;

;; Function q3a
(define (q3a p1? p2?)
  (cond
    [(and p1? p2?) 'left]
    [(and p1? (not p2?)) 'up]
    [(and (not p1?) p2?) 'down]
    [(and (not p1?) (not p2?)) 'right]))

;; Test Cases
(check-expect (q3a true true) 'left)
(check-expect (q3a true false) 'up)
(check-expect (q3a false true) 'down)
(check-expect (q3a false false) 'right)
    
;; Function q3b
(define (q3b p1? p2?)
  (cond
    [(or (and p1? p2?) (and p1? (not p2?)))'up]
    [(or (and (not p1?) p2?)(and (not p1?) (not p2?))) 'down]))

;; Test Cases
(check-expect (q3b true true) 'up)
(check-expect (q3b true false) 'up)
(check-expect (q3b false true) 'down)
(check-expect (q3b false false) 'down)


;;Function q3c
(define (q3c p1? p2?)
  (cond
    [(or (and p1? p2?) (and (not p1?) p2?) (and (not p1?) (not p2?)))'up]
    [(and p1? (not p2?)) 'down]))

;; Test Cases
(check-expect (q3c true true) 'up)
(check-expect (q3c true false) 'down)
(check-expect (q3c false true) 'up)
(check-expect (q3c false false) 'up)