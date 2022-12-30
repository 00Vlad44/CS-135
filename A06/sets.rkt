;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname sets) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;; CS 135 Fall 2020
;; Assignment 06, Problem 3
;; ***************************************************
;;

;;PURPOSE;;
;; (union s1 s2) checks if s1 is a member of 
;; s2 and combines them into a union

;; Examples:
(check-expect (union (list 1 3) (list 1 2)) (list 1 2 3))
(check-expect (union (list 1 4 5) (list 5 6)) (list 1 4 5 6))
(check-expect (union (list 1 2 3 4 5) (list 3 4 5 6 7 8 9)) (list 1 2 3 4 5 6 7 8 9))

;; union: list list -> list
(define (union s1 s2)
  (cond
    [(and (empty? s1) (empty? s2)) empty]
    [(and (empty? s1) (not (empty? s2))) s2]
    [(and (empty? s2) (not (empty? s1))) s1]
    [(= (first  s1) (first  s2)) (cons (first s1) (union (rest  s1)
       (rest  s2)))]
    [(< (first  s1) (first  s2))
     (cons (first s1) (union (rest  s1) s2))]
    [(> (first  s1) (first  s2))
      (cons (first s2) (union s1 (rest s2)))]))

;; Test Cases:
(check-expect (union (list 1) (list 1)) (list 1))
(check-expect (union (list 4 5 6 7 8) (list 4 5 8 11 21 23 37)) (list 4 5 6 7 8 11 21 23 37))
(check-expect (union (list 4 5 6 7 8) (list 4 5 6 7 9)) (list 4 5 6 7 8 9))

;;PURPOSE;;
;; (intersection s1 s2) checks if s1 has elements the
;; same as s2 and returns it in a list

;; Examples:
(check-expect (intersection (list 1 2 3 4 5) (list 5 6 7 8 9)) (list 5))
(check-expect (intersection (list 1 2 3 4 5) (list 3 4 5 6 7 8 9)) (list 3 4 5))
(check-expect (intersection (list 1 2 12) (list 1 2 12)) (list 1 2 12))

;; intersection: list list -> list
(define (intersection s1 s2)
  (cond
    [(or (empty? s1) (empty? s2)) empty]
    [(= (first  s1) (first  s2))
     (cons (first s1) (intersection (rest  s1)
       (rest  s2)))]
    [(< (first  s1) (first  s2))
     (intersection (rest  s1) s2)]
    [(> (first  s1) (first  s2))
      (intersection s1 (rest s2))]))

;; Test Cases:
(check-expect (intersection (list 1 2) (list 5 6 7 8 9)) (list))
(check-expect (intersection (list 1 2 3) (list 3 3 4 5 6 7 8 9)) (list 3))
(check-expect (intersection (list 1 13) (list 1 12 13)) (list 1 13)) 