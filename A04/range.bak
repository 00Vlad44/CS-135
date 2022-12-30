;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname range) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;; Vlad Comsa (21027115)
;; CS 135 Fall 2020
;; Assignment 04, Problem 2
;; ***************************************************
;;


;;PURPOSE;;
;; (in-range a b lst) finds the interval of
;; the numbers through a b inclusive

;; Examples:
(check-expect (in-range -4 12 (list 1 2 3 4 5 6 11)) 7)
(check-expect (in-range 0 10 (list)) 0)

;; Helper Function to find the numbers in the interval
(define (calc-interval a b lst tail lngth)
  (cond
    [(empty? lst) (- lngth tail)]
    [(or (< (first lst) (min a b)) (< (max a b)(first lst)))
     (calc-interval a b (rest lst) (+ 1 tail) lngth)]
    [true (calc-interval a b (rest lst) (+ 0 tail) lngth)]))

;; in-range: Num Num List -> Num
(define (in-range a b lst) (calc-interval a b lst 0 (length lst)))


;; Test Cases:
(check-expect (in-range -4 12 (list -3 1 2 3 4 5 6 11)) 8)
(check-expect (in-range 0 10 (list)) 0)
(check-expect (in-range 0 10 (list 3 23 10 20 40)) 2)
(check-expect (in-range 0 10 (list 0   10)) 2)
(check-expect (in-range 0 10 (list 0 -10)) 1)


;;PURPOSE;;
;; (spread ne-lst) takes in a non-empty
;; list and take the difference of the highest
;; element and the smallest element

;; Examples:
(check-expect (spread (list 10 2)) 8)
(check-expect (spread (list 2 10)) 8)
(check-expect (spread (list 0)) 0)
(check-expect (spread (list 2 3 32 19 40)) 38)


;; Helper function to recursively order elements from high-low
(define (insert1 n slon)
  (cond [(empty? slon) (cons n empty)]
        [(>= n (first slon)) (cons n slon)]
        [else (cons (first slon) (insert1 n (rest slon)))]))

(define (order1 lon)
  (cond [(empty? lon) empty]
        [else (insert1 (first lon) (order1 (rest lon)))]))

;; Helper function to recursively order elements from low-high
(define (insert2 n slon)
  (cond [(empty? slon) (cons n empty)]
        [(<= n (first slon)) (cons n slon)]
        [else (cons (first slon) (insert2 n (rest slon)))]))

(define (order2 lon)
  (cond [(empty? lon) empty]
        [else (insert2 (first lon) (order2 (rest lon)))]))

;; spread: listofNum -> num
(define (spread ne-lst)
  (abs (- (first (order1 ne-lst)) (first (order2 ne-lst)))))


;;Test Cases:
(check-expect (spread (list 10)) 0)
(check-expect (spread (list 10 2)) 8)
(check-expect (spread (list 1 -3 10 12)) 15)
(check-expect (spread (list 1 -3 10 -12)) 22)
(check-expect (spread (list 10 10)) 0)





