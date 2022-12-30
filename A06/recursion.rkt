;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname recursion) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;; CS 135 Fall 2020
;; Assignment 06, Problem 1
;; ***************************************************
;;


;;PURPOSE;;
;; (in-range a b lst) finds the interval of
;; the numbers through a b inclusive

;; Examples:
(check-expect (in-range -4 12 (list 1 2 3 4 5 6 11)) 7)
(check-expect (in-range 0 10 (list)) 0)
(check-expect (in-range -4 12 (list -3 1 2 3 4 5 6 11)) 8)

;; Helper Function to find the numbers in the interval
(define (calc-interval a b lst count)
  (cond
    [(empty? lst) count]
    [(<= (min a b) (first lst) (max a b))
     (calc-interval a b (rest lst) (+ 1 count))]
    [else (calc-interval a b (rest lst) (+ 0 count))]))

;; in-range: Num Num List -> Num
(define (in-range a b lst) (calc-interval a b lst 0))

;; Test Cases:
(check-expect (in-range 0 10 (list)) 0)
(check-expect (in-range 0 10 (list 3 23 10 20 40)) 2)
(check-expect (in-range 0 10 (list 0 10)) 2)
(check-expect (in-range 0 10 (list 0 -10)) 1)

;;PURPOSE;;
;; (spread ne-lst) takes in a non-empty
;; list and take the difference of the highest
;; element and the smallest element

;; Examples:
(check-expect (spread (list 1 -3 10 12)) 15)
(check-expect (spread (list 1 -3 10 -12)) 22)
(check-expect (spread (list 2 3 32 19 40)) 38)

;; Helper Function that subtracts the largest element
;; with the smallest element
(define (spread-acc lst max-so-far min-so-far)
  (cond
    [(empty? lst) (- max-so-far min-so-far)]
    [(> (first lst) max-so-far)
     (spread-acc (rest lst) (first lst) min-so-far)]
    [(< (first lst) min-so-far)
     (spread-acc (rest lst) max-so-far (first lst))]
    [else (spread-acc (rest lst) max-so-far min-so-far)]))

;; spread: list -> num
(define (spread lst)
  (spread-acc lst (first lst) (first lst)))

;; Test Cases:
(check-expect (spread (list 10 2)) 8)
(check-expect (spread (list 2 10)) 8)
(check-expect (spread (list 0)) 0)

;;PURPOSE;;
;; (sel-sort lst) takes a list of numbers
;; and sorts them into non-decreasing order

;; Examples:
(check-expect (sel-sort (list 10 2 9 12 1)) (list 1 2 9 10 12))
(check-expect (sel-sort (list 3 2 1)) (list 1 2 3))
(check-expect (sel-sort (list 10)) (list 10))

;; sel-sort: list -> list
(define (sel-sort lst)
  (cond
    [(empty? lst) empty]
    [else (sel-sort/sf (smallest-first lst))]))

;; Test Cases:
(check-expect (sel-sort (list 10 2 2 9)) (list 2 2 9 10))
(check-expect (sel-sort (list 3 2 11)) (list 2 3 11))
(check-expect (sel-sort (list 10 5 29 -2)) (list -2 5 10 29))


;;PURPOSE;;
;; (sel-sort/sf lst) takes a non-empty list of numbers
;; and sorts the list with the first of the list being static

;; Examples:
(check-expect (sel-sort/sf (list 1 10 2 9 12)) (list 1 2 9 10 12))
(check-expect (sel-sort/sf (list 1 3 2)) (list 1 2 3))
(check-expect (sel-sort/sf (list 10)) (list 10))

;; sel-sort/sf: list -> list
(define (sel-sort/sf lst)
  (cond
    [(empty? (rest lst)) (cons (first lst) empty)]
    [else (cons (first lst)
                (sel-sort/sf (smallest-first (rest lst))))]))

;; Test Cases:
(check-expect (sel-sort/sf (list 0 20 4 9 3)) (list 0 3 4 9 20))
(check-expect (sel-sort/sf (list 5 18 9 10)) (list 5 9 10 18))
(check-expect (sel-sort/sf (list 3)) (list 3))

;;PURPOSE;;
;; (smallest-first lst) takes a non-empty list of numbers
;; and takes the smallest element of the list and brings it to the
;; front of the list and not care about the order after the first number

;; Examples:
(check-expect (smallest-first (list 2 1 10 2 9 12)) (list 1 2 10 2 9 12))
(check-expect (smallest-first (list 6 1 3 2)) (list 1 6 3 2))
(check-expect (smallest-first (list 10)) (list 10))

;; smallest-first: list -> list
(define (smallest-first lst)
  (smallest-first/acc lst empty (find-smallest lst)))

;; Test Cases:
(check-expect (smallest-first (list 20 4 9 3)) (list 3 9 4 20))
(check-expect (smallest-first (list 15 11 9 10)) (list 9 11 15 10))
(check-expect (smallest-first (list 30 34 10 12)) (list 10 34 30 12))

;;PURPOSE;;
;; (smallest-first/acc lst lst num) takes a non-empty list of numbers
;; and takes the smallest element of the list and brings it to the
;; front of the list using accumalative recursion

;; Examples - smallest-first/acc
(check-expect (smallest-first/acc (list 2 1) (list 12) 1) (list 1 2 12))
(check-expect (smallest-first/acc (list 2 1 4) (list 12 5 6) 2) (list 2 12 5 6 1 4))

;; smallest-first/acc: list list num -> list
(define (smallest-first/acc lst acc-lst smallest)
  (cond
    [(= (first lst) smallest) (cons smallest (append acc-lst (rest lst)))]
    [else (smallest-first/acc (rest lst) (cons (first lst) acc-lst) smallest)]))

;; Test Cases:
(check-expect (smallest-first/acc (list 2 1 10 2 9 12) (list 12) 2) (list 2 12 1 10 2 9 12))
(check-expect (smallest-first/acc (list 2 1) (list 12) 1) (list 1 2 12))
(check-expect (smallest-first/acc (list 6 1 3 2) (list 1 6 3 2) 3) (list 3 1 6 1 6 3 2 2))

;;PURPOSE;;
;; (find-smallest lst) takes a non-empty list of numbers
;; finds the smallest element in the given list

;; Examples:
(check-expect (find-smallest (list 3 5 4 1 2)) 1)
(check-expect (find-smallest (list -3 1 2 -10 -10 4)) -10)
(check-expect (find-smallest (list 2 32 3 -23434 554)) -23434)

;; find-smallest: list -> num
(define (find-smallest lst)
  (cond
    [(empty? (rest lst)) (first lst)]
    [else (min (first lst) (find-smallest (rest lst)))]))

;; Test Cases:
(check-expect (find-smallest (list 3 -5 4 1 2)) -5)
(check-expect (find-smallest (list 10 2 2 5 20 3934)) 2)
(check-expect (find-smallest (list -1 0 324 -2321 -3 0)) -2321)