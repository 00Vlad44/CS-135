;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname nested) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***************************************************
;; Vlad Comsa (21027115)
;; CS 135 Fall 2022
;; Assignment 08, Problem 2
;; ***************************************************
;;


;; A (nested-listof X) is one of:
;; * empty
;; * (cons (nested-listof X) (nested-listof X))
;; * (cons X (nested-listof X))
;; Requires: X itself is not a list type
;; a NotAList is an Any that is not a list type

;; nested-listof-X-template: (nested-listof X) -> Any
(define (nested-listof-X-template nested-listof-X)
  (cond
    [(empty? nested-listof-X) ...]
    [(list? (first nested-listof-X))
            (... (nested-listof-X-template (first nested-listof-X))...
                 (nested-listof-X-template (rest nested-listof-X)) ...)]
    [else (... (first nested-listof-X) ...
               (nested-listof-X-template (rest nested-listof-X))...)]))


;;PURPOSE;;
;; (nested-count nested-listof-NotAList) counts every
;; NotAList value in the given nested list, which
;; produces the total count of NotAList elements


;; Examples:
(check-expect (nested-count (cons 3 (list 2))) 2)
(check-expect (nested-count (cons 4 (list))) 1)
(check-expect (nested-count (cons (list)(list))) 0)

;; Helper function that counts the number of NotAList elements there
;; are with an accumulator

;; nested-count: list num -> num
(define (nested-count NotAList)
  (cond
    [(empty? NotAList) 0]
    [(list? (first NotAList))
     (+ (nested-count (first NotAList)) (nested-count (rest NotAList)))]
    [else (+ 1 (nested-count (rest NotAList)))]))

;; Test Cases:
(check-expect (nested-count (cons 3 (list 2 3 2 3))) 5)
(check-expect (nested-count (cons (list 1) (list 2))) 2)
(check-expect (nested-count (cons (list 1 2 2) (list 2))) 4)

;;PURPOSE;;
;; (nested-sum nested-listof-NotAList) counts every
;; and adds every NotAList value in the given nested list, and
;; is combined into a toal value

;; Examples:
(check-expect (nested-sum (cons 3 (list 2 3 2 3))) 13)
(check-expect (nested-sum (cons (list 1) (list 2))) 3)
(check-expect (nested-sum (cons (list 1 2 2) (list 2))) 7)

;; nested-sum: list -> num
(define (nested-sum NotAList)
  (cond
    [(empty? NotAList) 0]
    [(list? (first NotAList))
     (+ (nested-sum (first NotAList)) (nested-sum (rest NotAList)))]
    [else (+ (first NotAList)(nested-sum (rest NotAList)))]))

;; Test Cases:
(check-expect (nested-sum (cons 3 (list 2))) 5)
(check-expect (nested-sum (cons 4 (list))) 4)
(check-expect (nested-sum (cons (list 5 0 1)(list 3 10 7))) 26)

;;PURPOSE;;
;; (nested-member? value NotAList) checks each element
;; to the given value and returns true or false depnding on if the
;; value exists in the list

;; Examples:
(check-expect (nested-member? 'hot-dog '((hot-dog) (hamburger) (((hotdog)))))  true)
(check-expect (nested-member? 'hot '((pizza) (hamburger)((((hot)))) (((hot-dog)))))  true)
(check-expect (nested-member? 'ff '((pizza) (hamburger) (creature) (ee) (ht)))  false)

;; nested-member?: Any list -> bool
(define (nested-member? value NotAList)
  (cond
    [(empty? NotAList) false]
    [(equal? value (first NotAList)) true]
    [(list? (first NotAList))
     (or (nested-member? value (first NotAList))
         (nested-member? value (rest NotAList)))]
    [else (nested-member? value (rest NotAList))]))

;; Test Cases:
(check-expect (nested-member? 'hot-dog '((hot-dog) (hamburger) (((hot-dog)))))  true)
(check-expect (nested-member? 'hot '((pizza) (hamburger) (((hot-dog)))))  false)
(check-expect (nested-member? 'ff '((pizza) (hamburger) (creature) (ee) (ff)))  true)

;;PURPOSE;;
;; (nested-ref NotAList value) checks a list with
;; NotAList values and determines the value associated with
;; the index of that list

;; Examples
(check-expect (nested-ref '((0) 1 2 (asdsa) 5 ((asdsa) (8)) 9 () (((hot-dog)))) 6) 8)
(check-expect (nested-ref '((0) 1 2 (3 4) 5 (6 7 (8)) 9 () (((hot-dog)))) 7) 7)
(check-expect (nested-ref '((asdsa) 2 24 (9 4) 9 () (((hot-dog)))) 6) 'hot-dog)

;; nested-ref: (nested-listOfNotAList) Nat -> Any
(define (nested-ref NotAList value)
  (cond
    [(not (list? (first NotAList)))
     (cond
       [(= value 0) (first NotAList)]
       [else (nested-ref (rest NotAList) (sub1 value))])]
    [(> (nested-count (first NotAList)) value)
     (nested-ref (first NotAList) value)]
    [else (nested-ref (rest NotAList)
                      (- value (nested-count (first NotAList))))]))

;; Test Cases:
(check-expect (nested-ref '((0) 1 2 (3 4) 5 (6 7 (8)) 9 () (((hot-dog)))) 0) 0)
(check-expect (nested-ref '((0) 1 2 (3 4) 5 (6 7 (8)) 9 () (((hot-dog)))) 8) 8)
(check-expect (nested-ref '((0) 1 2 (3 4) 5 (6 7 (8)) 9 () (((hot-dog)))) 3) 3)
(check-expect (nested-ref '((asdsa) 2 24 (9 4) 9 () (((hot-dog)))) 0) 'asdsa)



;; Examples 2f
(check-expect (nested-filter odd? (cons 1 (list 3 (cons 3 (list 3 1)))))
              (cons 1 (list 3 (cons 3 (list 3 1)))))


(define (nested-cleanup nested-list)
     (cond
       [(empty? nested-list) false]
       [(not (list? (first nested-list)))
        (local [(define rest-cleanup (nested-cleanup (rest nested-list)))]
          (cond [(false? rest-cleanup) (cons (first nested-list) empty)]
                [else (cons (first nested-list) rest-cleanup)]))]
        [else 
     (local [(define first-cleanup (nested-cleanup (first nested-list)))
             (define rest-cleanup (nested-cleanup (rest nested-list)))]
         (cond
           [(and (false? first-cleanup) (false? rest-cleanup)) false]
           [(and (not (false? first-cleanup)) (false? rest-cleanup)) (cons first-cleanup empty)]
           [(and (false? first-cleanup) (not (false? rest-cleanup))) rest-cleanup]
           [else (cons first-cleanup rest-cleanup)]))]))
           
;; Examples 2g
(check-expect (nested-cleanup '(1 () 2 () () 3)) '(1 2 3))
(check-expect (nested-cleanup '(())) false)
(check-expect (nested-cleanup '(()(() () 3 ()))) '((3)))


