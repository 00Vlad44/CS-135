;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname fizz-buzz-2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;PURPOSE;;
;; (make-div-pred n) produces a true or false, depending
;; if the number has a remainder of zero with the given dividend
;; and divisor

;; make-div-pred: predicate num -> boolean 
(define (make-div-pred n)
  (local
    [(define (g m) (= 0 (remainder m n)))]
    g))


;;PURPOSE;;
;; (fizz-buzz-2 start end list-pairs) produces a (listof Any)
;; based of the divisiblity of the numbers and decides whcih numbers
;; to leave and which to change to the desried specification


;; Examples:
(check-expect (fizz-buzz-2 8 15 (list (list 'honk (make-div-pred 15))
                                      (list 'fizz (make-div-pred 3))
                                      (list 'buzz (make-div-pred 5))))
              '(8 fizz buzz 11 fizz 13 14 honk))

(check-expect
 (fizz-buzz-2 -3 3 (list (list "donut" zero?)
(list "even" even?)
(list 'neg negative?))) '(neg "even" neg "donut" 1 "even" 3))

(check-expect
 (fizz-buzz-2 0 3 (list (list "easy" zero?)
                        (list "even" odd?)
                        (list 'neg positive?)))
 '("easy" "even" neg "even"))

(check-expect
 (fizz-buzz-2 0 3 (list (list "easy" zero?)))
 '("easy" 1 2 3))

;; Helper function that checks if the starting integer is divisble
;; and returns the desired list-pairs arguement
(define (check-pred num list-pairs)
  (cond
    [(empty? list-pairs) num]
    [((second (first list-pairs)) num) (first (first list-pairs))]
    [else (check-pred num (rest list-pairs))]))

;; fizz-buzz-2: Num Num (listof (list Any (Int -> Bool))) -> (listof Any)
(define (fizz-buzz-2 start end list-pairs)
  (cond
    [(> start end) empty]
    [else (cons (check-pred start list-pairs)
                (fizz-buzz-2 (add1 start) end list-pairs))]))

;; Test Cases:
(check-expect (fizz-buzz-2 0 1 (list (list "easy" zero?)
                        (list "even" negative?)
                        (list 'neg positive?)))
              '("easy" neg))

(check-expect
 (fizz-buzz-2 0 3 (list (list "donut" zero?)
(list "even" even?)
(list 'neg negative?))) '("donut" 1 "even" 3))

(check-expect
 (fizz-buzz-2 1 3 (list (list "easy" zero?)))
 '(1 2 3))

 