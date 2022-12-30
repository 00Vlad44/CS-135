;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Examples) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Module 2 examples:
;; Please note that these examples are from the study modules
;; the examples will be different from your current coruse and
;; I only did the coding examples

;; Example 1
(define (divisors num counter divisor)
  (cond
    [(> divisor num) counter]
    [(= (modulo num divisor) 0) (divisors num (add1 counter) (add1 divisor))]
    [else (divisors num counter (add1 divisor))]))

(define (count-divisors n)
  (divisors n 0 1))


;; Example 2
(+ 2 3)
(* 2 3)
(- 44 2)
(+ (* 3 4) 2)
(/ (+ 2 4) (- 5 1))
(* 3 (+ 1 (+ (/ 6 2) 5)))


;; Example 5
(quotient 10 3)
(remainder 10 3)
(expt 2 4)
(gcd 121 11)

;; Example 6
(define (add-twice a 2b)
  (+ a (* 2 b)))

;; Example 11
(define (g-cord x y)
  (+ (* x (sqrt x)) (sqr y)))