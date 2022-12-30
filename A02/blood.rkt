;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname blood) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; ***************************************************
;; CS 135 Fall 2020
;; Assignment 02, Problem 4
;; ***************************************************
;;

;; Question 4a
;; (can-donate-to/cond? donor recipient) produces a boolean
;; result of donor and recipient
;; Examples:
(check-expect (can-donate-to/cond? 'O- 'O-) true)
(check-expect (can-donate-to/cond? 'A- 'O+) false)

;; can-donate-to/cond?: Symbol1 Symbol2 -> Bolean

(define (can-donate-to/cond? donor recipient)
  (cond
    [(symbol=? donor 'O-) true]
    [(symbol=? recipient 'AB+) true]
    [(symbol=? donor recipient) true]
    [(symbol=? donor 'O+) (cond
                            [(symbol=? recipient 'A+) true]
                            [(symbol=? recipient 'B+) true]
                            [else false])]
                          
    [(symbol=? donor 'A-) (cond
                            [(symbol=? recipient 'A+) true]
                            [(symbol=? recipient 'AB-) true]
                            [else false])]
                      
    [(symbol=? donor 'B-) (cond
                            [(symbol=? recipient 'B+) true]
                            [(symbol=? recipient 'AB-) true]
                            [else false])]
    [else false]))

;; Tests:
(check-expect (can-donate-to/cond? 'A+ 'A+) true)
(check-expect (can-donate-to/cond? 'O- 'O+) true)
(check-expect (can-donate-to/cond? 'O+ 'O-) false)
(check-expect (can-donate-to/cond? 'AB+ 'O-) false)
(check-expect (can-donate-to/cond? 'B- 'B+) true)
(check-expect (can-donate-to/cond? 'A- 'AB-) true)
(check-expect (can-donate-to/cond? 'A- 'A+) true)
(check-expect (can-donate-to/cond? 'A- 'AB+) true)
(check-expect (can-donate-to/cond? 'O+ 'A+) true)
(check-expect (can-donate-to/cond? 'O+ 'B+) true)
(check-expect (can-donate-to/cond? 'B- 'AB-) true)
(check-expect (can-donate-to/cond? 'B- 'O-) false)

;; Question 4b
;; (can-donate-to/bool? donor recipient) produces a boolean
;; result of donor and recipient
;; Examples:
(check-expect (can-donate-to/bool? 'O- 'O-) true)
(check-expect (can-donate-to/bool? 'A- 'O+) false)

;; can-donate-to/bool?: Symbol1 Symbol2 -> Bolean
(define (can-donate-to/bool? donor recipient)
  (or (symbol=? donor 'O-)(symbol=? recipient 'AB+)(symbol=? donor recipient) 
  (and (symbol=? donor 'O+)(or (symbol=? recipient 'A+)(symbol=? recipient 'B+)))
  (and (symbol=? donor 'A-)(or (symbol=? recipient 'A+)(symbol=? recipient 'AB-)))
  (and (symbol=? donor 'B-)(or (symbol=? recipient 'B+)(symbol=? recipient 'AB-)))))

;; Tests:
(check-expect (can-donate-to/bool? 'O- 'O-) true)
(check-expect (can-donate-to/bool? 'O+ 'O-) false)
(check-expect (can-donate-to/bool? 'AB+ 'O-) false)
(check-expect (can-donate-to/bool? 'B- 'B+) true)
(check-expect (can-donate-to/bool? 'A- 'AB-) true)
(check-expect (can-donate-to/bool? 'B- 'AB-) true)