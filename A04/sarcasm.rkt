;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname sarcasm) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;; CS 135 Fall 2020
;; Assignment 04, Problem 4
;; ***************************************************
;;

;; A (pair-listof X) is one of:
;; * empty
;; * (cons X empty)
;; * (cons X (cons X (pair-listof X)))

;; pair-listof-X-template: List -> Num-Subsquent
(define (pair-listof-X-template  X)
  (cond
    [(empty? X)...]
    [(cons? X)...]
    [(...(cons? (first X))(...(second X)
                  (pair-listof-X-template (rest X)))) ...]))

;;PURPOSE;;
;; (sarcastic string) cases the string
;; to be sarcastic cased

;; Examples:
(check-expect (sarcastic "12321321321321") "12321321321321")
(check-expect (sarcastic "odd even")"OdD EvEn")
(check-expect (sarcastic "spacing")"SpAcInG")

;; Helper funcion that capitalizes the even indicies of a string
;; and makes the odd indicies lowercased
(define (sarcastic-helper sentence n tail)
    (cond
    [(empty? sentence) tail]
    [(odd? n) (sarcastic-helper (rest sentence) (+ n 1)
                                (string-append tail (string (char-downcase (first sentence)))))]
    [(even? n) (sarcastic-helper (rest sentence) (+ n 1)
                                  (string-append tail (string (char-upcase (first sentence)))))]))

;; sarcastic: string -> string-sarcastic
(define (sarcastic string)
    (sarcastic-helper (string->list string) 0 ""))

;; Test Cases:
(check-expect (sarcastic "GOOD LUCK... you'll need it!") "GoOd lUcK... yOu'lL NeEd iT!")
(check-expect (sarcastic "A!A!AAAAA slopy") "A!A!AaAaA SlOpY")
(check-expect (sarcastic "") "")
(check-expect (sarcastic " A ") " a ")