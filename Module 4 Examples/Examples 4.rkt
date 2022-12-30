;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Examples 4|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Module 4 examples:
;; Please note that these examples are from the study modules
;; the examples will be different from your current coruse and
;; I only did the coding examples


;; Example 4
(define (between x)
  (cond
    [(and (< 80 x)
          (<= x 100)) 1]
    [(and (< 0 x)
          (<= x 80)) -1]
    [else 0]))