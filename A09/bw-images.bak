;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname bw-images) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;; Vlad Comsa (21027115)
;; CS 135 Fall 2022
;; Assignment 09, Problem 3
;; ***************************************************
;;

;; BW-Pixel is (anyof 0 1)
;; 2D-Image is (listof (ne-listof BW-Pixel))
;; Requires: inner lists of 2D-Image are of same length

(define image-0 '((0 0 0 0)
(0 0 0 0)
(0 0 0 0)
(0 0 0 0)))

(define image-1 '((1 1 1 1)
(1 1 1 1)
(1 1 1 1)
(1 1 1 1)))

(define image-L '((1 0 0 0)
(1 0 0 0)
(1 0 0 0)
(1 1 1 1)))

(define cross '((1 0 1 0)
(1 0 1 0)
(1 0 1 0)
(1 0 1 0)))

(define image-L-reflect-x '((1 1 1 1)
(1 0 0 0)
(1 0 0 0)
(1 0 0 0)))

(define image-L-reflect-y '((0 0 0 1)
(0 0 0 1)
(0 0 0 1)
(1 1 1 1)))

;;PURPOSE;;
;; (invert image) produces a 2D-Image that
;; is inverted on the x-axis of each row

;; Examples:
(check-expect (invert cross)
(list
 (list 0 1 0 1)
 (list 0 1 0 1)
 (list 0 1 0 1)
 (list 0 1 0 1)))

(check-expect (invert image-L-reflect-x)
(list
 (list 0 0 0 0)
 (list 0 1 1 1)
 (list 0 1 1 1)
 (list 0 1 1 1)))

;; invert: 2D-Image -> 2D-Image
(define (invert 2D-image)
  (foldr (lambda (index x) 
    (cons (map (lambda (k) 
      (cond [(= 1 k) 0]
            [else 1])) index) x)) empty 2D-image))

;; Test Cases:
(check-expect (invert image-0) image-1)
(check-expect (invert image-L) '((0 1 1 1)
(0 1 1 1)
(0 1 1 1)
(0 0 0 0)))
(check-expect (invert image-L-reflect-x) '((0 0 0 0)
(0 1 1 1)
(0 1 1 1)
(0 1 1 1)))

;;PURPOSE;;
;; (reflect-x-axis image) produces a 2D-Image with
;; a given 2D-Image and reflects the over
;; the x-axis of the 2-Image

;; Examples:
(check-expect (reflect-x-axis image-L-reflect-x) image-L)
(check-expect (reflect-x-axis cross) cross)
(check-expect (reflect-x-axis image-0) image-0)

;; reflect-x-axis: 2D-Image -> 2D-Image
(define (reflect-x-axis 2D-image)
  (foldl cons empty 2D-image))

;; Test Cases:
(check-expect (reflect-x-axis image-L) image-L-reflect-x)
(check-expect (reflect-x-axis image-L-reflect-y)
              (list
               (list 1 1 1 1)
               (list 0 0 0 1)
               (list 0 0 0 1)
               (list 0 0 0 1)))

(check-expect (reflect-x-axis '((1 0 0 0)
(1 0 0 0)
(1 0 0 0)
(1 0 0 1)))
              '((1 0 0 1)
(1 0 0 0)
(1 0 0 0)
(1 0 0 0)))

;;PURPOSE;;
;; (reflect-y-axis image) produces a 2D-Image with
;; a given 2D-Image and reflects the over
;; the y-axis of the 2-Image

;; Examples:
(check-expect (reflect-y-axis image-L-reflect-x)
(list
 (list 1 1 1 1)
 (list 0 0 0 1)
 (list 0 0 0 1)
 (list 0 0 0 1)))
(check-expect (reflect-y-axis cross)
(list
 (list 0 1 0 1)
 (list 0 1 0 1)
 (list 0 1 0 1)
 (list 0 1 0 1)))

;; reflect-y-axis: 2D-Image -> 2D-Image
(define (reflect-y-axis 2D-image)
  (foldr (lambda (index x)
           (cons (foldl cons empty index) x)) empty 2D-image))

;; Test Cases:
(check-expect (reflect-y-axis image-0) image-0)
(check-expect (reflect-y-axis image-L) image-L-reflect-y)
(check-expect (reflect-y-axis '((1 1 0 0)
(1 1 0 0)
(1 1 0 0)
(0 0 1 1)))
             '((0 0 1 1)
(0 0 1 1)
(0 0 1 1)
(1 1 0 0)))
                              

;;PURPOSE;;
;; (transpose image) produces a 2D-Image where
;; the rows and columns are exchanged (i.e first row
;; first column, and so on). 

;; Examples:
(check-expect (transpose image-L-reflect-x)
(list
 (list 1 1 1 1)
 (list 1 0 0 0)
 (list 1 0 0 0)
 (list 1 0 0 0)))
(check-expect (transpose cross)
(list
 (list 1 1 1 1)
 (list 0 0 0 0)
 (list 1 1 1 1)
 (list 0 0 0 0)))
(check-expect (invert image-0) image-1)

;; transpose: 2D-Image -> 2D-Image
(define (transpose 2D-image)
  (cond 
  [(empty? 2D-image) empty]
  [else 
    (foldr (lambda(row1 col1) 
      (map (lambda (row2 col2) (cons row2 col2)) row1 col1)) 
        (map (lambda (row1) empty) (first 2D-image)) 2D-image)]))

;; Test Cases:
(check-expect (transpose image-L)
'((1 1 1 1)
(0 0 0 1)
(0 0 0 1)
(0 0 0 1)))
(check-expect (transpose image-L-reflect-x) image-L-reflect-x)
(check-expect (transpose '()) '())
(check-expect (transpose '((1 0 0 1 1)))
'((1) (0) (0) (1) (1)))