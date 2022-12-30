;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname box-office) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; ***************************************************
;; Vlad Comsa (21027115)
;; CS 135 Fall 2020
;; Assignment 02, Problem 5
;; ***************************************************
;;

;; (box-office-profits movie-name studio-name num-of-actors num-of-explosions)
;; produces the profit of the box-office based off
;; of movie-name studio-name num-of-actors num-of-explosions

;; Examples:
(check-expect (box-office-profits "Thor: Love and Thunder" "DC" 4 30 ) 110)
(check-expect (box-office-profits "The End" "James Industry" 9 200 ) 1605)

;; Helper Function for movie-name
(define (movie-points name)
  (cond
    [(< (string-length name) 3) 25]
    [(and (>= (string-length name) 10) (not (string=?(substring name 0 3) "The")))0]
    [(and (>= (string-length name) 10) (string=? (substring name 0 3) "The")) -50]
    [(and (< (string-length name) 10) (not (string=?(substring name 0 3) "The")))25]
    [(and (< (string-length name) 10) (string=? (substring name 0 3) "The"))-25]))

;; Helper Function for Studio-name
(define (studio-points name)
  (cond
    [(string=? name "Marvel") 500]
    [(string=? name "DC") -250]
    [else 0]))

;; Helper Function for num-of-actors
(define (actor-points num)
    (* num 50))

;; Helper Function for num-of-explosions
(define (explosions-points num)
    (- (* num 6) 20))


;; box-office-profits: Str Str Nat Nat -> Int
(define (box-office-profits movie-name studio-name num-of-actors num-of-explosions)
    (+(movie-points movie-name)(studio-points studio-name)
      (actor-points num-of-actors)(explosions-points num-of-explosions)))
    
    

;; Tests:
(check-expect (box-office-profits "Thor: Love and Thunder" "Marvel" 4 50 ) 980)
(check-expect (box-office-profits "The End Edssawe the" "Jas ndustry" 3 100 ) 680)
(check-expect (box-office-profits "tHEThe" "DC Marvel" 2 2 ) 117)
(check-expect (box-office-profits "Crzy" "Marvel" 23 560 ) 5015)
(check-expect (box-office-profits "dssssssssssssdssd" "James" 0 0 ) -20)
(check-expect (box-office-profits "The Experts" "Marvel" 0 3 ) 448)
(check-expect (box-office-profits "" "" 4 30 ) 385)
(check-expect (box-office-profits "          " "" 4 30 ) 360)