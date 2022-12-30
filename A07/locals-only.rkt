;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname locals-only) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;;
;; ***************************************************
;; CS 135 Fall 2022
;; Assignment 07, Problem 4
;; ***************************************************
;;

;; Examples
(check-expect (normalize '(2 4 6)) '(0 0.5 1))
(check-expect (normalize '(4 5)) '(0 1))
(check-expect (normalize '(2 4 8 10)) '(0 0.25 0.75 1))

;; normalize: (ne-listof Num) -> (ne-listof Num)
(define (normalize lon)
  (local [(define (find-min lon current-min)
            (cond
              [(empty? lon) current-min]
              [(< (first lon) current-min) (find-min (rest lon) (first lon))]
              [else (find-min (rest lon) current-min)]))
          (define (find-max lon current-max)
            (cond
              [(empty? lon) current-max]
              [(> (first lon) current-max) (find-max (rest lon) (first lon))]
              [else (find-max (rest lon) current-max)]))
          (define max-list (find-max (rest lon) (first lon)))
          (define min-list (find-min (rest lon) (first lon)))
          (define (norm-list lon)
            (cond
              [(empty? lon) empty]
              [else (cons (/ (- (first lon) min-list) (- max-list min-list))
                          (norm-list (rest lon)))]))]
    (cond
      [(empty? (rest lon)) lon]
      [(= min-list max-list) lon]
      [else (norm-list lon)])))