;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname tictactoe) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;; CS 135 Fall 2020
;; Assignment 05, Problem 3
;; ***************************************************
;;

;; ;;PURPOSE;;
;; (whose-turn T3Grid) determines
;; which player's turn it is

;; Examples:
(check-expect (whose-turn (list (list '_ '_ '_)
(list '_ '_ '_)
(list '_ '_ '_))) 'X)
(check-expect (whose-turn (list (list '_ 'X '_)
(list '_ '_ '_)
(list '_ '_ '_))) 'O)
(check-expect (whose-turn (list (list 'X))) 'O)

;; Helper function that determines the number of
;; Xs and Os
(define (helper-row-1d T3Grid count-X count-O)
    (cond
        [(empty? T3Grid) (- count-X count-O)]
        [(symbol=? (first T3Grid) '_) (helper-row-1d (rest T3Grid) count-X count-O)]
        [(symbol=? (first T3Grid) 'X) (helper-row-1d (rest T3Grid) (+ count-X 1) count-O)]
        [(symbol=? (first T3Grid) 'O) (helper-row-1d (rest T3Grid) count-X (+ count-O 1))]))

;; Helper function that determines the turn
;; based off of the total count of X
(define (helper-row T3Grid total-count-x)
    (cond
        [(empty? T3Grid) (= total-count-x 0)]
        [else (helper-row (rest T3Grid)
                          (+ total-count-x (helper-row-1d (first T3Grid) 0 0)))]))

;; whose-turn: list -> sym
(define (whose-turn T3Grid)
    (cond
    [(empty? T3Grid) 'X]
    [(helper-row T3Grid 0) 'X]
    [else 'O]))

;; Test Cases:
(check-expect (whose-turn (list (list '_ 'X 'O)
(list 'X 'O 'X) (list 'O 'X '_))) 'O)

(check-expect (whose-turn (list (list 'O 'X 'O)
(list 'X 'O 'X) (list 'O 'X '_))) 'X)

(check-expect (whose-turn (list (list 'O 'X 'X)
(list '_ 'O 'X)
(list '_ '_ '_))) 'O)


;; ;;PURPOSE;;
;; (grid-ref T3Grid row col) returns
;; the designated location of a specific
;; spot on a gird

;; Examples:
(check-expect (grid-ref (list (list '_ '_ '_)
(list '_ '_ '_)
(list '_ '_ '_)) 1 1) '_)

(check-expect (grid-ref (list (list '_ 'O '_)
(list '_ '_ '_)
(list '_ '_ 'X)) 2 2) 'X)

(check-expect (grid-ref (list (list '_ 'O 'X)
(list '_ '_ '_)
(list '_ '_ 'X)) 0 2) 'X)

;; grid-ref -> T3Grid num num -> sym
(define (grid-ref T3Grid row col)
    (list-ref (list-ref T3Grid row) col))

;; Test Cases
(check-expect (grid-ref (list (list '_ '_ '_)
(list '_ '_ '_)
(list '_ '_ '_)) 1 2) '_)

(check-expect (grid-ref (list (list '_ 'O '_)
(list '_ '_ '_)
(list '_ '_ '_)) 0 1) 'O)

(check-expect (grid-ref (list (list 'X '_ '_)
(list '_ '_ '_)
(list '_ '_ '_)) 0 0) 'X)

(check-expect (grid-ref (list (list 'X 'O '_)
(list '_ 'X '_)
(list 'O 'X 'O)) 2 1) 'X)

;; ;;PURPOSE;;
;; (get-column T3Grid col) determines a
;; list of symbols in that column

;; Examples
(check-expect (get-column (list (list '_ '_ '_)
(list '_ '_ '_)
(list '_ '_ '_)) 0) (list '_ '_ '_))

(check-expect (get-column (list (list '_ 'X '_)
(list '_ 'O '_)
(list '_ '_ '_)) 1) (list 'X 'O '_))

(check-expect (get-column (list (list '_ 'X '_)
(list '_ 'O 'X)
(list '_ '_ '_)) 2) (list '_ 'X '_))


;; get-colum: list num -> list
(define (get-column T3Grid col)
    (cond 
    [(empty? T3Grid) empty]
    [else (cons (list-ref (first T3Grid) col) (get-column (rest T3Grid) col))]))

;; Test Cases:
(check-expect (get-column (list (list '_ '_ '_)
(list 'X '_ '_)
(list '_ '_ '_)) 0) (list '_ 'X '_))

(check-expect (get-column (list (list '_ 'X '_)
(list '_ 'O '_)
(list '_ '_ 'X)) 2) (list '_ '_ 'X))

(check-expect (get-column (list (list 'X 'X '_)
(list '_ 'O 'X)
(list 'O '_ '_)) 0) (list 'X '_ 'O))

;; ;;PURPOSE;;
;; (will=win? T3Grid row col player) determines
;; if the player can we by marking a specific location

;; Examples:
(check-expect (will-win? (list (list '_ '_ '_)
(list '_ '_ '_)
(list '_ '_ '_)) 0 0 'X) false)

(check-expect (will-win? (list (list 'X '_ '_)
(list '_ 'X '_)
(list 'O 'O '_)) 2 2 'O) true)

(check-expect (will-win? (list (list '_ '_ 'O)
(list '_ 'X '_)
(list 'O 'O 'X)) 0 0 'X) false)

(check-expect (will-win? (list (list 'X 'X '_)
(list 'O 'X 'O)
(list 'O '_ '_))
0 2 'X) true)


;; Helper function that checks for the row and column
;; of symbols to see if there is an open space avalible
(define (count-sym list sym)
  (cond
    [(and (empty? (rest list)) (symbol=? (first list) sym)) 1]
    [(empty? (rest list)) 0]
    [(symbol=? (first list) sym) (+ 1 (count-sym (rest list) sym))]
    [else (count-sym (rest list) sym)]))

;; will-win?: list num num sym -> boolean
(define (will-win? T3Grid row column player)
    (cond
    [(and (symbol=? (grid-ref T3Grid row column) '_)
         (or  (= (+ 1 (count-sym (list-ref T3Grid row) player)) (length T3Grid))
              (= (+ 1 (count-sym (get-column T3Grid column) player)) (length T3Grid)))) true]
        [else false]))

;; Test Cases:
(check-expect (will-win? (list (list '_ '_ '_)
(list '_ 'X 'O)
(list '_ 'O 'X)) 0 1 'X) false)

(check-expect (will-win? (list (list 'X '_ '_)
(list '_ 'X '_)
(list 'O 'O 'X)) 2 2 'O) false)

(check-expect (will-win? (list (list '_ '_ 'O)
(list 'O 'X 'X)
(list 'O 'O 'X)) 0 1 'X) false)