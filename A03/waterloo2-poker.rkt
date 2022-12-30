;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname waterloo2-poker) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; ***************************************************
;; CS 135 Fall 2020
;; Assignment 03, Problem 2
;; ***************************************************
;;

;; A Hand is a: (cons Card (cons Card empty))
;; requires: the two cards are not identical

;; A Rank is one of: 2, 3, 4, 5, 6, 7, 8, 9, 10, 'J, 'Q, 'K, 'A
;; A Suit is one of: 'C, 'D, 'H, 'S
;; A Card is a: (cons Rank (cons Suit empty))

;; Examples
(check-expect (ordinality (cons 'J (cons 'C empty))) 11)
(check-expect (ordinality (cons 'A (cons 'D empty))) 14)
(check-expect (ordinality (cons 'K (cons 'H empty))) 13)
(check-expect (ordinality (cons 'Q (cons 'D empty))) 12)
(check-expect (ordinality (cons 10 (cons 'S empty))) 10)
(check-expect (ordinality (cons 6 (cons 'H empty))) 6)

;; suit: Card -> Suit
(define (suit c)
  (second c))

;; (ordinality c) produces the num of
;; the card h

;; ordinality: number/symbol -> number
(define (ordinality c)
  (cond [(number? (first c)) (first c)]
        [(symbol=? 'J (first c)) 11]
        [(symbol=? 'Q (first c)) 12]
        [(symbol=? 'K (first c)) 13]
        [(symbol=? 'A (first c)) 14]
        [else 0]))

;; Tests:
(check-expect (ordinality (cons 2 (cons 'H empty))) 2)
(check-expect (ordinality (cons 'J (cons 'H empty))) 11)
(check-expect (ordinality (cons 3 (cons 'H empty))) 3)
(check-expect (ordinality (cons 5 (cons 'A empty))) 5)
(check-expect (ordinality (cons 10 (cons 'H empty))) 10)


;; (strength h) produces the strength
;; of the hand h

;; Examples:
(check-expect (strength (cons (cons 'Q (cons 'S empty)) (cons (cons 'K (cons 'H empty)) empty))) 2)
(check-expect (strength (cons (cons 'K (cons 'S empty)) (cons (cons 'Q (cons 'H empty)) empty))) 2)
(check-expect (strength (cons (cons 'A (cons 'S empty)) (cons (cons 'A (cons 'H empty)) empty))) 3)
(check-expect (strength (cons (cons 'A (cons 'S empty)) (cons (cons 'A (cons 'H empty)) empty))) 3)

;; strength: symbol/number -> number
(define (strength h)
  (cond 
    [(and (or (= (- (ordinality (second h)) (ordinality (first h))) 1)
     (= (- (ordinality (first h)) (ordinality (second h))) 1))
          (symbol=? (suit(first h)) (suit(second h)))) 4]
    [(= (ordinality (first h)) (ordinality (second h))) 3]
    [(or (= (- (ordinality (second h)) (ordinality (first h))) 1)
     (= (- (ordinality (first h)) (ordinality (second h))) 1))2]
    [(symbol=? (suit(first h)) (suit(second h))) 1]
    [else 0]))

;; Tests:
(check-expect (strength (cons (cons 'A (cons 'H empty)) (cons (cons 'K (cons 'H empty)) empty))) 4)
(check-expect (strength (cons (cons 'A (cons 'H empty)) (cons (cons 'K (cons 'S empty)) empty))) 2)
(check-expect (strength (cons (cons 'K (cons 'S empty)) (cons (cons 'Q (cons 'H empty)) empty))) 2)
(check-expect (strength (cons (cons 10 (cons 'H empty)) (cons (cons 6 (cons 'H empty)) empty))) 1)
(check-expect (strength (cons (cons 10 (cons 'H empty)) (cons (cons 10 (cons 'D empty)) empty))) 3)
(check-expect (strength (cons (cons 3 (cons 'H empty)) (cons (cons 10 (cons 'D empty)) empty))) 0)





;; (hand<? h1 h2) produces a boolean
;; that determines which hand is higer

;; Examples:
(check-expect (hand<? (cons (cons 'A (cons 'S empty))
               (cons (cons 'J (cons 'S empty)) empty))
               (cons (cons 10 (cons 'C empty))
               (cons (cons 6 (cons 'H empty)) empty))) false)

(check-expect (hand<? (cons (cons 9 (cons 'S empty))
              (cons (cons 5 (cons 'H empty)) empty))
              (cons (cons 'A (cons 'C empty))
              (cons (cons 'K (cons 'D empty)) empty))) true)

(check-expect (hand<? (cons (cons 9 (cons 'S empty))
              (cons (cons 8 (cons 'S empty)) empty))
              (cons (cons 'A (cons 'C empty))
              (cons (cons 'K (cons 'D empty)) empty))) false)

;; hand<? symbol/symbol -> boolean (higher hand)
(define (hand<? h1 h2)
  (cond
    [(> (strength h2) (strength h1)) true]
    [else false]))

;; Tests:
(check-expect (hand<?
               (cons
                (cons 'A (cons 'S empty))
                (cons (cons 5 (cons 'H empty)) empty))
                (cons (cons 'A (cons 'C empty))
                (cons (cons 'A (cons 'D empty)) empty) )) true)

(check-expect (hand<?
               (cons
                (cons 'A (cons 'S empty))
                (cons (cons 'J (cons 'S empty)) empty))
                (cons (cons 2 (cons 'S empty))
                (cons (cons 4 (cons 'D empty)) empty) )) false)


;; (winner h1 h2) produces the higher
;; hand of h1 and h2

;; Examples:
(check-expect (winner
               (cons
                (cons 'K (cons 'S empty))
                (cons (cons 10 (cons 'H empty)) empty))
                (cons (cons 'A (cons 'C empty))
                (cons (cons 'A (cons 'D empty)) empty) )) 'hand2)

(check-expect (winner
               (cons
                (cons 'A (cons 'S empty))
                (cons (cons 'J (cons 'S empty)) empty))
                (cons (cons 3 (cons 'S empty))
                (cons (cons 2 (cons 'H empty)) empty) )) 'hand2)


;; winner: symbol/symbol -> symbol (higher hand)
(define (winner h1 h2)
  (cond
    [(> (strength h1) (strength h2)) 'hand1]
    [(> (strength h2) (strength h1)) 'hand2]
    [(= (strength h2) (strength h1)) 'tie]))

;; Tests:
(check-expect (winner
               (cons
                (cons 'A (cons 'S empty))
                (cons (cons 1 (cons 'H empty)) empty))
                (cons (cons 'A (cons 'C empty))
                (cons (cons 'A (cons 'D empty)) empty) )) 'hand2)

(check-expect (winner
               (cons
                (cons 'A (cons 'S empty))
                (cons (cons 'J (cons 'S empty)) empty))
                (cons (cons 2 (cons 'S empty))
                (cons (cons 4 (cons 'D empty)) empty) )) 'hand1)

(check-expect (winner
               (cons
                (cons 'J (cons 'C empty))
                (cons (cons 7 (cons 'C empty)) empty))
                (cons (cons 'J (cons 'S empty))
                (cons (cons 7 (cons 'S empty)) empty) )) 'tie)


;; (valid-hand? h) determines whether
;; the hand is valid using boolean

;; Examples:
(check-expect(valid-hand? (cons
          (cons 9 (cons 'D empty))
           (cons (cons 'J (cons 'C empty)) empty) )) true)

(check-expect(valid-hand? (cons
          (cons 'A (cons 'A empty))
           (cons (cons 'A (cons 'H empty)) empty) )) false)

(check-expect(valid-hand? (cons
          (cons 1 (cons 'A empty))
           (cons (cons 'A (cons 'H empty)) empty) )) false)

(check-expect(valid-hand? (cons
          (cons 'A (cons 'C empty))
           (cons (cons 'A (cons 'A empty)) empty) )) false)


;; valid-hand?: value/boolean -> boolean
(define (valid-hand? h)
  (cond
    [(and (list? h) (cons? h)
          (cons? (rest h)) (cons? (first h))
          (cons? (rest(first h)))(cons? (second h))
          (cons? (rest(second h)))
     (not (or (and (number? (first(first h))) (number? (first(second h)))
                   (=  (first(first h)) (first(second h)))
                   (symbol=? (second(first h)) (second(second h))))
     (and (symbol? (first(first h))) (symbol? (first(second h)))
          (symbol=? (first(first h)) (first(second h)))
          (symbol=? (second(first h)) (second(second h))))))
     (and (integer? (ordinality (first h)))
          (>= (ordinality (first h)) 2) (<= (ordinality (first h)) 14))
     (and (integer? (ordinality (second h)))
          (>= (ordinality (second h)) 2) (<= (ordinality (second h)) 14))
     (or (symbol=? 'H (second (first h))) (symbol=? 'D (second (first h)))
         (symbol=? 'S (second (first h))) (symbol=? 'C (second (first h))))
     (or (symbol=? 'H (second (second h))) (symbol=? 'D (second (second h)))
         (symbol=? 'S (second (second h))) (symbol=? 'C (second (second h))))
     (empty? (rest (rest (first h))))
          (empty? (rest (rest (second h)))) (empty? (rest(rest(second h)))))true]
    [else false]))
    
;; Tests:
(check-expect(valid-hand? (cons
          (cons 'A (cons 'S empty))
           (cons (cons 'A (cons 'H empty)) empty) )) true)

(check-expect(valid-hand? (cons
          (cons 10 (cons 'S empty))
           (cons (cons 'A (cons 'H empty)) empty) )) true)

(check-expect(valid-hand? (cons
          (cons 'y (cons 'S empty))
           (cons (cons 'A (cons 'H empty)) empty) )) false)

(check-expect(valid-hand? (cons
          (cons 'A (cons 'S empty))
           (cons (cons 'A (cons 'S empty)) empty) )) false)

(check-expect(valid-hand? (cons
          (cons 'A (cons 'S empty))
           (cons (cons 'K (cons 'S empty)) empty) )) true)

(check-expect(valid-hand? (cons
          (cons 8 (cons 'S empty))
           (cons (cons 8 (cons 'S empty)) empty) )) false)

(check-expect(valid-hand? (cons
          (cons 2.870 (cons 'S empty))
           (cons (cons 6 (cons 'S empty)) empty) )) false)