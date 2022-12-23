;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname tubes) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; A Game is (make-game Nat Nat (listof (listof Sym)))
(define-struct game (tubesize maxcolours tubes))

(define emptygame2
  (make-game 10 3 empty))

(define emptygame3
  (make-game 10 3 (list empty empty)))

(define smallgame1
  (make-game 2 2
             (list (list 'blue 'red)
                   (list 'blue 'red)
                   (list))))

(define smallgame2
  (make-game 2 3
             (list (list 'blue 'red)
                   (list 'blue 'red)
                   (list))))

(define smallinvalidgame1
  (make-game 2 1
             (list (list 'blue 'red)
                   (list 'blue 'red)
                   (list))))


(define smallinvalidgame2
  (make-game 2 2
             (list (list 'blue 'red)
                   (list 'blue 'blue)
                   (list))))

(define smallinvalidgame3
  (make-game 2 2
             (list (list 'blue 'red 'blue)
                   (list 'red)
                   (list))))


(define mediumgamestuck
  (make-game 2 3
             (list (list 'blue 'red)
                   (list 'red 'yellow)
                   (list 'yellow 'blue)
                   )))

(define largergame
  (make-game 3 3
             (list (list 'red 'red 'red)
                   (list 'yellow 'blue 'yellow)
                   (list 'red 'yellow 'blue)
                   (list))))

;; remove-duplicates: list -> list
(define (remove-duplicates lst)
  (foldr (lambda (sym1 sym2)
           (cons sym1 (filter (lambda (dup)
                                (not (symbol=? sym1 dup))) sym2))) empty lst))

(define (check-size size dup-lst lst tempcount)
  (cond
    [(and (empty? lst) (empty? dup-lst)) true]
    [(and (not (= size tempcount))
          (empty? lst)) false]
    [(and (= size tempcount)
          (empty? lst)) (check-size size (rest dup-lst) lst 0)]
    [(symbol=? (first dup-lst)
               (first lst)) (check-size size dup-lst (rest lst) (add1 tempcount))]
    [else (check-size size dup-lst (rest lst) tempcount)]))


(define (check-colour? size num los)
  (cond
    [(and (check-size size (remove-duplicates los) los 0)
          (= num (length (remove-duplicates los)))) true]
    [else false]))

;; Examples 1a:

(check-expect (check-colour? 1 1 '(blue red)) true)


;; Examples 1b:
(check-expect (valid-game? smallinvalidgame1) false)
(check-expect (valid-game? smallgame1) true)
(check-expect (valid-game? smallinvalidgame2) false)

;; Examples 1c:
(check-expect (remove-completed smallgame1) smallgame1)
(check-expect (remove-completed mediumgamestuck)
              (make-game 2 3
             (list (list 'blue 'red)
                   (list 'red 'yellow)
                   (list 'yellow 'blue)
                   )))

;; Examples 1d:
(check-expect (finished-game? emptygame2) true)
(check-expect (finished-game? emptygame3) true)
(check-expect (finished-game? mediumgamestuck) false)

;; Examples 1e:
(check-expect (num-blocks '('(a a a a))) 1)
(check-expect (num-blocks '('(a b a b))) 4)
(check-expect (num-blocks (list empty '(a a a) '(a a b a a))) 4)

;; Examples 1f:
(check-expect (equiv-game? emptygame2 smallgame2) false)
(check-expect (equiv-game? smallgame1 smallgame1) true)
(check-expect (equiv-game? smallgame1 smallgame1) true)
(check-expect (equiv-game? smallgame1 smallgame2) false)
(check-expect (equiv-game? smallgame1 mediumgamestuck) false)

(define (all-equiv? log1 log2) true)
;; Examples 1g:
(check-expect (all-equiv? (list smallgame1) (list smallgame1)) true)
(check-expect (all-equiv? (list smallgame1)(list mediumgamestuck)) false)


(define (next-games gm)
  empty)
(define (test-next-games gm expected) (all-equiv? (next-games gm) expected))
;; Examples Q1h:
(check-expect (test-next-games (make-game
                                2 2
                                (list
                                 (list 'A 'B)
                                 (list 'B 'A)
                                 (list)))
                               (list
                                (make-game 2 2 (list
                                                (list 'B)
                                                (list 'B 'A)
                                                (list 'A)))
                                (make-game 2 2 (list
                                                (list 'A 'B)
                                                (list 'A)
                                                (list 'B))))) true)



               