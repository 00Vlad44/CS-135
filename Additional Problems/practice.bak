;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname practice) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define somewhere (make-posn 7 2))

(define (my-posn-temp p)
  (sqrt (+ (sqr (- (posn-y (posn-x)) (posn-x (posn-x))))
        (sqr (- (posn-y (posn-y)) (posn-x (posn-y)))))))

;(check-expect (my-posn-temp (make-posn 0 7)
    ;                    (make-posn 0 0)) 7)

(define-struct book (title author year))
;; A book is a (make-book Str Str Num)
;; Requires: year >= 2010

(define book1 (make-book "Possiden" "Percy Jackson" 2022))

(book-title book1)
(book-author book1)
(book-year book1)

(define-struct inventory (desc price availible))
(define my-tree '(1 (2 (3 4) (5 6)) (7 (8 9) (10 11))))
(define (count-leaves t)
  (cond
    [(empty? t) 0]
    [else
     (+ (count-leaves (first t)) (count-leaves (rest t)))]))



(define (times-square lon)
  (foldr * 1 (filter (lambda (x) (integer? (sqrt x))) lon)))

(define (keep-multiples3 lon)
  (filter (lambda (x) (integer? (/ x 3))) lon))

(define (keep-multiples23s lon)
  (filter (lambda (x) (or (integer? (/ x 3))
                          (integer? (/ x 2)))) lon))

(define (10-30 lon)
  (filter (lambda (x) (and (>= x 10) (<= x 30))) lon))

(define (short-string los)
  (filter (lambda (x) (<= (string-length x) 6)) los))


(define (odds lst)
  (local [(define (f x) (odd? x))]
  (filter f lst)))

(define (sum-odd-or-sum-even lst)
  (local [(define (odd x) (odd? x))]
    (local [(define (even x) (even? x))]
      (cond
        [(> (length (filter odd lst)) (length (filter even lst)))
         (foldr + 0 (filter odd lst))]
        [else (foldr + 0 (filter even lst))]))))


(define (make-divisible? n lst)
  (map (lambda (x) (integer? (/ x n))) lst))


(define (isort pred? lst)
  (quicksort lst pred?))

(define-struct gnode (key children))

(define (reverse-gt gt)
  (cond
    [(empty? (gnode-children)) (make-gnode (gnode-key gt) empty)]
    [else (make-gnode (gnode-key gt)
                      (map reverse-gt (reverse (gnode-children gt))))]))

(define (squash-range lst)
  (map (lambda (x) (/ x 255)) lst))

(define (greet-each los)
  (map (lambda (x) (string-append "Hi " x "!")) los))

(define (neg-odd lon)
  (map (lambda (x)
         (cond
           [(odd? x) (* -1 x)]
           [else x])) lon))


;; Other ways to count-odd
;; (foldr + 0 (filter odd? lon))
;; (foldr + 0 (map (lambda (x)
              ;;      (cond
             ;;         [(odd? x) x]
             ;;         [else (* x 0)])) lon))
;; Using foldr twice
(define (count-odd lon)
  (foldr (lambda (x y)
           (cond
             [(odd? x) (+ x y)]
             [else y])) 0 lon))

(define (prod lon)
  (foldr * 1 lon))

(define (total-lengthV1 lon)
  (foldr (lambda (x y) (+ (length x) y)) 0 lon))

(define (total-lengthV2 lon)
  (foldr (lambda (x y) (+ (foldr (lambda (j k) 1) 0 x) y))  0 lon))

(define (average lon)
  (/ (foldr + 0 lon) (length lon)))

(define (averageV2 lon)
  (/ (foldr + 0 lon)
     (foldr (lambda (x y) (+ (length x) y)) 0 lon)))


(define (times-square-square lon)
  (foldr * 1 (filter (lambda (x) (integer? (sqrt x))) lon)))


(define (keep-evens2 lon)
  (foldr (lambda (f x)
           (cond
             [(even? f) (cons f x)]
             [else x])) '() lon))

(define (sum-even lon)
  (foldr + 0 (filter even? lon)))

(define (sum-even2 lon)
  (foldr + 0 (filter (lambda (x) (integer? (/ x 2))) lon)))


(define (sum-even3 lon)
  (foldr + 0 (foldr (lambda (x y)
           (cond
             [(even? x) (cons x y)]
             [else y])) '() lon)))

(define (multiply-each lst n)
  (map (lambda (x) (* x n)) lst))

(define (add-total lst)
  (map (lambda (x) (+ x (foldr + 0 lst))) lst))

(define-struct node (key left right))

(define (count-smaller n t count)
  (cond
    [(empty? t) count]
    [(< (node-key t) n) (add1 count)]
    [(= n (node-key t)) (count-smaller n (node-left t) count)]
    [else (count-smaller n (node-right t) count)]))


(define (count-small n t)
  (count-smaller n t 0))

(define (triangles k)
  (build-list k (lambda (x) (/ (* (+ 1 x) x) 2))))
  
  


(define example
  (make-node 5 empty empty))



(define (shift-left n lst)
  (cond
    [(= 0 n) lst]
    [else (append (list (first (reverse lst))) (reverse (rest (reverse lst))))]))


(define (destruction-sort lst)
  (foldr (lambda (x y)
           (cond
             [(member? x y) y]
             [else (cons x y)])) '() lst))



