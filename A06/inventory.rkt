;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname inventory) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;; CS 135 Fall 2020
;; Assignment 06, Problem 2
;; ***************************************************
;;


;; 2a
;; Ev: (make-ev str str num num num)
(define-struct ev (model year price mileage mpge))


;;PURPOSE;;
;; (adjust-prices lst num) changes the price
;; of the list-ev structure by a percentage (num)

;; Examples
(check-expect (adjust-prices (list (make-ev "Chevy" 2010 45000 3150 103)) 0.5)
              (list (make-ev "Chevy" 2010 67500 3150 103)))
(check-expect (adjust-prices (list (make-ev "Nissan Leaf" 2013 5000 230000 98)) -0.2)
              (list (make-ev "Nissan Leaf" 2013 4000 230000 98)))
(check-expect (adjust-prices (list (make-ev "Audi" 2011 41000 3000 23)) -0.5)
              (list (make-ev "Audi" 2011 20500 3000 23)))

;; adjust-prices: list num -> list
(define (adjust-prices lst num)
  (cond
    [(empty? lst) empty]
    [else (cons (make-ev (ev-model (first lst)) (ev-year (first lst))
                         (* (ev-price (first lst)) (+ 1 num))
                         (ev-mileage (first lst))
                         (ev-mpge (first lst))) (adjust-prices (rest lst) num))]))

;; Test Cases:
(check-expect (adjust-prices (list (make-ev "Nissan Leaf" 2013 5000 230000 98)) 0.1)
              (list (make-ev "Nissan Leaf" 2013 5500 230000 98)))
(check-expect (adjust-prices (list (make-ev "Nissan Leaf" 2013 5000 230000 98)) -0.1)
              (list (make-ev "Nissan Leaf" 2013 4500 230000 98)))
(check-expect (adjust-prices (list (make-ev "Tesla M3" 2022 60000 5 133)) -0.5)
              (list (make-ev "Tesla M3" 2022 30000 5 133)))


(define models (list "Hyundai Kona" "Audi e-Tron" "Chevy Bolt"
"BMW i4" "Tesla M3" "Nissan Leaf"))
(define years (list 2022 2021 2020 2022 2022 2013))
(define prices (list 36000 40000 35000 53000 60000 5000))
(define mileage (list 12000 4000 3050 25 5 230000))
(define mpge (list 132 63 123 75 133 98))


(define models1 (list "Hyundai" "Audi" "Chevy"
"BMW" "Tesla" "Nissan"))
(define years1 (list 2012 2011 2010 2002 2020 2013))
(define prices1 (list 34000 41000 45000 33000 62000 2000))
(define mileage1 (list 11000 3000 3150 250 15 20000))
(define mpge1 (list 132 23 103 35 113 78))

(define models2 (list "Hyundaii" "Audi" "Chevy"
"BMW"))
(define years2 (list 2012 2011 2010 2002))
(define prices2 (list 34000 41000 45000 33000))
(define mileage2 (list 11000 3000 3150 250))
(define mpge2 (list 132 23 103 35))

(define models3 (list "Hyundaii"))
(define years3 (list 2012))
(define prices3 (list 34000))
(define mileage3 (list 11000))
(define mpge3 (list 132))

;;PURPOSE;;
;; (build-inventory models years prices mileage mpge) takes 5 different
;; lists that all have the same length and puts it into a single
;; list of evs

;; Examples:
(check-expect (build-inventory models years prices mileage mpge)
(list (make-ev "Nissan Leaf" 2013 5000 230000 98)
(make-ev "Tesla M3" 2022 60000 5 133)
(make-ev "BMW i4" 2022 53000 25 75)
(make-ev "Chevy Bolt" 2020 35000 3050 123)
(make-ev "Audi e-Tron" 2021 40000 4000 63)
(make-ev "Hyundai Kona" 2022 36000 12000 132)))

;; Helpter function that accumalates the lists of lits in to
;; one list containing evs
(define (build-inventory/acc models years prices mileage mpge acc)
  (cond
  [(empty? models) acc]
    [else (build-inventory/acc (rest models) (rest years)
                        (rest prices) (rest mileage) (rest mpge)
                        (cons (make-ev (first models) (first years)
                         (first prices)
                         (first mileage)
                         (first mpge))acc))]))

;; build-inventory: list list list list list -> list
(define (build-inventory models years prices mileage mpge)
  (build-inventory/acc models years prices mileage mpge (list)))

;; Test Cases:
(check-expect (build-inventory models1 years1 prices1 mileage1 mpge1)
(list (make-ev "Nissan" 2013 2000 20000 78)
(make-ev "Tesla" 2020 62000 15 113)
(make-ev "BMW" 2002 33000 250 35)
(make-ev "Chevy" 2010 45000 3150 103)
(make-ev "Audi" 2011 41000 3000 23)
(make-ev "Hyundai" 2012 34000 11000 132)))

(check-expect (build-inventory models2 years2 prices2 mileage2 mpge2)
(list
(make-ev "BMW" 2002 33000 250 35)
(make-ev "Chevy" 2010 45000 3150 103)
(make-ev "Audi" 2011 41000 3000 23)
(make-ev "Hyundaii" 2012 34000 11000 132)))

(check-expect (build-inventory models3 years3 prices3 mileage3 mpge3)
              (list
               (make-ev "Hyundaii" 2012 34000 11000 132)))

;;PURPOSE;;
;; (compare-ev lst1 lst2) consumes two lists of evs and produces
;; a symbol that indicates which car is greater than, less than,
;; or equal to the other car


;; Examples:
(check-expect (compare-ev (make-ev "Nissan Leaf" 2013 5000 230000 98)
(make-ev "Nissan Leaf" 2013 5000 230000 98)) 'eq)

(check-expect (compare-ev (make-ev "Nissan Leaf" 2013 5000 230000 98)
(make-ev "BMW i4" 2022 53000 25 75)) 'lt)

(check-expect (compare-ev (make-ev "BMW i4" 2022 53000 25 75)
                          (make-ev "Nissan Leaf" 2013 5000 230000 98)) 'gt)

;; compare-ev: list list -> symbol
(define (compare-ev lst1 lst2)
  (cond
    [(or (> (ev-year  lst1) (ev-year  lst2))
         (and (= (ev-year lst1) (ev-year lst2)) (> (ev-mpge lst1) (ev-mpge lst2)))
         (and (= (ev-year lst1) (ev-year lst2)) (= (ev-mpge lst1) (ev-mpge lst2))
              (<  (ev-mileage lst1)  (ev-mileage lst2)))) 'gt]
    [(and (=  (ev-mileage lst1) (ev-mileage lst2))
              (= (ev-year lst1) (ev-year lst2))
              (= (ev-mpge lst1) (ev-mpge lst2))) 'eq]
    [else 'lt]))

;; Test Cases:
(check-expect (compare-ev (make-ev "Nissan Leaf" 2013 5000 130000 98)
(make-ev "Nissan Leaf" 2013 5000 230000 98)) 'gt)

(check-expect (compare-ev (make-ev "Nissan Leaf" 2013 5000 230000 98)
(make-ev "BMW i4" 2022 53000 2556666 7555)) 'lt)

(check-expect (compare-ev (make-ev "BMW i4" 2013 53000 25 995)
                          (make-ev "Nissan Leaf" 2013 5000 230000 98)) 'gt)


;;PURPOSE;;
;; (sort-evs list) consumes a list and sorts it from year (decscending order,
;; mpge (descending order), and by mileage (ascrnding order), depending on if
;; certain criteria are met

;; Examples
(check-expect (sort-evs (list (make-ev "Nissan" 2013 2000 20000 78)
(make-ev "Tesla" 2020 62000 15 113)
(make-ev "BMW" 2002 33000 250 35)
(make-ev "Chevy" 2010 45000 3150 103)
(make-ev "Audi" 2011 41000 3000 23)
(make-ev "Hyundai" 2012 34000 11000 132)))
              (list 
(make-ev "Tesla" 2020 62000 15 113)
(make-ev "Nissan" 2013 2000 20000 78)
(make-ev "Hyundai" 2012 34000 11000 132)
(make-ev "Audi" 2011 41000 3000 23)
(make-ev "Chevy" 2010 45000 3150 103)
(make-ev "BMW" 2002 33000 250 35)))

(check-expect (sort-evs (list (make-ev "Nissan Leaf" 2013 5000 230000 98)))
              (list (make-ev "Nissan Leaf" 2013 5000 230000 98)))

(define (smallest-first/acc lst mmin new-lst)
    (cond 
    [(empty? lst) (cons mmin new-lst)]
    [(compare-ev-alt mmin (first lst)) (smallest-first/acc (rest lst) (first lst) (cons mmin new-lst))]
    [(compare-ev-alt (first lst)  mmin) (smallest-first/acc (rest lst) mmin (cons (first lst) new-lst))]
    [else (smallest-first/acc (rest lst) mmin (cons (first lst) new-lst))]))

(define (smallest-first lst)
    (smallest-first/acc (rest lst) (first lst) empty))

(define (sel-sort/sf lst)
    (cond [(empty? (rest lst)) lst]
    [else (cons (first lst) (sel-sort/sf (smallest-first (rest lst))))]))

(define (sel-sort lst)
    (cond
    [(empty? lst) empty]
    [else (sel-sort/sf (smallest-first lst))]))

;; Set conditions to compare to
(define (compare-ev-alt lst1 lst2)
    (cond
    [(< (ev-year lst1) (ev-year lst2)) true]
    [(> (ev-year lst1) (ev-year lst2)) false]
    [(< (ev-mpge lst1) (ev-mpge lst2)) true]
    [(> (ev-mpge lst1) (ev-mpge lst2)) false]
    [(< (ev-mileage lst1) (ev-mileage lst2)) true]
    [(> (ev-mileage lst1) (ev-mileage lst2)) false]
    [else true]))

;; sort-evs: list -> list
(define (sort-evs lst)
    (sel-sort lst))
    
;; Test Cases:
(check-expect (sort-evs (list (make-ev "Tesla M3" 2022 60000 5 133)
(make-ev "Hyundai Kona" 2022 36000 12000 132)
(make-ev "BMW i4" 2022 53000 25 75)))
              (list (make-ev "Tesla M3" 2022 60000 5 133)
(make-ev "Hyundai Kona" 2022 36000 12000 132)
(make-ev "BMW i4" 2022 53000 25 75)))