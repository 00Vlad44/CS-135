;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q02) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;; Vlad Comsa (21027115)
;; CS 135 Fall 2022
;; Assignment 09, Problem 2
;; ***************************************************
;;

;;PURPOSE;;
;; (alphanumeric-only lst) produces a list of
;; string with alphanumeric-only charcters only
;; in each string in the list

;; Examples:
(check-expect (alphanumeric-only '())'())
(check-expect (alphanumeric-only '("5heloo")) '("5heloo"))
(check-expect (alphanumeric-only '("#5heloo")) '())

;; alphanumeric-only: (listOfString) -> (listOfString)
(define (alphanumeric-only lst)
  (filter (lambda (string) (= (length (string->list string))
                              (length (filter
                                       (lambda (char) (or (char-alphabetic? char)
                                           (char-numeric? char))) (string->list string))))) lst))

;; Test Cases:
(check-expect (alphanumeric-only '("5hello" "123hu" "89huyt#"))
'("5hello" "123hu"))
(check-expect (alphanumeric-only '("hello" "?u" "123hu"))
'("hello" "123hu"))
(check-expect (alphanumeric-only '("5h#el?$3#lo" "123#$@?hu#@#" "89huyt?"))
'())
(check-expect (alphanumeric-only '("4hello" "?u" "69killa"))
'("4hello" "69killa"))

;;PURPOSE;;
;; (remove-outliers lst) produces a list of numbers
;; that is less than (x < (mean + σ ) and
;; x > (mean − σ )

;; Examples:
(check-expect (remove-outliers '(1 -2 3 1 0 220)) '(1 -2 3 1 0))
(check-expect (remove-outliers '(1 2)) '(1 2))
  
;; remove-outliers: (listOfNumX) -> (listOfNumX)
(define (remove-outliers lst)
  (filter (lambda (x) (or (= (length lst) 1)(and (<= x (+ (/ (foldr + 0 lst) (length lst))
                           (sqrt (/ (foldr + 0 (map sqr (map (lambda (x) (- x (/ (foldr + 0 lst)
                                                        (length lst)))) lst)) lst) (length lst)))))
                          (>= x (- (/ (foldr + 0 lst) (length lst))
                              (sqrt (/ (foldr + 0 (map sqr (map (lambda (x) (- x (/ (foldr + 0 lst)
                                                        (length lst)))) lst)) lst)
                                            (length lst)))))))) lst)) 

;; Test Cases:
(check-expect (remove-outliers '(1)) '(1))
(check-expect (remove-outliers '(9 8 -3 -9 -8 0 0 0 0 3))
'(-3 0 0 0 0 3))
(check-expect (remove-outliers '(0 0 1 0 0)) '(0 0 0 0))

;;PURPOSE;;
;; (zero-fill string) produces a string that is 20 characters
;; long and fills the given string with 0's until its 20
;; characters long

;; Examples:
(check-expect (zero-fill "") "00000000000000000000")
(check-expect (zero-fill "thisshouldreturnnofi") "thisshouldreturnnofi")
(check-expect (zero-fill "thisshouldreturnnof") "thisshouldreturnnof0")

;; zero-fill: string -> string
(define (zero-fill string)
   (list->string (build-list 20 (lambda (x) 
    (cond
        [(< x (length (string->list string)))
         (list-ref (string->list string) x)]
        [else #\0])))))

;; Test Cases:
(check-expect (zero-fill "abcdefghijklmn") "abcdefghijklmn000000")
(check-expect (zero-fill "he00llo") "he00llo0000000000000")
(check-expect (zero-fill "nofaceno") "nofaceno000000000000")

;;PURPOSE;;
;; (remove-duplicates lst) produces a new list
;; and removes duplicate items in the list, and
;; orders the numbers in the order that they previosuly
;; came in

;; Examples:
(check-expect
(remove-duplicates (list 1 2 3 4 3 2 4 1)) (list 1 2 3 4))
(check-expect
(remove-duplicates (list 1 2 8)) (list 1 2 8))
(check-expect
(remove-duplicates (list 1)) (list 1))

;; remove-duplicates: list -> list
(define (remove-duplicates lst)
  (foldr (lambda (num1 num2)
           (cons num1 (filter (lambda (dup)
                                (not (= num1 dup))) num2))) empty lst))

;; Test Cases:
(check-expect
(remove-duplicates (list 1 9 9 2 1 2 9 1 2 1 1 2 2)) (list 1 9 2))
(check-expect
(remove-duplicates (list 0 0 0 0 1 9 9 2 1 2 9 0 1 2 1 1 2 2)) (list 0 1 9 2))
(check-expect
(remove-duplicates (list 10 1 9 9 2 1 2 9 1 2 1 1 2 2)) (list 10 1 9 2))