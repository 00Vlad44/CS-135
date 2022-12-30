;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname cs135search) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;; CS 135 Fall 2020
;; Assignment 05, Problem 2
;; ***************************************************
;;

;;PURPOSE;;
;; (both DL1 DL2) produces the common
;; element between DL1 and DL2

;;Examples:
(check-expect (both (list "b.txt") (list "b.txt" "c.txt")) (list "b.txt"))
(check-expect (both (list) (list)) (list))
(check-expect (both (list "a.txt") (list "b.txt" "c.txt")) (list))

;; both: doc-list doc-list -> doc-list
(define (both DL1 DL2)
    (cond
       [(or (empty? DL1) (empty? DL2)) empty]
       [(string<? (first DL1) (first DL2)) (both (rest DL1) DL2)]
       [(string>? (first DL1) (first DL2)) (both DL1 (rest DL2))]
       [else (cons (first DL1) (both (rest DL1) (rest DL2)))]))

;; Test Cases:
(check-expect (both (list "b.txt") (list "b.txt" "c.txt")) (list "b.txt"))
(check-expect (both (list "b.txt") (list)) (list))
(check-expect (both (list "a.txt") (list "b.txt" "k.txt")) (list))
(check-expect (both (list "b.txt" "k.txt") (list "b.txt" "k.txt")) (list "b.txt" "k.txt"))


;;PURPOSE;;
;; (exclude D1 D2) returns
;; a doc-list that only D1 has

;;Examples:
(check-expect (exclude (list "b.txt" "c.txt") (list "b.txt")) (list "c.txt"))
(check-expect (exclude (list "a.txt") (list)) (list "a.txt"))
(check-expect (exclude (list) (list)) empty)

;; exclude: doc-list doc-list -> doc-list
(define (exclude DL1 DL2)
    (cond
        [(empty? DL1) empty]
        [(and (cons? DL1) (empty? DL2))  DL1]
        [(empty? DL2) (cons (first DL1) (exclude (rest DL1) DL2))] 
        [(string=? (first DL1) (first DL2)) (exclude (rest DL1) (rest DL2))]
        [(string<? (first DL1) (first DL2)) (cons (first DL1) (exclude (rest DL1) DL2))]
        [(string>? (first DL1) (first DL2)) (exclude DL1 (rest DL2))]))

;; Test Cases:
(check-expect (exclude (list "b.txt" "c.txt") (list "b.txt" "c.txt")) (list))
(check-expect (exclude (list "c.txt") (list)) (list "c.txt"))
(check-expect (exclude (list "b.txt" "c.txt") (list)) (list "b.txt" "c.txt"))
(check-expect (exclude (list "a.txt" "b.txt" "c.txt" "d.txt")
                       (list "b.txt" "d.txt")) (list "a.txt" "c.txt"))

;;Purpose;;
;; (keys-retrieve doc an-il) returns
;; a listof Str with lexicographic ordering

;;Examples:
(check-expect (keys-retrieve  "a.txt"
                             (list (list "barks" (list "b.txt"))
(list "cat" (list "a.txt" "c.txt"))
(list "chases" (list "c.txt"))
(list "dog" (list "b.txt" "c.txt"))
(list "sleeps" (list "a.txt"))
(list "suddenly" (list "c.txt"))
(list "the" (list "a.txt" "b.txt" "c.txt")))) (list "cat" "sleeps" "the"))

(check-expect (keys-retrieve  "b.txt"
                             (list (list "barks" (list "b.txt"))
(list "cat" (list "a.txt" "c.txt"))
(list "chases" (list "c.txt"))
(list "dog" (list "b.txt" "c.txt"))
(list "sleeps" (list "a.txt"))
(list "suddenly" (list "c.txt"))
(list "the" (list "a.txt" "b.txt" "c.txt")))) (list "barks" "dog" "the"))

(check-expect (keys-retrieve  "c.txt"
                             (list (list "barks" (list "b.txt"))
(list "cat" (list "a.txt" "c.txt"))
(list "chases" (list "c.txt"))
(list "dog" (list "b.txt" "c.txt"))
(list "sleeps" (list "a.txt"))
(list "suddenly" (list "c.txt"))
(list "the" (list "a.txt" "b.txt" "c.txt")))) (list "cat" "chases" "dog" "suddenly" "the"))

;; Helper function that iterates through the
;; doc and checks the if the key word is in the IN
(define (in-list key-word lst)
    (cond
        [(empty? lst) false]
        [(string=? key-word (first lst)) true]
        [else (in-list key-word (rest lst))]))

;; keys-retrieve: Str IL -> listof Str
(define (keys-retrieve doc an-il)
    (cond
        [(empty? an-il) empty]
        [(in-list doc (second (first an-il)))
         (cons (first (first an-il)) 
            (keys-retrieve doc (rest an-il)))]
        [else (keys-retrieve doc (rest an-il))]))

;; Test Cases:
(check-expect (keys-retrieve "k.txt"
(list (list "jerk" (list "j.txt"))
      (list "creek" (list "k.txt"))
      (list "sweeep" (list "j.txt" "k.txt")))) (list "creek" "sweeep"))

(check-expect (keys-retrieve "j.txt"
(list (list "jerk" (list "j.txt"))
      (list "creek" (list "k.txt"))
      (list "sweeep" (list "j.txt" "k.txt")))) (list "jerk" "sweeep"))

(check-expect (keys-retrieve "a.txt"
(list (list "jerk" (list "j.txt"))
      (list "creek" (list "k.txt"))
      (list "sweeep" (list "j.txt" "k.txt")))) (list))
(check-expect (keys-retrieve "a.txt" (list)) (list))

;;Purpose;;
;; (search sym str1 str2 IL) searches an
;; inverted and determines the str that corresponds
;; to the given str1 str2 in the IL

;; Examples:
(check-expect (search 'both "barks" "dog" (list (list "barks" (list "b.txt"))
(list "cat" (list "a.txt" "c.txt"))
(list "chases" (list "c.txt"))
(list "dog" (list "b.txt" "c.txt"))
(list "sleeps" (list "a.txt"))
(list "suddenly" (list "c.txt"))
(list "the" (list "a.txt" "b.txt" "c.txt")))) (list "b.txt"))

(check-expect (search 'both "chases" "the" (list (list "barks" (list "b.txt"))
(list "cat" (list "a.txt" "c.txt"))
(list "chases" (list "c.txt"))
(list "dog" (list "b.txt" "c.txt"))
(list "sleeps" (list "a.txt"))
(list "suddenly" (list "c.txt"))
(list "the" (list "a.txt" "b.txt" "c.txt")))) (list "c.txt"))

(check-expect (search 'exclude "the" "barks" (list (list "barks" (list "b.txt"))
(list "cat" (list "a.txt" "c.txt"))
(list "chases" (list "c.txt"))
(list "dog" (list "b.txt" "c.txt"))
(list "sleeps" (list "a.txt"))
(list "suddenly" (list "c.txt"))
(list "the" (list "a.txt" "b.txt" "c.txt")))) (list "a.txt" "c.txt"))

;; Helper function that checks values of the two DLs
(define (check-DLs key an-il)
  (cond
    [(empty? an-il) empty]
    [(string=? key (first (first an-il)))
     (second (first an-il))]
    [else
     (check-DLs key (rest an-il))]))

;; search: sym str1 str2 IL -> doc-list
(define (search sym key1 key2 an-il)
  (cond
    [(empty? an-il) empty]
    [(symbol=? 'both sym)
     (both (check-DLs key1 an-il) (check-DLs key2 an-il))]
    [else
     (exclude (check-DLs key1 an-il) (check-DLs key2 an-il))]))

;; Test Cases:
(check-expect (search 'both "barks" "dog" (list (list "barks" (list "b.txt"))
(list "cat" (list "a.txt" "c.txt"))
(list "chases" (list "c.txt"))
(list "dog" (list "b.txt" "c.txt"))
(list "sleeps" (list "a.txt"))
(list "suddenly" (list "c.txt"))
(list "the" (list "a.txt" "b.txt" "c.txt")))) (list "b.txt"))

(check-expect (search 'both "chases" "the" (list (list "barks" (list "b.txt"))
(list "cat" (list "a.txt" "c.txt"))
(list "chases" (list "c.txt"))
(list "dog" (list "b.txt" "c.txt"))
(list "sleeps" (list "a.txt"))
(list "suddenly" (list "c.txt"))
(list "the" (list "a.txt" "b.txt" "c.txt")))) (list "c.txt"))

(check-expect (search 'exclude "chases" "the" (list (list "barks" (list "b.txt"))
(list "cat" (list "a.txt" "c.txt"))
(list "chases" (list "c.txt"))
(list "dog" (list "b.txt" "c.txt"))
(list "sleeps" (list "a.txt"))
(list "suddenly" (list "c.txt"))
(list "the" (list "a.txt" "b.txt" "c.txt")))) (list))

(check-expect (search 'exclude "chases" "the" (list (list "barks" (list "b.txt"))
(list "cat" (list "a.txt" "c.txt"))
(list "chases" (list))
(list "sleeps" (list "a.txt"))
(list "suddenly" (list "c.txt"))
(list "the" (list "a.txt" "b.txt" "c.txt")))) (list))

(check-expect (search 'exclude "chases" "the" (list)) (list))