;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname heap) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***************************************************
;; CS 135 Fall 2022
;; Assignment 08, Problem 4
;; ***************************************************
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (define-struct hnode (key left right))
;;
;; A (heapof X) is one of:
;; * empty
;; * (make-hnode X (heapof X) (heapof X))
;; requires: all elements in left are >= key
;;           all elements in right are >= key
;;
;; The following function is available for your use:
;; 
;; (heap-print heap key->string) pretty prints the provided (heapof X)
;;   using the key->string function to convert type X to a string
;; NOTE: displays the heap rotated counter-clockwise of an angle of 90 degrees
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define example-heap (make-hnode 1
                                 (make-hnode 15
                                             (make-hnode 60
                                                         (make-hnode 70 '() '())
                                                         '())
                                             (make-hnode 20
                                                         (make-hnode 40 '() '())
                                                         '()))
                                 (make-hnode 5
                                             (make-hnode 10
                                                         (make-hnode 50 '() '())
                                                         '())
                                             (make-hnode 30 '() '()))))

(define example1-heap (make-hnode 1
                                 (make-hnode 15 '() '())
                                 (make-hnode 5 '() '())))

(define example2-heap (make-hnode 1 '() '()))
(define example3-heap (make-hnode 1 (make-hnode 5 '() '()) '()))

;;Examples 4a
;(define-struct hnode (key left right))
;; A (heapof X) is one of:
;; * empty
;; * (make-hnode X (heapof X) (heapof X))
;; requires: all elements in left are >= key
;; all elements in right are >= key

;;PURPOSE;;
;; (heap-add element heap compare) produces the new heap
;; with the newly added heap in the correct position of the of
;; the original heap

;; Examples:
(check-expect (heap-add 25 example-heap <=)
              (make-hnode 1 (make-hnode 5
                                        (make-hnode 25 (make-hnode 30 '() '()) '())
  (make-hnode 10 (make-hnode 50 '() '()) '()))
 (make-hnode 15 (make-hnode 60 (make-hnode 70 '() '()) '())
(make-hnode 20 (make-hnode 40 '() '()) '()))))


;; heap-add: (anyof X) heap bool -> (heapof X)
(define (heap-add element heap compare)
  (cond
    [(empty? heap) (make-hnode element empty empty)]
    [(< element (hnode-key heap))
     (heap-add (hnode-key heap) (make-hnode element (hnode-left heap)
                                            (hnode-right heap)) compare)]
    [else (make-hnode (hnode-key heap) (heap-add element (hnode-right heap) compare)
                      (hnode-left heap))]))

;; Test Cases:
(check-expect (heap-add 10 example2-heap <=)
              (make-hnode 1
                          (make-hnode 10 '() '())
                          '()))
(check-expect (heap-add 15 example3-heap <=)
              (make-hnode 1
 (make-hnode 15 '() '())
 (make-hnode 5 '() '())))

(check-expect (heap-add 3 example1-heap <=)
              (make-hnode 1 (make-hnode 3
  (make-hnode 5 '() '()) '())
 (make-hnode 15 '() '())))

;;PURPOSE;;
;; (heap-remove-min heap compare) produces the new heap
;; with the lowest heap removed from the heap and
;; reorganized to a new heap

;; Examples:
(check-expect (heap-remove-min example3-heap <=)
              (make-hnode 5 '() '()))
(check-expect (heap-remove-min (make-hnode '() '() '()) <=)
              '())

;; heap-remove-min: (heapof X) bool -> (heapof X)
(define (heap-remove-min heap compare)
  (cond
    [(or (empty? heap)
         (and (empty? (hnode-left heap)) (empty? (hnode-right heap)))) empty]
    [(empty? (hnode-right heap)) (hnode-left heap)]
    [(empty? (hnode-left heap)) (hnode-right heap)]
    [(compare (hnode-key (hnode-right heap))
              (hnode-key (hnode-left heap)))
     (make-hnode (hnode-key (hnode-right heap))
                 (hnode-left heap)
                 (heap-remove-min (hnode-right heap) compare))]
    [else (make-hnode (hnode-key (hnode-left heap))
                      (heap-remove-min (hnode-left heap) compare)
                      (hnode-right heap))]))

;; Test Cases:
(check-expect (heap-remove-min example2-heap <=)
              empty)

(check-expect (heap-remove-min example-heap <=)
              (make-hnode 5 (make-hnode 15
  (make-hnode 60 (make-hnode 70 '() '()) '())
  (make-hnode 20 (make-hnode 40 '() '()) '()))
 (make-hnode 10 (make-hnode 50 '() '())
  (make-hnode 30 '() '()))))

(check-expect (heap-remove-min example1-heap <=)
              (make-hnode 5 (make-hnode 15 '() '()) '()))

;;PURPOSE;;
;; (list->heap list compare) produces a heap
;; based off the given list provided and the
;; comparison operator

;; Examples:
(check-expect (list->heap '(70 1 15 5 60 50 40 30 20 10) <=) example-heap)

;; list->heap: (numlist) bool -> (heapof X)
(define (list->heap list compare)
  (cond
    [(empty? list) empty]
    [else (heap-add (first list)
                    (list->heap (rest list) compare) compare)]))

;; Test Cases:
(check-expect (list->heap '(1) <=) example2-heap)
(check-expect (list->heap '(15 5 1) <=) example1-heap)
(check-expect (list->heap '(5 1) <=) example3-heap)

;;PURPOSE;;
;; (heap->list heap compare) produces a list
;; based off the heap in ascending order

;; Examples:
(check-expect (heap->list example-heap <=) '(1 5 10 15 20 30 40 50 60 70))

;; heap->list: (heapof X) bool -> (listof X)
(define (heap->list heap compare)
  (cond
    [(empty? heap) empty]
    [else (cons (hnode-key heap)
                (heap->list (heap-remove-min heap compare) compare))]))

;; Test Cases:
(check-expect (heap->list example1-heap <=) '(1 5 15))
(check-expect (heap->list example2-heap <=) '(1))
(check-expect (heap->list example3-heap <=) '(1 5))

;;PURPOSE;;
;; (heap-sort list compare) produces a sorted
;; list given a list and the desired ordering of
;; the list (ascending or descending)

;; Examples:
(check-expect (heap-sort '(70 1 15 30 20 10) <=)
              '(1 10 15 20 30 70))
(check-expect (heap-sort '() <=)'())
(check-expect (heap-sort '(1) <=) '(1))

;; heap-sort: (listof X) bool -> (sorted-listof X)
(define (heap-sort list compare)
  (cond
    [(empty? list) empty]
    [else (heap->list (list->heap list compare) compare)]))

;; Test Cases:
(check-expect (heap-sort '(70 1 15 5 60 50 40 30 20 10) <=)
              '(1 5 10 15 20 30 40 50 60 70))

(check-expect (heap-sort '(70 1 15 8) <=)
              '(1 8 15 70))

(check-expect (heap-sort '(7 10 3 1) <=)
              '(1 3 7 10))