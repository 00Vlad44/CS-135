;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname nat-list) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))

(define (helper-log n tail) (if (= n 0) 0 (if (<= n (expt 10 tail)) (if (= n (expt 10 tail))tail (- tail 1)) (helper-log n (+ tail 1)))))

(define (nat->list-helper n initial counter)
    (cond
    [ (= counter initial) empty]
    [else (cons (modulo n 10) (nat->list-helper (floor (/ n 10)) (+ 1 initial) counter))]
    ))


(define (nat->list n)
    (nat->list-helper n 0 (+ 1(helper-log n 0)))
)



(define (list->nat-helper lst acc tail)
    (cond
    [(empty? lst) acc]
    [else (list->nat-helper (rest lst) (+ acc (* (expt 10 tail) (first lst))) (+ 1 tail))]
    )
)

(define (list->nat lst)
  (list->nat-helper lst 0 0)
  ) 