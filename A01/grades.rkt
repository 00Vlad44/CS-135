;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname grades) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;Final CS grade
(define (final-cs135-grade cp mid1 mid2 exam assingments)
  (+ (* cp 0.05)(* mid1 0.07)(* mid2 0.13)(* exam 0.3)(* assingments 0.45)))

;;Tests:
(final-cs135-grade 90 90 90 90 90)

;;Final CS grade needed
(define (cs135-final-exam-grade-needed cp mid1 mid2 assigments)
  (/ (- 60 (+ (* cp 0.05)(* mid1 0.07)(* mid2 0.13)(* assigments 0.45))) 0.3))

;;Tests:
(cs135-final-exam-grade-needed 60 60 60 60)
(cs135-final-exam-grade-needed 50 50 50 50)