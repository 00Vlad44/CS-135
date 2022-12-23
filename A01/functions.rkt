;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname functions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;Manhattan distance: |x1-x2| + |y1-y2| -> Num
(define (manhattan-distance x1 y1 x2 y2)
  (+(abs(- x1 x2)) (abs(- y1 y2))))

;; Tests:
(manhattan-distance 2 2 3 3)

;; Ideal Gas Law: (/ (* n R T) V) -> Num
(define R 8.3144626)
(define (pressure n T V)
  (/ (* n R T) V))

;; Tests:
(pressure 1 1 1)

;; Logit:
(define (logit p)
  (log(/ p (- 1 p))))

;; Test:
(logit 0.025)

;;Exponential decay of a radioactive substance:
(define (q s d t)
  (* s (expt e(* d t -1))))

;;Tests:
(q 1 2 3)


;;Surface area of right circular cone
(define (cone-area r h)
  (*(* pi r)(+ r (sqrt(+(sqr h)(sqr r))))))

;;Tests;
(cone-area 2 3)

;;Black-Scholes formula
(define (d1 maturity rate volatility spot-price strike-price)
  (*(/ 1(* volatility (sqrt maturity)))
    (+ (log (/ spot-price strike-price))
       (*(+ rate(/(sqr volatility ) 2)) maturity))))
(d1 1 2 3 4 5)

            