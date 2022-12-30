;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname conversion) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;mph to m/s converter
(define mileTometer 1609.344)
(define hoursToseconds 3600)
(define (mph->m/s mph)
  (* mph(/ mileTometer hoursToseconds)))

;;Tests:
(mph->m/s 5)
(mph->m/s 10)
(mph->m/s -10)

;;Psi to Pa converter
(define lbf-N 4.4482)
(define foot-inch 12)
(define foot-meter 0.3048)
(define (psi->pa PSI)
  (*(/ (* PSI lbf-N)(sqr foot-meter))(sqr foot-inch)))

;;Tests:
(psi->pa 2)

;;Torque function
(define (lbf-ft->Nm torque)
  (*(* torque lbf-N) foot-meter))

;;Tests:
(lbf-ft->Nm 50)
