;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ancestor-trees) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;; CS 135 Fall 2022
;; Assignment 07, Problem 1
;; ***************************************************
;;

(define-struct anode (name father mother))
;; An ANode is a (make-anode Str AT AT)
;; An ancestor tree (AT) is one of:
;; * empty
;; * ANode
;;
;; Requires: each name (Str) is unique

(define copyright-free-ancestors
  (make-anode "Liso" (make-anode "Homern't"
                                 (make-anode "Abrahum" (make-anode "Arville" empty empty) empty)
                                 (make-anode "Mana" empty empty))
              (make-anode "Merge"
                          (make-anode "Cloncy" empty (make-anode "Bombi" empty empty)) empty)))

;; listof-anode-template: ANode
(define (anode-template ANode)
  (cond
    [(empty? (first ANode)) ...]
    [else (anode-template
           (make-anode
           (anode-name (... (frist ANode)))
               (anode-father (... (second ANode)))
               (anode-mother (... (third ANode)))))]))

;; at-template: (listof ANode) -> Any
(define (at-template AT)
  (cond
    [(empty? AT) ...]
        [(anode? AT) (... (anode-name AT)
                          (at-template (anode-father AT))
                          (at-template (anode-mother AT)))]))

;; Examples 1a:


;; find-subtree: AT AT -> AT
(define (find-subtree AT name)
  (cond
    [(empty? AT) empty]                 
    [(string=? name (anode-name AT)) AT]
    [else
     (local [(define father-tree (find-subtree (anode-father AT) name))]
      (local [(define mother-tree (find-subtree (anode-mother AT) name))]
          (cond
            [(not (empty? father-tree)) father-tree]
            [(not (empty? mother-tree)) mother-tree]
            [else empty])))]))
            
            
    

;; Examples 1b:
(check-expect (find-subtree copyright-free-ancestors "Bombi") (make-anode "Bombi" empty empty))
(check-expect (find-subtree copyright-free-ancestors "Merge") (make-anode "Merge"
                          (make-anode "Cloncy" empty (make-anode "Bombi" empty empty)) empty))
     
    


;;Examples 1c:
(check-expect (get-f-generation copyright-free-ancestors 1)
              (list "Homern't" "Merge"))

(check-expect (get-f-generation copyright-free-ancestors 2)
              (list "Abrahum" "Mana" "Cloncy"))

(check-expect (get-f-generation copyright-free-ancestors 3)
              (list "Arville" "Bombi"))

;; get-f-generation: AT Nat -> (listof Str)
(define (get-f-generation at generation)
  (local [(define (get-f-generation/acc at generation acc)
            (cond
              [(empty? at) acc]
              [(zero? generation) (cons (anode-name at) acc)]
              [else (get-f-generation/acc
                     (anode-father at) (sub1 generation)
                     (get-f-generation/acc
                      (anode-mother at) (sub1 generation) acc))]))]
  (get-f-generation/acc at generation empty)))
                                           


;;Examples 1d:
(check-expect (get-f-descendants-path copyright-free-ancestors "Arville")
              (list "Arville" "Abrahum" "Homern't" "Liso"))

(check-expect (get-f-descendants-path copyright-free-ancestors "Liso")
              (list "Liso"))

;; get-f-descendants-path/acc: AT Str (listof Str) -> (listof Str)
(define (get-f-descendants-path/acc at name lod)
  (cond
    [(empty? at) empty]
    [(string=? (anode-name at) name) (cons name lod)]
    [else (local [(define father-path
                    (get-f-descendants-path/acc
                                       (anode-father at) name (cons (anode-name at) lod)))
                  (define mother-path
                    (get-f-descendants-path/acc (anode-mother at) name (cons (anode-name at) lod)))]
            (cond
                    [(empty? mother-path) father-path]
                    [else mother-path]))]))
                  
;; get-f-descendants-path: AT Str -> (listof Str)
(define (get-f-descendants-path at name)
  (get-f-descendants-path/acc at name empty))