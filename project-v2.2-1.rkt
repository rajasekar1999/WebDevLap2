#lang racket
;;--------------------------------------------------------------------------------------------------------
;;                                     PROJECT INFORMATION
;;--------------------------------------------------------------------------------------------------------

;; Group 9 -

;; Problem Statement: Fraction Calculator

;;  -----------------------------------
;; | Team Members:                     |
;; |                                   |
;; | Ashwin B - CB.EN.U4CSE17310       | 
;; | Arjun Krish - CB.EN.U4CSE17369    |
;; | Arun Sathvik - CB.EN.U4CSE17334   |
;; | Rajasekar M - CB.EN.U4CSE17349    |
;;  -----------------------------------

;--------------------------------------------------------------------------------------------------------

;; Fractions are given in the form of a list

;; Improper or Proper fractions are list of length 2.
;; The first term is the numerator and the second term is the denominator
;; '(1 2), '(3 4), '(3 2) and '(10 3) are examples valid proper/improper fractions 

;; Mixed fractions are list of length 3.
;; The first term is the whole number term, followed by the numerator and the denominator
;; '(1 2 3) and '(2 3 4) are examples of valid mixed fractions


;; Examples of Invalid fractions: '(1 2 3 4), '(2), ...
;; ie any list with more than 3 elements or less than 2 elements are invalid

;;--------------------------------------------------------------------------------------------------------

;; Modules done so far...

;;--------------------------------------------------------------------------------------------------------
;;    MODULE NAME                                  |             DONE BY
;;-------------------------------------------------|------------------------------------------------------
;; 1) Fraction type check and Fraction Invertion   |      NAME 1 (CB.EN.U4CSE173XX)
;;                                                 |
;; 2) Convertion between Fractions and reducing it |      NAME 2 (CB.EN.U4CSE173XX)
;;    to lowest term                               |
;;                                                 |
;; 3) Fraction Addition and Subtraction            |      NAME 3 (CB.EN.U4CSE173XX)
;;                                                 |
;; 4) Fraction Multiplication and Division         |      NAME 4 (CB.EN.U4CSE173XX)
;;--------------------------------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------------------------------
;;                                              CODE
;;--------------------------------------------------------------------------------------------------------

;; Helper functions:-
;; 1. finds GCD of given two numbers:
(define (GCD a b)
  (if (= b 0)
      a
      (GCD b (remainder a b))
      )
  )

;; 2. make the signs of fractions proper:
;; Note: Takes only proper or improper fraction as input
(define (invertSigns fraction)
  (let ([numer (first fraction)]
        [denom (last fraction)])
    (cond [(and (negative? numer) (positive? denom)) (list numer denom)]
          [(and (negative? numer) (negative? denom)) (list (abs numer) (abs denom))]
          [(and (positive? numer) (negative? denom)) (list (* numer -1) (* denom -1))]
          [(and (positive? numer) (positive? denom)) (list numer denom)]
          [else "An error occured"])
    )
  )

;; 3. Check if fraction is valid:
(define (valid fraction)
  (if (and (list? fraction) (or (= (length fraction) 2) (= (length fraction) 3)))
      #t
      #f
      )
  )

;;--------------------------------------------------------------------------------------------------------
;;--------------------------------------------------------------------------------------------------------

;; Module 1:-

;; Module for finding the type of fractions
(define (ftype fraction)
  (let ([len (length fraction)]
        [num1 (first fraction)]
        [num2 (last fraction)])
    (cond [(and (= len 2) (< num1 num2)) "proper"]
          [(and (= len 2) (>= num1 num2)) "improper"]
          [(and (= len 3)) "mixed"]
          [else "Error: Invalid Input"])
    )
  )
;; Examples:
;; (ftype '(1 2))     => "proper"
;; (ftype '(12 7))    => "improper"
;; (ftype '(1 1 2))   => "mixed"
;; (ftype '(1))       => "Error: Invalid Input"
;; (ftype '(1 1 2 3)) => "Error: Invalid Input"

;;--------------------------------------------------------------------------------------------------------

;; Module 2:-

;; Invertion of fractions
(define (invert fraction)
  (if (valid fraction)
      (let ([fract (m2i fraction)])
    (invertSigns (reduced (list (last fract) (first fract))))
    )
      "Invalid Input"
      )
  )
;; Examples:
;; (invert '(3 2))   => '(2 3)
;; (invert '(1 2 4)) => '(2 3)
;; (invert '(-2 3))  => '(-3 2)

;;--------------------------------------------------------------------------------------------------------

;; Module 3:-

;; Fraction Convertion
;; a) Mixed to Improper fraction
(define (m2i fraction)
  (let ([len (length fraction)]
        [whole (first fraction)]
        [numer (first (cdr fraction))]
        [denom (last fraction)])
    (cond [(= len 3) (reduced (list (+ (* whole denom) numer) denom))]
          [(= len 2) (reduced fraction)]
          [else "Error converting fraction"])
    )
  )
;; b) Improper to Mixed fraction
(define (i2m fraction)
  (let ([len (length fraction)]
        [numer (first fraction)]
        [denom (last fraction)])
    (cond [(= len 2) (list (quotient numer denom) (remainder numer denom) denom)]
          [(= len 3) fraction]
          [else "Error converting fraction"])
    )
  )
;; Examples:
;; (m2i '(1 2 4)) => '(3 2)
;; (m2i '(3 2))   => '(3 2)
;; (i2m '(1 1 2)) => '(1 1 2)
;; (i2m '(3 2))   => '(1 1 2)

;;--------------------------------------------------------------------------------------------------------

;; Module 4:-

;; Convert Proper or Improper fraction to lowest term
(define (reduced fraction)
  (cond
    ;; Case 1:
    ;; Case 1.1: proper or improper fraction given
    [(and (= (length fraction) 2) (integer? (first fraction)) (integer? (last fraction)))
     (let ([numer (first fraction)]
           [denom (last fraction)]
           [common (GCD (first fraction) (last fraction))])
       (invertSigns (list (/ numer common) (/ denom common))))]

    ;; Case 1.2: proper or improper fraction where numerator or denominator are non integers
    [(and (= (length fraction) 2) (or (not (integer? (first fraction))) (not (integer? (last fraction)))))
     (invertSigns fraction)]

    ;; Case 2: mixed fraction given
    [(= (length fraction) 3) (invertSigns (reduced (m2i fraction)))]

    ;; Error case:
    [else "Error reducing fraction"]
        )
  )
;; Examples:
;; (reduced '(1 2 4))   => '(3 2)
;; (reduced '(4 -18))    => '(-2 9)
;; (reduced '(2.5 4.3)) =>  '(2.5 4.3)
;; (reduced '(4.2 -18))  => '(-4.2 18)

;;--------------------------------------------------------------------------------------------------------

;; Module 5:-

;; Addition and Subtraction of fractions
(define (HigherOrderSumDiff operator fract1 fract2)
  (let ([n1 (first fract1)]
        [d1 (last fract1)]
        [n2 (first fract2)]
        [d2 (last fract2)])
    ;; Note: The result by default will be a proper/improper fraction
    (invertSigns (reduced (list (operator (* n1 d2) (* n2 d1)) (* d1 d2))))
    )
  )

;; a) Sum:-
(define (sum fract1 fract2)
  ;; convert mixed fractions to improper fractions before operation
  (let ([f1 (m2i fract1)]
        [f2 (m2i fract2)]
        [operator +])
    (HigherOrderSumDiff operator f1 f2)
    )
  )
;; Examples:
;; (sum '(2 4) '(2 3 4))   => '(13 4)
;; (sum '(2 1 4) '(2 3 4)) => '(5 1)
;; (sum '(2 2 4) '(3 4))   => '(13 4)
;; (sum '(1 4) '(3 4))     => '(1 1)

;; b) Difference:-
(define (diff fract1 fract2)
  ;; convert mixed fractions to improper fractions before operation
  (let ([f1 (m2i fract1)]
        [f2 (m2i fract2)]
        [operator -])
    (HigherOrderSumDiff operator f1 f2)
    )
  )
;; Examples:
;; (diff '(2 4) '(2 3 4))   => '(-9 4)
;; (diff '(2 1 4) '(2 3 4)) => '(-1 2)
;; (diff '(2 2 4) '(3 4))   => '(7 4)
;; (diff '(1 4) '(3 4))     => '(-1 2)

;;--------------------------------------------------------------------------------------------------------

;; Module 6:-

;; Product and Division of fractions
(define (HigherOrderMulDiv operator fract1 fract2)
  (let ([n1 (first fract1)]
        [d1 (last fract1)]
        [n2 (first fract2)]
        [d2 (last fract2)])
  (cond [(equal? operator *) (invertSigns (reduced (list (* n1 n2) (* d1 d2))))]
        [else "An error occured"]))
  )

;; a) Product:-
(define (mul fract1 fract2)
  ;; convert mixed fractions to improper fractions before operation
  (let ([f1 (m2i fract1)]
        [f2 (m2i fract2)])
    (HigherOrderMulDiv * f1 f2)
    )
  )
;; Examples:

;; b) Division:-
(define (div fract1 fract2)
  ;; convert mixed fractions to improper fractions before operation
  (let ([f1 (m2i fract1)]
        [f2 (m2i fract2)])
    (HigherOrderMulDiv * f1 (invert f2))
    )
  )
;; Examples:

;;--------------------------------------------------------------------------------------------------------
;;                                   YOU HAVE REACHED THE END OF THE CODE
;;--------------------------------------------------------------------------------------------------------

;; Things left to do in this project:

;; 1) Power of fractions
;; 2) Fraction Equality check
;; 3) Complex fraction ie combination of all the above operations

;;--------------------------------------------------------------------------------------------------------


