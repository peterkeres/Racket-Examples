#lang racket
; Integer, Integer -> Integer
; Computes the greatest common divisor of two integers
; Given: 18 24 expect: 6
; Given: 72 45 expect: 9
(define euclid
  (lambda (a b)
    (cond
      ((= b 0) a)
      ( else (euclid b (modulo a b))))))
 
; Integer, Integer -> List of three integers
; Computes the greatest common divisor d of two integers and represents
;  it as a linear combination of the input values d = ax + by
; Given: 72 45 expect: (2 -3 9)  9 = 72(2) + 45(-3)
; Given: 18 24 expect: (-1 1 6)  6 = 18(-1) + 24(1)
(define extended-euclid
  (lambda (a b)
    (cond
      ((= b 0) (list 1 0 a))
      ( else (let* ((rl (extended-euclid b (modulo a b)))
                    (x (car rl))
                    (y (cadr rl))
                    (d (caddr rl)))
                   (list y (- x (* (quotient a b) y)) d))))))
 
; Integer Integer Integer -> Integer
; Computes x^y Mod n
; Given 3 4 5    expect: 1
; Given 2 345 31 expect: 1
; Given 5 15 7   expect: 6
(define modexp
  (lambda (x y n)
    (cond
      ((= y 0) 1)
      ( else (let
                 ((z (modexp x (quotient y 2) n)))
                  (cond
                    ((even? y) (modulo (* z z) n))
                    ( else (modulo (* x z z) n))))))))