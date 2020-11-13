#lang racket
;function 2 for homework 1
;by: peter keres


;this method takes 2 paramters and makes a list going from x to y
(define make-seq-list
  (lambda (x y)
    (cond
      ;if x is greater then y, we are at the end of the list we want to make
      [ (> x y) '()]
      ;else, we add x to a list, and then call this method again adding 1 to x
      [ else (cons x (make-seq-list (+ x 1) y))]
     )
   )
 )


;this method will make a list going from 1 to x
(define make-whole-list
  (lambda (x)
    ;calling the helper function but starting at 1
    (make-seq-list 1 x)
   )
 )
    