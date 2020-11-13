#lang racket
;function 1 of homework 1
;by: peter keres

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
