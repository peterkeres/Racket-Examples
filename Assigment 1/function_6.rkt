#lang racket
;function 6 for homework 1
;by: peter keres



(define concat-list
  (lambda (list1 list2)
    (cond
     [ (empty? list1) list2]
     [ else (cons (car list1) (concat-list (cdr list1) list2))]
     )
    )
  )


