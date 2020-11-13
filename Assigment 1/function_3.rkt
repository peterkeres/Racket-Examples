#lang racket
;function 3 for homework 1
;by: peter keres


(define find-ith-element
  (lambda (mylist i)
    (cond
      ;if the list is empty, we was given a number that was bigger then the list
      [(null? mylist) #f] 
      ;if i is 0, we are at the element 
      [(= i 1) (display (car mylist))]
      ;else i is not 0, we need to keep looking. taking ith item off the list
      [else (find-ith-element (cdr mylist) (- i 1))]
     )
    )
  )