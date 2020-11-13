#lang racket
;function 5 for homework 1
;by: peter keres


(define list-length
  (lambda (mylist)
    (cond
      ;if no more items in list, return a 0
      [(null? mylist) 0]
      ;there more items in the list, keep adding a 1 to the temp counter
      [else (+ 1 (list-length (cdr mylist)))]
     )

   )
 )