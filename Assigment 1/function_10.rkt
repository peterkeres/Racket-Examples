#lang racket
;functinon 10 for homework 1
;by: peter keres



(define merge
  (lambda (list1 list2)
    (merge-help list1 list2 '())
    )
  )


(define merge-help
  (lambda (list1 list2 list3)
    (cond
      ;if both lists are empty, we return the combined list
      [(and (empty? list1) (empty? list2)) list3]
      ;if list 1 is empty, we add list2 highest number to our 3rd list and call fucntion again
      [(empty? list1) (cons (car list2) (merge-help list1 (cdr list2) list3))]
       ;if list 1 is empty, we add list2 highest number to our 3rd list and call fucntion again
      [(empty? list2) (cons (car list1) (merge-help (cdr list1) list2 list3))]
      ;if item in list 2 is bigger, we take it out 
      [(< (car list1) (car list2)) (cons (car list1) (merge-help (cdr list1) list2 list3 ))]
      ;if item in list 1 is bigger or same, we take it out
      [ else (cons (car list2) (merge-help list1 (cdr list2) list3 ))]
      )
    )
  )

