#lang racket
;function 8 for homeowrk 1
;by: peter keres



(define max-element
  (lambda (list1)
    (max-element-help list1 (car list1))
    )
  )

(define max-element-help
  (lambda (list1 x)
    (cond
      ;we are done searching
      [(empty? list1) x]
      ;if the next element in the list is greater then x, we swap
      [(> (car list1) x) (max-element-help (cdr list1) (car list1))]
      ;x is the same as current or is smaller, so no change
      [else (max-element-help (cdr list1) x)]
      )
    )
  )
;=============================================================================
;=============================================================================
;=============================================================================
(define min-element
  (lambda (list1)
    (min-element-help list1 (car list1))
    )
  )

(define min-element-help
  (lambda (list1 x)
    (cond
      ;we are done searching
      [(empty? list1) x]
      ;if the next element in the list is smaller then x, we swap
      [(< (car list1) x) (min-element-help (cdr list1) (car list1))]
      ;x is the same as current or is bigger, so no change
      [else (min-element-help (cdr list1) x)]
      )
    )
  )