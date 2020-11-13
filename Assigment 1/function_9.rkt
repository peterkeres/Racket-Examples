#lang racket
;function 9 for homework 1
;by: peter keres


(define odd-f
  (lambda (list1)
    (odd-f-help list1 #t (list ))
    )
  )

;every pass we take turns keeping the curretn element into a blank list
;we use a boolean to keep track of what pass we are on
(define odd-f-help
  (lambda (list1 current list2)
    (cond
      [(empty? list1) list2]
      [current (odd-f-help (cdr list1) #f (cons (car list1) list2))]
      [ else (odd-f-help (cdr list1) #t list2)]
      )
    )
 )
;=============================================================================
;=============================================================================
;=============================================================================
(define even-f
  (lambda (list1)
    (odd-f-help list1 #f (list ))
    )
  )

;every pass we take turns keeping the curretn element into a blank list
;we use a boolean to keep track of what pass we are on
(define even-f-help
  (lambda (list1 current list2)
    (cond
      [(empty? list1) list2]
      [current (even-f-help (cdr list1) #t (cons (car list1) list2))]
      [ else (even-f-help (cdr list1) #f list2)]
      )
    )
 )