#lang racket
;function 11 for homework 1
;could not get to work, feel like i am close 
;by: peter keres

(define merge-sort
  (lambda (list1)
    (cond
     [(and (empty? (even-f list1)) (empty? (odd-f list1)))    ]
     [(empty? (cdr list1)) (car list1)]
     [ else (merge(merge-sort(even-f list1)) (merge-sort(odd-f list1)))]
    )
  )
 )



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