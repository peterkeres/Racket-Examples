#lang racket
;function 4 for homework 1
;by: peter keres


;helper funcion
;used to see if the next item in the list is null. aka only 1 item in list
;T: there is another item in the list
;F; there is no other item in the list
(define peek
  (lambda (mylist)
    (cond
      [(null?(cdr mylist)) #f]
      [else #t]
    )
   )
 )


;used to find the last element in a list
(define last-element
  (lambda (mylist)
    (cond
      ;if the peek is ture on the copyed list, there is more items in the list
      [(peek mylist) (last-element (cdr mylist))]
      ;if the peek is not ture on the bopyed list, there is no other items in the list. we are on last item 
      [else (display (car mylist))]
     )
   )
 )

      