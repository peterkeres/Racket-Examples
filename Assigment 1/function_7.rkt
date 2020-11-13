#lang racket
;function 7 for homwork 1
;by: peter keres



(define reverse-list
  (lambda (list1)
    (reverse-list-help list1 (list ))
    )
  )

(define reverse-list-help
  (lambda (list1 list2)
    (cond
      [(empty? list1) list2]
      [else (reverse-list-help (cdr list1) (cons (car list1) list2))]
      )
    )
  )
