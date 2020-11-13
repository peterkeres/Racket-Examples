#lang racket
;; lis :list, k: int, a:list, b:list, c:list -> list of 3 lists
;; Splits a list into three sublists: a b c  
;; Elements of a are less than k
;; Elements of b are equal to k
;; Elements of c are greater than k
(define (splithelper lis k a b c)
     (cond
       ((null? lis) (list a b c))
       ((< (first lis) k) (splithelper (rest lis) k (cons (first lis) a) b c))
       ((> (first lis) k) (splithelper (rest lis) k a b (cons (first lis) c)))
       ( else             (splithelper (rest lis) k a (cons (first lis) b) c))))

;; lis: list k:int -> list of 3 lists
;; Splits a list into three sublists: a b c  
;; Elements of a are less than k
;; Elements of b are equal to k
;; Elements of c are greater than k
(define (split lis k)
  (splithelper lis k '() '() '()))
;(test (split '(1 1 2 3 3 3 4 4 4 5 5 5 5 6 6 6 7 8 9 9 9 9) 5)  '((4 4 4 3 3 3 2 1 1) (5 5 5 5) (9 9 9 9 8 7 6 6 6 )))


;peter keres
;finds the smallest k itme in a list
(define (kth-smallest lis k)
  ;take the list sent, split into 3 lists based on a random number
  (let ([splitlist (split lis (car lis))])
    ;if statment on if we can find the smallest element in this list, or we call this method again with new k and smaller list
    (cond
      ; if lenght of all 3 sub lists are 1 or null, we can find smallest based on k's value, base case 
      [(and (or (= 1 (length (first splitlist))) (= 0 (length (first splitlist))))  (and(or (= 1 (length (second splitlist))) (= 0 (length (second splitlist)))) (or (= 1 (length (third splitlist))) (= 0 (length (third splitlist))))))
       (cond
         ;k is 1, return the frist sub list
         [(= 1 k)  (first splitlist)]
         ;k is 2, return the seconnd sub list
         [(= 2 k)  (second splitlist)]
         ;k is 3, return the thrid sub list
         [(= 3 k)  (third splitlist)]
         )
         ]
      ;we need to shrink the total list again
      [else
       ;if inside of this else
       ;we need to call this method agan, but sending it a new list based on what k is in relation to the length of the lists remaning 
       (cond
         ;k is in the last sub list. lenght of first 2 list is less then k. call this again with sublist 3 and k = k - length of sub 1 and 2
         [(>= k (+ (length(first splitlist)) (length(second splitlist))) )       (kth-smallest (third splitlist) (- k (+ (length (first splitlist))(length(second splitlist))) ) ) ]
         ; k is in the second sub list. lenght of first list is less then k. call this again with sublist 2 and k = k - lenght of sub 1
         [(>= k (length (first splitlist)))    ((kth-smallest (second splitlist) (- k (length (first splitlist))))) ]
         ;k is in the first sub list. length of the first list is greater then or = to k. call this again with the sublist 1 and k = k
         [else                                 ((kth-smallest (first splitlist) k))]
         )]
    )
    )
  )
   
  


;; lis: list -> int
;; returns the median of a list
(define (median lis)
  (kth-smallest lis (floor (/ (+ 1 (length lis)) 2))))
;(test (median '(1 2 3 3 4 4 4 5 5 5 6 6 6 6 6)) 4)
;(test (median '(1 2 3 3  5 5 5 6 6 6 6 6)) 5)
;(test (median '(11 11 2 3 33 2 9 8 77 3 5 66 7 12 32 32 56)) 9)