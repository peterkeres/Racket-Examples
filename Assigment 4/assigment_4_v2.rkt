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
    ;debug statments
    (println splitlist)
    (println (>= 1 (length (first splitlist))))
    (println (>= 1 (length (third splitlist))))
    (println (and (>= 1 (length (first splitlist)))  (>= 1 (length (third splitlist)))))
    ;if statment on if we can find the smallest element in this list, or we call this method again with new k and smaller list
    (cond
      ; if the first and third list is empty, we have found the item. its in the middle list. base case 
      [(and (>= 0 (length (first splitlist)))  (>= 0 (length (third splitlist))))
       ;debug statments
       (println "in base case")
       (print "return should be : ")
       (println (first (second splitlist)))
       ;kth smallest element. 
       ((first (second splitlist)))
         ]
      ;else, we need to shrink the total list again
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