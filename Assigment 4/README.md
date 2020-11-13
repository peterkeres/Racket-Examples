Finding the median of a list of integers

Read sections 2.1-2.4 in our text. One of the algorithms the authors describe is how to find the median of a list using a divide and conquer algorithm. I want us to implement this algorithm in Scheme. I've started the assignment for you and written a good bit of the code. I'm asking you to complete the work. Here is the code I've written:

#lang plai
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
(test (split '(1 1 2 3 3 3 4 4 4 5 5 5 5 6 6 6 7 8 9 9 9 9) 5)  '((4 4 4 3 3 3 2 1 1) (5 5 5 5) (9 9 9 9 8 7 6 6 6 )))

(define (kth-smallest lis k)
  (
  ;; your code goes here  
   )
;; lis: list -> int
;; returns the median of a list
(define (median lis)
  (kth-smallest lis (floor (/ (+ 1 (length lis)) 2))))
(test (median '(1 2 3 3 4 4 4 5 5 5 6 6 6 6 6)) 4)
(test (median '(1 2 3 3  5 5 5 6 6 6 6 6)) 5)
(test (median '(11 11 2 3 33 2 9 8 77 3 5 66 7 12 32 32 56)) 9)

I'll post a video on how this code works. I'm using a verson of Scheme that includes PLAI library that has a "test" method in it that is helpful for testing. Other than that, it should not be much different than using #lang racket. You will have to write the code to find the k-th smallest element in a list - this is described on page 54.