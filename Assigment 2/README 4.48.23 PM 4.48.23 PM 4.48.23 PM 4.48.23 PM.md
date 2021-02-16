Algorithms for Arithmetic

Since many of you are new to Scheme, I'm providing a good bit of code for this assignment. Your first task it to understand what I've written, and after that, you can begin to add to it. I'll post a video lecture that discusses how the provided code works
Undergraduate Assignment

        Implement the multiplication algorithm described in Figure 1.1. of our textbook.
        Comment your code using the following design recipe example:

            ;; Contract: area-of-ring : number number -> number

            ;; Purpose: to compute the area of a ring whose radius is 
            ;; outer and whose hole has a radius of inner

            ;; Example: (area-of-ring 5 3) should produce 50.24

            ;; Definition: [refines the header] 
               (define (area-of-ring outer inner) 
                  (- (area-of-disk outer) 
                  (area-of-disk inner)))

            ;; Tests: 
               (test (area-of-ring 5 3)  50.24) 

   

    Drop your code in the dropbox

Graduate Assignment

        Implement the multiplication algorithm described in Figure 1.1 of our textbook.
        Implement the division algorithm in Figure 1.2.
        Comment your code using the design recipe example:

            ;; Contract: area-of-ring : number number -> number

            ;; Purpose: to compute the area of a ring whose radius is 
            ;; outer and whose hole has a radius of inner

            ;; Example: (area-of-ring 5 3) should produce 50.24

            ;; Definition: [refines the header] 
               (define (area-of-ring outer inner) 
                  (- (area-of-disk outer) 
                  (area-of-disk inner)))

            ;; Tests: 
               (test (area-of-ring 5 3)  50.24) 

   

    Drop your code in the dropbox

SampleCode Base

The following code implements the addition algorithm as described in Chapter 1. I have chosen to represent each integer as a list of 0's and 1's written in reverse order. For example '(1 0 1 0 0 1) represents 1 + 4 + 32 = 37.

#lang plai
;Algorithms - Figure 1.2
;; ints are represented as reverse lists of binary digits which I call bitlists. Example:  (0 0 1) = 4
(define x (list 1 1 1 0 0 1)) ; = 39
(define y (list 1 1 0 1 1 0)) ; = 27
(define z (list 0 0 0 0 0 0)) ; = 0

;; bitlist->int : bitlist -> int
;; a conversion function from bitlist to int
;; example:  (bitlist-int '(0 1 1 0 1 1 0))
(define (bitlist->int lis)
  (cond
    ((null? (rest lis)) (first lis))
    ( #t   (+ (first lis)(* 2 (bitlist->int (rest lis))))))) 

(test (bitlist->int '(1 1 0 0 1 1)) 51)
(test (bitlist->int x) 39)
(test (bitlist->int y) 27)
(test (bitlist->int z) 0)


;; int->bitlist : bitlist -> int
;; a conversion function from int to bitlist
;; when converting to a bitlist, the number of digits might vary,
;; so we supply the length explicitly
;; example: (int-bitlist 32)
(define (int->bitlist n noDigits)  
  (cond
    ((=  noDigits 0) '())
    ( #t (cons (remainder n 2)(int->bitlist (quotient n 2)(- noDigits 1))))))

(test (int->bitlist 39 6) x)
(test (int->bitlist 12 7) '(0 0 1 1 0 0 0))
(test (int->bitlist 27 6) y)
(test (int->bitlist  0 6) z)


;; zero? : bitlist -> boolean
;; a predicate function that returns true if the bitlist represents a 0
;; example:  (zero? '(1 1 0 0 0 1))
;; example:  (zero? '(0 0 0))
(define (zero? x)
  (cond
     ((null? x) #t)
     ((= (first x) 1) #f)
     ( #t  (zero? (rest x))))) 

(test (zero? x) #f)
(test (zero? z) #t)

;; addWithCarry : bitlist, bitlist, integer -> bitlist
;; computes a bitlist that is the sum of the two input bitlists and the carry
;; the two bitlists can vary in size
;; example:  (addWithCarry '(0 1 0 1)'(1 1 0) 1)
(define (addWithCarry x y carry)
  (cond
    ((and (null? x)(null? y)) (if (= carry 0) '() '(1)))
    ((null? x) (addWithCarry '(0) y carry))
    ((null? y) (addWithCarry x '(0) carry))
    ( #t  (let ((bit1 (first x))
                (bit2 (first y)))
               (cond
                 ((= (+ bit1 bit2 carry) 0) (cons 0 (addWithCarry (rest x) (rest y) 0)))
                 ((= (+ bit1 bit2 carry) 1) (cons 1 (addWithCarry (rest x) (rest y) 0)))
                 ((= (+ bit1 bit2 carry) 2) (cons 0 (addWithCarry (rest x) (rest y) 1)))
                 (   #t                     (cons 1 (addWithCarry (rest x) (rest y) 1))))))))

(test (addWithCarry '(1 1 1)'(1) 0)'(0 0 0 1))
(test (addWithCarry '(1 1 1)'(1) 1)'(1 0 0 1))

;; add : bitlist, bitlist -> bitlist
;; a function that adds two bitlists with an implied carry of 0
;; works with different size bitlists
;; example: (test (add '(1 1 1)'(1 0 1)) '(0 0 1 1)) 
(define (add x y)
  (addWithCarry x y 0))

(test (add '(1 0 1 0 1 0 1)'(1 1 0 0 0 1 1)) '(0 0 0 1 1 1 0 1))    
(test (add '(1 1 0 0 0 1 1)'(1 1 0 0 0 1 1)) '(0 1 1 0 0 0 1 1)) 
(test (add '(1 1 0)'(1 1 1 1 ))'(0 1 0 0 1))
(test (add '(1)'(1 1 1 1 )) '(0 0 0 0 1))


