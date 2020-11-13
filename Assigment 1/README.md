# Instructions
> Dr Racket

Begin by downloading and installing Racket from the Rice website, or use one of the machines in the computer science lab. After starting DrRacket, click on "Language" and then "Choose Language", then select "Use the language declared in the source". The environment defines a variety of language levels and services. Try typing the number 3 at the > prompt in the interactions window. The interpreter will read the 3 when you press the return key. The "expression" is evaluated and its value is returned. (In this case 3 is returned.) Now try another value, 4.5. And another, 2/3. Yes, Racket supports numbers in fractional form. Try a few more values,

a
4.5e-19
#t
true
false
#f
'( )
"Hello, World!"
'Hello
(define a 3)
a
(+  2  3)
#\a
 (define prompt "Hello")
 prompt
 (list 1 2 3 4 5)
 (car  (list 1 2 3 4 5))
 (cdr (list 1 2 3 4 5))
 

Try typing ESC-p. You should see the last expression you typed in the interactions window. Typing ESC-p multiple times will cycle back through the expressions you have been typing. Use ESC-n to move forward. Here is another tip: If you move the flashing caret after an old expression and press the enter key, the expression will appear next to the > prompt.

The Racket interpreter expects you to type your input in the form of s-expressions (symbolic expressions). S-expressions have a simple format. Each s-expression is either a symbol, a number or a list of s-expressions. Each list is enclosed in parentheses. Symbols and numbers are also called atoms. Here are some s-expressions.
(1 2 3) ( + 2 3 4 5) (cons (list 1 2 3) (list 5 8 3))
Racket Functions

Many s-expressions consist of a list whose first element is a function and whose subsequent elements are parameters. For example (+ 2 3) is an s-expression of this type. In this case the values 2 and 3 are added and the sum is returned as the value of the expression. Here is a short list of functions that you will need for this assignment.

- list : Takes a collection of s-expressions as parameters and returns the list consisting of all the s-expressions. Example: (list 1 2 3 4 5 6) returns (1 2 3 4 5 6)
    
- car : Takes a non-empty list as a parameter and returns the first s-expression in the list. Example: (car (list 9 8 7 6)) returns 9.

- cdr : Takes a non-empty list as a parameter and returns the list derived by deleting its car. Example: (cdr (list (list 1 2 3) 5 8 (list 4 5))) returns (5 8 (4 5)).

- cons : Is passed an s-expression and a list and returns the list after inserting the s-expression as its "car". Example (cons 5 (list 1 2 (list 5 6) 3 )) returns (5 1 2 (5 6) 3).

- null? : This is a predicate function that takes a list as its parameter and returns #t if the list is empty, otherwise it returns #f. Example: (null? (list )) returns true.
    
- if : Provided with 3 parameters. If the first parameter evaluates to true, the second parameter is evaluated and returned, otherwise the third parameter is evaluated and returned. Example: (if (= 4 (car (list 3 4 5))) 8 9) returns 9.
    
- cond : Racket's version of a case statement, this function is supplied with one or more lists , each of which contains two s-expressions (call them "duals"). Think of the first expression in the dual as a question. If the question evaluates to true, the second expression in the dual is evaluated and returned. Otherwise, the next dual is processed. An "else" expression can also be supplied as the "question" in the last dual.
    
- Example: (cond [(= 2 3) 5] [(= 3 5) 4] [(= 3 3) 8] [else 9]) returns 8. The only values that could possibly be returned by this function are 5, 4, 8, or 9.
    
- define : This function accepts two parameters. The first is an atom and the second is an s-expression. The value of the s-expression is assigned as the value of the atom. This function can be used to initialize a variable. This initialization is called a "side effect". No value is returned. Example: (define a 3) assigns the value 3 to the variable a.
    
- min : Returns the minimum value obtained by evaluating the supplied parameters. Example: (min 5 3 7 8) returns 3.
    
- max : Returns the maximum value obtained by evaluating the supplied parameters. Example (max 3 (+ 3 5) (- 4 5)) returns 8.
    
- lambda : This function is used when we want to create a programmer-defined function. It takes two parameters. The first parameter is a list of variables that will be the formal parameters of our function. The second parameter is an s-expression which uses the formal parameters to define the "body" of the function. Example: (define square (lambda (n) (* n n))) This expression defines "square" as a lambda function. The function has one formal parameter "n", and the body of the function is a function which returns the square of n. The lambda function is often used to define a "named " function. ecursion in Racket

Since Racket does not provide a loop construct, we must use function calls, particularly recursive function calls to accomplish many tasks. Lets look at a sample problem and a recursive solution in Racket. Assume we need to write a function call "sumlist" which is passed a list of numbers and which returns the sum. Here is the code that will accomplish this task.
(define sumlist (lambda (lst) (if (empty? lst) 0 (+ (car lst) (sumlist (cdr lst))))))

This function is very typical. We use "define" to associate the symbol "sumlist" with the lambda expression which defines the formal parameter (lst) and the function body. The function body is an "if" function which returns 0 if the list is empty (base case return value) and a recursive value otherwise.

For this assignment you need to create and test the following functions. When you have completed the functions, drop them in Web-CT.

- Function 1 Write a function called make-seq-list which accepts two parameters (x y) and returns the list containing x, x+1, x+2,...,y. You can assume that x and y are integers and x < y.
    
- Function 2 Write a function called make-whole-list which accepts one parameter (x) and returns a list containing 1, 2, 3, ..., x. You may assume that x is an integer and 1 < x.
    
- Function 3 Write a function called find-ith-element which accepts a list as the first parameter and an integer, say i, as the second parameter, and which returns the element in the ith location of the given list. Return false if the list does not have an ith element. For example, (find-ith-element (list 8 9 6 3) 3) should return 6.
    
- Function 4 Write a function called last-element which accepts a list as a parameter and which returns the last element in the list. You may assume that the list is non-empty.
    
- Function 5 Write a function called list-length which accepts a list as a parameter and which returns the number of items in the list. The function should work for empty lists.
    
- Function 6 Write a function called concat-list which accepts two lists and that returns the list derived by concatenating the lists into one list. For example (concat-list (list 1 2 3) (list 4 5) ) would return (list 1 2 3 4 5). Do not use the "append" function.
    
- Function 7 Write a function called reverse-list which accepts a list as a parameter and that returns the original list in reverse order.
    
- Function 8 Write a min-element and a max-element function. Each function is passed a list of numbers and returns the appropriate number. You may assume that the parameter list is non-empty.
    
- Function 9 Write an even and an odd function. Each function is passed a list. The even function returns the list of elements at positions 2, 4 , 6, ... as a list. The odd function returns the list of elements at positions 1, 3, 5, ... as a list. For example (even (list 8 3 5 4 6 9 1)) would return the list (3 4 9) and (odd (list 8 3 5 4 6 9 1) would return the list (8 5 6 1).
    
- Function 10 Write a function merge which is passed two lists (representing sorted lists) and that returns the single list obtained by merging the given lists. For example, (merge (list 1 4 5 9 19) (list (2 6 7 10)) returns the list (1 2 4 5 6 7 9 10 19).
    
- Function 11 Write a merge-sort function which is passed a list and which returns the list in sorted order from smallest number to largest. Use even and odd to split the original list into two parts. Sort them and merge them using the functions you have already written.

End Date
	
Sep 11, 2019 11:30 PM