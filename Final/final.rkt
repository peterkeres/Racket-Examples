#lang racket
(require data/heap)
(require rackunit)

; peter keres
; final for algrithms
; just run the finalExam fucntion to see print out of both fucntions needed for final exam
; currntly set up to run the bigger quote and ALPHABET


; A message that will be compressed
 (define quote "To be, or not to be, that is the question:
 Whether 'tis nobler in the mind to suffer
 The slings and arrows of outrageous fortune,
 Or to take arms against a sea of troubles
 And by opposing end them. To die—to sleep,
 No more; and by a sleep to say we end
 The heart-ache and the thousand natural shocks
 That flesh is heir to: 'tis a consummation
 Devoutly to be wish'd. To die, to sleep;
 To sleep, perchance to dream—ay, there's the rub:
 For in that sleep of death what dreams may come,
 When we have shuffled off this mortal coil,
 Must give us pause—there's the respect
 That makes calamity of so long life.
 For who would bear the whips and scorns of time,
 Th'oppressor's wrong, the proud man's contumely,
 The pangs of dispriz'd love, the law's delay,
 The insolence of office, and the spurns
 That patient merit of th'unworthy takes,
 When he himself might his quietus make
 With a bare bodkin? Who would fardels bear,
 To grunt and sweat under a weary life,
 But that the dread of something after death,
 The undiscovere'd country, from whose bourn
 No traveller returns, puzzles the will,
 And makes us rather bear those ills we have
 Than fly to others that we know not of?
 Thus conscience does make cowards of us all,
 And thus the native hue of resolution
 Is sicklied o'er with the pale cast of thought,
 And enterprises of great pitch and moment
 With this regard their currents turn awry
 And lose the name of action.")

; A message that will be compressed.
;(define quote "abcdefghaaaaabbcccccccccccccccccccccccccccc")


; The list of characters in the alphabet that was used to compose the message.
(define ALPHABET "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz ,'?;:-\n.— ")
;(define ALPHABET "abcdefgh")

(define N (string-length ALPHABET)) ;N represents the number of characters in the alphabet

; We are building a vector, f, that holds the frequency (weights) of each character of the alphabet in the qiven quote.
; The weights of each character occur in the first N positions of the vector f and represent the leaves of a tree.
; The positions N ... (2N - 2) contain the internal nodes of the tree with the root at index (2N - 2).
(define ROOT (- (* N 2) 2))
(define FLEN (- (* 2 N) 1)) ; The length of function f

; f is the frequency array.
(define f (make-vector FLEN 0))

; A helper function to find the position of a character in the alphabet string.
; First char at pos 0.
;
(define findCharPosHelper
(lambda (ch pos)
(cond
((>= pos N) "Character not found in alphabet")
((char=? ch (string-ref ALPHABET pos)) pos)
( else (findCharPosHelper ch (+ pos 1))))))

; A function that is given a character and returns the position of the character in the alphabet
; char -> int
(define findCharPos
(lambda (ch) (findCharPosHelper ch 0)))

; Use these tests for the 64 character alphabet
; (check-equal? (findCharPos #\A) 0)
; (check-equal? (findCharPos #\a) 26)
; (check-equal? (findCharPos #\—) 61)

; Use these tests for the 8 character alphabet
;(check-equal? (findCharPos #\a) 0)

; This function computes the weights in the first N positions of the frequency
; function f. It processes the message one letter at a time. Function f is initially filled
; with 0's indicating the number of times the letter has occurred in the message.
; The position of the letter (in the alphabet) is used as an index into f. We add one to the
; count for that letter.
;
; string, vector -> (void)
(define build-frequencies-function
(lambda (msg func)
(for ([ch msg])
(let ([i (findCharPos ch)])
(vector-set! func i (+ (vector-ref func i) 1))))))

; Invoke the function to initialize the N weights of the frequency vector
(build-frequencies-function quote f)

; We want to keep indexes organized in a priority queue based on their frequency
; This function allows us to compare characters (indexes of characters, really) using their frequencies
(define ij<=?
(lambda (i j)
(<= (vector-ref f i)(vector-ref f j))))

; The priority queue based on character frequency
(define PQ (make-heap ij<=?))

; Add all the characters to the priority queue (based on frequencies)
(for ([i (string-length ALPHABET)])
(heap-add! PQ i))

; This function retrieves the two "smallest" characters in the queue
; The characters are returned in a list
; priority queue -> list
(define getIJ
(lambda (Q)
(let ([i (heap-min Q)])
(begin
(heap-remove-min! Q)
(let ([j (heap-min Q)])
(begin
(heap-remove-min! Q)
(list i j)))))))

; Each character in the alphabet will be represented by a binary string
; The bits vector contains each binary string
; The ith element of bits is the binary representation of the ith letter in the alphabet
(define bits (make-vector FLEN ""))

; For internal vertices in the tree, the ith element of LC is the index of the left child
(define LC (make-vector FLEN -1))

; For internal vertices in the tree, the ith element of LC is the index of the left child
(define RC (make-vector FLEN -1))

; Huffman's algorithm!
; Remove the two smallest values from the queue
; Add back (to the queue) their combined weights as the frequency of the parent
; Repeat until there is a single weight that represents the weight of the tree

(for ([k (in-range N (- (* N 2) 1))])
(let* ([lst (getIJ PQ)] ;remove the two smallest values from the queue in a list
[i (car lst)] ; i is the smallest value
[j (cadr lst)]) ; j is the next smallest value
(begin
(vector-set! LC k i) ; set the left child pointer at the smallest value
(vector-set! RC k j) ; set the right child pointer at the next smallest value
(vector-set! f k (+ (vector-ref f i)(vector-ref f j))) ;add the sum of the values at the next sequential spot in f
(heap-add! PQ k)))) ; add the index (associated with the sum) back to the priority queue

; After running Huffman's algorithm, vector f is initialized with frequencies.
; This function computes all the binary representations of weights in the function f
; v - the frequency vector
; i - the index of a vertex in the tree
; code - the binary code that has been constructed so far
;
; The function is first invoked at the root of the tree.
; The initial code is the empty string "".
; If the function moves left, a 0 is added to the code.
; If the function moves right a 1 is added to the code.
; The code string is returned when we encounter a leaf (a weight).
; This function follows every path from the root to leaf
;
; vector, int, string -> (void)
(define makebits
(lambda (v i code)
(cond
((< i N) (begin
(vector-set! v i code)
; (display code)
; (newline)
))

( else (let ([lc (vector-ref LC i)]
(rc (vector-ref RC i)))
(begin
(makebits v lc (string-append code "0"))
(makebits v rc (string-append code "1"))))))))

; Invoke the code that builds the binary representations of each character
; in the alphabet
(makebits bits ROOT "")

; Converts a character to a binary code
; Find the position of the character in the alphabet,
; Use the position to reference the code in the bits vector
(define char->code
(lambda (ch)
(vector-ref bits (findCharPos ch))))

; Use these checks for the 64 character alphabet
;(check-equal? (char->code #\A) "10101100")
;(check-equal? (char->code #\a) "0101")
; Use these checks for the 8 character alphabet
;(check-equal? (char->code #\a) "00")
;(check-equal? (char->code #\g) "01100")

; You will need to be familiar with these vectors;
; f - the frequency vector
; LC - the left children vector
; RC - the right children vector
; bits - the binary representaton vector

; Write a function called msg->code, that converts a message string to its binary Huffman representation.
; Print out the length of the binary message and the length of the binary message if we had simply
; used 7 bits for each character in the message.
; The following code converts an "a" to "01100" in the first alphabet.
; (vector-ref bits (vector-ref f (findCharPos #\a)))



;Just run this, it will call the other 2 functions along with some printouts of values
;currently, the code is set up for the bigger quote and ALPHABET.
;if you want to see the smaller ones, just comment and uncomment at the top for your quote and alphabet
;the printout is long so you will need to scroll for a sec.
(define finalExam
  (lambda ()
    
    (msg->code)
    (code->msg)

  ))



;this is my message to code funciton
;it uses 2 stroing variables
;it prints values to the screen along with some messages 


(define bMessage "");holds the binary messages aka coded message
(define oldLen 0); holds the lenght of the old uncoeded message

;the fucntion that will take a message and convert it to a binary code
(define msg->code
  (lambda ()

    (set! oldLen (* 7 (string-length quote)));gets how much space the uncoded message would have taken up

       ; goes though each letter in the quote and converts it to a binay value
       (for-each (lambda (letter)
                   (set! bMessage (string-append bMessage (char->code letter)  ))

               )
              (string->list quote))


    ;some print outs of our values
    (writeln "MESSAGE WAS:")
    (writeln quote)
    (write "WITH BINARY LENGHT OF: " )
    (writeln oldLen)
    (writeln "")

    (writeln "MESSAGE WHEN WE CONVERT WITH HUFFMAN'S ALGORITHM")
    (writeln bMessage)
    (write "WITH NEW BINARY LENGHT OF: " )
    (writeln (string-length bMessage))
    
    ))


; Write a function called code->msg, that converts a binary Huffman message back to its original character representation.
; Test your code with both messages and alphabets.



; the following is my code->msg fucntion
; it has 2 holding varabiles and uses one helper function

(define curBit ""); what currnt sub bit string we are working on
(define deCodeMsg ""); the decoded message

;this will take a string of binary code and covert it to a message
; it will print the values to the screen along with some messages about the values
(define code->msg
  (lambda ()



    (writeln "")
    (writeln "")

    ;print outs of values before the decoting process
    (writeln "WE ARE NOW DECODING THE MESSAGE")
    (writeln "MESSAGE SHOULD BE:")
    (writeln quote)
    (writeln "")
    (writeln "WE ARE DECODING THE BINARY STRING OF:")
    (writeln bMessage)

    ;converts the bianry message to a list
    (set! bMessage (string->list bMessage))

    ;goes thougth each bit in the stirng and goes down the tree to find its alpha value
    (for-each (lambda (bit)
                ; sets up our current substring bit value
                (set! curBit (string-append curBit (string bit) ))

                ; checks to see if the substring is a valid substring for a alpha value
                ; if it is, it will run a helper function to convert that substyring into a alpha character
                (cond
                  [ (equal? #f (vector-member curBit bits))                          ]
                  [  else                                                    (set! deCodeMsg (string-append (string(code->char curBit))deCodeMsg))
                                                                              (set! curBit "")
                                                                             ]                                                                           
                  )

                
               )
              bMessage)

    ; this just reverses the order of the decored message
    (set! deCodeMsg (list->string(reverse (string->list deCodeMsg))))

    ;display for the decoded message
    (writeln "")
    (writeln "THE DECODED MESSAGE IS:")
    (writeln deCodeMsg)


  ))



; the follow is the helper fucntion that takes a substring of binary value and converts it into a character value
; it uses one funciton and one storing variable

(define curTreeLevel ROOT);holds what current level we are in going down the tree

;takes a binay substring and makes into a char value
(define code->char
  (lambda (bitString)
    
    (set! curTreeLevel ROOT);sets to root on every run

    ;this will go down ether the left or right child depending on if its a 0 or 1
    (for-each (lambda (bit)


                (cond
                  [ (equal? bit #\0)              (set! curTreeLevel (vector-ref LC curTreeLevel))]
                  [ (equal? bit #\1)              (set! curTreeLevel (vector-ref RC curTreeLevel))]
                  )
                
                
               )
              (string->list bitString))

    ;looks at the alphabet for the correft character based on where we eneded in the tree terversal
    (string-ref ALPHABET curTreeLevel)


    ))
  