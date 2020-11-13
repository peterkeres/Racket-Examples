; peter keres
; oct 19 2019
; hash assigment



#lang racket
(require rackunit)
; State zip codes are stored as a list of sublists. Each sublist describes single range of zip codes in a given state. Each state can have multiple entries.
(define zipcodes '(
                   ("Alabama" "AL" 35004 36925)
                   ("Arkansas" "AR" 71601 72959)
                   ("Arkansas" "AR" 75502 75502)
                   ("Arizona" "AZ" 85001 86556)
                   ("California" "CA" 90001 96162)
                   ("Colorado" "CO" 80001 81658)
                   ("Connecticut" "CT" 06001 06389)
                   ("Connecticut" "CT" 06401 06928)
                   ("District of Columbia" "DC" 20001 20039)
                   ("District of Columbia" "DC" 20042 20599)
                   ("District of Columbia" "DC" 20799 20799)
                   ("Delaware" "DE" 19701 19980)
                   ("Florida" "FL" 32004 34997)
                   ("Georgia" "GA" 30001 31999)
                   ("Georga" "GA" 39901 39901)
                   ("Hawaii" "HI" 96701 96898)
                   ("Iowa" "IA" 50001 52809)
                   ("Iowa" "IA" 68119 68120)
                   ("Idaho" "ID" 83201 83876)
                   ("Illinois" "IL" 60001 62999)
                   ("Indiana" "IN" 46001 47997)
                   ("Kansas" "KS" 66002 67954)
                   ("Kentucky" "KY" 40003 42788)
                   ("Louisiana" "LA" 70001 71232)
                   ("Louisiana" "LA" 71234 71497)
                   ("Massachusetts" "MA" 01001 02791)
                   ("Massachusetts" "MA" 05501 05544)
                   ("Maryland" "MD" 20331 20331)
                   ("Maryland" "MD" 20335 20797)
                   ("Maryland" "MD" 20812 21930)
                   ("Maine" "ME" 03901 04992)
                   ("Michigan" "MI" 48001 49971)
                   ("Minnesota" "MN" 55001 56763)
                   ("Mississippi" "MS" 38601 39776)
                   ("Mississippi" "MS" 71233 71233)
                   ("Montana" "MT" 59001 59937)
                   ("North Carolina" "NC" 27006 28909)
                   ("North Dakota" "ND" 58001 58856)
                   ("Nebraska" "NE" 68001 68118)
                   ("Nebraska" "NE" 68122 69367)
                   ("New Hampshire" "NH" 03031 03897)
                   ("New Jersey" "NJ" 07001 08989)
                   ("New Mexico" "NM" 87001 88441)
                   ("Nevada" "NV" 88901 89883)
                   ("New York" "NY" 06390 06390)
                   ("New York" "NY" 10001 14975)
                   ("Ohio" "OH" 43001 45999)
                   ("Oklahoma" "OK" 73001 73199)
                   ("Oklahoma" "OK" 73401 74966)
                   ("Oregon" "OR" 97001 97920)
                   ("Pennsylvania" "PN" 15001 19640)
                   ("Puerto Rico" "PR" 00601 00988)
                   ("Rhode Island" "RI" 02801 02940)
                   ("South Carolina" "SC" 29001 29948)
                   ("South Dakota" "SD" 57001 57799)
                   ("Tennessee" "TN" 37010 38589)
                   ("Texas" "TX" 73301 73301)
                   ("Texas" "TX" 75001 75501)
                   ("Texas" "TX" 75503 79999)
                   ("Texas" "TX" 88510 88589)
                   ("Utah" "UT" 84001 84784)
                   ("Virginia" "VA" 20040 20041)
                   ("Virginia" "VA" 20040 20167)
                   ("Virginia" "VA" 20042 20042)
                   ("Virginia" "VA" 22001 24658)
                   ("Vermont" "VT" 05001 05495)
                   ("Vermont" "VT" 05601 05907)
                   ("Washington" "WA" 98001 99403)
                   ("Wisconsin" "WI" 53001 54990)
                   ("West Virginia" "WV" 24701 26886)
                   ("Wyoming" "WY" 82001 83128)
                   ))

; the table that will hold the states once hashed 
(define hashtable
  (make-vector 149))
; makes each stop in the table to be 0, this is mostly for debuging 
(vector-fill! hashtable '())

; this will hold the value of each letter.
; this will help in making the keys for the hash
(define valueChart
  (vector #\- #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z))

; just tempary  varables
(define curHash 0)
(define let1 " ")
(define let2 " ")
(define unqValue " ")
(define tempHash null)
(define section null)
(define stateTest #f)
(define zipTest #f)
(define goodZip #f)


; statedata item -> void
; Prints a single statedata item: (state name, state abbreviation, low zip code, high zip code)

(define printState
  (lambda (stdata)
    (begin
      (display (car stdata)) (display " ")
      (display (cadr stdata)) (display " ")
      (display (caddr stdata)) (display " ")
      (display (cadddr stdata)) (newline)
      )))

; list of statedata items -> void
; Prints each statedata item
; Each statedata item is a list: (state name, state abbreviation, low zip code, high zip code)

(define printStates
  (lambda (allzips)
    (cond
      ((null? allzips) (newline))
      ( else (begin
               (printState (car allzips))
               (printStates (cdr allzips)))))))



; this method will take a state abbravtion and convert it into a number that will be used as the intdex
; to the hash table 
(define hash
  (lambda (stateAbb)
    ;(+ (vector-member (string-ref stateAbb 0) valueChart) (vector-member (string-ref stateAbb 1) valueChart))

    ;(modulo(- 1(* (vector-member (string-ref stateAbb 0) valueChart) (vector-member (string-ref stateAbb 1) valueChart))) 149)

    ; making each lettter a number value
    (set! let1 (vector-member (string-ref stateAbb 0)valueChart))
    (set! let2 (vector-member (string-ref stateAbb 1)valueChart))

    ; creating based on the state abb
    (set! unqValue (+ (* 7(+ 3 let1)) (* 3(+ 13 let2))) )


    ; moding the key by the lenght of the hash table so it will fit in the index range 
    (modulo unqValue 149)
    
    ))


; this method wil build a hash table of state zips
(define buildTable
  (lambda ()

    ; forloop that goes over each zip in our zipcode table and hashs them
    ; and finally puts them in the hash table
    (for-each(lambda (zip)

               (set! curHash (hash (second zip)))

               (vector-set! hashtable curHash  (cons zip (vector-ref hashtable curHash) ))


               )
             zipcodes)

    )
  )

; this will check to see if a passing state is the same state as the section pulled from the hash table
(define checkState
  (lambda (state section)

    (cond
      [(string=? state (second section))       (set! stateTest #t) ]
      [else                                   (set! stateTest #f) ]
      )

    ))

; this will check to see if the passing zip is the same zip as the section pulled from the has table 
(define checkZip
  (lambda (zip section)

    (cond
      [(and (>= zip (third section))     (<= zip (fourth section)) )      (set! zipTest #t) ]
      [else                                                               (set! zipTest #f) ]
      )

    ))


; will see if a given zip and state is correct
(define reasonableZip?
  (lambda (table stateAbb zip)

    (set! tempHash (hash stateAbb))
    (set! section (vector-ref table tempHash))

    ; checks to see if the given state is good or not
    (cond
      [(null? section)           #f "reasonableZip? - bad state"]
      [else

       ; checks the bucket in the has table and compares our sending data to whats in there
       (for-each(lambda (part)
                  (checkState stateAbb part)

                  ;sees if the state test pass, if so do zip test
                  (cond
                    [ stateTest               (checkZip zip part)]
                    )

                  ;if both tests pass, we have a good zip
                  (cond
                    [ (and stateTest zipTest)          (set! goodZip #t)]
                    )
   
                  )
                section)
       
       ; returns a good zip or a bad zip depending on global 
       (cond
         [ goodZip                       #t "reasonableZip? - good zip"]
         [else                           #f "reasonableZip? - bad zip"]
         )

       ]


      )



    ))


; resets globals 
(define reset
  (lambda ()
    (set! stateTest #f)
    (set! zipTest #f)
    (set! goodZip #f)

    ))

; runs the first test 
(define test1
  (lambda ()
    (buildTable)
    (println "checking for 'GA' 55555")
    (println "should return a bad zip")
    (println (reasonableZip? hashtable "GA" 55555))
    (println "")
    (println "")

    ))

; runs the second test 
(define test2
  (lambda ()
    (buildTable)
    (println "checking for 'GA' 30240")
    (println "should return a good zip")
    (println (reasonableZip? hashtable "GA" 30240))   
    (println "")
    (println "")
    
    ))

;runs the thrid test 
(define test3
  (lambda ()
    (buildTable)
    (println "checking for 'BB' 30240")
    (println "should return a bad  state")
    (println (reasonableZip? hashtable "BB" 30240))   
    (println "")
    (println "")
    
    ))


;runs the forth test 
(define test4
  (lambda ()
    (buildTable)
    (println "checking for 'DC' 20020")
    (println "should return a good zip")
    (println (reasonableZip? hashtable "DC" 20020))   
    (println "")
    (println "")
    
    ))

;runs the fith test 
(define test5
  (lambda ()
    (buildTable)
    (println "checking for 'WA' 98000")
    (println "should return a bad zip")
    (println (reasonableZip? hashtable "WA" 98000))   
    (println "")
    (println "")
    
    ))

;runs the six test 
(define test6
  (lambda ()
    (buildTable)
    (println "checking for 'XX' 20020")
    (println "should return a bad state")
    (println (reasonableZip? hashtable "XX" 20020))   
    (println "")
    (println "")
    
    ))



; calls all tests in the system and resets globasl inbetween each call
(define testAll
  (lambda ()
    (test1)
    (reset)
    (test2)
    (reset)
    (test3)
    (reset)
    (test4)
    (reset)
    (test5)
    (reset)
    (test6)
    (reset)



    ))
; The goal is to hash all the state information into a hash table.
; The hash table is a vector.
; Each position in the vector is a bucket (list).
; Each bucket contains zero or more state lists.
; Each state list looks like this: (state name, state abbreviation, low zip, high zip).
; For example, here is one of the state lists for Georgia: ("Georgia" "GA" 30001 31999) .
; If Georgia was assigned a unique bucket by the hash function, the bucket would look like this: (("Georgia" "GA" 30001 31999)("Georga" "GA" 39901 39901))

; Define a function "hash" which receives a two-character state abbreviation and which returns an index into a vector.
; Test the hash function, to make sure it distributes the states uniformly. You can do this by storing a 0 in each bucket (instead of lists).
; Add one to the number in the bucket each time a state abbreviation hashes to the bucket.
; Examine the table to make sure the distribution looks uniform. Change the hash function if it isn't. Choose a prime number for the size of the table
; that is about twice the size of the number of state entries.

; After the hash function is working properly, define a function "buildTable" that uses the zipcodes list to build a hash table containing the state information.

; Define a function "reasonableZip?" that is passed the hashTable, a state abbreviation, and a zip code. Return #t if the zip is in range for the given state,
; otherwise return #f.

; Define any other functions you need to make this work.

; Write enough rackunit tests to convince me (and you) that the code works.
; Here are two examples:
;(check-equal? (reasonableZip? yourHashTable "GA" 31907) #t "reasonableZip - good zip")
;(check-equal? (reasonableZip? yourHashTable "GA" 90210) #f "reasonableZip - bad zip")
;(check-equal? (reasonableZip? yourHashTable "XX" 11111) #f "reasonableZip - bad state")