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

;(printStates zipcodes)

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