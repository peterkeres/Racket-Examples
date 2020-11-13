#lang racket
(require rackunit)


;peter keres
;breath first search algrithms


(define graph
 '((0 (1 2))
   (1 (0 3 4))
   (2 (0 5 9))
   (3 (1))
   (4 (1))
   (5 (2 6 7 8))
   (6 (5))
   (7 (5))
   (8 (5))
   (9 (2 10 11))
   (10 (9))
   (11 (9))))

(define graph1
  '((0 (1 2 3))
    (1 (0 5 6))
    (2 (0 3 5))
    (3 (0 2 4 5))
    (4 (3 5 8))
    (5 (1 2 3 4 6))
    (6 (1 5 7))
    (7 (6 8 9))
    (8 (4 7 9))
    (9 (7 8))))

(define graph2
  '((0 (1 2))
    (1 (0 3))
    (2 (0 3))
    (3 (1 2))))

; Graph, vertext -> list of adjacent vertices
; Returns the vertices in G that are adjacent to v
(define adjacentVertices
  (lambda (G v)
    (cond
      ((null? G) '())
      ((eq? v (caar G)) (cadar G))
      ( else (adjacentVertices (cdr G) v)))))
(check-equal? (adjacentVertices graph 1) '(0 3 4) "adjacentVertices")

; environment, a set of vertices -> ()
; Explores each vertex in the given set of vertices
(define explore-for-each
  (lambda (env graph aSet)
    (cond
      ((null? aSet) '())
      ( else (begin
               (explore env graph (car aSet))
               (explore-for-each env graph (cdr aSet)))))))

; environment, a vertice -> ()
; Performs a depth-first search on a connected component in a graph starting at v
(define explore
 (lambda (env graph v)
   (let ((newenv (fvisit env v)))  ;visit the vertex, changing the environement
   (begin
      (previsit v)
      (explore-for-each newenv graph (filterout-visited  newenv (adjacentVertices graph v)))
      (postvisit v)))))

; Do something with vertex v
(define previsit
  (lambda (v)
    (display v)))

; Do something with vertext v
(define postvisit
  (lambda (v)
    (void)))

; () -> empty environment
; Returns an empty environment of visited vertices
(define empty
  (lambda () (list 'empty)))
 (check-equal? (empty) '(empty) "empty")

; vertex, environment -> environment of visited vertices
; Adds the vertex to the list of visited vertices
(define fvisit
  (lambda (env ver)
     (list 'visited ver env)))
(check-equal? (fvisit (empty) 'x) '(visited x (empty)))
(check-equal? (fvisit (fvisit (empty) 'x) 'y) '(visited y (visited x (empty))))

; environment, vertex -> boolean
; Returns true if the vertex is in the environment of visited vertices
(define visited?
  (lambda (env search-var)
    (cond
      ((eqv? (car env) 'empty) #f)
      ((eqv? (car env) 'visited) (let ((saved-var (cadr env))
                                       (saved-env (caddr env)))
                                    (if (eqv? search-var saved-var)
                                        #t
                                        (visited? saved-env search-var))))
      ( else #f))))

(define filterout-visited
  (lambda (env aSet )
    (cond
      ((null? aSet) '())
      (else (cond
              ((visited? env (car aSet))  (filterout-visited env (cdr aSet)))
              ( else (cons (car aSet)(filterout-visited env (cdr aSet)))))))))

(define a (empty))
(define b (fvisit a 'x ))
(define c (fvisit b 'y ))
(check-equal? (visited? c 'x) #t)
(check-equal? (visited? c 'z) #f)

(check-equal? (filterout-visited  c '(a b c d e x y z)) '(a b c d e z))

(explore  (empty) graph 0)
(newline)
(explore (empty) graph1 0)
(newline)
(explore (empty) graph2 0)



;breaht first search
(define exploreBreath
 (lambda (env graph v)
   (let ((newenv (fvisit env v)))  ;visit the vertex, changing the environement
   (begin
      (previsit v)
      (explore-for-each newenv graph (filterout-visited  newenv (adjacentVertices graph v)))
      (postvisit v)))))

