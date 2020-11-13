;peter keres
;otc 6 2019

;midterm


; NOTE: just run the 'main' fucntion to see answers for all 4 graphs

#lang racket

(define Graph1  ; A set of edges; each edge consists of two vertices and a weight (v1 v2 w)
  '((0 1 6)(0 2 8)(0 3 1)(1 2 2)(1 5 1)(1 6 12)(2 5 2)(2 3 1)(3 5 5)(3 4 8)(6 5 2)(5 4 3)(4 8 9)(7 8 1)(6 7 3)(7 9 8)(8 9 4)))
(define Vertices1 '(0 1 2 3 4 5 6 7 8 9))  ; A set of vertices in the graph

(define Graph2
  '((0 1 1)(1 2 1)(0 2 4))) ; A set of edges; each edge consists of two vertices and a weight (v1 v2 w)
(define Vertices2 '(0 1 2)) ; A set of vertices in the graph

;set for graph 3
(define Graph3
  '( (0 1 1) (0 6 8) (1 2 1) (2 3 2) (2 4 2) (2 5 5) (3 4 6) (5 6 4)))
(define Vertices3 '(0 1 2 3 4 5 6))

;set for graph 4
(define Graph4
  '((0 1 1) (1 2 4) (2 3 2) (3 4 6) (4 5 7) (5 0 9) (5 2 8) (5 1 9)))
(define Vertices4 '(0 1 2 3 4 5))

; This implementation uses a vector to represent a function that maps each vertex to its parent
(define Parent null)
; This implementation uses a vector to represent a function that maps each vertex to its rank in the tree
(define Rank null)

;this holds our minnimum weighted spanning tree. aka 'x'
(define minSpanTree null)
;this holds a sorted of a copy of a graph. sotred from lowest weight in front, highest in back
(define sortedGraph null)
;this holds what sport in the sorted graph to add the next spot
(define sortedSlot 0)
;this holds what the highest weight is in a set of edges
(define highestWeight 0)
;this holds the edge removed
(define removedEdge null)



; Retrieves a parent given a vertex
(define get-parent
  (lambda (i)
    (vector-ref Parent i)))

; Sets the parent of a given vertex
(define set-parent!
  (lambda (i p)
    (vector-set! Parent i p)))

; Retrieves a rank for a given vertex
(define get-rank
  (lambda (i)
    (vector-ref Rank i)))

; Sets the rank of a given vertex
(define set-rank!
  (lambda (i r)
    (vector-set! Rank i r)))

; Makes a vertex x its own parent and gives it rank 0
(define makeset
  (lambda (x)
    (begin
      (set-parent! x x)
      (set-rank! x 0))))

; Walks up the chain of parent references and returns
; the parent at the top of the chain
(define find
  (lambda (x)
    (cond
      ((= x (get-parent x)) x)
      ( else (find (get-parent x))))))

; Unions the tree that contains x with the tree that contains y
(define union
  (lambda (x y)
    (let* ((rx (find x))
           (ry (find y))
           (rankx (get-rank rx))
           (ranky (get-rank ry)))
      (cond
        ((= rx ry) (void))
        ((> rankx ranky) (set-parent! ry rx))
        ( else (begin
                 (set-parent! rx ry)
                 (cond
                   ((= rankx ranky) (set-rank! ry (+ rankx 1))))))))))


;runs makeset on each vertex in a list of vertexs
; vertexList: a list of vertexs for the graph
(define makesetall
  (lambda (vertexList)
    (for-each (lambda (vertex)
                (makeset vertex)   
                )
              vertexList)
    )
  )


;looks though a list of edges and finds the highest weight in the set
;each of the following method workst with a different graph. could not find a pass by reference ;(
;sets this value to the global varable 'highestWeight'-
; edgesList: the graph you are working with 
(define getHighestWeight
  (lambda ()
    (set! highestWeight 0)
    (for-each (lambda (edge)
                (cond
                  [(< highestWeight (last edge))     (set! highestWeight (last edge))]
                )
               )
              Graph1)

  )
)

;for graph 2
(define getHighestWeightG2
  (lambda ()
    (set! highestWeight 0)
    (for-each (lambda (edge)
                (cond
                  [(< highestWeight (last edge))     (set! highestWeight (last edge))]
                )
               )
              Graph2)

  )
)

;for graph 3
(define getHighestWeightG3
  (lambda ()
    (set! highestWeight 0)
    (for-each (lambda (edge)
                (cond
                  [(< highestWeight (last edge))     (set! highestWeight (last edge))]
                )
               )
              Graph3)

  )
)

;for graph 4
(define getHighestWeightG4
  (lambda ()
    (set! highestWeight 0)
    (for-each (lambda (edge)
                (cond
                  [(< highestWeight (last edge))     (set! highestWeight (last edge))]
                )
               )
              Graph4)

  )
)



;this gets the first edge from graph1 where its weight matches the weight given.
; each of the following method workst with a different graph. could not find a pass by reference ;(
(define getEdgeByWeight
  (lambda ()
    (set! removedEdge null)
    (for-each (lambda (edge)
                (cond
                  [(= highestWeight (last edge))             (set! removedEdge edge)]
                  )                                                     
                )                
              Graph1)
   (set! Graph1 (remove removedEdge Graph1)
  )
)
)

;for graph2
(define getEdgeByWeightG2
  (lambda ()
    (set! removedEdge null)
    (for-each (lambda (edge)
                (cond
                  [(= highestWeight (last edge))             (set! removedEdge edge)]
                  )                                                     
                )                
              Graph2)
   (set! Graph2 (remove removedEdge Graph2)
  )
)
)

; for graph 3
(define getEdgeByWeightG3
  (lambda ()
    (set! removedEdge null)
    (for-each (lambda (edge)
                (cond
                  [(= highestWeight (last edge))             (set! removedEdge edge)]
                  )                                                     
                )                
              Graph3)
   (set! Graph3 (remove removedEdge Graph3)
  )
)
)

;for graph 4
(define getEdgeByWeightG4
  (lambda ()
    (set! removedEdge null)
    (for-each (lambda (edge)
                (cond
                  [(= highestWeight (last edge))             (set! removedEdge edge)]
                  )                                                     
                )                
              Graph4)
   (set! Graph4 (remove removedEdge Graph4)
  )
)
)

;addeds the removed edged to the sortedGraph list
(define addToSort
  (lambda ()
    (set! sortedGraph (list-set sortedGraph sortedSlot removedEdge))
    (set! sortedSlot (- sortedSlot 1))
    (set! removedEdge null)
    )
  )

;creates a sorted list of edges sorting by weight, lowest at the front
;each of the following method workst with a different graph. could not find a pass by reference ;(
(define sort
  (lambda ()
    (set! sortedGraph (make-list (length Graph1) '(0 0 0)))
    (set! sortedSlot (length Graph1))
    (set! sortedSlot (- sortedSlot 1))
    (for ([i (length Graph1)])
      (getHighestWeight)
      (getEdgeByWeight)
      (addToSort)
      )  
     )
   )

;for grpah 2
(define sortG2
  (lambda ()
    (set! sortedGraph (make-list (length Graph2) '(0 0 0)))
    (set! sortedSlot (length Graph2))
    (set! sortedSlot (- sortedSlot 1))
    (for ([i (length Graph2)])
      (getHighestWeightG2)
      (getEdgeByWeightG2)
      (addToSort)
      )  
     )
   )

;for graph 3
(define sortG3
  (lambda ()
    (set! sortedGraph (make-list (length Graph3) '(0 0 0)))
    (set! sortedSlot (length Graph3))
    (set! sortedSlot (- sortedSlot 1))
    (for ([i (length Graph3)])
      (getHighestWeightG3)
      (getEdgeByWeightG3)
      (addToSort)
      )  
     )
   )

;for graph 4
(define sortG4
  (lambda ()
    (set! sortedGraph (make-list (length Graph4) '(0 0 0)))
    (set! sortedSlot (length Graph4))
    (set! sortedSlot (- sortedSlot 1))
    (for ([i (length Graph4)])
      (getHighestWeightG4)
      (getEdgeByWeightG4)
      (addToSort)
      )  
     )
   )



;the following runs each graph thought the algrithum
;also prints a statment of what the graph ogrianly was
;also prints out min spanning tree, in rev order so easer to read
;each of the following method workst with a different graph. could not find a pass by reference ;(
(define runGraph1
 (lambda ()
   (set!  Parent (make-vector (length Vertices1) 0))
   (set!  Rank (make-vector (length Vertices1) 0))
   (println "orginal graph 1")
   (println Graph1)
   (makesetall Vertices1)
   (sort)
   (for-each (lambda (edge)
               (cond
               [ (= (find (first edge)) (find (second edge)))                                           ]
               [else
                (set! minSpanTree (cons edge minSpanTree))
                (union (first edge) (second edge))
                ]
               )
               )  
             sortedGraph)
   (println "min weight spanning tree for graph 1")
   (println (reverse minSpanTree))
)
  )

;for grpah 2
(define runGraph2
 (lambda ()
   (set!  Parent (make-vector (length Vertices2) 0))
   (set!  Rank (make-vector (length Vertices2) 0))
   (println "orginal graph 2")
   (println Graph2)
   (makesetall Vertices2)
   (sortG2)
   (for-each (lambda (edge)
               (cond
               [ (= (find (first edge)) (find (second edge)))                                           ]
               [else
                (set! minSpanTree (cons edge minSpanTree))
                (union (first edge) (second edge))
                ]
               )
               )  
             sortedGraph)
   (println "min weight spanning tree for graph 2")
   (println (reverse minSpanTree))
)
  )

;for graph 3
(define runGraph3
 (lambda ()
   (set!  Parent (make-vector (length Vertices3) 0))
   (set!  Rank (make-vector (length Vertices3) 0))
   (println "orginal graph 3")
   (println Graph3)
   (makesetall Vertices3)
   (sortG3)
   (for-each (lambda (edge)
               (cond
               [ (= (find (first edge)) (find (second edge)))                                           ]
               [else
                (set! minSpanTree (cons edge minSpanTree))
                (union (first edge) (second edge))
                ]
               )
               )  
             sortedGraph)
   (println "min weight spanning tree for graph 3")
   (println (reverse minSpanTree))
)
  )

;for graph 4
(define runGraph4
 (lambda ()
   (set!  Parent (make-vector (length Vertices4) 0))
   (set!  Rank (make-vector (length Vertices4) 0))
   (println "orginal graph 4")
   (println Graph4)
   (makesetall Vertices4)
   (sortG4)
   (for-each (lambda (edge)
               (cond
               [ (= (find (first edge)) (find (second edge)))                                           ]
               [else
                (set! minSpanTree (cons edge minSpanTree))
                (union (first edge) (second edge))
                ]
               )
               )  
             sortedGraph)
   (println "min weight spanning tre for graph 4")
   (println (reverse minSpanTree))
)
  )


;this sets all our global fields to null / 0
; aka starting point 
(define nullOut
  (lambda ()
  (set! Parent null)
  (set! Rank null)
  (set! minSpanTree null)
  (set! sortedGraph null)
  (set! sortedSlot 0)
  (set! highestWeight 0)
  (set! removedEdge null)

  )
)


; this main runs all 4 graphs and also zeros out all globals after each run 
(define main
  (lambda ()
    (runGraph1)
    (nullOut)
    (runGraph2)
    (nullOut)
    (runGraph3)
    (nullOut)
    (runGraph4)
    (nullOut)
    )
  )


