#lang racket
;Racket Lab 1 - Luis Bobadilla
(provide Node)
(provide Leaf)
(provide sum_tree)
(provide deepest)

(struct Leaf (num))
(struct Node (left right))

;test trees
(define t0 (Node (Node (Leaf 9) (Leaf 3) ) (Leaf 4) ))
(define t1 (Node (Leaf 4) (Node (Node (Leaf 3) (Leaf 7) ) (Leaf 4 ))))

;sum_tree def
(define (sum_tree tree)
  (cond
  [(Leaf? tree) (Leaf-num tree) ]
  [(Node? tree)  (+ (sum_tree (Node-left tree)) (sum_tree(Node-right tree)))]))

;height def
(define (height tree ) 
	(cond
		[(Leaf? tree  ) 0    ]
		[ else (+ 1 (max (height (Node-left  tree))
                    (height (Node-right tree))))] ))

;how_deep def
(define (how_deep tree) 
	(cond
		[(Leaf? tree) (list (Leaf-num tree)) ]
            [(< (height (Node-left tree)) (height (Node-right tree))) (let ((k (how_deep (Node-right tree)))) k)]
            [(equal? (height (Node-left tree)) (height (Node-right tree))) (let ((kx (how_deep (Node-left tree))) (ky (how_deep (Node-right tree)))) (append kx ky))  ]                                   
            [else (let ((j (how_deep (Node-left tree))))   j)]))

; deepest definition
(define (deepest tree)
  (values (height tree) (how_deep tree)))
 
