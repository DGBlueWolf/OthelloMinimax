#|================ Minimax with a-b pruning ================

Author:     Noah Brubaker
Program:    generic minimax with a-b pruning
Course:     CSC447 AI
Date:       Spring 2018

Purpose:    A Lisp program which runs minimax with a-b pruning
            given functions (eval-state), (move-generator)
            and parameters `ply`

Usage:      (minimax state generate-successors eval-state ply)

Change Log:

|#

;================ Graph Structure ================
(defstruct node 
    (parent nil)
    (best-child nil)
    (state nil)
    (rank 0.0 :type double-float)
    (alpha most-negative-double-float :type double-float)
    (beta most-negative-double-float :type double-float)
)

(swap comparator)

;================ Minimax ===============
(defun minimax (state player move-generator eval-state deep-enough ply)
    ; Initialize local variables
    (let*
        ((root ; If we were given a state, make this the root node
            (if (equal (type-of state) node) 
                state 
                (make-node ; Build a root node from state if not node
                    :state state
                )
            )   
        )(skey ; Set the key comparator for presorting (children are not player)
            (if (not player)
                (lambda (x y) (> (node-rank x) (node-rank y))) ; x > y is in order for the max player
                (lambda (x y) (< (node-rank x) (node-rank y))) ; x < y is in order for the min player
            )
        )(nkey ; Set the key comparator for maximizing / minimizing (root is player)
            (if player
                (lambda (x y) (> (node-bound x) (node-bound y))) ; x > y is in order for the max player
                (lambda (x y) (< (node-bound x) (node-bound y))) ; x < y is in order for the min player
            )
        ))

        ; If we've hit the bottom, set bound to the value of rank and return root
        (when (funcall deep-enough (node-state root) ply bound skey) (progn
            (setf (node-bound root) (node-rank root)) 
            (return-from 'minimax root)))

        ; Otherwise loop through successors
        (loop for n in
                ; Sort each generated successor by it's evaluation function
                (sort (copy-seq (loop for move in (funcall move-generator (node-state root))
                    collecting (make-node
                        :parent root
                        :state move
                        :rank (funcall eval-state move)
                        :bound (if (not player) alpha beta))
                )) :key skey)
            
            for result = nil then (minimax n move-generator eval-state deep-enough (1- ply))   
            when (funcall nkey root result) do (setf 
                (node-best-child root n)
            )
        )
    )
)