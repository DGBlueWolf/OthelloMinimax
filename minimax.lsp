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
    20180323 - Completed basic implementation of the algorithm
    20180326 - General debugging (still an issue with node reference)
|#

;================ Graph Structure ================
(defstruct node 
    (parent nil)
    (best-child nil)
    (state nil)
    (rank 0.0 :type float)
    (alpha most-negative-single-float :type float)
    (beta most-positive-single-float :type float)
)

;================ Minimax ===============
; This function runs minimax with alpha-beta pruning using a best first traversal
;
; It uses a doubly linked list (which admittedly doesn't lend itself to be printed in CLISP) to
; store the best path.
;
; It works by checking whether the start state is `deep-enough` (no ply, pruned, or game end).
; If not, it uses generate-successors to generate a list of successor states.
; The successor states are processed recursively in sorted order of their evaluation function with the toggled player.
; During processing either alpha or beta is updated as each is processed according to whether the current player
; is MAX or MIN (player = T or player = nil). The best-child is updated at the same time.
;
; The root node with the best-child set is returned by the alorithm.
; 
; Parameters:
;      state            - The start state
;      player [T/nil]   - Whether the current player is the max player
;      move-generaotr   - A function which takes state as an argument and returns a list of possible result states
;      eval-state       - A function which takes state and returns a `single-float` value
;      end-condition    - A function which takes state and returns whether the game has ended or not
;      ply [int]        - The maximum recursion depth of the minimax algorithm (how many game-steps to look into the future) 
;
(defun minimax (root player move-generator eval-state end-condition ply)
    ; Initialize local variables
    (let*
        ((skey ; Set the key comparator for sorting (children are not player)
            (if (not player)
                (lambda (x y) (< (node-rank x) (node-rank y))) ; x < y is in order for the min player
                (lambda (x y) (> (node-rank x) (node-rank y))) ; x > y is in order for the max player
            )
        )(nkey ; Set the key comparator for maximizing / minimizing (root is player)
            (if player
                (lambda (x y) (< (node-alpha x) (node-beta  y))) ; root < child is out of order for the max player
                (lambda (x y) (> (node-beta  x) (node-alpha y))) ; root > child is out of order for the min player
            )
        ))

        ;(format t ">Root (~A~A, ~A, alpha: ~A, beta: ~A):  ~A~%" 
            ;(if player 'B 'W) ply (node-rank root) (node-alpha root) (node-beta root) (othello-state-move (node-state root)))
        ; If we've hit the bottom, set bound to the value of rank and return root
        (when (deep-enough root ply end-condition) (progn
            (funcall end-condition (node-state root))
            (if player
                (setf (node-alpha root) (node-rank root))
                (setf (node-beta  root) (node-rank root))
            ) 
            (return-from minimax root))
        )
        ; Otherwise loop through successors
        (loop for n in
                ; Sort each generated successor by it's evaluation function
                (sort 
                    (copy-seq ; A copy of the move sequence
                        (loop for move in (funcall move-generator (node-state root))
                            collecting (make-node
                                :parent root
                                :state move
                                :rank (funcall eval-state move))
                        )
                    ) 
                    skey ; The sorting predicate
                )  

            ; Result of making recursive call to minimax
            with result = nil

            ; Child of max player gets root's current alpha value
            if player
                do (setf (node-alpha n) (node-alpha root))
            ; Child of min player gets root's current beta value
            else 
                do (setf (node-beta n) (node-beta root))

            ; Make a recursive call and update best if necessary
            do (setf result (minimax n (not player) move-generator eval-state end-condition (1- ply)))

            ; DEBUGGING
            ;(format t "Player: ~:[Min~;Max~] Ply: ~A ::: Curr: Alpha: ~13@A Beta: ~13@A~%" 
            ;    (not player) 
            ;    (1- ply) 
            ;    (node-alpha result) 
            ;    (node-beta result)
            ;)
            ;(format t "Player: ~:[Min~;Max~] Ply: ~A ::: Best: Alpha: ~13@A Beta: ~13@A~2%" 
            ;    (not player) 
            ;    (1- ply) 
            ;    (when (node-best-child root) (node-alpha (node-best-child root)))
            ;    (when (node-best-child root) (node-beta  (node-best-child root)))
            ;)

            ; Update when the result is better than roots current best value
            when (funcall nkey root result) do (progn 
                (setf (node-best-child root) n)
            
                (if player 
                    ; If max player update the alpha of root with beta of child
                    (setf (node-alpha root) (node-beta n))
                    ; If min player update the beta of root with alpha of child
                    (setf (node-beta root) (node-alpha n))
                )
            )
            when (>= (node-alpha root) (node-beta root)) do (return-from minimax root)
        )
        (return-from minimax root)
    )
)

;================ Deep-Enough ================
; This function determines when we've gone deep enough. 
; This happens 
;       when we run out of ply, 
;       when alpha > beta indicating a pruned branch, or
;       when we reach the game's end condition.

(defun deep-enough (root ply end-condition) 
    (or 
        (<= ply 0) ; Out of ply
        (>= (node-alpha root) (node-beta root)) ; alpha > beta already
        (funcall end-condition (node-state root)) ; game over
    )
)

;(trace deep-enough)
