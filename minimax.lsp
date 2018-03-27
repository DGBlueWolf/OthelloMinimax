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
    20182303 - Completed basic implementation of the algorithm
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
(defun minimax (state player move-generator eval-state end-condition ply)
    ; Initialize local variables
    (let*
        ((root ; If we were given a state, make this the root node
            (if (equal (type-of state) 'node) 
                state 
                (make-node ; Build a root node from state if not node
                    :state state
                )
            )   
        )(skey ; Set the key comparator for sorting (children are not player)
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

        ; If we've hit the bottom, set bound to the value of rank and return root
        (when (deep-enough root ply end-condition) (progn
            (if player
                (setf (node-alpha root) (node-rank root))
                (setf (node-beta  root) (node-rank root))
            ) 
            (return-from minimax root))
        )

        ;(format t "~%~%>Root (~A~A, ~A, alpha: ~A, beta: ~A):  ~A~%~%" 
         ;   (if player 'B 'W) ply (node-rank root) (node-alpha root) (node-beta root) (othello-state-move (node-state root)))
        ; Otherwise loop through successors
        (loop for n in
                ; Sort each generated successor by it's evaluation function
                (sort 
                    (copy-seq (loop for move in (funcall move-generator (node-state root))
                        collecting (make-node
                            :parent root
                            :state move
                            :rank (funcall eval-state move))))
                    skey
                )
            with result = nil
            ; Child of max player gets root's current alpha value
            if player
                do (setf (node-alpha n) (node-alpha root))
            ; Child of min player gets root's current beta value
            else 
                do (setf (node-beta n) (node-beta root))

            ;do (format t "--> Child(~A~A, ~A, alpha: ~A, beta: ~A):  ~A~%" 
             ;   (if player 'B 'W) ply (node-rank n) (node-alpha n) (node-beta n) (othello-state-move (node-state n)))

            ; Make a recursive call and update best if necessary
            do (setf result (minimax n (not player) move-generator eval-state end-condition (1- ply)))   
            when (funcall nkey root result) do (progn 
                (setf (node-best-child root) n)
            
                (if player 
                    ; If max player update the alpha of root with beta of child
                    (setf (node-alpha root) (node-beta n))
                    ; If min player update the beta of root with alpha of child
                    (setf (node-beta root) (node-alpha n))
                )

               ; (format t "~%~%>Root (~A~A, ~A, alpha: ~A, beta: ~A):  ~A~%~%" 
                ;    (if player 'B 'W) ply (node-rank root) (node-alpha root) (node-beta root) (othello-state-move (node-state root)))
            )
            when (>= (node-alpha root) (node-beta root)) do (return-from minimax root)
        )
        ;(format t "~%<Root (~A~A, ~A, alpha: ~A, beta: ~A):  ~A~%~%"
         ;   (if player 'B 'W) ply (node-rank root) (node-alpha root) (node-beta root) (othello-state-move (node-state root)))
        (return-from minimax root)
    )
)

;================ Deep-Enough ================
(defun deep-enough (root ply end-condition) 
    (or 
        (<= ply 0) ; Out of ply
        (>= (node-alpha root) (node-beta root)) ; alpha > beta already
        (funcall end-condition (node-state root)) ; game over
    )
)
