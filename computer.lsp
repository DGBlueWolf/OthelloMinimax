#|================ Minimax with a-b pruning ================

Author:     Noah Brubaker
Program:    generic minimax with a-b pruning
Course:     CSC447 AI
Date:       Spring 2018

Purpose:    A Lisp module which builds a minimax a-b pruning
            agent to play Othello

Usage:      

Change Log:
|#

;================ Imports ================
(load 'minimax)
;(load 'othello-jw)

;================ Game State ================
(defstruct othello-state 
    (board nil)
    (move nil)
    (piece 'B)
)

;================ Minimax Functions ================
(defun end-condition (state)
    (game-over (othello-state-board state))
)

(defun move-generator (state)
    (loop for position from 0 to (1- (* *size* *size*))
        when (p-valid-move (othello-state-board state) position (othello-state-piece state)) collect it
    )
)

(defun p-valid-move (board position piece)
    (let* ((row (1+ (floor position *size*))
          )(col (1+ (mod position *size*))
          )(new-board (valid-move board (1- row) (1- col) piece)))
        (when new-board 
            (make-othello-state 
                :board new-board
                :move `(,row ,col)
                :piece (if (equal piece 'B) 'W 'B)
            )
        )
    ) 
)

(defun random-eval-state (state) (random 3.0))

(defun othello-minimax (state player eval-state ply) 
    (othello-state-move (node-state (node-best-child 
        (minimax state player #'move-generator eval-state #'end-condition ply))))
)

(defun computer-move (board player depth heuristic)
    (othello-minimax 
        (make-othello-state 
            :board board
            :move nil
            :piece player
        )
        (equal player 'B) ; The max player is black
        heuristic
        depth
    )
)
