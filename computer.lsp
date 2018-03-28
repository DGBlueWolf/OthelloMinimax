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
    (or 
        ;Gets a list of possible moves
        (loop for position from 0 to (1- (* *size* *size*))
            when (p-valid-move (othello-state-board state) position (othello-state-piece state)) collect it
        )
        ; If no moves are possible, it returns list with one element,
        ;    the previous board state with nil move and next players piece
        `(,(make-othello-state 
            :board (othello-state-board state)
            :move nil
            :piece (if (equal (othello-state-piece state) 'B) 'W 'B)
        ))
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

;================ Heuristics ================
(defun random-eval-state (state) (random 3.0))

(defun simple-eval-state (state) 
    (float (apply #'- (count-pieces (othello-state-board state))))
)

(defun weighted-eval-state (state) 
    (float (loop for w in *static-weights*
                 for piece in (othello-state-board state)
        when (equal 'B piece) sum w
        when (equal 'W piece) sum (- w)
    ))
)

(defvar *static-weights* 
    '(32  2  4  4  4  4  2 32
       2  0  1  1  1  1  0  2
       4  1  4  2  2  4  1  4
       4  1  2  8  8  2  1  4
       4  1  2  8  8  2  1  4
       4  1  4  2  2  4  1  4
       2  0  1  1  1  1  0  2
      32  2  4  4  4  4  2 32)
)

; ref: Othello Heuristics @Kartik Kukreja 
; https://kartikkukreja.wordpress.com/2013/03/30/heuristic-function-for-reversiothello/

(defun coin-difference (state) 
    (let ((score (count-pieces (othello-state-board state))))
        (/ (float (apply #'- score)) (float (apply #'+ score)))
    )
)

(defun mobility-stability (state) 
    (let (  
            (maxmoves 
                (loop for move in (generate-successors (othello-state-board state) 'B) 
                    collecting move
                )
            )
            (minmoves 
                (loop for move in (generate-successors (othello-state-board state) 'W) 
                    collecting move
                )
            )
            (mobility 0)
            (stability 0)
        )
        (when (/= (length maxmoves) (length minmoves)) 
            (setf mobility (/ (float (- maxmoves minmoves)) (+ maxmoves minmoves)))      
        )
    )