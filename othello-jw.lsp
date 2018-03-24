#|================  Othello  ================

Author:     John M. Weiss, Ph.D.
Program:    Othello game
Course:     CSC447 AI
Date:       Spring 2018

Purpose:    This file contains a Lisp program that plays Othello.

Usage:      (othello)

Modifications:
180322 - Stripped down version with interface and basic functionality for Noah Brubaker.
    Noah will add minimax etc.
|#

;================  Global Constants ================

(defvar *SIZE* 8)       ; board dimensions (8x8)
(defvar *BOARD*)        ; board representation (list)
(defvar *RANDOMSTATE*)  ; random number generator state

;================  Othello  ================

(defun othello ( )
    ; call the init function:
    ;   sets board dimensions to 8x8 (*SIZE*)
    ;   stores initial position (*BOARD*)
    ;   initializes random number generator (*RANDOMSTATE*)
    (othello-init)

    ; local vars
    (let ( (board *BOARD*) (two-players t) moves move row col)

        ; begin play
        (print-board board)
        (do ()
            ((game-over board) (format t "~%Game over!~%"))    ; exit when game over

            ; move for player 1
            ; to do: add minimax for computer opponent
            (cond
                (two-players
                    (setf moves (generate-moves board 'B))
                    (cond
                        (moves
                            (format t "~%Valid moves for Black:")
                            (dolist (move moves) (format t " ~a" move))
                            (format t "~%")
                            ; get move from player
                            ; (setf move (player-move board 'B))
                            ; OR select move randomly
                            (setf move (nth (random (length moves) *RANDOMSTATE*) moves))
                            (setf row (first move) col (second move))
                            (format t "Selected move: ~a ~a~%~%" row col)
                            (setf board (valid-move board (1- row) (1- col) 'B))
                        )
                        (t (format t "~%Black cannot move!~%~%"))
                    )
                )
            )
            (print-board board)

            ; move for player 2
            ; to do: add minimax for computer opponent
            (cond
                (two-players
                    (setf moves (generate-moves board 'W))
                    (cond
                        (moves
                            (format t "~%Valid moves for White:")
                            (dolist (move moves) (format t " ~a" move))
                            (format t "~%")
                            ; get move from player
                            ; (setf move (player-move board 'W))
                            ; OR select move randomly
                            (setf move (nth (random (length moves) *RANDOMSTATE*) moves))
                            (setf row (first move) col (second move))
                            (format t "Selected move: ~a ~a~%~%" row col)
                            (setf board (valid-move board (1- row) (1- col) 'W))
                        )
                        (t (format t "~%White cannot move!~%~%"))
                    )
                )
            )
            (print-board board)
        )

        ; output the results
        (setf score (count-pieces board))
        (format t "Score: Black ~a, White ~a~%" (car score) (cadr score))
        (cond
            ((> (car score) (cadr score)) (format t "Black wins!~%"))
            ((< (car score) (cadr score)) (format t "White wins!~%"))
            (t (format t "Tie game!~%"))
        )
    )

    "Thanks for playing!"
)

;================  initialize board  ================

(defun initial-position()
    "returns initial board position, as list of printable symbols"
    '(- - - - - - - -
      - - - - - - - -
      - - - - - - - -
      - - - W B - - -
      - - - B W - - -
      - - - - - - - -
      - - - - - - - -
      - - - - - - - -)
)

;================  Get Play Type  ================

; Returns the desired type of play:
;   1 = watch computer play against itself
;   2 = play another person
;   3 = play against computer as B
;   4 = play against computer as W

(defun get-play-type ()
    (let (selection)
        (format t "~%1) Play the computer")
        (format t "~%2) Watch the computer play")
        (format t "~%3) Play another person")
        (format t "~%Your choice? ")
        (setf selection (read))

        ; check for valid selection
        (do ()
            ((and (> selection 0) (<= selection 3)))
            (format t "~%Please choose 1, 2 or 3: ")
            (setf selection (read))
        )

        (cond
            ((= selection 3)
                (if (y-or-n-p "Would you like to play first (y or n)?") selection (1+ selection))
            )
        )

        selection
    )
)

;================  Game Over  ================

; Returns true if the game is over for the given board configuration.
(defun game-over (board)
    (not (or (can-move board 'B) (can-move board 'W)))
)

; Returns true if the board is full (no '-', all B/W)
(defun board-full (board)
    (dotimes (i (* *SIZE* *SIZE*))
        (when (eq (nth i board) '-) (return-from game-over nil))
    )
    t
)

;================  Can Move  ================

; Returns true if the given piece has a move for the given board configuration.

(defun can-move (board piece)
    (dotimes (i *SIZE*)
        (dotimes (j *SIZE*)
            (when (valid-move board i j piece) (return-from can-move t))
        )
    )
    nil
)

;================  Count Pieces  ================

; returns list containing the number of black and white pieces on the board

(defun count-pieces (board)
    (let ((b 0) (w 0))
        (dolist (pos board (list b w))
            (cond
                ((eq pos 'B) (incf b))
                ((eq pos 'W) (incf w))
            )
        )
    )
)

;================  Print Board  ================

; print the board

(defun print-board (board)
   (let ((position board))
        ; print numbers along the top
        (format t " ")
        (dotimes (i *SIZE*) (format t " ~A" (+ i 1)) )
        (format t "~%")

        ; print each row of the board
        (dotimes (i *SIZE*)
             ; print the number along the side
            (format t "~A" (+ i 1))

            ; print the board row
            (dotimes (j *SIZE*)
                (format t " ~A" (car board))
                (setf board (cdr board))
            )
            (format t "~%")
        )
    )
)

;================  Generate Moves  ================

; Returns a list of all possible successor board configurations

(defun generate-successors (board piece)
    (let (moves valid)
        ; for each possible move
        (dotimes (i *SIZE* moves)
            (dotimes (j *SIZE*)
                ; if it results in a valid position, add it to list
                (cond
                    ((setf valid (valid-move board i j piece))
                        (push valid moves)
                    )
                )
            )
        )
    )
)

; Returns a list of all valid moves
(defun generate-moves (board piece)
    (let (moves)
        ; for each possible move
        (dotimes (row *SIZE*)
            (dotimes (col *SIZE*)
                ; if it results in a valid position, add it to list
                (if (valid-move board row col piece) (push (list (1+ row) (1+ col)) moves))
            )
        )
        moves
    )
)

;================  Valid Move  ================

; If (row col) is a valid move for the given piece on
; the given board, then return a new board with the piece added.
; If the move is invalid, nil is returned.

(defun valid-move (board row col piece)
    (let
        (
            (position (+ (* row *SIZE*) col))
            (new-board (copy-list board))
        )

        ; check that the position is empty
        (if (not (eq (nth position board) '-)) (return-from valid-move nil))

        ; place a piece in the position
        (setf (nth position new-board) piece)

        ; try to flip pieces in the various directions
        ; if flipping pieces was successful in some direction,
        ; then return the new board; otherwise return nil
        (if
            (or
                (flip-pieces new-board position piece 1 0)
                (flip-pieces new-board position piece -1 0)
                (flip-pieces new-board position piece *SIZE* 0)
                (flip-pieces new-board position piece (- *SIZE*) 0)
                (flip-pieces new-board position piece (- (1+ *SIZE*)) 0)
                (flip-pieces new-board position piece (- 1 *SIZE*) 0)
                (flip-pieces new-board position piece (1- *SIZE*) 0)
                (flip-pieces new-board position piece (1+ *SIZE*) 0)
            )
            new-board
            nil
        )
    )
)

;================  Flip Pieces  ================

; Recursive function for flipping pieces.
; Board is only modified if the direction for flipping is valid.
; Position is the position on the board being considered for flipping.
; Piece is what the pieces are being flipped to.
; Step indicates the direction for flipping.
; Count is used to test for special cases.
; Recursive calls are used to search for another of the given piece.
; If a match is found, then flipping is valid and true is returned.
; If true is returned from the recursive call, then flip the piece at that position.

(defun flip-pieces (board position piece step count)
    (let (flip (next-pos (+ position step)))
        (cond
            ; check if we've reached a match to the given piece
            ((and (eq (nth position board) piece) (> count 1)) t)

            ; check if the first match is after one step
            ((and (eq (nth position board) piece) (= count 1)) nil)

            ; check if we've reached the edge of the board
            ((< next-pos 0) nil)
            ((> next-pos (* *SIZE* *SIZE*)) nil)
            ((and (= (1- *SIZE*) (mod position *SIZE*)) (= 0 (mod next-pos *SIZE*))) nil)
            ((and (= 0 (mod position *SIZE*)) (= (1- *SIZE*) (mod next-pos *SIZE*))) nil)

            ; check if we've reached an empty space
            ((eq (nth position board) '-) nil)

            (T
                ; do recursive call
                (setf flip (flip-pieces board next-pos piece step (1+ count)))
                ; flip piece if valid
                (if flip (setf (nth position board) piece))
                ; return flip validity
                flip
            )
        )
    )
)

;================  Player Move  ================

; get valid move from player, then returns the move as (row col) list
(defun player-move (board piece)
   (let (row col)
        (format t "~%Enter the move for ~A [row column]: " piece)
        (setf row (read) col (read))

        ; loop until player enters valid move
        (do ( (newboard (valid-move board (1- row) (1- col) piece) (valid-move board (1- row) (1- col) piece)) )
            (newboard (list row col))
            (format t "~%Invalid move, enter a different move [row col]: ")
            (setf row (read) col (read))
        )
    )
)

; apply (row col) move to board
; note that this destructively changes the board
(defun apply-move( board move piece )
    (setf (nth (+ (* (car move) *SIZE*) (cadr move)) board) piece)
)

;================ required functions for tournament play ================

(defun othello-init ()
    "initialization code for othello"
    (setf *SIZE* 8)
    (setf *BOARD* (initial-position))
    (setf *RANDOMSTATE* (make-random-state t))
)

(defun make-move (position player depth)
    "make move in given position, returns (row col)"
    (if (can-move position player)
        (setf move (computer-move position player depth heuristic))
        (format t "~%Player ~A cannot move" player)
    )
    move
)

;================

(othello)
