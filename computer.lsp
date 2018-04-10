#|================  Minimax with a-b pruning ================

Author:     Noah Brubaker
Program:    generic minimax with a-b pruning
Course:     CSC447 AI
Date:       Spring 2018

Purpose:    A Lisp module which builds a minimax a-b pruning
            agent to play Othello

Usage:      

Change Log:
|#

;================  Dependency  ======================

(load 'minimax)

;================  Traversal  ====================
; This macro creates a loop that traverses the board in each of the 4 flipping directions
; This is useful when computing stability
;
; It uses idx by default as the board index and ridx as the row index for a given direction
;
(defmacro traverse-board (&key (idx 'idx) (ridx 'ridx) backward-let forward-let row-let col-let inner-let body ending)
    (let ((i (gensym))
          (j (gensym)))
        `(progn 
            ; init variables for the forward diagonal
            (let (,@forward-let)
                ; from top diagonally down to the left
                (loop for ,i from 0 below *size*
                    do (let (,idx (,ridx ,i) ,@inner-let)
                        (loop for ,j from 0 below (1+ ,i)
                            do (progn
                                (setf ,idx (+ ,i (* (1- *size*) ,j)))
                                ,@body
                            )
                            finally (progn ,@ending)
                        )
                    )
                )
                ; from right digonally down to the bottom
                (loop for ,i from 1 below *size* 
                    do (let (,idx (,ridx (+ ,i *size* -1)) ,@inner-let)
                        (loop for ,j from 0 below (- *size* ,i) 
                            do (progn 
                                (setf ,idx (+ 7 (* ,i *size*) (* (1- *size*) ,j)))
                                ,@body
                            )
                            finally (progn ,@ending)
                        )
                    )   
                )
            )
            ; init variables for the backward diagonal
            (let (,@backward-let)
                ; iterate from left hand side diagonally down
                (loop for ,i from (1- *size*) downto 1
                    do (let (,idx (,ridx (- *size* ,i 1)) ,@inner-let)
                        (loop for ,j from 0 below (- *size* ,i)
                            do (progn 
                                (setf ,idx (+ (* ,i *size*) (* (1+ *size*) ,j)))
                                ,@body
                            )
                            finally (progn ,@ending)
                        )
                    )
                )
                ; iterate from the top diagonally down
                (loop for ,i from 0 below *size* 
                    do (let (,idx (,ridx (+ ,i *size* -1)) ,@inner-let)
                        (loop for ,j from 0 below (- *size* ,i) 
                            do (progn 
                                (setf ,idx (+ ,i (* (1+ *size*) ,j)))    
                                ,@body
                            )
                            finally (progn ,@ending)
                        )
                    )   
                )
            )
            ; init variables for the row
            (let (,@row-let)
                ; iterate over rows
                (loop for ,i from 0 below *size*
                    do (let (,idx (,ridx ,i) ,@inner-let)
                        (loop for ,j from 0 below *size*
                            do (progn 
                                (setf ,idx (+ (* ,i *size*) ,j))
                                ,@body
                            )
                            finally (progn ,@ending)
                        )
                    )
                )
            )
            ; init variables for the column
            (let (,@col-let)
                ; iterate over cols
                (loop for ,i from 0 below *size*
                    do (let (,idx (,ridx ,i) ,@inner-let)
                        (loop for ,j from 0 below *size*
                            do (progn 
                                (setf ,idx (+ (* ,j *size*) ,i))
                                ,@body
                            )
                            finally (progn ,@ending)
                        )
                    )
                )
            )
        )
    )
)

;================  Global Variables  ================

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

;================  Game State  ======================

(defstruct othello-state 
    (board nil)
    (move nil)
    (piece 'B)
)

;================ Minimax Functions ================

; defines when the gave is over
(defun end-condition (state)
    (game-over (othello-state-board state))
)

; generates the next set of moves
(defun move-generator (state)
    (or 
        ;Gets a list of possible moves
        (loop for position from 0 below (* *size* *size*)
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

; checks if a move is possible and returns the result of the move if so
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

; sets up the function call to minimax for the othello game configuration
(defun othello-minimax (state player eval-state ply) 
    (othello-state-move (node-state (node-best-child 
        (minimax state player #'move-generator eval-state #'end-condition ply))))
)

; sets up the function call for the computers move to interface with the game
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
; Just generates a random number for variety, similar to a random player
(defun random-eval-state (state) (random 3.0))

; The simple eval state is the difference between the number of black coins and the number of white coins
(defun simple-eval-state (state) 
    (float (apply #'- (count-pieces (othello-state-board state))))
)

; The weighted eval state assigns values to a piece at each board position (not a great strategy)
(defun weighted-eval-state (state) 
    (float (loop for w in *static-weights*
                 for piece in (othello-state-board state)
        when (equal 'B piece) sum w
        when (equal 'W piece) sum (- w)
    ))
)

; Gets the number of moves for each player in the given state
(defun mobility (state) 
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
        )
        (- (length maxmoves)  (length minmoves))
    )
)

; A complicated function that counts the difference between the number of stable coins for
; black and the number of stable coins for white. A stable coin is one that cannot be
; flipped again for the rest of the game
(defun stability (state)
    (let (
            witnesses  ; pieces that are stable and can generate new candidates
            (stable (make-array `(,(* *size* *size*))))
            (row-full (make-array `(,*size*)))
            (col-full (make-array `(,*size*)))
            (for-full (make-array `(,(+ *size* *size* -1))))
            (bak-full (make-array `(,(+ *size* *size* -1))))
            (stab-full (make-array `(,(* *size* *size*) 4)))
            (board (make-array `(,(* *size* *size*)) :initial-contents (othello-state-board state)))
        )
        ; check if rows and columns are full
        (traverse-board 
            :backward-let ((row-ref bak-full))
            :forward-let  ((row-ref for-full))
            :row-let      ((row-ref row-full))
            :col-let      ((row-ref col-full))

            :body       ((when (equal (aref board idx) '-) (return nil)))
            :ending     ((setf (aref row-ref ridx) t))
        )
        ; for the rows and columns that are full set the stab-full for each element
        ; also set stab full for edge pieces
        (traverse-board
            :backward-let ((row-idx 0) (row-ref bak-full))
            :forward-let  ((row-idx 1) (row-ref for-full))
            :row-let      ((row-idx 2) (row-ref row-full))
            :col-let      ((row-idx 3) (row-ref col-full))
            :inner-let    ((edge-flag t))

            :body       (
                ; if it's an edge piece it is stable in that direction
                (when (and (not (equal (aref board idx) '-)) edge-flag) (setf (aref stab-full idx row-idx) t))
                (setf edge-flag nil) ; no longer an edge piece
                ; piece is stable in the given direction if the row is full in that direction
                (when (aref row-ref ridx) (setf (aref stab-full idx row-idx) t))
            )
            :ending     (
                ; last index could be an edge piece
                (when (not (equal (aref board idx) '-)) (setf (aref stab-full idx row-idx) t))
            )
        )
        (setf witnesses (loop for idx from 0 below (* *size* *size*)
            when (and (aref stab-full idx 0) 
                      (aref stab-full idx 1)
                      (aref stab-full idx 2)
                      (aref stab-full idx 3)) 
                do (setf (aref stable idx) (aref board idx)) 
                and collect idx
        ))
        (do ((candidates (gen-candidates board witnesses stab-full) (gen-candidates board witnesses stab-full)))
            ((not candidates))
            (setf witnesses (loop for idx in candidates 
                when (and (aref stab-full idx 0) 
                          (aref stab-full idx 1)
                          (aref stab-full idx 2)
                          (aref stab-full idx 3)) 
                    do (setf (aref stable idx) (aref board idx)) 
                    and collect idx
            ))
        )

    )
)

; A piece neighboring a stable piece of the same color is stable in that direction and could potentially be a stable piece itself
(defun gen-candidates (witnesses board stab-full)
    (loop for idx in witnesses with ndx = nil
        do (setf ndx (- idx 1)) ; go left direction = 2 (row)
        when (and (in-bounds idx ndx) (equal (aref board idx) (aref board ndx)))
            do (setf (aref stab-full ndx 2) t) and collect ndx
    
        do (setf ndx (+ idx 1)) ; go right direction = 2 (row)
        when (and (in-bounds idx ndx) (equal (aref board idx) (aref board ndx)))
            do (setf (aref stab-full ndx 2) t) and collect ndx

        do (setf ndx (- idx *size*)) ; go up direction = 3 (col)
        when (and (in-bounds idx ndx) (equal (aref board idx) (aref board ndx)))
            do (setf (aref stab-full ndx 3) t) and collect ndx

        do (setf ndx (+ idx *size*)) ; go up direction = 3 (col)
        when (and (in-bounds idx ndx) (equal (aref board idx) (aref board ndx)))
            do (setf (aref stab-full ndx 3) t) and collect ndx

        do (setf ndx (- idx *size* 1)) ; go NW direction = 0 (bak)
        when (and (in-bounds idx ndx) (equal (aref board idx) (aref board ndx)))
            do (setf (aref stab-full ndx 0) t) and collect ndx
        
        do (setf ndx (+ idx *size* 1)) ; go SE direction = 0 (bak)
        when (and (in-bounds idx ndx) (equal (aref board idx) (aref board ndx)))
            do (setf (aref stab-full ndx 0) t) and collect ndx

        do (setf ndx (- idx *size* -1)) ; go NE direction = 1 (for)
        when (and (in-bounds idx ndx) (equal (aref board idx) (aref board ndx)))
            do (setf (aref stab-full ndx 1) t) and collect ndx

        do (setf ndx (+ idx *size* -1)) ; go SW direction = 1 (for)
        when (and (in-bounds idx ndx) (equal (aref board idx) (aref board ndx)))
            do (setf (aref stab-full ndx 1) t) and collect ndx
    )
)

(defun in-bounds (position next-pos)
    (cond 
        ((< next-pos 0) nil)
        ((> next-pos (* *SIZE* *SIZE*)) nil)
        ((and (= (1- *SIZE*) (mod position *SIZE*)) (= 0 (mod next-pos *SIZE*))) nil)
        ((and (= 0 (mod position *SIZE*)) (= (1- *SIZE*) (mod next-pos *SIZE*))) nil)
        (t t)
    )
)


(defun fancy-eval-state (state) 
    (+ (weighted-eval-state state) 
        (* 20.0 (mobility state))
        ;(progn (print (stability state)) 0.0)
    )
)

(defun centerdist (pos)
    (+ 
        (abs (- (mod pos *size*) (/ (1- *size*) 2.0)))
        (abs (- (floor pos *size*) (/ (1- *size*) 2.0)))
    )
)