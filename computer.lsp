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

;================  Initializers  ====================

(defmacro forward-diagonal-loop (&key (idx 'idx) outer-let inner-let body ending) 
    (let ((i (gensym))
          (j (gensym)))
        `(let (,@outer-let)
            ; from top diagonally down to the left
            (loop for ,i from 0 below *size*
                do (let (,@inner-let)
                    (loop for ,j from 0 below (1+ ,i)
                        do (let ((,idx (+ ,i (* (1- *size*) ,j))))
                            ,@body
                        )
                        finally (progn ,@ending)
                    )
                )
            )
            ; from right digonally down to the bottom
            (loop for ,i from 1 below *size* 
                do (let (,@inner-let)
                    (loop for ,j from 0 below (- *size* ,i) 
                        do (let ((,idx (+ 7 (* ,i *size*) (* (1- *size*) ,j))))
                            ,@body
                        )
                        finally (progn ,@ending)
                    )
                )   
            )
        )
    )
)

(defmacro backward-diagonal-loop (&key (idx 'idx) outer-let inner-let body ending) 
    (let ((i (gensym))
          (j (gensym)))
        `(let (,@outer-let)
            ; iterate from left hand side diagonally down
            (loop for ,i from (1- *size*) downto 1
                do (let (,@inner-let)
                    (loop for ,j from 0 below (- *size* ,i)
                        do (let ((,idx (+ (* ,i *size*) (* (1+ *size*) ,j))))
                            ,@body
                        )
                        finally (progn ,@ending)
                    )
                )
            )
            ; iterate from the top diagonally down
            (loop for ,i from 0 below *size* 
                do (let (,@inner-let)
                    (loop for ,j from 0 below (- *size* ,i) 
                        do (let ((,idx (+ ,i (* (1+ *size*) ,j))))
                            ,@body
                        )
                        finally (progn ,@ending)
                    )
                )   
            )
        )
    )
)

(defmacro traverse-board (&key (idx 'idx) outer-let inner-let body ending)
    (let ((i (gensym))
          (j (gensym)))
        `(let (,@outer-let)
            ; from top diagonally down to the left
            (loop for ,i from 0 below *size*
                do (let (,@inner-let)
                    (loop for ,j from 0 below (1+ ,i)
                        do (let ((,idx (+ ,i (* (1- *size*) ,j))))
                            ,@body
                        )
                        finally (progn ,@ending)
                    )
                )
            )
            ; from right digonally down to the bottom
            (loop for ,i from 1 below *size* 
                do (let (,@inner-let)
                    (loop for ,j from 0 below (- *size* ,i) 
                        do (let ((,idx (+ 7 (* ,i *size*) (* (1- *size*) ,j))))
                            ,@body
                        )
                        finally (progn ,@ending)
                    )
                )   
            )
            ; iterate from left hand side diagonally down
            (loop for ,i from (1- *size*) downto 1
                do (let (,@inner-let)
                    (loop for ,j from 0 below (- *size* ,i)
                        do (let ((,idx (+ (* ,i *size*) (* (1+ *size*) ,j))))
                            ,@body
                        )
                        finally (progn ,@ending)
                    )
                )
            )
            ; iterate from the top diagonally down
            (loop for ,i from 0 below *size* 
                do (let (,@inner-let)
                    (loop for ,j from 0 below (- *size* ,i) 
                        do (let ((,idx (+ ,i (* (1+ *size*) ,j))))
                            ,@body
                        )
                        finally (progn ,@ending)
                    )
                )   
            )
            ; iterate over rows
            (loop for ,i from 0 below *size*
                do (let (,@inner-let)
                    (loop for ,j from 0 below *size*
                        do (let ((,idx (+ (* ,i *size*) ,j)))
                            ,@body
                        )
                        finally (progn ,@ending)
                    )
                )
            )
            ; iterate over cols
            (loop for ,i from 0 below *size*
                do (let (,@inner-let)
                    (loop for ,j from 0 below *size*
                        do (let ((,idx (+ (* ,j *size*) ,i)))
                            ,@body
                        )
                        finally (progn ,@ending)
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
(defvar *previous-player-mobility* 0)

;================  Game State  ======================

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

; ref: Othello Heuristics @Kartik Kukreja 
; https://kartikkukreja.wordpress.com/2013/03/30/heuristic-function-for-reversiothello/

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

(defun stability (state)
    (let (
            (stable (make-array (list (* *size* *size*))))
            (board (make-array (list (* *size* *size*)) :initial-contents (othello-state-board state)))
        )
        (loop for idx in (sort 
                (loop for i from 0 below (* *size* *size*) collecting i) 
                (lambda (a b) (> (centerdist a) (centerdist b)))) ; end sort
            do (print idx)
            when (and 
                    (not (equal (aref board idx) '-)) ;piece isn't blank
                    ; Finds connected groups of stable pieces
                    (loop for dir in '((0 1) (1 0) (1 1) (-1 1)) ; for each direction around it
                        do (print dir)
                        when (cond
                            ((< (- (mod idx *size*) (abs (car dir))) 0) (progn (print 'left) nil)) ; out of bounds left
                            ((< (- (floor idx *size*) (cadr dir)) 0) (progn (print 'top) nil)) ; out of bounds top
                            ((>= (+ (mod idx *size*) (abs (car dir))) *size*) (progn (print 'right) nil)) ; out of bounds right
                            ((>= (+ (floor idx *size*) (cadr dir)) *size*) (progn (print 'bottom) nil)) ; out of bounds bottom
                            ((equal (aref board idx) ; matching stable piece in negative direction
                                    (aref stable (- idx (car dir) (* *size* (cadr dir))))) (progn (print dir) nil))
                            ((equal (aref board idx) ; matching stable piece in positive direction
                                    (aref stable (+ idx (car dir) (* *size* (cadr dir))))) (progn (print dir) nil))
                            (t t)
                        ) do (return nil)
                        finally (return (progn (print idx) t))
                    ) 
            ; if the when condition is satisfied, the piece is stable add it to the stable list
            ; and add to the score, 1 or 'B and -1 for 'W
            ) do (progn (print "setting stable") (setf (aref stable idx) (aref board idx))) and sum (if (equal (aref board idx) 'B) 1 -1) 
        ) ; end loop
    )
)

(defun fancy-eval-state (state) 
    (+ (weighted-eval-state state) 
        (* 16.0 (mobility state))
        ;(* 8.0 (stability state))
    )
)

(defun centerdist (pos)
    (+ 
        (abs (- (mod pos *size*) (/ (1- *size*) 2.0)))
        (abs (- (floor pos *size*) (/ (1- *size*) 2.0)))
    )
)