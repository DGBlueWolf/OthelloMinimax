(load 'minimax)

(defun generate-successors (state) 
    (cond 
        ((equal state 'a) '(b c))
        ((equal state 'b) '(d e))
        ((equal state 'c) '(f g))
        ((equal state 'd) '(x0 x1))
        ((equal state 'e) '(x2 x3))
        ((equal state 'f) '(x4 x5))
        ((equal state 'g) '(x6 x7))
        (t nil)
    )
)

(defun eval-state (state) 
    (cond 
        ((equal state 'x0)  3.0)
        ((equal state 'x1)  5.0)
        ((equal state 'x2)  6.0)
        ((equal state 'x3)  9.0)
        ((equal state 'x4)  1.0)
        ((equal state 'x5)  2.0)
        ((equal state 'x6)  0.0)
        ((equal state 'x7) -1.0)
        (t 0.0)
    )
)

(defun end-condition (state) (not (generate-successors state)))

(setf root (minimax 'a t 'generate-successors 'eval-state 'end-condition 3))
(setf child1 (node-best-child root))
(setf child2 (node-best-child child1))
(setf child3 (node-best-child child2))
(format t "Player: ~:[Min~;Max~] Ply: ~A ::: State: ~A Rank: ~13@A Alpha: ~13@A Beta: ~13@A Child: ~:[No~;Yes~]~%" 
    t
    3
    (node-state root)
    (node-rank root)
    (node-alpha root) 
    (node-beta root)
    (when (node-best-child root) t)
)
(format t "Player: ~:[Min~;Max~] Ply: ~A ::: State: ~A Rank: ~13@A Alpha: ~13@A Beta: ~13@A Child: ~:[No~;Yes~]~%" 
    nil
    2
    (node-state child1)
    (node-rank child1)
    (node-alpha child1) 
    (node-beta child1)
    (when (node-best-child child1) t)
)
(format t "Player: ~:[Min~;Max~] Ply: ~A ::: State: ~A Rank: ~13@A Alpha: ~13@A Beta: ~13@A Child: ~:[No~;Yes~]~%" 
    t
    1
    (node-state child2)
    (node-rank child2)
    (node-alpha child2) 
    (node-beta child2)
    (when (node-best-child child2) t)
)
(format t "Player: ~:[Min~;Max~] Ply: ~A ::: State: ~A Rank: ~13@A Alpha: ~13@A Beta: ~13@A Child: ~:[No~;Yes~]~%" 
    nil
    0
    (node-state child3)
    (node-rank child3)
    (node-alpha child3) 
    (node-beta child3)
    (when (node-best-child child3) t)
)
