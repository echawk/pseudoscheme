;;;; A small game: Connect Four, with the rules and the search in Scheme
;;;; (tests/programs/lib/games/connect-four.sld) and the game itself in
;;;; Common Lisp: CLOS players, a game loop, a seeded Mersenne Twister
;;;; (mt19937) for the random player, a board drawn with cl-ansi-text's
;;;; colors, and an evaluation function written in Lisp that the Scheme
;;;; search calls at every leaf.
;;;;
;;;;   sbcl --dynamic-space-size 4GB --control-stack-size 500MB \
;;;;        --script tests/programs/connect-four.lisp

(load (merge-pathnames "harness.lisp" (or *load-truename* *load-pathname*)))

(program-tests:quickload :mt19937 :cl-ansi-text)

(defpackage "CONNECT-FOUR" (:use "COMMON-LISP" "PROGRAM-TESTS"))
(in-package "CONNECT-FOUR")

;;; The Scheme library's procedures become Lisp functions in this
;;; package; LINES, a Scheme variable, a symbol macro.
(r7rs:import (prefix (games connect-four) c4-))

;;; ------------------------------------------------------------------
;;; Players

(defclass player ()
  ((piece :initarg :piece :reader piece)))

(defgeneric choose (player board)
  (:documentation "The column PLAYER plays on BOARD, a Scheme vector."))

(defclass scripted-player (player)
  ((moves :initarg :moves :accessor moves)))

(defmethod choose ((p scripted-player) board)
  (declare (ignore board))
  (pop (moves p)))

(defclass random-player (player)
  ((state :initarg :state :reader state)))

(defmethod choose ((p random-player) board)
  (let ((moves (c4-legal-moves board)))
    (nth (mt19937:random (length moves) (state p)) moves)))

(defclass searching-player (player)
  ((depth :initarg :depth :reader depth)
   (evaluation :initarg :evaluation :initform nil :reader evaluation)))

(defmethod choose ((p searching-player) board)
  (if (evaluation p)
      (c4-choose-move board (piece p) (depth p) (evaluation p))
      (c4-choose-move board (piece p) (depth p))))

;;; ------------------------------------------------------------------
;;; The game loop

(defun play-game (player-1 player-2 &key (start (c4-make-board)))
  "Play to the end: the winner (1 or 2, or 0 for a draw), the final
board, and the moves."
  (loop with board = start
	for player = player-1 then (if (eq player player-1) player-2 player-1)
	for move = (choose player board)
	do (setf board (c4-play board move (piece player)))
	collect move into moves
	until (or (plusp (c4-winner board)) (c4-full? board))
	finally (return (values (c4-winner board) board moves))))

;;; ------------------------------------------------------------------
;;; Drawing: the board is a Scheme vector, which is a Lisp simple-vector

(defun draw (board &key color)
  (with-output-to-string (out)
    (loop for row from (1- c4-rows) downto 0
	  do (loop for column below c4-columns
		   for cell = (svref board (+ (* row c4-columns) column))
		   do (write-string (case cell
				      (0 ".")
				      (1 (if color (cl-ansi-text:red "X") "X"))
				      (2 (if color (cl-ansi-text:yellow "O") "O")))
				    out))
	     (terpri out))
    (write-string "0123456" out)))

;;; ------------------------------------------------------------------
;;; An evaluation in Lisp, the same as the library's: lines held by one
;;; player only, weighted, and the center column.  The search calls it
;;; with a Scheme vector and the player to move.

(defun lisp-evaluate (board player)
  (+ (loop for line in c4-lines	; Scheme's list of lists, read in Lisp
	   sum (loop for i in line
		     for cell = (svref board i)
		     count (= cell player) into mine
		     count (and (/= cell 0) (/= cell player)) into theirs
		     finally (return (cond ((and (plusp mine) (plusp theirs)) 0)
					   ((= mine 3) 5) ((= mine 2) 2)
					   ((= theirs 3) -5) ((= theirs 2) -2)
					   (t 0)))))
     (* 3 (loop for row below c4-rows
		for cell = (svref board (+ (* row c4-columns) 3))
		sum (cond ((= cell player) 1) ((= cell 0) 0) (t -1))))))

;;; ------------------------------------------------------------------

(test-equal "69 lines of four" 69 (length c4-lines))
(test-equal "an empty board has every move, center first" '(3 2 4 1 5 0 6)
	    (c4-legal-moves (c4-make-board)))
(test-assert "a board is a simple-vector" (typep (c4-make-board) '(simple-vector 42)))

(test-equal "drawing a position"
	    (format nil ".......~%.......~%.......~%...O...~%..OO...~%.XXX...~%0123456")
	    (draw (c4-play-moves '(3 3 2 2 1 3))))

(test-assert "drawn in color with cl-ansi-text"
	     (let ((text (draw (c4-play-moves '(3)) :color t)))
	       (and (search (string #\Esc) text) (search "X" text))))

(test-equal "a vertical win" 1 (c4-winner (c4-play-moves '(0 1 0 1 0 1 0))))
(test-equal "a diagonal win" 1 (c4-winner (c4-play-moves '(0 1 1 2 2 3 2 3 3 5 3))))
(test-equal "no winner yet" 0 (c4-winner (c4-play-moves '(0 1 2 3))))
(test-error "a full column is an illegal move, a Scheme error in Lisp"
	    (c4-play (c4-play-moves '(0 0 0 0 0 0)) 0 1))

;; X to move with three in the bottom row: the search takes the win
(test-equal "the search takes an immediate win" 3
	    (c4-choose-move (c4-play-moves '(0 0 1 1 2 2)) 1 4))
;; O to move, X threatens 3: the search blocks
(test-equal "the search blocks a threat" 3
	    (c4-choose-move (c4-play-moves '(0 6 1 6 2)) 2 4))
(test-assert "and searched a few hundred positions to find it"
	     (< 50 (c4-search-count) 5000))

;; The Scheme search calling Lisp's evaluation at its leaves
(defparameter *positions*
  (mapcar #'c4-play-moves '(() (3) (3 3) (3 2 4) (3 3 3 2 4 4) (0 1 2 3 4 5 6 3 3))))

(test-equal "the Lisp evaluation agrees with Scheme's"
	    (mapcar (lambda (b) (list (c4-evaluate b 1) (c4-evaluate b 2))) *positions*)
	    (mapcar (lambda (b) (list (lisp-evaluate b 1) (lisp-evaluate b 2))) *positions*))
(test-equal "so the search chooses the same moves with either"
	    (mapcar (lambda (b) (c4-choose-move b 1 4)) *positions*)
	    (mapcar (lambda (b) (c4-choose-move b 1 4 #'lisp-evaluate)) *positions*))
(test-equal "a Lisp closure as the evaluation: always the first move considered"
	    '(3 3 3)
	    (let ((calls 0))
	      (prog1 (mapcar (lambda (b) (c4-choose-move b 1 2 (lambda (board player)
								   (declare (ignore board player))
								   (incf calls)
								   0)))
			     (subseq *positions* 0 3))
		(assert (plusp calls)))))

;; Whole games
(test-equal "a scripted game: X wins on the diagonal" '(1 (0 1 1 2 2 3 2 3 3 5 3))
	    (multiple-value-bind (winner board moves)
		(play-game (make-instance 'scripted-player :piece 1 :moves (list 0 1 2 2 3 3))
			   (make-instance 'scripted-player :piece 2 :moves (list 1 2 3 3 5)))
	      (declare (ignore board))
	      (list winner moves)))

(test-equal "the search beats a seeded random player, five games out of five" '(1 1 1 1 1)
	    (loop for seed from 1 to 5
		  collect (play-game (make-instance 'searching-player :piece 1 :depth 4)
				     (make-instance 'random-player :piece 2
							:state (mt19937::make-random-object
								:state (mt19937:init-random-state seed))))))

(test-assert "search against search: a legal game to the end"
	     (multiple-value-bind (winner board moves)
		 (play-game (make-instance 'searching-player :piece 1 :depth 3)
			    (make-instance 'searching-player :piece 2 :depth 3 :evaluation #'lisp-evaluate))
	       (and (member winner '(0 1 2))
		    ;; replaying the moves in Scheme gives the same board
		    (equalp board (c4-play-moves moves))
		    (or (plusp winner) (c4-full? board)))))

(finish)
