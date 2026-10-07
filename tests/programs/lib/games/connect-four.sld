;;; Connect Four: the board, the rules, and an alpha-beta (negamax)
;;; search.  Used from Common Lisp by tests/programs/connect-four.lisp.
;;;
;;; A board is a vector of 42 cells, row 0 at the bottom: cell (column,
;;; row) is at row * 7 + column, and holds 0 (empty), 1 or 2.  Boards
;;; aren't changed; play returns a new one.  The search takes the
;;; evaluation of a position as an argument, so the caller -- Lisp, here
;;; -- can supply its own.

(define-library (games connect-four)
  (export columns rows make-board board? board-ref play legal-moves
          winner full? opponent play-moves lines evaluate
          choose-move search-count)
  (import (scheme base))
  (begin
    (define columns 7)
    (define rows 6)

    (define (make-board) (make-vector (* columns rows) 0))
    (define (board? x) (and (vector? x) (= (vector-length x) (* columns rows))))
    (define (board-ref board column row) (vector-ref board (+ (* row columns) column)))

    (define (opponent player) (- 3 player))

    ;; The row a piece dropped in COLUMN lands in, or #f if it's full
    (define (landing-row board column)
      (let loop ((row 0))
        (cond ((= row rows) #f)
              ((zero? (board-ref board column row)) row)
              (else (loop (+ row 1))))))

    (define (play board column player)
      (let ((row (and (<= 0 column (- columns 1)) (landing-row board column))))
        (unless row (error "illegal move" column))
        (let ((new (vector-copy board)))
          (vector-set! new (+ (* row columns) column) player)
          new)))

    ;; Center columns first: alpha-beta prunes more when the likely
    ;; best moves come first.
    (define move-order '(3 2 4 1 5 0 6))

    (define (legal-moves board)
      (let loop ((order move-order) (moves '()))
        (cond ((null? order) (reverse moves))
              ((landing-row board (car order)) (loop (cdr order) (cons (car order) moves)))
              (else (loop (cdr order) moves)))))

    (define (full? board) (null? (legal-moves board)))

    ;; Every line of four cells, as a list of indexes
    (define lines
      (let ((result '()))
        (do ((row 0 (+ row 1))) ((= row rows) (reverse result))
          (do ((column 0 (+ column 1))) ((= column columns))
            (for-each
             (lambda (direction)
               (let ((dc (car direction)) (dr (cdr direction)))
                 (when (and (<= 0 (+ column (* 3 dc)) (- columns 1))
                            (<= 0 (+ row (* 3 dr)) (- rows 1)))
                   (set! result
                         (cons (map (lambda (i) (+ (* (+ row (* i dr)) columns) (+ column (* i dc))))
                                    '(0 1 2 3))
                               result)))))
             '((1 . 0) (0 . 1) (1 . 1) (1 . -1)))))))

    ;; 1 or 2 if that player has four in a line, else 0
    (define (winner board)
      (let loop ((lines lines))
        (if (null? lines)
            0
            (let ((first (vector-ref board (caar lines))))
              (if (and (not (zero? first))
                       (let all ((cells (cdar lines)))
                         (or (null? cells)
                             (and (= (vector-ref board (car cells)) first) (all (cdr cells))))))
                  first
                  (loop (cdr lines)))))))

    (define (play-moves moves)
      (let loop ((board (make-board)) (moves moves) (player 1))
        (if (null? moves)
            board
            (loop (play board (car moves) player) (cdr moves) (opponent player)))))

    ;; The default evaluation, from PLAYER's side: lines that only one
    ;; player has pieces in, weighted by how many, and the center column.
    (define (evaluate board player)
      (define (line-score line)
        (let loop ((cells line) (mine 0) (theirs 0))
          (if (null? cells)
              (cond ((and (> mine 0) (> theirs 0)) 0)
                    ((= mine 3) 5) ((= mine 2) 2)
                    ((= theirs 3) -5) ((= theirs 2) -2)
                    (else 0))
              (let ((cell (vector-ref board (car cells))))
                (cond ((= cell player) (loop (cdr cells) (+ mine 1) theirs))
                      ((zero? cell) (loop (cdr cells) mine theirs))
                      (else (loop (cdr cells) mine (+ theirs 1))))))))
      (let loop ((lines lines) (score 0))
        (if (null? lines)
            (+ score
               (* 3 (let count ((row 0) (n 0))
                      (if (= row rows)
                          n
                          (count (+ row 1)
                                 (+ n (let ((cell (board-ref board 3 row)))
                                        (cond ((= cell player) 1) ((zero? cell) 0) (else -1)))))))))
            (loop (cdr lines) (+ score (line-score (car lines)))))))

    (define win-score 1000000)

    ;; Positions the last search evaluated, for the curious
    (define searched 0)
    (define (search-count) searched)

    ;; Negamax with alpha-beta: the value of BOARD for PLAYER, to move.
    (define (negamax board player depth alpha beta evaluate)
      (set! searched (+ searched 1))
      (let ((w (winner board)))
        (cond ((= w (opponent player)) (- (+ win-score depth))) ; sooner losses are worse
              ((or (= depth 0) (full? board)) (evaluate board player))
              (else
               ;; call/cc to leave the loop on a cutoff
               (call-with-current-continuation
                (lambda (return)
                  (let loop ((moves (legal-moves board)) (alpha alpha))
                    (if (null? moves)
                        alpha
                        (let ((value (- (negamax (play board (car moves) player) (opponent player)
                                                 (- depth 1) (- beta) (- alpha) evaluate))))
                          (when (>= value beta) (return value))
                          (loop (cdr moves) (max alpha value)))))))))))

    ;; The best move for PLAYER, searching DEPTH plies, with EVALUATE (or
    ;; the default).  Ties go to the earlier move in move-order.
    (define (choose-move board player depth . options)
      (let ((evaluate (if (pair? options) (car options) evaluate)))
        (set! searched 0)
        (let loop ((moves (legal-moves board)) (best #f) (best-value (- (* 2 win-score))))
          (if (null? moves)
              best
              (let ((value (- (negamax (play board (car moves) player) (opponent player)
                                       (- depth 1) (- (* 2 win-score)) (- best-value) evaluate))))
                (if (> value best-value)
                    (loop (cdr moves) (car moves) value)
                    (loop (cdr moves) best best-value)))))))))
