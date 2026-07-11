;;; takuzu-solver.el --- Solver and difficulty grader for Takuzu -*- lexical-binding: t -*-

;; Author: Craig Jennings <craigmartinjennings@gmail.com>
;; Keywords: games

;;; Commentary:
;; Two jobs, kept separate.
;;
;; The search (`takuzu-solve', `takuzu-count-solutions', `takuzu-unique-p') is a
;; backtracking depth-first fill with constraint propagation, used to find a
;; solution and to prove a puzzle has exactly one.  `takuzu-count-solutions'
;; short-circuits at its LIMIT (default 2), which is all a uniqueness check needs.
;;
;; The grader (`takuzu-grade') solves with escalating human techniques and reports
;; the strongest one it needed: naked-single propagation alone is `easy'; needing
;; depth-1 hypothesis (try a color, refute by contradiction) is `medium'; when
;; those stall it is `hard'.

;;; Code:

(require 'cl-lib)
(require 'takuzu-board)

(defvar takuzu-solve-randomize nil
  "When non-nil, the search tries the two colors in random order at each branch.
Used by the generator to draw a random full solution.")

(defun takuzu--branch-values ()
  "The order to try the two colors at a branch, randomized when enabled."
  (if (and takuzu-solve-randomize (= 0 (random 2))) '(1 0) '(0 1)))

(defun takuzu--line-unique-among-p (line lines)
  "Non-nil if the complete LINE is not duplicated among complete lines of LINES."
  (= 1 (cl-count-if (lambda (l)
                      (and (takuzu--line-complete-p l) (equal l line)))
                    lines)))

(defun takuzu--placement-ok-p (board row col)
  "Non-nil if the value now at ROW,COL keeps its row and column legal."
  (let* ((n (takuzu-board-size board))
         (r (takuzu-board-row board row))
         (c (takuzu-board-col board col)))
    (and (not (takuzu--line-has-triple-p r))
         (takuzu--line-count-legal-p r n)
         (not (takuzu--line-has-triple-p c))
         (takuzu--line-count-legal-p c n)
         (or (not (takuzu--line-complete-p r))
             (takuzu--line-unique-among-p r (takuzu-board-rows board)))
         (or (not (takuzu--line-complete-p c))
             (takuzu--line-unique-among-p c (takuzu-board-cols board))))))

(defun takuzu--legal-values (board row col)
  "The values in (0 1) legal at the empty cell ROW,COL of BOARD.
BOARD is left unchanged."
  (let (vals)
    (dolist (v '(1 0))
      (takuzu-board-set board row col v)
      (when (takuzu--placement-ok-p board row col)
        (push v vals))
      (takuzu-board-set board row col nil))
    vals))

(defun takuzu--propagate (board)
  "Fill naked-single forced cells in BOARD in place until fixpoint.
Return \\='contradiction if an empty cell has no legal value, else the number of
cells filled."
  (let ((n (takuzu-board-size board))
        (filled 0) (changed t) (dead nil))
    (while (and changed (not dead))
      (setq changed nil)
      (cl-block scan
        (dotimes (row n)
          (dotimes (col n)
            (when (null (takuzu-board-ref board row col))
              (let ((vals (takuzu--legal-values board row col)))
                (cond
                 ((null vals) (setq dead t) (cl-return-from scan))
                 ((null (cdr vals))
                  (takuzu-board-set board row col (car vals))
                  (setq filled (1+ filled) changed t)))))))))
    (if dead 'contradiction filled)))

(defun takuzu--solutions (board limit)
  "Up to LIMIT solved-board clones reachable from BOARD.
BOARD is mutated during the search and restored to its entry state before
returning."
  (if (or (<= limit 0) (not (takuzu-board-legal-p board)))
      nil
    (let* ((cells (takuzu-board-cells board))
           (snapshot (copy-sequence cells))
           (res nil))
      (if (eq (takuzu--propagate board) 'contradiction)
          (setq res nil)
        (let ((idx (cl-position nil cells)))
          (if (null idx)
              (setq res (when (takuzu-board-solved-p board)
                          (list (takuzu-board-clone board))))
            (let ((n (takuzu-board-size board)))
              (cl-block branch
                (dolist (v (takuzu--branch-values))
                  (aset cells idx v)
                  (when (takuzu--placement-ok-p board (/ idx n) (mod idx n))
                    (setq res (append res
                                      (takuzu--solutions
                                       board (- limit (length res)))))
                    (when (>= (length res) limit)
                      (cl-return-from branch)))
                  (aset cells idx nil)))))))
      (dotimes (i (length snapshot)) (aset cells i (aref snapshot i)))
      res)))

(defun takuzu-count-solutions (board &optional limit)
  "Number of solutions of BOARD, capped at LIMIT (default 2).  BOARD is unchanged."
  (length (takuzu--solutions (takuzu-board-clone board) (or limit 2))))

(defun takuzu-solve (board)
  "Return one solved board reachable from BOARD, or nil.  BOARD is unchanged."
  (car (takuzu--solutions (takuzu-board-clone board) 1)))

(defun takuzu-unique-p (board)
  "Non-nil if BOARD has exactly one solution."
  (= 1 (takuzu-count-solutions board 2)))

;; --- difficulty grader ---

(defun takuzu--forced-by-hypothesis (board row col)
  "The value forced at empty ROW,COL by depth-1 hypothesis, or nil if none.
Return \\='contradiction when neither color survives propagation."
  (let (survivors)
    (dolist (v '(0 1))
      (let ((trial (takuzu-board-clone board)))
        (takuzu-board-set trial row col v)
        (when (and (takuzu--placement-ok-p trial row col)
                   (not (eq (takuzu--propagate trial) 'contradiction)))
          (push v survivors))))
    (cond ((null survivors) 'contradiction)
          ((null (cdr survivors)) (car survivors))
          (t nil))))

(defun takuzu--hypothesis-step (board)
  "Fill one cell of BOARD by hypothesis and propagate.  Return t if it progressed."
  (let ((n (takuzu-board-size board)) (moved nil))
    (cl-block hp
      (dotimes (r n)
        (dotimes (c n)
          (when (null (takuzu-board-ref board r c))
            (let ((f (takuzu--forced-by-hypothesis board r c)))
              (cond
               ((eq f 'contradiction) (cl-return-from hp))
               (f (takuzu-board-set board r c f)
                  (takuzu--propagate board)
                  (setq moved t)
                  (cl-return-from hp))))))))
    moved))

(defun takuzu-grade (board)
  "Grade puzzle BOARD as \\='easy, \\='medium, or \\='hard.
easy: naked-single propagation solves it.  medium: depth-1 hypothesis is needed.
hard: neither fully solves it."
  (let ((work (takuzu-board-clone board)))
    (takuzu--propagate work)
    (if (takuzu-board-full-p work)
        'easy
      (let ((progress t))
        (while (and progress (not (takuzu-board-full-p work)))
          (setq progress (takuzu--hypothesis-step work)))
        (if (takuzu-board-full-p work) 'medium 'hard)))))

(provide 'takuzu-solver)
;;; takuzu-solver.el ends here
