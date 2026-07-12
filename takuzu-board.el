;;; takuzu-board.el --- Board representation and rules for Takuzu -*- lexical-binding: t -*-

;; Author: Craig Jennings <craigmartinjennings@gmail.com>
;; Keywords: games
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; The board data structure and the three Takuzu rules as pure predicates.
;; A board is an even n-by-n grid; each cell is nil (empty), 0, or 1.  The two
;; on-screen colors map to 0 and 1 in the UI only; this layer is color-agnostic.
;;
;; Rules:
;;   1. no three equal cells adjacent in a row or column (triple-free);
;;   2. each row and column holds equal counts of 0 and 1;
;;   3. no two rows and no two columns are identical.

;;; Code:

(require 'cl-lib)
(require 'seq)

(cl-defstruct (takuzu-board (:constructor takuzu-board--new)
                            (:copier takuzu-board-copy))
  "A Takuzu board: SIZE-by-SIZE grid of CELLS with a GIVENS lock mask.
CELLS and GIVENS are row-major vectors of length SIZE*SIZE.  Each cell is
nil, 0, or 1; each given is non-nil when its cell is a locked clue."
  size cells givens)

(defun takuzu-make-board (size &optional cells givens)
  "Create a SIZE-by-SIZE board.
CELLS and GIVENS, when given, are sequences of length SIZE*SIZE: CELLS holds
nil/0/1, GIVENS holds non-nil for locked cells.  Both default to all-nil."
  (let ((n2 (* size size)))
    (takuzu-board--new
     :size size
     :cells (if cells (vconcat cells) (make-vector n2 nil))
     :givens (if givens (vconcat givens) (make-vector n2 nil)))))

(defun takuzu-board-clone (board)
  "Return a deep copy of BOARD, with its own CELLS and GIVENS vectors."
  (takuzu-board--new
   :size (takuzu-board-size board)
   :cells (copy-sequence (takuzu-board-cells board))
   :givens (copy-sequence (takuzu-board-givens board))))

(defsubst takuzu--index (size row col)
  "Row-major index of ROW, COL in a grid SIZE cells wide."
  (+ (* row size) col))

(defun takuzu-board-ref (board row col)
  "Value at ROW, COL of BOARD (nil, 0, or 1)."
  (aref (takuzu-board-cells board)
        (takuzu--index (takuzu-board-size board) row col)))

(defun takuzu-board-set (board row col val)
  "Set ROW, COL of BOARD to VAL and return VAL."
  (aset (takuzu-board-cells board)
        (takuzu--index (takuzu-board-size board) row col)
        val)
  val)

(defun takuzu-board-given-p (board row col)
  "Non-nil if ROW, COL of BOARD is a locked given."
  (aref (takuzu-board-givens board)
        (takuzu--index (takuzu-board-size board) row col)))

(defun takuzu-board-row (board r)
  "List of the values in row R of BOARD."
  (let ((n (takuzu-board-size board)))
    (cl-loop for c from 0 below n collect (takuzu-board-ref board r c))))

(defun takuzu-board-col (board c)
  "List of the values in column C of BOARD."
  (let ((n (takuzu-board-size board)))
    (cl-loop for r from 0 below n collect (takuzu-board-ref board r c))))

(defun takuzu-board-rows (board)
  "List of all rows of BOARD, top to bottom."
  (cl-loop for r from 0 below (takuzu-board-size board)
           collect (takuzu-board-row board r)))

(defun takuzu-board-cols (board)
  "List of all columns of BOARD, left to right."
  (cl-loop for c from 0 below (takuzu-board-size board)
           collect (takuzu-board-col board c)))

(defun takuzu-board-full-p (board)
  "Non-nil if BOARD has no empty cells."
  (not (seq-some #'null (takuzu-board-cells board))))

;; --- line rules ---

(defun takuzu--line-has-triple-p (line)
  "Non-nil if LINE has three equal, non-nil values in consecutive positions."
  (let* ((v (vconcat line))
         (found nil))
    (dotimes (i (max 0 (- (length v) 2)))
      (let ((a (aref v i)))
        (when (and a (eql a (aref v (1+ i))) (eql a (aref v (+ i 2))))
          (setq found t))))
    found))

(defun takuzu--line-count-legal-p (line size)
  "Non-nil if neither color in LINE exceeds SIZE/2.  Nils are ignored."
  (let ((half (/ size 2)))
    (and (<= (cl-count 0 line) half)
         (<= (cl-count 1 line) half))))

(defun takuzu--line-complete-p (line)
  "Non-nil if LINE has no empty cells."
  (not (memq nil line)))

(defun takuzu--line-complete-valid-p (line size)
  "Non-nil if LINE of width SIZE is complete, evenly split, and triple-free."
  (and (takuzu--line-complete-p line)
       (= (cl-count 0 line) (/ size 2))
       (not (takuzu--line-has-triple-p line))))

;; --- board rules ---

(defun takuzu--all-lines-legal-p (lines size)
  "Non-nil if every line in LINES is triple-free and count-legal for SIZE."
  (cl-every (lambda (l)
              (and (not (takuzu--line-has-triple-p l))
                   (takuzu--line-count-legal-p l size)))
            lines))

(defun takuzu--no-dup-complete-lines-p (lines)
  "Non-nil if no two COMPLETE lines in LINES are identical."
  (let ((complete (cl-remove-if-not #'takuzu--line-complete-p lines)))
    (= (length complete)
       (length (cl-remove-duplicates complete :test #'equal)))))

(defun takuzu-board-legal-p (board)
  "Non-nil if BOARD breaks no rule so far (partial legality).
Every row and column is triple-free and count-legal, and no two complete rows
or complete columns are identical."
  (let ((n (takuzu-board-size board))
        (rows (takuzu-board-rows board))
        (cols (takuzu-board-cols board)))
    (and (takuzu--all-lines-legal-p rows n)
         (takuzu--all-lines-legal-p cols n)
         (takuzu--no-dup-complete-lines-p rows)
         (takuzu--no-dup-complete-lines-p cols))))

(defun takuzu-board-solved-p (board)
  "Non-nil if BOARD is a complete, valid solution."
  (let ((n (takuzu-board-size board))
        (rows (takuzu-board-rows board))
        (cols (takuzu-board-cols board)))
    (and (takuzu-board-full-p board)
         (cl-every (lambda (l) (takuzu--line-complete-valid-p l n)) rows)
         (cl-every (lambda (l) (takuzu--line-complete-valid-p l n)) cols)
         (takuzu--no-dup-complete-lines-p rows)
         (takuzu--no-dup-complete-lines-p cols))))

(provide 'takuzu-board)
;;; takuzu-board.el ends here
