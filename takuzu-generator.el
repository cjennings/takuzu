;;; takuzu-generator.el --- Puzzle generator for Takuzu -*- lexical-binding: t -*-

;; Author: Craig Jennings <craigmartinjennings@gmail.com>
;; Keywords: games

;;; Commentary:
;; Generate a uniquely-solvable puzzle: draw a random full solution, then carve
;; cells away in random order, keeping a removal only while the puzzle stays both
;; uniquely solvable and within the requested difficulty tier.  This is the hybrid
;; model in one pass: the tier bounds how far the carve may go (easy stops while
;; propagation alone still solves it; hard carves to a minimal clue set), so clue
;; count falls out of the tier rather than needing a second add-back phase.

;;; Code:

(require 'cl-lib)
(require 'takuzu-board)
(require 'takuzu-solver)

(defun takuzu--shuffle (seq)
  "Return the elements of SEQ in random order, as a list."
  (let ((v (vconcat seq)))
    (cl-loop for i from (1- (length v)) downto 1 do
             (let ((j (random (1+ i)))
                   (tmp (aref v i)))
               (aset v i (aref v j))
               (aset v j tmp)))
    (append v nil)))

(defun takuzu--full-puzzle (solution)
  "A board equal to SOLUTION with every cell marked as a given."
  (let ((n (takuzu-board-size solution)))
    (takuzu-make-board n
                       (copy-sequence (takuzu-board-cells solution))
                       (make-vector (* n n) t))))

(defun takuzu--propagation-solves-p (board)
  "Non-nil if naked-single propagation alone completes BOARD (an easy puzzle)."
  (let ((w (takuzu-board-clone board)))
    (takuzu--propagate w)
    (takuzu-board-full-p w)))

(defun takuzu--carve-keep-p (board tier)
  "Non-nil if BOARD is still an acceptable puzzle for TIER after a removal.
For \\='easy, propagation alone must still solve it (which also proves it
unique).  For \\='medium, it must be unique and grade no harder than medium.
For \\='hard or nil, uniqueness alone bounds it (a minimal clue set)."
  (pcase tier
    ('easy (takuzu--propagation-solves-p board))
    ('medium (and (takuzu-unique-p board)
                  (memq (takuzu-grade board) '(easy medium))))
    (_ (takuzu-unique-p board))))

(defun takuzu--carve (board tier)
  "Carve clues from full BOARD, keeping each removal that suits TIER.
Cells are cleared in random order and a removal is kept only while
`takuzu--carve-keep-p' holds for TIER.  BOARD is mutated in place and returned."
  (let ((n (takuzu-board-size board)))
    (dolist (idx (takuzu--shuffle (number-sequence 0 (1- (* n n)))))
      (let* ((row (/ idx n))
             (col (mod idx n))
             (val (takuzu-board-ref board row col)))
        (when val
          (takuzu-board-set board row col nil)
          (aset (takuzu-board-givens board) idx nil)
          (unless (takuzu--carve-keep-p board tier)
            (takuzu-board-set board row col val)
            (aset (takuzu-board-givens board) idx t)))))
    board))

(defun takuzu-generate (size &optional difficulty)
  "Generate a uniquely-solvable Takuzu puzzle SIZE cells on a side.
Return a plist (:board B :solution S :grade G).  DIFFICULTY, when \\='easy,
\\='medium, or \\='hard, bounds how far the carve removes clues so the puzzle
grades near that tier; the reported :grade is the puzzle's actual grade."
  (let* ((takuzu-solve-randomize t)
         (solution (takuzu-solve (takuzu-make-board size)))
         (board (takuzu--carve (takuzu--full-puzzle solution) difficulty)))
    (list :board board :solution solution :grade (takuzu-grade board))))

(defun takuzu--encode-result (result)
  "Encode generator RESULT into a flat, `read'-able plist.
Boards become their raw cell/given vectors so the whole result survives a
`prin1'/`read' round trip across a process boundary."
  (let ((board (plist-get result :board))
        (solution (plist-get result :solution)))
    (list :size (takuzu-board-size board)
          :cells (takuzu-board-cells board)
          :givens (takuzu-board-givens board)
          :solution (takuzu-board-cells solution)
          :grade (plist-get result :grade))))

(defun takuzu--decode-result (data)
  "Rebuild a generator result plist from encoded DATA.
See `takuzu--encode-result' for the wire form."
  (let ((size (plist-get data :size)))
    (list :board (takuzu-make-board size (plist-get data :cells) (plist-get data :givens))
          :solution (takuzu-make-board size (plist-get data :solution))
          :grade (plist-get data :grade))))

(provide 'takuzu-generator)
;;; takuzu-generator.el ends here
