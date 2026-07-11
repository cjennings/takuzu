;;; test-takuzu-solver.el --- Tests for takuzu-solver -*- lexical-binding: t -*-

;;; Commentary:
;; The uniqueness search and the difficulty grader.

;;; Code:

(require 'ert)
(require 'takuzu-board)
(require 'takuzu-solver)

(defconst test-takuzu-solver--solved-4
  (vector 0 0 1 1
          1 1 0 0
          1 0 0 1
          0 1 1 0)
  "A valid, complete 4x4 solution.")

(defun test-takuzu-solver--blank (cells idx)
  "Return a copy of CELLS with IDX set to nil."
  (let ((v (copy-sequence cells)))
    (aset v idx nil)
    v))

;; --- propagation ---

(ert-deftest test-takuzu-propagate-count-forces ()
  "Normal: two 0s in a size-4 row force the remaining cells to 1."
  (let ((b (takuzu-make-board 4)))
    (takuzu-board-set b 0 0 0)
    (takuzu-board-set b 0 1 0)
    (should (integerp (takuzu--propagate b)))
    (should (= (takuzu-board-ref b 0 2) 1))
    (should (= (takuzu-board-ref b 0 3) 1))))

(ert-deftest test-takuzu-propagate-contradiction ()
  "Error: a cell with no legal value reports a contradiction."
  ;; Row 0 = 0 0 _ 0 leaves cell (0,2) with no legal value (0 -> triple/over-count,
  ;; 1 -> row has three-plus of a color impossible either way).
  (let ((b (takuzu-make-board 4)))
    (takuzu-board-set b 0 0 0)
    (takuzu-board-set b 0 1 0)
    (takuzu-board-set b 0 3 0)
    (should (eq (takuzu--propagate b) 'contradiction))))

;; --- solve / count / unique ---

(ert-deftest test-takuzu-solve-empty ()
  "Normal: solving an empty board yields a valid solution."
  (let ((sol (takuzu-solve (takuzu-make-board 4))))
    (should sol)
    (should (takuzu-board-solved-p sol))))

(ert-deftest test-takuzu-count-empty-not-unique ()
  "Boundary: an empty 4x4 has more than one solution (capped at 2)."
  (should (= (takuzu-count-solutions (takuzu-make-board 4) 2) 2)))

(ert-deftest test-takuzu-count-full-is-one ()
  "Boundary: a completed valid board counts as exactly one solution."
  (should (= (takuzu-count-solutions
             (takuzu-make-board 4 test-takuzu-solver--solved-4))
            1)))

(ert-deftest test-takuzu-unique-one-blank ()
  "Normal: blanking a single cell leaves a unique solution recovering the original."
  (let* ((cells (test-takuzu-solver--blank test-takuzu-solver--solved-4 0))
         (b (takuzu-make-board 4 cells)))
    (should (takuzu-unique-p b))
    (should (equal (takuzu-board-cells (takuzu-solve b))
                   test-takuzu-solver--solved-4))))

(ert-deftest test-takuzu-solve-contradiction ()
  "Error: an illegal board (a triple among givens) has no solution."
  (let ((b (takuzu-make-board 4 (vector 0 0 0 1
                                        nil nil nil nil
                                        nil nil nil nil
                                        nil nil nil nil))))
    (should (null (takuzu-solve b)))
    (should (= (takuzu-count-solutions b 2) 0))))

(ert-deftest test-takuzu-solve-does-not-mutate ()
  "Normal: solving leaves the input board untouched."
  (let ((b (takuzu-make-board 4)))
    (takuzu-solve b)
    (should-not (takuzu-board-full-p b))))

;; --- grader ---

(ert-deftest test-takuzu-grade-symbol ()
  "Normal: grade returns one of the three difficulty symbols."
  (should (memq (takuzu-grade (takuzu-make-board 4)) '(easy medium hard))))

(ert-deftest test-takuzu-grade-one-blank-easy ()
  "Boundary: a one-blank puzzle is easy (naked-single propagation solves it)."
  (let ((b (takuzu-make-board
            4 (test-takuzu-solver--blank test-takuzu-solver--solved-4 0))))
    (should (eq (takuzu-grade b) 'easy))))

(ert-deftest test-takuzu-hypothesis-step-contradiction ()
  "Error: hypothesis-step gives up (returns nil) on a contradictory board.
The first empty cell sits in a row that already holds a triple, so neither
colour survives -- exercising the contradiction branch of the grader step."
  (let ((board (takuzu-make-board 4 (vector 0 0 0 nil  nil nil nil nil
                                            nil nil nil nil  nil nil nil nil))))
    (should (null (takuzu--hypothesis-step board)))))

(provide 'test-takuzu-solver)
;;; test-takuzu-solver.el ends here
