;;; test-takuzu-generator.el --- Tests for takuzu-generator -*- lexical-binding: t -*-

;;; Commentary:
;; Generation invariants: every puzzle is uniquely solvable, its clues match the
;; seed solution, and the reported grade is valid.  Properties hold for any
;; random path, so no seeding is needed.

;;; Code:

(require 'ert)
(require 'takuzu-board)
(require 'takuzu-solver)
(require 'takuzu-generator)

(defun test-takuzu-gen--clues-match-solution (board solution)
  "Non-nil if every non-empty cell of BOARD equals SOLUTION and is a given."
  (let ((bc (takuzu-board-cells board))
        (sc (takuzu-board-cells solution))
        (gv (takuzu-board-givens board))
        (ok t))
    (dotimes (i (length bc))
      (when (aref bc i)
        (unless (and (equal (aref bc i) (aref sc i)) (aref gv i))
          (setq ok nil))))
    ok))

(ert-deftest test-takuzu-generate-unique ()
  "Normal: a generated puzzle is uniquely solvable and its solution is the seed."
  (let* ((g (takuzu-generate 4))
         (board (plist-get g :board))
         (solution (plist-get g :solution)))
    (should (= (takuzu-board-size board) 4))
    (should (takuzu-unique-p board))
    (should (takuzu-board-solved-p solution))
    (should (equal (takuzu-board-cells (takuzu-solve board))
                   (takuzu-board-cells solution)))))

(ert-deftest test-takuzu-generate-clues-subset ()
  "Boundary: some cells are blanked, and every clue matches the solution."
  (let* ((g (takuzu-generate 6))
         (board (plist-get g :board))
         (solution (plist-get g :solution)))
    (should (member nil (append (takuzu-board-cells board) nil)))
    (should (test-takuzu-gen--clues-match-solution board solution))))

(ert-deftest test-takuzu-generate-grade-symbol ()
  "Normal: the reported grade is a valid difficulty symbol."
  (should (memq (plist-get (takuzu-generate 4) :grade) '(easy medium hard))))

(ert-deftest test-takuzu-generate-easy-is-easy ()
  "Normal: requesting easy yields an easy-graded puzzle."
  (should (eq (plist-get (takuzu-generate 6 'easy) :grade) 'easy)))

(ert-deftest test-takuzu-encode-decode-round-trip ()
  "Normal: a result survives encode -> prin1 -> read -> decode unchanged.
This is the wire format the async generator ships from the child process."
  (let* ((g (takuzu-generate 6 'easy))
         (wire (read (prin1-to-string (takuzu--encode-result g))))
         (back (takuzu--decode-result wire))
         (b (plist-get g :board)) (rb (plist-get back :board)))
    (should (= (takuzu-board-size b) (takuzu-board-size rb)))
    (should (equal (takuzu-board-cells b) (takuzu-board-cells rb)))
    (should (equal (takuzu-board-givens b) (takuzu-board-givens rb)))
    (should (equal (takuzu-board-cells (plist-get g :solution))
                   (takuzu-board-cells (plist-get back :solution))))
    (should (eq (plist-get g :grade) (plist-get back :grade)))))

(provide 'test-takuzu-generator)
;;; test-takuzu-generator.el ends here
