;;; gen-parity-fixtures.el --- Generate the engine parity fixture corpus -*- lexical-binding: t -*-

;; Author: Craig Jennings <craigmartinjennings@gmail.com>
;; Keywords: games
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Writes tests/fixtures/parity-cases.json: a corpus of boards with the
;; engine's own answers (legal, full, solved, unique, grade, forced cell,
;; solution) frozen alongside them.  The ERT suite replays the corpus
;; against this engine (test-takuzu-parity.el) and the HTML port replays it
;; against its JavaScript engine, so the two can't silently drift.
;;
;; Every expected value is computed by the engine at generation time, never
;; asserted by hand -- the hand-built part is only the board layouts.
;; Regenerate (rarely, and deliberately) with:
;;
;;   emacs -Q --batch -L . -L tests -l tests/gen-parity-fixtures.el

;;; Code:

(require 'json)
(require 'takuzu-generator)
(require 'takuzu-ui)

(defun takuzu-fix--board-string (cells)
  "Encode the CELLS vector as a row-major string of 0, 1, and dots."
  (mapconcat (lambda (v) (pcase v (0 "0") (1 "1") (_ "."))) cells ""))

(defun takuzu-fix--givens-string (givens)
  "Encode the GIVENS vector as a row-major string of g and dots."
  (mapconcat (lambda (v) (if v "g" ".")) givens ""))

(defun takuzu-fix--parse-cells (str)
  "Decode STR (0, 1, dots) into a cells vector."
  (vconcat (mapcar (lambda (ch) (pcase ch (?0 0) (?1 1) (_ nil))) str)))

(defun takuzu-fix--forced-cell (board)
  "The first forced cell of BOARD as a vector [ROW COL VALUE], or nil.
Calls the real `takuzu--forced-cell' through its buffer-local state."
  (with-temp-buffer
    (setq-local takuzu--size (takuzu-board-size board))
    (setq-local takuzu--board board)
    (let ((found (takuzu--forced-cell)))
      (and found (vconcat found)))))

(defun takuzu-fix--case (name board)
  "Freeze NAME + BOARD with every engine answer as a fixture plist."
  (let* ((unique (and (takuzu-unique-p board) t))
         (solution (and unique (takuzu-solve (takuzu-board-clone board)))))
    (list :name name
          :size (takuzu-board-size board)
          :cells (takuzu-fix--board-string (takuzu-board-cells board))
          :givens (takuzu-fix--givens-string (takuzu-board-givens board))
          :legal (if (takuzu-board-legal-p board) t :false)
          :full (if (takuzu-board-full-p board) t :false)
          :solved (if (takuzu-board-solved-p board) t :false)
          :unique (if unique t :false)
          :grade (if unique (symbol-name (takuzu-grade board)) :null)
          :forced (or (takuzu-fix--forced-cell board) :null)
          :solution (if solution
                        (takuzu-fix--board-string (takuzu-board-cells solution))
                      :null))))

(defun takuzu-fix--hand-board (name cells &optional givens)
  "A fixture case NAME from CELLS/GIVENS strings (side length is sqrt)."
  (let* ((n (truncate (sqrt (length cells))))
         (board (takuzu-make-board
                 n
                 (takuzu-fix--parse-cells cells)
                 (and givens (vconcat (mapcar (lambda (ch) (eq ch ?g)) givens))))))
    (takuzu-fix--case name board)))

(defun takuzu-fix--generate ()
  "Build the full corpus and write tests/fixtures/parity-cases.json."
  (let ((cases '()))
    ;; hand-built edges: layouts by hand, answers by engine
    (push (takuzu-fix--hand-board "empty-4" "................") cases)
    (push (takuzu-fix--hand-board "solved-4" "0110101001011001"
                                  "gggg............") cases)
    (push (takuzu-fix--hand-board "one-blank-4" "0110101001011.01") cases)
    (push (takuzu-fix--hand-board "full-invalid-triple-4" "0001101101101001") cases)
    (push (takuzu-fix--hand-board "full-invalid-duplines-4" "0110100101101001") cases)
    (push (takuzu-fix--hand-board "progress-4" "00..............") cases)
    (push (takuzu-fix--hand-board "halfrow-6"
                                  (concat "010101" (make-string 30 ?.))) cases)
    ;; generated puzzles across the size/difficulty matrix
    (dolist (size '(4 6 8 10 12))
      (dolist (diff '(easy medium hard))
        (let* ((result (takuzu-generate size diff))
               (board (plist-get result :board)))
          (push (takuzu-fix--case (format "gen-%d-%s" size diff) board)
                cases))))
    (let ((json-payload (list :cases (vconcat (nreverse cases)))))
      (make-directory "tests/fixtures" t)
      (with-temp-file "tests/fixtures/parity-cases.json"
        (insert (json-serialize json-payload)))
      (message "Wrote %d cases to tests/fixtures/parity-cases.json"
               (length (plist-get json-payload :cases))))))

(takuzu-fix--generate)

(provide 'gen-parity-fixtures)
;;; gen-parity-fixtures.el ends here
