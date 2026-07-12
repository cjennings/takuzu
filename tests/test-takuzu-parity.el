;;; test-takuzu-parity.el --- Replay the parity fixture corpus -*- lexical-binding: t -*-

;;; Commentary:
;; Replays tests/fixtures/parity-cases.json against the engine.  The corpus
;; froze the engine's own answers at generation time (see
;; gen-parity-fixtures.el); the HTML port replays the same file against its
;; JavaScript engine, so a failure here means the Elisp engine drifted from
;; the frozen contract, and a failure there means the two engines disagree.

;;; Code:

(require 'ert)
(require 'takuzu)

(defconst test-takuzu-parity--file
  (expand-file-name "fixtures/parity-cases.json"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Path to the frozen fixture corpus.")

(defun test-takuzu-parity--cases ()
  "The corpus as a list of case plists."
  (with-temp-buffer
    (insert-file-contents test-takuzu-parity--file)
    (append (plist-get (json-parse-buffer :object-type 'plist
                                          :false-object nil)
             :cases)
            nil)))

(defun test-takuzu-parity--board (case-plist)
  "Rebuild the board a fixture CASE-PLIST describes."
  (takuzu-make-board
   (plist-get case-plist :size)
   (vconcat (mapcar (lambda (ch) (pcase ch (?0 0) (?1 1) (_ nil)))
                    (plist-get case-plist :cells)))
   (vconcat (mapcar (lambda (ch) (eq ch ?g))
                    (plist-get case-plist :givens)))))

(defun test-takuzu-parity--forced (board)
  "The engine's forced cell for BOARD, as a list or nil."
  (with-temp-buffer
    (setq-local takuzu--size (takuzu-board-size board))
    (setq-local takuzu--board board)
    (takuzu--forced-cell)))

(ert-deftest test-takuzu-parity-corpus-replays-clean ()
  "Normal/Boundary/Error: every frozen fixture answer still holds.
One test over the whole corpus: a failing case names itself in the
`should' report via the NAME field.  Tagged slow (~8s, uniqueness proofs
on 12x12 boards) so the per-edit hook skips it; make test runs it."
  :tags '(:slow)
  (dolist (c (test-takuzu-parity--cases))
    (let ((name (plist-get c :name))
          (board (test-takuzu-parity--board c)))
      (should (equal (list name (plist-get c :legal))
                     (list name (and (takuzu-board-legal-p board) t))))
      (should (equal (list name (plist-get c :full))
                     (list name (and (takuzu-board-full-p board) t))))
      (should (equal (list name (plist-get c :solved))
                     (list name (and (takuzu-board-solved-p board) t))))
      (should (equal (list name (plist-get c :unique))
                     (list name (and (takuzu-unique-p board) t))))
      (let ((grade (plist-get c :grade)))
        (should (equal (list name (if (eq grade :null) nil grade))
                       (list name (and (eq (plist-get c :unique) t)
                                       (symbol-name (takuzu-grade board)))))))
      (let ((forced (plist-get c :forced)))
        (should (equal (list name (if (eq forced :null) nil (append forced nil)))
                       (list name (test-takuzu-parity--forced board)))))
      (let ((sol (plist-get c :solution)))
        (unless (eq sol :null)
          (let ((solved (takuzu-solve (takuzu-board-clone board))))
            (should (equal (list name sol)
                           (list name (mapconcat
                                       (lambda (v) (if (eql v 0) "0" "1"))
                                       (takuzu-board-cells solved) ""))))))))))

(provide 'test-takuzu-parity)
;;; test-takuzu-parity.el ends here
