;;; test-takuzu-stats.el --- Tests for takuzu-stats -*- lexical-binding: t -*-

;;; Commentary:
;; The stats layer is pure data: load/save a printed alist, record results,
;; query entries and totals.  Every test binds `takuzu-stats-file' to a temp
;; path so no run touches the developer's real stats.

;;; Code:

(require 'ert)
(require 'takuzu-stats)
(require 'testutil-takuzu)

;; Belt and suspenders alongside the per-test binding: a future test that
;; forgets `takuzu-testutil-with-stats-file' still can't touch the real file.
(setq takuzu-stats-file (make-temp-file "takuzu-stats-suite-" nil ".eld"))

;; --- load / save ---

(ert-deftest test-takuzu-stats-load-missing-file-empty ()
  "Boundary: a missing stats file loads as empty stats."
  (takuzu-testutil-with-stats-file
    (should (null (takuzu-stats-load)))))

(ert-deftest test-takuzu-stats-load-corrupt-file-empty ()
  "Error: a corrupt stats file loads as empty stats instead of erroring."
  (takuzu-testutil-with-stats-file
    (with-temp-file takuzu-stats-file (insert "(((4 . easy"))
    (should (null (takuzu-stats-load)))))

(ert-deftest test-takuzu-stats-save-load-round-trip ()
  "Normal: saved stats read back structurally equal."
  (takuzu-testutil-with-stats-file
    (let ((stats '(((6 . medium) :wins 2 :losses 1 :best 133))))
      (takuzu-stats-save stats)
      (should (equal (takuzu-stats-load) stats)))))

;; --- record ---

(ert-deftest test-takuzu-stats-record-first-win ()
  "Normal: the first win creates the entry with a best time."
  (takuzu-testutil-with-stats-file
    (takuzu-stats-record 6 'medium 'win 90)
    (let ((entry (takuzu-stats-entry (takuzu-stats-load) 6 'medium)))
      (should (= (plist-get entry :wins) 1))
      (should (= (plist-get entry :losses) 0))
      (should (= (plist-get entry :best) 90)))))

(ert-deftest test-takuzu-stats-record-loss-keeps-best-unset ()
  "Normal: a loss increments losses and never sets a best time."
  (takuzu-testutil-with-stats-file
    (takuzu-stats-record 4 'easy 'loss 45)
    (let ((entry (takuzu-stats-entry (takuzu-stats-load) 4 'easy)))
      (should (= (plist-get entry :wins) 0))
      (should (= (plist-get entry :losses) 1))
      (should (null (plist-get entry :best))))))

(ert-deftest test-takuzu-stats-record-best-is-minimum ()
  "Normal: a slower second win keeps the faster best; a faster one replaces it."
  (takuzu-testutil-with-stats-file
    (takuzu-stats-record 6 'hard 'win 120)
    (takuzu-stats-record 6 'hard 'win 200)
    (should (= (plist-get (takuzu-stats-entry (takuzu-stats-load) 6 'hard) :best) 120))
    (takuzu-stats-record 6 'hard 'win 80)
    (let ((entry (takuzu-stats-entry (takuzu-stats-load) 6 'hard)))
      (should (= (plist-get entry :wins) 3))
      (should (= (plist-get entry :best) 80)))))

(ert-deftest test-takuzu-stats-record-loss-then-win-sets-best ()
  "Boundary: a win after losses sets the entry's first best time."
  (takuzu-testutil-with-stats-file
    (takuzu-stats-record 8 'medium 'loss 30)
    (takuzu-stats-record 8 'medium 'win 210)
    (let ((entry (takuzu-stats-entry (takuzu-stats-load) 8 'medium)))
      (should (= (plist-get entry :wins) 1))
      (should (= (plist-get entry :losses) 1))
      (should (= (plist-get entry :best) 210)))))

(ert-deftest test-takuzu-stats-record-zero-second-win ()
  "Boundary: a zero-second elapsed still records as a valid best."
  (takuzu-testutil-with-stats-file
    (takuzu-stats-record 4 'easy 'win 0)
    (should (= (plist-get (takuzu-stats-entry (takuzu-stats-load) 4 'easy) :best) 0))))

(ert-deftest test-takuzu-stats-record-separate-keys ()
  "Normal: size/grade pairs tally independently."
  (takuzu-testutil-with-stats-file
    (takuzu-stats-record 4 'easy 'win 10)
    (takuzu-stats-record 6 'easy 'loss 20)
    (takuzu-stats-record 4 'hard 'win 30)
    (let ((stats (takuzu-stats-load)))
      (should (= (plist-get (takuzu-stats-entry stats 4 'easy) :wins) 1))
      (should (= (plist-get (takuzu-stats-entry stats 6 'easy) :losses) 1))
      (should (= (plist-get (takuzu-stats-entry stats 4 'hard) :wins) 1)))))

;; --- queries ---

(ert-deftest test-takuzu-stats-entry-unseen-key-nil ()
  "Boundary: querying a never-played size/grade returns nil."
  (should (null (takuzu-stats-entry '(((4 . easy) :wins 1 :losses 0)) 12 'hard))))

(ert-deftest test-takuzu-stats-totals ()
  "Normal/Boundary: totals aggregate across entries; empty stats total zero."
  (should (equal (takuzu-stats-totals nil) '(0 . 0)))
  (should (equal (takuzu-stats-totals
                  '(((4 . easy) :wins 2 :losses 1)
                    ((6 . hard) :wins 1 :losses 3)))
                 '(3 . 4))))

(provide 'test-takuzu-stats)
;;; test-takuzu-stats.el ends here
