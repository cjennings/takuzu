;;; test-takuzu-ui.el --- Tests for takuzu-ui helpers -*- lexical-binding: t -*-

;;; Commentary:
;; The pure helpers.  The SVG faceplate itself is verified visually.

;;; Code:

(require 'ert)
(require 'takuzu-board)
(require 'takuzu-ui)

(ert-deftest test-takuzu-ui-fmt-time ()
  "Normal/Boundary: seconds format as M:SS with zero-padding."
  (should (equal (takuzu--fmt-time 0) "0:00"))
  (should (equal (takuzu--fmt-time 9) "0:09"))
  (should (equal (takuzu--fmt-time 75) "1:15"))
  (should (equal (takuzu--fmt-time 600) "10:00")))

(ert-deftest test-takuzu-ui-cell-size-shrinks ()
  "Boundary: cell size shrinks as the board grows."
  (should (> (takuzu--cell-size 4) (takuzu--cell-size 8)))
  (should (> (takuzu--cell-size 8) (takuzu--cell-size 12)))
  (should (integerp (takuzu--cell-size 6))))

(ert-deftest test-takuzu-ui-error-vector-off ()
  "Normal: with assist off, no errors are reported even on a broken board."
  (with-temp-buffer
    (setq takuzu--size 4 takuzu--assist nil
          takuzu--board (takuzu-make-board 4 (vector 0 0 0 1 nil nil nil nil
                                                     nil nil nil nil nil nil nil nil)))
    (should (null (takuzu--error-vector)))))

(ert-deftest test-takuzu-ui-error-vector-marks-triple ()
  "Error: with assist on, a row triple marks all of that row's cells."
  (with-temp-buffer
    (setq takuzu--size 4 takuzu--assist t
          takuzu--board (takuzu-make-board 4 (vector 0 0 0 nil nil nil nil nil
                                                     nil nil nil nil nil nil nil nil)))
    (let ((e (takuzu--error-vector)))
      (should e)
      (should (aref e 0))
      (should (aref e 3))
      (should-not (aref e 4)))))

(provide 'test-takuzu-ui)
;;; test-takuzu-ui.el ends here
