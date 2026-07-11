;;; test-takuzu-board.el --- Tests for takuzu-board -*- lexical-binding: t -*-

;;; Commentary:
;; Board representation and the three Takuzu rules.

;;; Code:

(require 'ert)
(require 'takuzu-board)

(defconst test-takuzu-board--solved-4
  (vector 0 0 1 1
          1 1 0 0
          1 0 0 1
          0 1 1 0)
  "A valid, complete 4x4 Takuzu solution (rows and cols distinct, even, no triple).")

;; --- construction / accessors ---

(ert-deftest test-takuzu-board-make-empty ()
  "Normal: a fresh board has the right size and all-nil cells."
  (let ((b (takuzu-make-board 4)))
    (should (= (takuzu-board-size b) 4))
    (should (null (takuzu-board-ref b 0 0)))
    (should-not (takuzu-board-full-p b))))

(ert-deftest test-takuzu-board-ref-set ()
  "Normal: set then ref round-trips with row-major indexing."
  (let ((b (takuzu-make-board 4)))
    (takuzu-board-set b 1 2 1)
    (should (= (takuzu-board-ref b 1 2) 1))
    (should (null (takuzu-board-ref b 2 1)))))

(ert-deftest test-takuzu-board-from-cells ()
  "Normal: build from a cells vector and read rows and columns."
  (let ((b (takuzu-make-board 4 test-takuzu-board--solved-4)))
    (should (equal (takuzu-board-row b 0) '(0 0 1 1)))
    (should (equal (takuzu-board-col b 0) '(0 1 1 0)))
    (should (takuzu-board-full-p b))))

(ert-deftest test-takuzu-board-givens ()
  "Normal: the givens mask marks locked cells."
  (let ((b (takuzu-make-board 4 test-takuzu-board--solved-4
                              (vector t nil nil nil
                                      nil nil nil nil
                                      nil nil nil nil
                                      nil nil nil nil))))
    (should (takuzu-board-given-p b 0 0))
    (should-not (takuzu-board-given-p b 0 1))))

(ert-deftest test-takuzu-board-clone-independent ()
  "Normal: a clone shares no mutable state with the original."
  (let* ((b (takuzu-make-board 4 test-takuzu-board--solved-4))
         (c (takuzu-board-clone b)))
    (takuzu-board-set c 0 0 1)
    (should (= (takuzu-board-ref b 0 0) 0))
    (should (= (takuzu-board-ref c 0 0) 1))))

;; --- triple rule ---

(ert-deftest test-takuzu-line-triple ()
  "Boundary: a triple is detected at the start, end, and middle; pairs and nils do not."
  (should (takuzu--line-has-triple-p '(0 0 0 1)))
  (should (takuzu--line-has-triple-p '(1 0 0 0)))
  (should (takuzu--line-has-triple-p '(1 0 0 0 1)))
  (should-not (takuzu--line-has-triple-p '(0 0 1 1)))
  (should-not (takuzu--line-has-triple-p '(0 nil 0 0)))
  (should-not (takuzu--line-has-triple-p '())))

;; --- count rule ---

(ert-deftest test-takuzu-line-count-legal ()
  "Boundary: neither color may exceed size/2; nils are ignored."
  (should (takuzu--line-count-legal-p '(0 0 1 1) 4))
  (should (takuzu--line-count-legal-p '(0 0 nil nil) 4))
  (should-not (takuzu--line-count-legal-p '(0 0 0 nil) 4))
  (should (takuzu--line-count-legal-p '(nil nil nil nil) 4)))

;; --- complete-line validity ---

(ert-deftest test-takuzu-line-complete-valid ()
  "Normal/Error: a complete line is valid iff it is evenly split and triple-free."
  (should (takuzu--line-complete-valid-p '(0 0 1 1) 4))
  (should (takuzu--line-complete-valid-p '(1 0 0 1) 4))
  (should-not (takuzu--line-complete-valid-p '(0 0 0 1) 4))
  (should-not (takuzu--line-complete-valid-p '(0 1 nil 1) 4)))

;; --- board legality (partial) ---

(ert-deftest test-takuzu-board-legal-partial ()
  "Normal: an empty board is legal; a triple in a row makes it illegal."
  (let ((b (takuzu-make-board 4)))
    (should (takuzu-board-legal-p b))
    (takuzu-board-set b 0 0 0)
    (takuzu-board-set b 0 1 0)
    (takuzu-board-set b 0 2 0)
    (should-not (takuzu-board-legal-p b))))

(ert-deftest test-takuzu-board-legal-dup-rows ()
  "Error: two identical complete rows make the board illegal."
  (let ((b (takuzu-make-board 4 (vector 0 0 1 1
                                        0 0 1 1
                                        1 1 0 0
                                        1 1 0 0))))
    (should-not (takuzu-board-legal-p b))))

;; --- solved ---

(ert-deftest test-takuzu-board-solved ()
  "Normal: the fixture solution is solved; an empty board is not."
  (should (takuzu-board-solved-p
           (takuzu-make-board 4 test-takuzu-board--solved-4)))
  (should-not (takuzu-board-solved-p (takuzu-make-board 4))))

(ert-deftest test-takuzu-board-solved-rejects-dup ()
  "Error: a full, even, triple-free board with duplicate lines is not solved."
  (let ((b (takuzu-make-board 4 (vector 0 1 0 1
                                        0 1 0 1
                                        1 0 1 0
                                        1 0 1 0))))
    (should-not (takuzu-board-solved-p b))))

(provide 'test-takuzu-board)
;;; test-takuzu-board.el ends here
