;;; test-takuzu-ui.el --- Tests for takuzu-ui -*- lexical-binding: t -*-

;;; Commentary:
;; The pure helpers and game-action logic are unit-tested directly.  The SVG
;; faceplate is exercised through `takuzu--svg' (it builds a DOM with no display
;; needed), so the draw helpers run in batch; their pixels are verified visually.

;;; Code:

(require 'ert)
(require 'takuzu)

;; A known-valid 4x4 solution used across win/render tests.
(defconst test-takuzu-ui--solution-4
  (vector 0 0 1 1
          1 1 0 0
          0 1 1 0
          1 0 0 1)
  "A legal 4x4 Takuzu solution (rows/cols balanced, unique, no triples).")

(defmacro test-takuzu-ui--with-buffer (&rest body)
  "Run BODY in a live `takuzu-mode' buffer, cancelling timers and process after."
  (declare (indent 0))
  `(let ((buf (get-buffer-create " *takuzu-test*")))
     (unwind-protect
         (with-current-buffer buf (takuzu-mode) ,@body)
       (with-current-buffer buf (ignore-errors (takuzu--cleanup)))
       (ignore-errors (kill-buffer buf)))))

(defun test-takuzu-ui--setup-4 (&optional cells givens)
  "Install a 4x4 board from CELLS/GIVENS (defaults: empty) into the current buffer."
  (setq takuzu--size 4 takuzu--generating nil takuzu--armed nil
        takuzu--won nil takuzu--proven nil takuzu--assist nil
        takuzu--history nil takuzu--status "" takuzu--cursor '(0 . 0)
        takuzu--start-time (current-time)
        takuzu--board (takuzu-make-board 4 cells givens)
        takuzu--solution (takuzu-make-board 4 test-takuzu-ui--solution-4)))

;; --- pure helpers ---

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

(ert-deftest test-takuzu-ui-empty-count ()
  "Normal/Boundary: empty-count counts nil cells."
  (with-temp-buffer
    (setq takuzu--size 4 takuzu--board (takuzu-make-board 4))
    (should (= (takuzu--empty-count) 16))
    (takuzu-board-set takuzu--board 0 0 1)
    (should (= (takuzu--empty-count) 15))))

(ert-deftest test-takuzu-ui-fill-pct ()
  "Boundary: fill percent spans 0 (empty) to 100 (full)."
  (with-temp-buffer
    (setq takuzu--size 4 takuzu--board (takuzu-make-board 4))
    (should (= (takuzu--fill-pct) 0.0))
    (setq takuzu--board (takuzu-make-board 4 test-takuzu-ui--solution-4))
    (should (= (takuzu--fill-pct) 100.0))
    (takuzu-board-set takuzu--board 0 0 nil)
    (should (< 90.0 (takuzu--fill-pct) 100.0))))

(ert-deftest test-takuzu-ui-elapsed ()
  "Normal/Boundary: 0 before start, frozen once finished, else running."
  (with-temp-buffer
    (setq takuzu--won nil takuzu--proven nil takuzu--start-time nil)
    (should (= (takuzu--elapsed) 0))
    (setq takuzu--start-time (time-subtract (current-time) (seconds-to-time 30)))
    (should (<= 29 (takuzu--elapsed) 31))
    (setq takuzu--won t takuzu--won-elapsed 42)
    (should (= (takuzu--elapsed) 42))))

(ert-deftest test-takuzu-ui-refresh-interval ()
  "Boundary: refresh interval is clamped to [0.2, 1.0]."
  (let ((takuzu-flash-period 1.0)) (should (= (takuzu--refresh-interval) 0.5)))
  (let ((takuzu-flash-period 0.2)) (should (= (takuzu--refresh-interval) 0.2)))
  (let ((takuzu-flash-period 8.0)) (should (= (takuzu--refresh-interval) 1.0))))

(ert-deftest test-takuzu-ui-dial-glow ()
  "Normal: dial glow returns a hex string, brighter at higher brightness."
  (should (string-match-p "^#[0-9a-f]\\{6\\}$" (takuzu--dial-glow 0.0)))
  (should (string-match-p "^#[0-9a-f]\\{6\\}$" (takuzu--dial-glow 1.0)))
  (should (string-lessp (takuzu--dial-glow 0.1) (takuzu--dial-glow 0.9))))

(ert-deftest test-takuzu-ui-faceplate-dims ()
  "Normal: faceplate width/height are positive and grow with board size."
  (with-temp-buffer
    (setq takuzu--size 4) (setq takuzu--board (takuzu-make-board 4))
    (let ((w4 (takuzu--faceplate-width)) (h4 (takuzu--faceplate-height)))
      (setq takuzu--size 12 takuzu--board (takuzu-make-board 12))
      (should (> (takuzu--faceplate-width) w4))
      (should (> (takuzu--faceplate-height) h4))
      (should (integerp w4)) (should (integerp h4)))))

(ert-deftest test-takuzu-ui-curp-and-glyph ()
  "Normal: cursor predicate and cell glyph."
  (with-temp-buffer
    (setq takuzu--cursor '(2 . 3))
    (should (takuzu--curp 2 3))
    (should-not (takuzu--curp 2 2)))
  (should (equal (takuzu--glyph 0) "O"))
  (should (equal (takuzu--glyph 1) "X"))
  (should (equal (takuzu--glyph nil) ".")))

(ert-deftest test-takuzu-ui-draw-cursor-corner-brackets ()
  "Normal: the cursor draws four corner brackets (eight line segments)."
  (let ((svg (svg-create 100 100)))
    (takuzu--draw-cursor svg 0 0 40)
    ;; four corners, two arms each -> eight line elements, all gold
    (let ((lines (dom-by-tag svg 'line)))
      (should (= (length lines) 8))
      (should (cl-every (lambda (l) (equal (dom-attr l 'stroke) (takuzu--c :gold)))
                        lines)))))

(ert-deftest test-takuzu-ui-help-toggle ()
  "Normal: help toggles the overlay flag and the help SVG renders."
  (test-takuzu-ui--with-buffer
    (test-takuzu-ui--setup-4)
    (should-not takuzu--help)
    (takuzu-help)
    (should takuzu--help)
    (should (eq (car (takuzu--svg-help)) 'svg))
    (takuzu-help)
    (should-not takuzu--help)))

(ert-deftest test-takuzu-ui-help-dismissed-by-key ()
  "Normal: with help up, a game key dismisses it instead of acting."
  (test-takuzu-ui--with-buffer
    (test-takuzu-ui--setup-4)
    (setq takuzu--help t takuzu--cursor '(1 . 1))
    (takuzu-right)
    (should-not takuzu--help)
    (should (equal takuzu--cursor '(1 . 1)))
    (setq takuzu--help t)
    (takuzu-cycle)
    (should-not takuzu--help)))

(ert-deftest test-takuzu-ui-render-text ()
  "Normal: the text fallback marks the cursor cell with brackets."
  (with-temp-buffer
    (test-takuzu-ui--setup-4)
    (let ((out (takuzu--render-text)))
      (should (string-match-p "\\[.\\]" out))
      (should (= (1+ (cl-count ?\n out)) 5)))))

(ert-deftest test-takuzu-ui-error-vector-off ()
  "Normal: with assist off, no errors are reported even on a broken board."
  (with-temp-buffer
    (setq takuzu--size 4 takuzu--assist nil
          takuzu--board (takuzu-make-board 4 (vector 0 0 0 1 nil nil nil nil
                                                     nil nil nil nil nil nil nil nil)))
    (should (null (takuzu--error-vector)))))

(ert-deftest test-takuzu-ui-error-vector-marks-triple ()
  "Error: assist on, a row triple marks that whole row."
  (with-temp-buffer
    (setq takuzu--size 4 takuzu--assist t
          takuzu--board (takuzu-make-board 4 (vector 0 0 0 nil nil nil nil nil
                                                     nil nil nil nil nil nil nil nil)))
    (let ((e (takuzu--error-vector)))
      (should e) (should (aref e 0)) (should (aref e 3)) (should-not (aref e 4)))))

(ert-deftest test-takuzu-ui-error-vector-marks-column-triple ()
  "Error: assist on, a column triple marks that whole column."
  (with-temp-buffer
    (setq takuzu--size 4 takuzu--assist t
          takuzu--board (takuzu-make-board 4 (vector 1 nil nil nil
                                                     1 nil nil nil
                                                     1 nil nil nil
                                                     nil nil nil nil)))
    (let ((e (takuzu--error-vector)))
      (should e) (should (aref e 0)) (should (aref e 8)) (should-not (aref e 1)))))

(ert-deftest test-takuzu-ui-forced-cell ()
  "Normal: forced-cell finds a single-legal-value cell, nil when none."
  (with-temp-buffer
    (setq takuzu--size 4
          takuzu--board (takuzu-make-board 4 (vector 0 0 nil nil nil nil nil nil
                                                     nil nil nil nil nil nil nil nil)))
    ;; cell (0,2) cannot be 0 (would make 0 0 0 triple), so it is forced to 1.
    (let ((f (takuzu--forced-cell)))
      (should f) (should (equal (list (nth 0 f) (nth 1 f) (nth 2 f)) '(0 2 1)))))
  (with-temp-buffer
    (setq takuzu--size 4 takuzu--board (takuzu-make-board 4))
    (should (null (takuzu--forced-cell)))))

;; --- game-action logic ---

(ert-deftest test-takuzu-ui-move-clamps ()
  "Boundary: movement clamps at the board edges."
  (test-takuzu-ui--with-buffer
    (test-takuzu-ui--setup-4)
    (setq takuzu--cursor '(0 . 0))
    (takuzu--move -1 -1)
    (should (equal takuzu--cursor '(0 . 0)))
    (setq takuzu--cursor '(3 . 3))
    (takuzu--move 5 5)
    (should (equal takuzu--cursor '(3 . 3)))))

(ert-deftest test-takuzu-ui-move-clears-transient-status ()
  "Normal: a move clears a transient status but keeps the win note."
  (test-takuzu-ui--with-buffer
    (test-takuzu-ui--setup-4)
    (setq takuzu--status "Nothing to undo.")
    (takuzu--move 0 1)
    (should (equal takuzu--status ""))
    (setq takuzu--won t takuzu--status "Solved.")
    (takuzu--move 0 1)
    (should (equal takuzu--status "Solved."))))

(ert-deftest test-takuzu-ui-cycle ()
  "Normal: cycling a cell steps empty -> 0 -> 1 -> empty."
  (test-takuzu-ui--with-buffer
    (test-takuzu-ui--setup-4)
    (setq takuzu--cursor '(0 . 0))
    (takuzu-cycle) (should (eql (takuzu-board-ref takuzu--board 0 0) 0))
    (takuzu-cycle) (should (eql (takuzu-board-ref takuzu--board 0 0) 1))
    (takuzu-cycle) (should (null (takuzu-board-ref takuzu--board 0 0)))))

(ert-deftest test-takuzu-ui-given-refused ()
  "Error: a given cell cannot be changed."
  (test-takuzu-ui--with-buffer
    (test-takuzu-ui--setup-4
     (vector 0 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil)
     (vector t nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil))
    (setq takuzu--cursor '(0 . 0))
    (takuzu-cycle)
    (should (eql (takuzu-board-ref takuzu--board 0 0) 0))
    (should (string-match-p "given" takuzu--status))))

(ert-deftest test-takuzu-ui-undo ()
  "Normal/Boundary: undo reverts the last placement; nothing to undo when empty."
  (test-takuzu-ui--with-buffer
    (test-takuzu-ui--setup-4)
    (setq takuzu--cursor '(1 . 1))
    (takuzu-cycle)
    (should (eql (takuzu-board-ref takuzu--board 1 1) 0))
    (takuzu-undo)
    (should (null (takuzu-board-ref takuzu--board 1 1)))
    (takuzu-undo)
    (should (string-match-p "Nothing to undo" takuzu--status))))

(ert-deftest test-takuzu-ui-check-win-detects-solved ()
  "Normal: a solved board is noted as won and freezes the clock."
  (test-takuzu-ui--with-buffer
    (test-takuzu-ui--setup-4 (copy-sequence test-takuzu-ui--solution-4))
    (takuzu--check-win)
    (should takuzu--won)
    (should (string-match-p "Solved" takuzu--status))))

(ert-deftest test-takuzu-ui-check-unfinished ()
  "Normal: check on an unfinished board reports the cells left."
  (test-takuzu-ui--with-buffer
    (test-takuzu-ui--setup-4)
    (takuzu-check)
    (should-not takuzu--won)
    (should (string-match-p "16 cells left" takuzu--status))))

(ert-deftest test-takuzu-ui-prove-fills-solution ()
  "Normal: prove fills the full solution and marks the puzzle proven."
  (test-takuzu-ui--with-buffer
    (test-takuzu-ui--setup-4)
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
      (takuzu-prove))
    (should takuzu--proven)
    (should (equal (takuzu-board-cells takuzu--board) test-takuzu-ui--solution-4))))

(ert-deftest test-takuzu-ui-reset-clears-nongivens ()
  "Normal: reset clears placed cells but keeps givens."
  (test-takuzu-ui--with-buffer
    (test-takuzu-ui--setup-4
     (vector 0 1 nil nil nil nil nil nil nil nil nil nil nil nil nil nil)
     (vector t nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil))
    (takuzu-reset)
    (should (eql (takuzu-board-ref takuzu--board 0 0) 0))   ; given kept
    (should (null (takuzu-board-ref takuzu--board 0 1)))))   ; placed cleared

(ert-deftest test-takuzu-ui-toggle-assist ()
  "Normal: toggle flips assist and reports it."
  (test-takuzu-ui--with-buffer
    (test-takuzu-ui--setup-4)
    (should-not takuzu--assist)
    (takuzu-toggle-assist)
    (should takuzu--assist)
    (should (string-match-p "Assist on" takuzu--status))))

(ert-deftest test-takuzu-ui-playing-only-guard ()
  "Error: board commands are inert while armed (no puzzle yet)."
  (test-takuzu-ui--with-buffer
    (setq takuzu--size 4 takuzu--board (takuzu-make-board 4)
          takuzu--armed '(:size 4 :difficulty easy) takuzu--cursor '(0 . 0))
    (takuzu-cycle)
    (should (null (takuzu-board-ref takuzu--board 0 0)))))

(ert-deftest test-takuzu-ui-begin-play-starts-clock ()
  "Normal: begin-play installs the board and starts the clock."
  (test-takuzu-ui--with-buffer
    (setq takuzu--armed '(:size 4 :difficulty easy))
    (takuzu--begin-play
     (current-buffer)
     (list :board (takuzu-make-board 4 test-takuzu-ui--solution-4)
           :solution (takuzu-make-board 4 test-takuzu-ui--solution-4)
           :grade 'easy))
    (should (null takuzu--armed))
    (should takuzu--start-time)
    (should (eq takuzu--grade 'easy))))

;; --- SVG faceplate (exercises the draw-* helpers headlessly) ---

(defun test-takuzu-ui--playing-state (size grade)
  "Set a mid-game SIZE/GRADE playing state in the current buffer."
  (let ((g (takuzu-generate size 'easy)))
    (setq takuzu--size size takuzu--board (plist-get g :board)
          takuzu--solution (plist-get g :solution) takuzu--grade grade
          takuzu--difficulty grade takuzu--cursor '(0 . 0)
          takuzu--start-time (current-time) takuzu--status ""
          takuzu--generating nil takuzu--armed nil takuzu--clock-flash 0)))

(ert-deftest test-takuzu-ui-svg-renders-each-size ()
  "Normal: the faceplate builds an SVG DOM for every offered size.
Boards are constructed (not generated) so this stays fast while still exercising
the size-dependent layout and both disc styles (a given and a placed cell)."
  (with-temp-buffer
    (dolist (n '(4 6 8 10 12))
      (let ((cells (make-vector (* n n) nil))
            (givens (make-vector (* n n) nil)))
        (aset cells 0 0) (aset givens 0 t)   ; a given disc
        (aset cells 1 1)                      ; a placed disc
        (setq takuzu--size n takuzu--board (takuzu-make-board n cells givens)
              takuzu--solution (takuzu-make-board n cells) takuzu--grade 'easy
              takuzu--difficulty 'easy takuzu--cursor '(0 . 0)
              takuzu--start-time (current-time) takuzu--status ""
              takuzu--generating nil takuzu--armed nil takuzu--clock-flash 0))
      (let ((svg (takuzu--svg)))
        (should svg) (should (eq (car svg) 'svg))))))

(ert-deftest test-takuzu-ui-svg-renders-states ()
  "Normal: the faceplate builds for won, proven, assist-on, and flashing states."
  (with-temp-buffer
    (test-takuzu-ui--playing-state 6 'medium)
    (setq takuzu--won t takuzu--won-elapsed 30 takuzu--status "Solved.")
    (should (eq (car (takuzu--svg)) 'svg))
    (setq takuzu--won nil takuzu--proven t)
    (should (eq (car (takuzu--svg)) 'svg))
    (setq takuzu--proven nil takuzu--assist t)
    (should (eq (car (takuzu--svg)) 'svg))
    (setq takuzu--clock-flash 3)
    (should (eq (car (takuzu--svg)) 'svg))))

(ert-deftest test-takuzu-ui-svg-generating ()
  "Normal: the generating faceplate builds while armed/generating."
  (with-temp-buffer
    (setq takuzu--size 8 takuzu--spinner 2
          takuzu--generating '(:size 8 :difficulty hard))
    (should (eq (car (takuzu--svg-generating)) 'svg))))

(ert-deftest test-takuzu-ui-redraw-text-fallback ()
  "Normal: redraw uses the text board when SVG is unavailable (batch)."
  (test-takuzu-ui--with-buffer
    (test-takuzu-ui--setup-4)
    (takuzu--redraw)
    (should (> (buffer-size) 0))
    (should (string-match-p "Takuzu" (buffer-string)))))

(defmacro test-takuzu-ui--arming (&rest body)
  "Run BODY with `switch-to-buffer' stubbed; tear down *Takuzu* after."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'switch-to-buffer) #'ignore))
     (unwind-protect (progn ,@body)
       (let ((b (get-buffer "*Takuzu*")))
         (when b
           (with-current-buffer b (ignore-errors (takuzu--cleanup)))
           (ignore-errors (kill-buffer b)))))))

(ert-deftest test-takuzu-ui-error-vector-duplicate-lines ()
  "Error: assist on, two identical complete rows both flag as duplicates."
  (with-temp-buffer
    (setq takuzu--size 4 takuzu--assist t
          takuzu--board (takuzu-make-board 4 (vector 0 1 0 1  0 1 0 1
                                                     nil nil nil nil  nil nil nil nil)))
    (let ((e (takuzu--error-vector)))
      (should e) (should (aref e 0)) (should (aref e 4)) (should-not (aref e 8)))))

(ert-deftest test-takuzu-ui-movement-commands ()
  "Normal: the hjkl/arrow commands move the cursor through `takuzu--move'."
  (test-takuzu-ui--with-buffer
    (test-takuzu-ui--setup-4)
    (setq takuzu--cursor '(1 . 1))
    (takuzu-up)    (should (equal takuzu--cursor '(0 . 1)))
    (takuzu-down)  (should (equal takuzu--cursor '(1 . 1)))
    (takuzu-left)  (should (equal takuzu--cursor '(1 . 0)))
    (takuzu-right) (should (equal takuzu--cursor '(1 . 1)))))

(ert-deftest test-takuzu-ui-set-current-finished ()
  "Error: once finished, a cell placement is refused."
  (test-takuzu-ui--with-buffer
    (test-takuzu-ui--setup-4)
    (setq takuzu--won t takuzu--cursor '(1 . 1))
    (takuzu--set-current 0)
    (should (null (takuzu-board-ref takuzu--board 1 1)))
    (should (string-match-p "finished" takuzu--status))))

(ert-deftest test-takuzu-ui-hint-fills-forced-cell ()
  "Normal: hint fills a forced cell and says so."
  (test-takuzu-ui--with-buffer
    (setq takuzu--size 4 takuzu--generating nil takuzu--armed nil
          takuzu--won nil takuzu--proven nil takuzu--assist nil takuzu--status ""
          takuzu--cursor '(3 . 3) takuzu--history nil takuzu--start-time (current-time)
          takuzu--solution (takuzu-make-board 4 test-takuzu-ui--solution-4)
          takuzu--board (takuzu-make-board 4 (vector 0 0 nil nil nil nil nil nil
                                                     nil nil nil nil nil nil nil nil)))
    (takuzu-hint)
    (should (eql (takuzu-board-ref takuzu--board 0 2) 1))
    (should (string-match-p "forced" takuzu--status))))

(ert-deftest test-takuzu-ui-check-full-but-wrong ()
  "Error: a full but rule-breaking board reports the break."
  (test-takuzu-ui--with-buffer
    (test-takuzu-ui--setup-4 (vector 0 0 0 0  1 1 1 1  0 0 0 0  1 1 1 1))
    (takuzu-check)
    (should-not takuzu--won)
    (should (string-match-p "rule is broken" takuzu--status))))

(ert-deftest test-takuzu-ui-cycle-size ()
  "Normal: cycling size arms a fresh game at the next size."
  (test-takuzu-ui--arming
    (with-temp-buffer
      (takuzu-mode)
      (setq takuzu--size 6 takuzu--difficulty 'easy)
      (takuzu-cycle-size)
      (with-current-buffer "*Takuzu*"
        (should takuzu--armed)
        (should (= takuzu--size 8))))))

(ert-deftest test-takuzu-ui-cycle-difficulty ()
  "Normal: cycling difficulty arms a fresh game at the next difficulty."
  (test-takuzu-ui--arming
    (with-temp-buffer
      (takuzu-mode)
      (setq takuzu--size 6 takuzu--difficulty 'easy)
      (takuzu-cycle-difficulty)
      (with-current-buffer "*Takuzu*"
        (should takuzu--armed)
        (should (eq takuzu--difficulty 'medium))))))

(ert-deftest test-takuzu-ui-new-starts-pending ()
  "Normal: New with a puzzle already pending begins play immediately."
  (test-takuzu-ui--with-buffer
    (setq takuzu--size 4 takuzu--armed '(:size 4 :difficulty easy)
          takuzu--pending (list :board (takuzu-make-board 4 test-takuzu-ui--solution-4)
                                :solution (takuzu-make-board 4 test-takuzu-ui--solution-4)
                                :grade 'easy))
    (takuzu-new)
    (should (null takuzu--armed))
    (should takuzu--start-time)))

(ert-deftest test-takuzu-ui-new-rearms-when-playing ()
  "Normal: New while playing arms a fresh game."
  (test-takuzu-ui--arming
    (with-temp-buffer
      (takuzu-mode)
      (setq takuzu--size 4 takuzu--difficulty 'easy takuzu--armed nil
            takuzu--board (takuzu-make-board 4 test-takuzu-ui--solution-4))
      (takuzu-new)
      (with-current-buffer "*Takuzu*"
        (should takuzu--armed)))))

(ert-deftest test-takuzu-ui-new-waits-when-not-ready ()
  "Normal: New while armed but not yet generated shows the spinner and waits."
  (test-takuzu-ui--with-buffer
    (setq takuzu--size 4 takuzu--armed '(:size 4 :difficulty easy) takuzu--pending nil)
    (takuzu-new)
    (should takuzu--pending-start)
    (should takuzu--generating)))

(ert-deftest test-takuzu-ui-svg-covers-meter-and-legend-branches ()
  "Normal: renders that hit the amber/green meters, error strokes, armed legend."
  (with-temp-buffer
    (setq takuzu--size 4 takuzu--grade 'easy takuzu--difficulty 'easy
          takuzu--cursor '(0 . 0) takuzu--start-time (current-time) takuzu--status ""
          takuzu--generating nil takuzu--armed nil takuzu--clock-flash 0 takuzu--won nil
          takuzu--proven nil takuzu--assist nil
          takuzu--solution (takuzu-make-board 4 test-takuzu-ui--solution-4))
    ;; near-full valid board -> green lamp + green ring arc
    (let ((c (copy-sequence test-takuzu-ui--solution-4)))
      (aset c 15 nil)
      (setq takuzu--board (takuzu-make-board 4 c)))
    (should (eq (car (takuzu--svg)) 'svg))
    ;; ~62% board -> amber lamp + amber ring
    (let ((c (copy-sequence test-takuzu-ui--solution-4)))
      (dotimes (i 6) (aset c (+ 10 i) nil))
      (setq takuzu--board (takuzu-make-board 4 c)))
    (should (eq (car (takuzu--svg)) 'svg))
    ;; assist on + a triple -> error stroke on the affected sockets
    (setq takuzu--assist t
          takuzu--board (takuzu-make-board 4 (vector 0 0 0 1 nil nil nil nil
                                                     nil nil nil nil nil nil nil nil)))
    (should (eq (car (takuzu--svg)) 'svg))
    ;; armed -> the flashing New key path in the legend
    (setq takuzu--assist nil takuzu--armed '(:size 4 :difficulty easy)
          takuzu--board (takuzu-make-board 4))
    (should (eq (car (takuzu--svg)) 'svg))))

(provide 'test-takuzu-ui)
;;; test-takuzu-ui.el ends here
