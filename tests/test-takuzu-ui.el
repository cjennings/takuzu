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
  "Normal/Boundary: seconds format as MM:SS from the very first second."
  (should (equal (takuzu--fmt-time 0) "00:00"))
  (should (equal (takuzu--fmt-time 9) "00:09"))
  (should (equal (takuzu--fmt-time 75) "01:15"))
  (should (equal (takuzu--fmt-time 600) "10:00")))

(ert-deftest test-takuzu-ui-draw-nixie-time-always-four-tubes ()
  "Boundary: the clock draws four digit tubes plus the colon even at 0:00.
MM:SS from the start means the tube count never changes mid-game."
  (with-temp-buffer
    (setq takuzu--won nil takuzu--proven nil takuzu--start-time nil)
    (let ((svg (svg-create 200 60)))
      (takuzu--draw-nixie-time svg 100 10)
      ;; each tube draws a glass rect + highlight rect; 4 digits at 00:00
      (should (= (length (dom-by-tag svg 'rect)) 8)))))

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

(ert-deftest test-takuzu-ui-draw-cursor-bezel ()
  "Normal: the cursor draws a machined bezel ring on the socket rim.
The ring's stroke carries a user-space linear gradient (the turned-metal
catch, bright top-left to shadowed bottom-right); a single path draws the
specular arc on the top-left corner."
  (let ((svg (svg-create 100 100)))
    (takuzu--draw-cursor-bezel svg 10 10 50)
    (should (= (length (dom-by-tag svg 'linearGradient)) 1))
    (let ((ring (seq-find (lambda (r)
                            (equal (dom-attr r 'stroke)
                                   "url(#takuzu-cursor-bezel-g)"))
                          (dom-by-tag svg 'rect))))
      (should ring)
      (should (equal (dom-attr ring 'fill) "none")))
    (should (= (length (dom-by-tag svg 'path)) 1))
    ;; grounding shadow, the ring, its turning groove, the inner lip
    (should (= (length (dom-by-tag svg 'rect)) 4))))

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

(ert-deftest test-takuzu-ui-reset-after-win-restarts-refresh-timer ()
  "Error: resetting a finished game restarts the stopped refresh timer.
The tick cancels the timer at the win; without a restart here the clock
would sit frozen through the replayed game."
  (test-takuzu-ui--with-buffer
    (test-takuzu-ui--setup-4 (copy-sequence test-takuzu-ui--solution-4))
    (setq takuzu--won t takuzu--timer nil)
    (takuzu-reset)
    (should (timerp takuzu--timer))))

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
          takuzu--generating nil takuzu--armed nil)))

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
              takuzu--generating nil takuzu--armed nil))
      (let ((svg (takuzu--svg)))
        (should svg) (should (eq (car svg) 'svg))))))

(ert-deftest test-takuzu-ui-svg-renders-states ()
  "Normal: the faceplate builds for won, proven, assist-on, and event states."
  (with-temp-buffer
    (test-takuzu-ui--playing-state 6 'medium)
    (setq takuzu--won t takuzu--won-elapsed 30 takuzu--status "Solved.")
    (should (eq (car (takuzu--svg)) 'svg))
    (setq takuzu--won nil takuzu--proven t)
    (should (eq (car (takuzu--svg)) 'svg))
    (setq takuzu--proven nil takuzu--assist t)
    (should (eq (car (takuzu--svg)) 'svg))
    (setq takuzu--event 'hint takuzu--event-time (current-time))
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

(ert-deftest test-takuzu-ui-hint-escalates-to-solution ()
  "Normal: with no logic-derivable cell, hint fills from the solution and says so.
An empty board has no forced cell and no hypothesis-resolvable cell, so the
hint's last tier answers with an honest from-the-solution label."
  (test-takuzu-ui--with-buffer
    (setq takuzu--size 4 takuzu--generating nil takuzu--armed nil
          takuzu--won nil takuzu--proven nil takuzu--assist nil takuzu--status ""
          takuzu--cursor '(3 . 3) takuzu--history nil takuzu--start-time (current-time)
          takuzu--solution (takuzu-make-board 4 test-takuzu-ui--solution-4)
          takuzu--board (takuzu-make-board 4))
    (takuzu-hint)
    (should (eql (takuzu-board-ref takuzu--board 0 0)
                 (aref test-takuzu-ui--solution-4 0)))
    (should (equal takuzu--cursor '(0 . 0)))
    (should (string-match-p "solution" takuzu--status))))

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

(ert-deftest test-takuzu-ui-cycle-level ()
  "Normal: cycling level arms a fresh game at the next level."
  (test-takuzu-ui--arming
    (with-temp-buffer
      (takuzu-mode)
      (setq takuzu--size 6 takuzu--difficulty 'easy)
      (takuzu-cycle-level)
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

(ert-deftest test-takuzu-ui-svg-covers-gauge-and-legend-branches ()
  "Normal: renders that hit gauge sweeps, error strokes, and the armed legend."
  (with-temp-buffer
    (setq takuzu--size 4 takuzu--grade 'easy takuzu--difficulty 'easy
          takuzu--cursor '(0 . 0) takuzu--start-time (current-time) takuzu--status ""
          takuzu--generating nil takuzu--armed nil takuzu--won nil
          takuzu--proven nil takuzu--assist nil
          takuzu--solution (takuzu-make-board 4 test-takuzu-ui--solution-4))
    ;; near-full valid board -> needle near full sweep
    (let ((c (copy-sequence test-takuzu-ui--solution-4)))
      (aset c 15 nil)
      (setq takuzu--board (takuzu-make-board 4 c)))
    (should (eq (car (takuzu--svg)) 'svg))
    ;; ~62% board -> needle mid-sweep
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

;; --- instrument panel helpers ---

(ert-deftest test-takuzu-ui-lerp-color ()
  "Normal: colour lerp hits both endpoints exactly and blends between them."
  (should (equal (takuzu--lerp-color "#141210" "#a8843a" 0) "#141210"))
  (should (equal (takuzu--lerp-color "#141210" "#a8843a" 1) "#a8843a"))
  (let ((mid (takuzu--lerp-color "#000000" "#ff0000" 0.5)))
    (should (string-match-p "^#[0-9a-f]\\{6\\}$" mid))
    (should (string-lessp "#000000" mid))
    (should (string-lessp mid "#ff0000"))))

(ert-deftest test-takuzu-ui-game-state ()
  "Normal: game state is ready/solving/solved; a proven board is solved too."
  (with-temp-buffer
    (setq takuzu--armed nil takuzu--won nil takuzu--proven nil)
    (should (eq (takuzu--game-state) 'solving))
    (setq takuzu--won t)
    (should (eq (takuzu--game-state) 'solved))
    (setq takuzu--won nil takuzu--proven t)
    (should (eq (takuzu--game-state) 'solved))
    (setq takuzu--armed '(:size 4 :difficulty easy))
    (should (eq (takuzu--game-state) 'ready))))

(ert-deftest test-takuzu-ui-strip-width-grows-with-size ()
  "Normal: the annunciator strip width is positive and grows with board size."
  (with-temp-buffer
    (setq takuzu--size 4)
    (let ((w4 (takuzu--strip-width)))
      (should (> w4 0))
      (setq takuzu--size 12)
      (should (> (takuzu--strip-width) w4)))))

;; --- event machinery ---

(ert-deftest test-takuzu-ui-set-status-signals-explicit-event ()
  "Normal: an explicit EVENT arg lights that lamp, with no echo."
  (with-temp-buffer
    (unwind-protect
        (let ((captured nil))
          (cl-letf (((symbol-function 'message)
                     (lambda (fmt &rest args) (setq captured (apply #'format fmt args)))))
            (takuzu--set-status "Nothing to undo." 'nothing)
            (should (eq takuzu--event 'nothing))
            (should-not captured)))
      (takuzu--signal-event nil))))

(ert-deftest test-takuzu-ui-set-status-no-event-clears-lamp ()
  "Boundary: a status without an EVENT clears any lit lamp."
  (with-temp-buffer
    (unwind-protect
        (progn
          (takuzu--set-status "Generation failed -- press n to retry." 'gen-fail)
          (should (eq takuzu--event 'gen-fail))
          (takuzu--set-status "")
          (should-not takuzu--event))
      (takuzu--signal-event nil))))

(ert-deftest test-takuzu-ui-signal-event-sets-and-clears ()
  "Normal: signalling an event records it with a timestamp; nil clears both."
  (with-temp-buffer
    (takuzu--signal-event 'hint)
    (should (eq takuzu--event 'hint))
    (should takuzu--event-time)
    (takuzu--signal-event nil)
    (should-not takuzu--event)
    (should-not takuzu--event-time)))

(ert-deftest test-takuzu-ui-signal-event-expires-without-window ()
  "Error: an event fired in an undisplayed buffer still gets an expiry timer.
Without one, a lamp lit while the buffer is buried (a background GEN FAIL,
say) breathes forever once the player returns."
  (with-temp-buffer
    (unwind-protect
        (progn
          (takuzu--signal-event 'gen-fail)
          (should (timerp takuzu--event-timer)))
      (takuzu--signal-event nil))))

(ert-deftest test-takuzu-ui-event-pulse-ends-dark ()
  "Boundary: the pulse duration is a whole number of breathing cycles.
Otherwise the lamp snaps off near peak brightness instead of fading out."
  (with-temp-buffer
    (let ((t0 (current-time)))
      (setq takuzu--event-time t0)
      (cl-letf (((symbol-function 'current-time)
                 (lambda () (time-add t0 (seconds-to-time takuzu--event-dur)))))
        (should (< (takuzu--event-intensity) 0.05))))))

(ert-deftest test-takuzu-ui-set-status-echoes-unmapped ()
  "Normal: a status with no annunciator lamp echoes so the key isn't silent.
The check key's \"Not finished\" report has no lamp; without the echo it
gives no feedback at all in the graphical UI."
  (with-temp-buffer
    (let ((captured nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq captured (apply #'format fmt args)))))
        (takuzu--set-status "Not finished -- 3 cells left.")
        (should (equal captured "Not finished -- 3 cells left."))
        (setq captured nil)
        (takuzu--set-status "Nothing to undo." 'nothing) ; lamp, no echo
        (should-not captured)
        (takuzu--set-status "")                          ; empty -> no echo
        (should-not captured)))))

(ert-deftest test-takuzu-ui-refresh-tick-stops-once-finished ()
  "Normal: the refresh tick cancels its own timer once the game is over.
A won or proven board's clock is frozen, so the per-second redraw only
burns cycles."
  (let ((buf (get-buffer-create " *takuzu-refresh-test*")))
    (unwind-protect
        (with-current-buffer buf
          (takuzu-mode)
          (setq takuzu--won t takuzu--armed nil)
          (takuzu--start-refresh-timer buf)
          (should (timerp takuzu--timer))
          (takuzu--refresh-tick buf)
          (should-not takuzu--timer))
      (kill-buffer buf))))

(ert-deftest test-takuzu-ui-refresh-tick-skips-undisplayed-buffer ()
  "Boundary: an undisplayed buffer is not redrawn, but the timer survives.
The tick must keep running so the clock resumes the moment the buffer is
shown again."
  (let ((buf (get-buffer-create " *takuzu-refresh-test*"))
        (drawn nil))
    (unwind-protect
        (with-current-buffer buf
          (takuzu-mode)
          (takuzu--start-refresh-timer buf)
          (cl-letf (((symbol-function 'takuzu--redraw)
                     (lambda (&optional _) (setq drawn t))))
            (takuzu--refresh-tick buf))
          (should-not drawn)
          (should (timerp takuzu--timer)))
      (kill-buffer buf))))

(ert-deftest test-takuzu-ui-refresh-tick-redraws-displayed-buffer ()
  "Normal: a displayed, in-play buffer redraws on the tick."
  (let ((buf (get-buffer-create "*takuzu-refresh-test*"))
        (drawn nil))
    (unwind-protect
        (with-current-buffer buf
          (takuzu-mode)
          (set-window-buffer (selected-window) buf)
          (takuzu--start-refresh-timer buf)
          (cl-letf (((symbol-function 'takuzu--redraw)
                     (lambda (&optional _) (setq drawn t))))
            (takuzu--refresh-tick buf))
          (should drawn)
          (should (timerp takuzu--timer)))
      (kill-buffer buf))))

(ert-deftest test-takuzu-ui-run-buffer-timer-runs-fn-on-live-buffer ()
  "Normal: the self-cancelling buffer timer calls FN with BUF while it lives."
  (let ((buf (generate-new-buffer " *takuzu-timer-test*"))
        (got nil)
        timer)
    (unwind-protect
        (progn
          (setq timer (takuzu--run-buffer-timer 60 60
                                                (lambda (b) (setq got b)) buf))
          (should (timerp timer))
          (apply (timer--function timer) (timer--args timer))
          (should (eq got buf))
          (should (memq timer timer-list)))
      (when (timerp timer) (cancel-timer timer))
      (kill-buffer buf))))

(ert-deftest test-takuzu-ui-run-buffer-timer-cancels-on-dead-buffer ()
  "Error: a tick on a dead buffer cancels the timer and never calls FN.
The buffer-local timer handle is unreachable from a dead buffer, so
without self-cancel a missed cleanup leaks a repeating timer forever."
  (let* ((buf (generate-new-buffer " *takuzu-timer-test*"))
         (called nil)
         (timer (takuzu--run-buffer-timer 60 60
                                          (lambda (_) (setq called t)) buf)))
    (kill-buffer buf)
    (should (memq timer timer-list))
    (apply (timer--function timer) (timer--args timer))
    (should-not called)
    (should-not (memq timer timer-list))))

(ert-deftest test-takuzu-ui-refresh-timer-self-cancels-when-buffer-killed ()
  "Error: a refresh tick on a killed buffer cancels its own timer.
Covers the missed-cleanup path (kill outside the kill-buffer hook): the
tick must not silently reschedule forever against a dead buffer."
  (let ((buf (get-buffer-create " *takuzu-refresh-test*"))
        timer)
    (with-current-buffer buf
      (takuzu-mode)
      (remove-hook 'kill-buffer-hook #'takuzu--cleanup t)
      (takuzu--start-refresh-timer buf)
      (setq timer takuzu--timer))
    (kill-buffer buf)
    (should (memq timer timer-list))
    (apply (timer--function timer) (timer--args timer))
    (should-not (memq timer timer-list))))

(ert-deftest test-takuzu-ui-event-timer-self-cancels-when-buffer-killed ()
  "Error: an event pulse on a killed buffer cancels its own timer.
Same missed-cleanup hardening as the refresh tick -- the pulse timer
must expire with its buffer, not outlive it."
  (let ((buf (get-buffer-create " *takuzu-event-test*"))
        timer)
    (with-current-buffer buf
      (takuzu-mode)
      (remove-hook 'kill-buffer-hook #'takuzu--cleanup t)
      (takuzu--signal-event 'invalid)
      (setq timer takuzu--event-timer))
    (kill-buffer buf)
    (should (memq timer timer-list))
    (apply (timer--function timer) (timer--args timer))
    (should-not (memq timer timer-list))))

(ert-deftest test-takuzu-ui-check-win-freezes-elapsed-at-win ()
  "Error: winning records the elapsed time at the win, not a stale zero.
Flipping the won flag before reading the clock makes `takuzu--elapsed'
return the old frozen value, so every win reported 0:00."
  (test-takuzu-ui--with-buffer
    (test-takuzu-ui--setup-4 (copy-sequence test-takuzu-ui--solution-4))
    (setq takuzu--start-time (time-subtract (current-time) (seconds-to-time 42)))
    (takuzu--check-win)
    (should takuzu--won)
    (should (= takuzu--won-elapsed 42))))

(ert-deftest test-takuzu-ui-prove-freezes-elapsed-at-reveal ()
  "Error: proving records the elapsed time at the reveal, not a stale zero."
  (test-takuzu-ui--with-buffer
    (test-takuzu-ui--setup-4)
    (setq takuzu--start-time (time-subtract (current-time) (seconds-to-time 42)))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
      (takuzu-prove))
    (should takuzu--proven)
    (should (= takuzu--won-elapsed 42))))

(ert-deftest test-takuzu-ui-nixie-size-single-digit-ghost ()
  "Normal: single-digit sizes show a ghost 0 in the tens tube.
An empty dark tube next to the lit digit reads as a dead socket."
  (let ((svg (svg-create 100 60)))
    (takuzu--draw-nixie-size svg 50 10 8)
    (should (member "0" (mapcar #'dom-text (dom-by-tag svg 'text))))))

(ert-deftest test-takuzu-ui-legend-renders-in-caps ()
  "Normal: the control legends render in caps, matching the panel labels.
Lowercase words were the one typographic outlier on the faceplate."
  (with-temp-buffer
    (setq takuzu--armed nil takuzu--assist nil)
    (let ((svg (svg-create 600 100)))
      (takuzu--draw-legend svg 0 20 500)
      (let ((texts (dom-by-tag svg 'text)))
        (should texts)
        (dolist (node texts)
          (let ((s (dom-text node)))
            (should (string= s (upcase s)))))))))

(ert-deftest test-takuzu-ui-on-generated-failure ()
  "Error: a failed background generation reports and rearms cleanly.
The spinner stops, the generating flag clears, and the GEN FAIL lamp fires
so the player knows to press n again."
  (test-takuzu-ui--with-buffer
    (setq takuzu--size 4 takuzu--generating '(:size 4 :difficulty easy)
          takuzu--spinner 2 takuzu--gen-process nil
          takuzu--board (takuzu-make-board 4) takuzu--status "")
    (takuzu--on-generated (current-buffer) nil)
    (should-not takuzu--generating)
    (should-not (timerp takuzu--spinner-timer))
    (should (eq takuzu--event 'gen-fail))
    (should (string-prefix-p "Generation failed" takuzu--status))))

(ert-deftest test-takuzu-ui-on-generated-cancelled-is-silent ()
  "Error: a cancelled generation neither reports failure nor clobbers state.
Cycling size or level cancels the old child and starts a new one; the old
sentinel's callback must not flash GEN FAIL on the re-armed buffer, and it
must not nil out the new in-flight process reference."
  (test-takuzu-ui--with-buffer
    (setq takuzu--size 4 takuzu--generating '(:size 4 :difficulty easy)
          takuzu--gen-process 'placeholder-for-new-process
          takuzu--board (takuzu-make-board 4) takuzu--status "" takuzu--event nil)
    (takuzu--on-generated (current-buffer) 'cancelled)
    (should (eq takuzu--gen-process 'placeholder-for-new-process))
    (should takuzu--generating)
    (should-not (eq takuzu--event 'gen-fail))
    (should (string-empty-p takuzu--status))))

(ert-deftest test-takuzu-ui-panel-min-height ()
  "Boundary: every board size leaves the panel at least its minimum height.
At small sizes the board alone is shorter than the instrument stack, and the
instruments overprint each other unless the stage grows to fit them."
  (with-temp-buffer
    (dolist (n '(4 6 8 10 12))
      (setq takuzu--size n)
      (should (>= (- (takuzu--stage-bottom) (takuzu--panel-top))
                  takuzu--panel-min-h)))))

(ert-deftest test-takuzu-ui-event-pulse-lifecycle ()
  "Normal: the pulse keeps a fresh event alive and clears an expired one."
  (with-temp-buffer
    (setq takuzu--event 'invalid takuzu--event-time (current-time))
    (takuzu--event-pulse (current-buffer))
    (should (eq takuzu--event 'invalid))
    (setq takuzu--event-time
          (time-subtract (current-time) (seconds-to-time (+ takuzu--event-dur 1))))
    (takuzu--event-pulse (current-buffer))
    (should-not takuzu--event)
    (should-not takuzu--event-time)))

(ert-deftest test-takuzu-ui-event-intensity ()
  "Boundary: intensity is 0 with no event, ~0 at ignition, ~1 at half period."
  (with-temp-buffer
    (setq takuzu--event-time nil)
    (should (= (takuzu--event-intensity) 0))
    (let ((t0 (current-time)))
      (setq takuzu--event-time t0)
      (cl-letf (((symbol-function 'current-time) (lambda () t0)))
        (should (< (takuzu--event-intensity) 0.01)))
      (cl-letf (((symbol-function 'current-time)
                 (lambda () (time-add t0 (seconds-to-time 1.4)))))
        (should (> (takuzu--event-intensity) 0.99))))))

;; --- instrument draw helpers (headless DOM builds) ---

(ert-deftest test-takuzu-ui-draw-nixie-time ()
  "Normal: the nixie clock draws for a running and a finished game."
  (with-temp-buffer
    (setq takuzu--won nil takuzu--proven nil
          takuzu--start-time (time-subtract (current-time) (seconds-to-time 83)))
    (let ((svg (svg-create 200 60)))
      (takuzu--draw-nixie-time svg 100 10)
      (should (eq (car svg) 'svg)))
    (setq takuzu--won t takuzu--won-elapsed 754)
    (let ((svg (svg-create 200 60)))
      (takuzu--draw-nixie-time svg 100 10)
      (should (eq (car svg) 'svg)))))

(ert-deftest test-takuzu-ui-draw-rotary-level ()
  "Normal: the LEVEL chicken-head lever aims at each level's angle.
The lever is the indicator, so its rotate transform must carry the
selected level's angle; grade falls back to difficulty when unset."
  (with-temp-buffer
    (dolist (spec '((easy . -46) (medium . 0) (hard . 46)))
      (setq takuzu--grade (car spec) takuzu--difficulty (car spec))
      (let ((svg (svg-create 120 120)))
        (takuzu--draw-rotary-level svg 60 60)
        (let ((lever (car (dom-by-tag svg 'path))))
          (should lever)
          (should (equal (dom-attr lever 'transform)
                         (format "rotate(%d 60 60)" (cdr spec)))))))
    (setq takuzu--grade nil takuzu--difficulty 'medium)
    (let ((svg (svg-create 120 120)))
      (takuzu--draw-rotary-level svg 60 60)
      (should (equal (dom-attr (car (dom-by-tag svg 'path)) 'transform)
                     "rotate(0 60 60)")))))

(ert-deftest test-takuzu-ui-draw-needle-gauge ()
  "Boundary: the needle gauge draws at empty, mid, and full sweep."
  (dolist (pct '(0 62.5 100))
    (let ((svg (svg-create 120 120)))
      (takuzu--draw-needle-gauge svg 60 60 26 pct 7)
      (should (eq (car svg) 'svg)))))

(ert-deftest test-takuzu-ui-draw-state-lamps ()
  "Normal: the STATE lamp group draws exactly READY/SOLVING/SOLVED.
SHOWN and ASSIST are gone: a proven board is a failed solve (red SOLVED),
and assist lives in the strip under the board."
  (with-temp-buffer
    (dolist (spec '((ready . ((:size 4 :difficulty easy) nil nil))
                    (solving . (nil nil nil))
                    (solved . (nil t nil))
                    (solved . (nil nil t))))
      (pcase-let ((`(,armed ,won ,proven) (cdr spec)))
        (setq takuzu--armed armed takuzu--won won takuzu--proven proven)
        (should (eq (takuzu--game-state) (car spec)))
        (let ((svg (svg-create 200 200)))
          (takuzu--draw-state-lamps svg 8 8 160 86)
          (let ((texts (mapcar #'dom-text (dom-by-tag svg 'text))))
            ;; STATE header + one label per lamp, no retired labels
            (should (= (length texts) 4))
            (should-not (member "SHOWN" texts))
            (should-not (member "ASSIST" texts))))))))

(ert-deftest test-takuzu-ui-state-lamps-solved-color ()
  "Normal: SOLVED lights green on a win and red on a proven (failed) board."
  (with-temp-buffer
    (setq takuzu--armed nil takuzu--won t takuzu--proven nil)
    (let ((solved (assoc "SOLVED" (takuzu--state-lamps))))
      (should (nth 2 solved))
      (should (equal (nth 1 solved) (takuzu--c :lamp-green))))
    (setq takuzu--won nil takuzu--proven t)
    (let ((solved (assoc "SOLVED" (takuzu--state-lamps))))
      (should (nth 2 solved))
      (should (equal (nth 1 solved) (takuzu--c :fail))))))

(ert-deftest test-takuzu-ui-state-lamps-solving-flashes ()
  "Normal: the SOLVING lamp follows the flash cycle mid-game, off once solved."
  (with-temp-buffer
    (setq takuzu--armed nil takuzu--won nil takuzu--proven nil)
    (cl-letf (((symbol-function 'takuzu--flash-on-p) (lambda () t)))
      (should (nth 2 (assoc "SOLVING" (takuzu--state-lamps)))))
    (cl-letf (((symbol-function 'takuzu--flash-on-p) (lambda () nil)))
      (should-not (nth 2 (assoc "SOLVING" (takuzu--state-lamps)))))
    (setq takuzu--won t)
    (cl-letf (((symbol-function 'takuzu--flash-on-p) (lambda () t)))
      (should-not (nth 2 (assoc "SOLVING" (takuzu--state-lamps)))))))

(ert-deftest test-takuzu-ui-legend-assist-lit ()
  "Normal: assist mode lights the ASSIST legend word in the strip under the board."
  (test-takuzu-ui--with-buffer
    (test-takuzu-ui--setup-4)
    (setq takuzu--assist nil)
    (let ((off (let ((svg (takuzu--svg)))
                 (with-temp-buffer (svg-print svg) (buffer-string)))))
      (should-not (string-match-p (takuzu--c :lamp-cyan) off)))
    (setq takuzu--assist t)
    (let ((on (let ((svg (takuzu--svg)))
                (with-temp-buffer (svg-print svg) (buffer-string)))))
      (should (string-match-p (takuzu--c :lamp-cyan) on)))))

(ert-deftest test-takuzu-ui-event-intensity-invalid-holds ()
  "Boundary: the INVALID lamp holds full brightness for its hold window, then dims.
Craig's spec: stay lit two seconds, then dim -- not the breathing pulse the
other events use."
  (with-temp-buffer
    (let ((t0 (current-time)))
      (setq takuzu--event 'invalid takuzu--event-time t0)
      (cl-letf (((symbol-function 'current-time)
                 (lambda () (time-add t0 0.1))))
        (should (= (takuzu--event-intensity) 1.0)))
      (cl-letf (((symbol-function 'current-time)
                 (lambda () (time-add t0 (- takuzu--invalid-hold 0.1)))))
        (should (= (takuzu--event-intensity) 1.0)))
      (cl-letf (((symbol-function 'current-time)
                 (lambda () (time-add t0 (+ takuzu--invalid-hold
                                            (* 0.5 takuzu--invalid-fade))))))
        (let ((k (takuzu--event-intensity)))
          (should (< 0.3 k 0.7))))
      (cl-letf (((symbol-function 'current-time)
                 (lambda () (time-add t0 (+ takuzu--invalid-hold
                                            takuzu--invalid-fade 0.1)))))
        (should (= (takuzu--event-intensity) 0))))))

(ert-deftest test-takuzu-ui-event-pulse-invalid-expiry ()
  "Boundary: the invalid pulse survives past the breathing duration, then clears.
The hold-plus-fade window is longer than the one-breath duration other
events get, so the pulse timer must not cut it short."
  (test-takuzu-ui--with-buffer
    (test-takuzu-ui--setup-4)
    (let ((t0 (current-time)))
      (setq takuzu--event 'invalid takuzu--event-time t0)
      (cl-letf (((symbol-function 'current-time)
                 (lambda () (time-add t0 (- takuzu--invalid-hold 0.1)))))
        (takuzu--event-pulse (current-buffer))
        (should (eq takuzu--event 'invalid)))
      (cl-letf (((symbol-function 'current-time)
                 (lambda () (time-add t0 (+ takuzu--invalid-hold
                                            takuzu--invalid-fade 0.2)))))
        (takuzu--event-pulse (current-buffer))
        (should (null takuzu--event))))))

(ert-deftest test-takuzu-ui-draw-event-annunciator ()
  "Normal: the annunciator draws six legend cells; the active one lights up."
  (with-temp-buffer
    (setq takuzu--size 4)
    ;; event mid-breath, so the active lamp is near peak brightness
    (setq takuzu--event 'invalid
          takuzu--event-time (time-subtract (current-time)
                                            (seconds-to-time (/ takuzu--event-breath 2))))
    (let ((svg (svg-create 600 60)))
      (takuzu--draw-event-annunciator svg 10 10 (takuzu--strip-width) takuzu--event-h)
      ;; strip background + six legend cells; the lit cell's fill stands out
      (should (= (length (dom-by-tag svg 'rect)) 7))
      (should (= (length (dom-by-tag svg 'text)) 6))
      (let ((fills (mapcar (lambda (r) (dom-attr r 'fill))
                           (cdr (dom-by-tag svg 'rect)))))
        (should (= (length (delete-dups (copy-sequence fills))) 2))))
    (setq takuzu--event nil takuzu--event-time nil)
    (let ((svg (svg-create 600 60)))
      (takuzu--draw-event-annunciator svg 10 10 (takuzu--strip-width) takuzu--event-h)
      ;; with no active event every cell wears the same idle fill
      (let ((fills (mapcar (lambda (r) (dom-attr r 'fill))
                           (cdr (dom-by-tag svg 'rect)))))
        (should (= (length (delete-dups (copy-sequence fills))) 1))))))

(ert-deftest test-takuzu-ui-draw-jewel ()
  "Normal: the jewel lamp draws lit and unlit."
  (dolist (on '(t nil))
    (let ((svg (svg-create 40 40)))
      (takuzu--draw-jewel svg 20 20 6 "#6fce33" on)
      (should (eq (car svg) 'svg)))))

(provide 'test-takuzu-ui)
;;; test-takuzu-ui.el ends here
