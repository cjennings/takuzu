;;; test-takuzu-ui.el --- Tests for takuzu-ui -*- lexical-binding: t -*-

;;; Commentary:
;; The pure helpers and game-action logic are unit-tested directly.  The SVG
;; faceplate is exercised through `takuzu--svg' (it builds a DOM with no display
;; needed), so the draw helpers run in batch; their pixels are verified visually.

;;; Code:

(require 'ert)
(require 'takuzu)
(require 'testutil-takuzu)

;; Redirect stats writes for the whole batch: the win/prove tests record
;; results, and without this every suite and hook run would write the
;; developer's real stats file.
(setq takuzu-stats-file (make-temp-file "takuzu-stats-suite-" nil ".eld"))

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
  "Normal/Boundary: seconds format as MM:SS from the very first second,
pegging at 99:99 once the display runs out of digits."
  (should (equal (takuzu--fmt-time 0) "00:00"))
  (should (equal (takuzu--fmt-time 9) "00:09"))
  (should (equal (takuzu--fmt-time 75) "01:15"))
  (should (equal (takuzu--fmt-time 600) "10:00"))
  (should (equal (takuzu--fmt-time 5999) "99:59"))
  (should (equal (takuzu--fmt-time 6000) "99:99"))
  (should (equal (takuzu--fmt-time 999999) "99:99")))

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
  "Boundary: refresh interval is clamped to [0.1, 0.15] for a smooth breath."
  (let ((takuzu-flash-period 1.0)) (should (= (takuzu--refresh-interval) 0.15)))
  (let ((takuzu-flash-period 8.0)) (should (= (takuzu--refresh-interval) 0.15)))
  (let ((takuzu-flash-period 0.1)) (should (= (takuzu--refresh-interval) 0.1))))

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

(ert-deftest test-takuzu-ui-render-text-marks-rule-breaks-with-assist ()
  "Normal: assist on, the text fallback wears the error face on a broken row."
  (with-temp-buffer
    (test-takuzu-ui--setup-4 (vector 0 0 0 nil nil nil nil nil
                                     nil nil nil nil nil nil nil nil))
    (setq takuzu--assist t)
    (let* ((out (takuzu--render-text))
           (first-glyph (string-match (takuzu--glyph 0) out)))
      (should first-glyph)
      (should (eq (get-text-property first-glyph 'face out) 'error))
      ;; a cell on a clean row stays unfaced (row 0 is marked whole,
      ;; empties included, so probe past its newline)
      (let ((clean (string-match (regexp-quote (takuzu--glyph nil)) out
                                 (string-match "\n" out))))
        (should clean)
        (should (null (get-text-property clean 'face out)))))))

(ert-deftest test-takuzu-ui-render-text-no-face-with-assist-off ()
  "Boundary: assist off, the same broken board renders with no error faces."
  (with-temp-buffer
    (test-takuzu-ui--setup-4 (vector 0 0 0 nil nil nil nil nil
                                     nil nil nil nil nil nil nil nil))
    (setq takuzu--assist nil)
    (let* ((out (takuzu--render-text))
           (first-glyph (string-match (takuzu--glyph 0) out)))
      (should (null (get-text-property first-glyph 'face out))))))

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
    (test-takuzu-ui--setup-4 (vector 0 0 nil nil  nil nil nil nil
                                     nil nil nil nil  nil nil nil nil))
    (setq takuzu--cursor '(3 . 3))
    (takuzu-hint)
    (should (eql (takuzu-board-ref takuzu--board 0 2) 1))
    (should (string-match-p "forced" takuzu--status))))

(ert-deftest test-takuzu-ui-hint-escalates-to-solution ()
  "Normal: with no logic-derivable cell, hint fills from the solution and says so.
An empty board has no forced cell and no hypothesis-resolvable cell, so the
hint's last tier answers with an honest from-the-solution label."
  (test-takuzu-ui--with-buffer
    (test-takuzu-ui--setup-4)
    (setq takuzu--cursor '(3 . 3))
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

(ert-deftest test-takuzu-ui-digit-size ()
  "Normal/Boundary: digit keys map to board sizes; 1 walks 10 -> 12 -> 10.
Direct digits pick their size; 1 lands on 10 first, a second press advances
to 12, and after that it toggles between the two; other digits map to nil."
  (with-temp-buffer
    (setq takuzu--size 6)
    (should (= (takuzu--digit-size ?4) 4))
    (should (= (takuzu--digit-size ?6) 6))
    (should (= (takuzu--digit-size ?8) 8))
    (should (= (takuzu--digit-size ?1) 10))
    (setq takuzu--size 10)
    (should (= (takuzu--digit-size ?1) 12))
    (setq takuzu--size 12)
    (should (= (takuzu--digit-size ?1) 10))
    (should-not (takuzu--digit-size ?5))
    (should-not (takuzu--digit-size ?0))))

(ert-deftest test-takuzu-ui-jump-size-arms-at-digit ()
  "Normal: a digit key arms a fresh game at that size; a dead digit is a no-op."
  (test-takuzu-ui--arming
    (with-temp-buffer
      (takuzu-mode)
      (setq takuzu--size 6 takuzu--difficulty 'easy)
      (let ((last-command-event ?4))
        (takuzu-jump-size))
      (with-current-buffer "*Takuzu*"
        (should takuzu--armed)
        (should (= takuzu--size 4)))))
  (with-temp-buffer
    (takuzu-mode)
    (setq takuzu--size 6)
    (let ((last-command-event ?5))
      (takuzu-jump-size))
    (should (= takuzu--size 6))))

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

(ert-deftest test-takuzu-integration-tty-full-game-flow ()
  "Integration: the text fallback carries a full game end to end.

Components integrated:
- takuzu-ui-arm and takuzu--begin-play (real)
- movement, cycle, undo, hint, check, assist, reset, prove commands (real)
- takuzu--redraw textual path (real -- batch Emacs has no display)
- takuzu-generate (real, sync, for the puzzle)

Validates: every core command functions with no graphics available, and the
rendered buffer text stays sensible at each step (no stray nil in the armed
header, grade + clock present in play, cursor visible, coins render)."
  (skip-unless (not (display-graphic-p)))
  (test-takuzu-ui--arming
    (takuzu-ui-arm 4 'easy)
    (with-current-buffer "*Takuzu*"
      ;; armed: header must not print a nil grade
      (should-not (string-match-p "nil" (buffer-string)))
      (should (string-match-p "Press n to begin" (buffer-string)))
      ;; begin play with a real generated puzzle
      (takuzu--begin-play (current-buffer) (takuzu-generate 4 'easy))
      (should (string-match-p "Takuzu  4x4" (buffer-string)))
      (should (string-match-p "[0-9][0-9]:[0-9][0-9]" (buffer-string)))
      (should (string-match-p "\\[.\\]" (buffer-string)))
      ;; movement moves the rendered cursor
      (let ((before (buffer-string)))
        (takuzu-right)
        (should-not (equal (buffer-string) before)))
      ;; cycle places a coin at the cursor, undo takes it back -- park the
      ;; cursor on the first non-given cell, wherever this puzzle put one
      (cl-block park
        (dotimes (rr 4)
          (dotimes (cc 4)
            (unless (takuzu-board-given-p takuzu--board rr cc)
              (setq takuzu--cursor (cons rr cc))
              (cl-return-from park)))))
      (let ((r (car takuzu--cursor)) (c (cdr takuzu--cursor)))
        (takuzu-cycle)
        (should (takuzu-board-ref takuzu--board r c))
        (takuzu-undo)
        (should-not (takuzu-board-ref takuzu--board r c)))
      ;; hint, check, assist, reset all function and report
      (takuzu-hint)
      (should (string-match-p "forced\\|Hypothesis\\|solution" takuzu--status))
      (takuzu-check)
      (should-not (string-empty-p takuzu--status))
      (takuzu-toggle-assist)
      (should takuzu--assist)
      (takuzu-reset)
      (should (cl-every (lambda (i)
                          (or (aref (takuzu-board-givens takuzu--board) i)
                              (null (aref (takuzu-board-cells takuzu--board) i))))
                        (number-sequence 0 15)))
      ;; prove fills the whole board from the solution (confirmation mocked)
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
        (takuzu-prove))
      (should takuzu--proven)
      (should (takuzu-board-full-p takuzu--board))
      (should (string-match-p "Solution shown" (buffer-string))))))

(ert-deftest test-takuzu-ui-tty-legend-derives-from-table ()
  "Normal: the tty key legend is generated from the shared legend table.
One data table drives the SVG legend and the tty fallback, so a key added
to one surface cannot silently miss the other."
  (with-temp-buffer
    (let ((line (takuzu--tty-legend)))
      (should (string-prefix-p "arrows move" line))
      (dolist (frag '("SPC cycle" "u undo" "? hint" "c check" "a assist"
                      "n new" "r reset" "s size" "l level" "p prove"
                      "i instructions" "q quit"))
        (should (string-search frag line))))))

(ert-deftest test-takuzu-ui-draw-socket ()
  "Normal/Error: one socket renders its cup, disc, cursor, and error stroke."
  (with-temp-buffer
    (setq takuzu--size 4 takuzu--cursor '(0 . 0)
          takuzu--board (takuzu-make-board 4 (vector 0 nil nil nil
                                                     nil nil nil nil
                                                     nil nil nil nil
                                                     nil nil nil nil)))
    ;; cursor cell with a disc: cup rect + bezel + disc circles
    (let ((svg (svg-create 100 100)))
      (takuzu--draw-socket svg 10 10 50 0 0 nil)
      (should (dom-by-tag svg 'rect))
      (should (dom-by-tag svg 'circle)))
    ;; error stroke: the cup rect carries the fail colour (non-cursor cell,
    ;; so the cup is the only rect drawn)
    (let ((svg (svg-create 100 100)))
      (takuzu--draw-socket svg 10 10 50 0 1 t)
      (let ((cup (car (dom-by-tag svg 'rect))))
        (should (equal (dom-attr cup 'stroke) (takuzu--c :fail)))))
    ;; empty non-cursor cell: no disc
    (let ((svg (svg-create 100 100)))
      (takuzu--draw-socket svg 10 10 50 1 1 nil)
      (should-not (dom-by-tag svg 'circle)))))

(ert-deftest test-takuzu-ui-coin-fills-more-of-its-socket ()
  "Normal: a coin occupies 37 percent of its socket's width as its radius.
The larger face makes the device read more clearly while preserving a generous
rim of the existing recess, cursor, and fixed-state markings."
  (with-temp-buffer
    (setq takuzu--size 4
          takuzu--board (takuzu-make-board 4 (vector 0 nil nil nil
                                                       nil nil nil nil
                                                       nil nil nil nil
                                                       nil nil nil nil)))
    (let (radius)
      (cl-letf (((symbol-function 'takuzu--draw-disc)
                 (lambda (_svg _cx _cy r _val _given) (setq radius r))))
        (takuzu--draw-socket (svg-create 100 100) 10 10 50 0 0 nil))
      (should (= radius 18)))))

(ert-deftest test-takuzu-ui-html-coin-radius-matches-emacs ()
  "Error: the HTML mirror must use the same coin-radius multiplier as Emacs."
  (let* ((root (locate-dominating-file default-directory "Makefile"))
         (html (with-temp-buffer
                 (insert-file-contents
                  (expand-file-name "docs/prototypes/takuzu-hifi.html" root))
                 (buffer-string))))
    (should (string-match-p "Math.round(cell \\* 0.37)" html))))

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
  "Normal/Boundary: the jewel draws spent, mid-breath, and full without error.
The dark off-jewel is always laid down first; the lit overlay only appears
once INTENSITY clears the near-zero threshold."
  (dolist (intensity '(0.0 0.35 0.5 1.0))
    (let ((svg (svg-create 40 40)))
      (takuzu--draw-jewel svg 20 20 6 "#6fce33" intensity)
      (should (eq (car svg) 'svg))))
  ;; a spent lamp draws only the off-jewel; a lit one adds the glow overlay
  (let ((dark (svg-create 40 40)) (lit (svg-create 40 40)))
    (takuzu--draw-jewel dark 20 20 6 "#6fce33" 0.0)
    (takuzu--draw-jewel lit 20 20 6 "#6fce33" 1.0)
    (should (< (length (dom-by-tag dark 'circle))
               (length (dom-by-tag lit 'circle))))))

;; --- stats wiring ---

(ert-deftest test-takuzu-ui-check-win-records-win ()
  "Normal: completing the board records a win for the puzzle's size and grade."
  (takuzu-testutil-with-stats-file
    (test-takuzu-ui--with-buffer
      (test-takuzu-ui--setup-4 (copy-sequence test-takuzu-ui--solution-4))
      (setq takuzu--grade 'easy)
      (takuzu--check-win)
      (should takuzu--won)
      (let ((entry (takuzu-stats-entry (takuzu-stats-load) 4 'easy)))
        (should (= (plist-get entry :wins) 1))
        (should (numberp (plist-get entry :best)))))))

(ert-deftest test-takuzu-ui-check-win-no-win-records-nothing ()
  "Boundary: an unsolved board records nothing."
  (takuzu-testutil-with-stats-file
    (test-takuzu-ui--with-buffer
      (test-takuzu-ui--setup-4)
      (setq takuzu--grade 'easy)
      (takuzu--check-win)
      (should-not takuzu--won)
      (should (null (takuzu-stats-load))))))

(ert-deftest test-takuzu-ui-prove-records-loss ()
  "Normal: proving the board records a loss and never a best time."
  (takuzu-testutil-with-stats-file
    (test-takuzu-ui--with-buffer
      (test-takuzu-ui--setup-4)
      (setq takuzu--grade 'medium)
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
        (takuzu-prove))
      (should takuzu--proven)
      (let ((entry (takuzu-stats-entry (takuzu-stats-load) 4 'medium)))
        (should (= (plist-get entry :losses) 1))
        (should (= (plist-get entry :wins) 0))
        (should (null (plist-get entry :best)))))))

(ert-deftest test-takuzu-ui-prove-declined-records-nothing ()
  "Boundary: declining the prove prompt records nothing."
  (takuzu-testutil-with-stats-file
    (test-takuzu-ui--with-buffer
      (test-takuzu-ui--setup-4)
      (setq takuzu--grade 'medium)
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) nil)))
        (takuzu-prove))
      (should-not takuzu--proven)
      (should (null (takuzu-stats-load))))))

(ert-deftest test-takuzu-ui-stats-summary ()
  "Normal/Boundary: the summary names the current key and overall totals;
with no games recorded it says so."
  (takuzu-testutil-with-stats-file
    (test-takuzu-ui--with-buffer
      (test-takuzu-ui--setup-4)
      (setq takuzu--grade 'hard)
      (should (string-match-p "No games recorded" (takuzu--stats-summary)))
      (takuzu-stats-record 4 'hard 'win 75)
      (takuzu-stats-record 4 'hard 'loss 30)
      (takuzu-stats-record 6 'easy 'win 10)
      (let ((s (takuzu--stats-summary)))
        (should (string-match-p "4x4 hard" s))
        (should (string-match-p "1W 1L" s))
        (should (string-match-p "01:15" s))
        (should (string-match-p "2W 1L" s))))))

(ert-deftest test-takuzu-ui-stats-summary-no-grade-overall-only ()
  "Boundary: with no grade yet (armed, nothing generated) only the overall tally shows."
  (takuzu-testutil-with-stats-file
    (test-takuzu-ui--with-buffer
      (test-takuzu-ui--setup-4)
      (setq takuzu--grade nil takuzu--difficulty nil)
      (takuzu-stats-record 6 'easy 'win 10)
      (let ((s (takuzu--stats-summary)))
        (should (string-match-p "overall 1W 0L" s))
        (should-not (string-match-p "nil" s))))))

(ert-deftest test-takuzu-ui-stats-summary-unplayed-key ()
  "Boundary: games on other keys still summarize; the current key shows 0W 0L."
  (takuzu-testutil-with-stats-file
    (test-takuzu-ui--with-buffer
      (test-takuzu-ui--setup-4)
      (setq takuzu--grade 'easy)
      (takuzu-stats-record 12 'hard 'loss 5)
      (let ((s (takuzu--stats-summary)))
        (should (string-match-p "4x4 easy" s))
        (should (string-match-p "0W 0L" s))
        (should (string-match-p "0W 1L" s))))))

;; --- coin skins ---

(ert-deftest test-takuzu-ui-coin-skin-default-and-set ()
  "Normal: the skin defcustom defaults to the drum's head; order is stable."
  (should (eq (eval (car (get 'takuzu-coin-skin 'standard-value)))
              (car takuzu--coin-skins)))
  (should (equal takuzu--coin-skins '(wood terra collegiate gestell))))

(ert-deftest test-takuzu-ui-reset-returns-drum-to-head ()
  "Normal: r (refresh) turns the coin drum back to coinset 1."
  (test-takuzu-ui--with-buffer
    (test-takuzu-ui--setup-4)
    (let ((takuzu-coin-skin 'terra))
      (takuzu-reset)
      (should (eq takuzu-coin-skin (car takuzu--coin-skins))))))

(ert-deftest test-takuzu-ui-wood-lip-marks-fixed ()
  "Normal: every wood coin is a flat matte one-tone disc -- all coal
for 0, all beech for 1, no gradient field, no sheen, no heart.  Both
coins wear a lip: thin but noticeable on a user coin, noticeably
thicker and prominent on a FIXED coin."
  (let ((takuzu-coin-skin 'wood))
    (let ((c0 (svg-create 100 100)) (c1 (svg-create 100 100))
          (f0 (svg-create 100 100)) (f1 (svg-create 100 100)))
      (takuzu--draw-disc c0 50 50 33 0 nil)
      (takuzu--draw-disc c1 50 50 33 1 nil)
      (takuzu--draw-disc f0 50 50 33 0 t)
      (takuzu--draw-disc f1 50 50 33 1 t)
      ;; flat matte: the body is a solid tone of its own wood -- no
      ;; gradient fill anywhere on the coin
      (dolist (pair (list (cons c0 'coal) (cons f0 'coal)
                          (cons c1 'beech) (cons f1 'beech)))
        (should (seq-find (lambda (n)
                            (equal (dom-attr n 'fill)
                                   (takuzu--metal (cdr pair) 2)))
                          (dom-by-tag (car pair) 'circle)))
        (should-not (seq-find (lambda (n)
                                (let ((fill (dom-attr n 'fill)))
                                  (and (stringp fill)
                                       (string-prefix-p "url(#m-" fill))))
                              (dom-by-tag (car pair) 'circle))))
      ;; rims on every coin are flat bands of the same wood one step
      ;; lighter -- never the struck gradient: thin on user, wide on
      ;; fixed, and floored in pixels so the fixed rim survives 12x12
      (let ((band-width
             (lambda (svg wood)
               (dom-attr (seq-find (lambda (n)
                                     (equal (dom-attr n 'stroke)
                                            (takuzu--metal wood 1)))
                                   (dom-by-tag svg 'circle))
                         'stroke-width))))
        (dolist (svg (list c0 c1 f0 f1))
          (should-not (seq-find (lambda (n)
                                  (let ((s (dom-attr n 'stroke)))
                                    (and (stringp s)
                                         (string-prefix-p "url(#m-" s))))
                                (dom-by-tag svg 'circle))))
        (dolist (pair (list (cons c0 'coal) (cons c1 'beech)))
          (let ((w (funcall band-width (car pair) (cdr pair))))
            (should w)
            (should (<= w (* 33 0.08)))))
        (dolist (case (list (list f0 c0 'coal) (list f1 c1 'beech)))
          (let ((fw (funcall band-width (nth 0 case) (nth 2 case)))
                (uw (funcall band-width (nth 1 case) (nth 2 case))))
            (should (>= fw (* 33 0.18)))
            (should (>= fw (* uw 3)))))
        ;; the pixel floor: a 12x12-scale fixed rim never drops below 4px
        (let ((tiny (svg-create 100 100)))
          (takuzu--draw-disc tiny 50 50 10 0 t)
          (should (>= (funcall band-width tiny 'coal) 4))))
      ;; no hole anywhere, no pin anywhere, and matte -- no specular sheen
      (dolist (svg (list c0 c1 f0 f1))
        (should-not (seq-find (lambda (n)
                                (equal (dom-attr n 'fill) (takuzu--c :socket)))
                              (dom-by-tag svg 'circle)))
        (should-not (seq-find (lambda (n)
                                (or (equal (dom-attr n 'fill) (takuzu--metal 'coal 1))
                                    (equal (dom-attr n 'fill) (takuzu--metal 'sunflower 1))))
                              (dom-by-tag svg 'circle)))
        (should (= (length (dom-by-tag svg 'ellipse)) 0))))))

(ert-deftest test-takuzu-ui-cycle-skin-cycles ()
  "Normal: the skin command walks the whole list and wraps back around."
  (test-takuzu-ui--with-buffer
    (test-takuzu-ui--setup-4)
    (let ((takuzu-coin-skin 'wood))
      (takuzu-cycle-skin)
      (should (eq takuzu-coin-skin 'terra))
      (dotimes (_ (1- (length takuzu--coin-skins)))
        (takuzu-cycle-skin))
      (should (eq takuzu-coin-skin 'wood)))))

(ert-deftest test-takuzu-ui-cycle-skin-back-walks-and-wraps ()
  "Normal/Boundary: W walks the drum backward and wraps past the head."
  (test-takuzu-ui--with-buffer
    (test-takuzu-ui--setup-4)
    (should (eq (keymap-lookup takuzu-mode-map "w") 'takuzu-cycle-skin))
    (should (eq (keymap-lookup takuzu-mode-map "W") 'takuzu-cycle-skin-back))
    (let ((takuzu-coin-skin 'wood))
      (takuzu-cycle-skin-back)
      (should (eq takuzu-coin-skin 'gestell))
      (dotimes (_ (1- (length takuzu--coin-skins)))
        (takuzu-cycle-skin-back))
      (should (eq takuzu-coin-skin 'wood)))))

(ert-deftest test-takuzu-ui-every-skin-has-a-drawer ()
  "Normal: every skin in the cycle list resolves to a draw function.
A skin added to the list without a drawer would silently fall back to terra."
  (dolist (skin takuzu--coin-skins)
    (should (functionp (takuzu--coin-skin-drawer skin)))))

(ert-deftest test-takuzu-ui-metal-skins-have-pairs ()
  "Normal: every registry entry with metals names them from the tone table."
  (dolist (entry takuzu--coin-skin-registry)
    (should (functionp (nth 1 entry)))
    (dolist (m (cddr entry))
      (when m (should (assq m takuzu--coin-metal-tones))))))

(ert-deftest test-takuzu-ui-all-skins-draw-both-scales ()
  "Normal/Boundary: every skin draws both colours, fixed and placed, at 2x
and at board scale, without error and with visible shapes."
  (dolist (skin takuzu--coin-skins)
    (let ((takuzu-coin-skin skin))
      (dolist (r '(16 33))
        (dolist (given '(nil t))
          (let ((svg (svg-create 100 100))
                (before 0))
            (setq before (length (dom-children svg)))
            (takuzu--draw-disc svg 50 50 r 0 given)
            (takuzu--draw-disc svg 50 50 r 1 given)
            (should (> (length (dom-children svg)) before))))))))

(ert-deftest test-takuzu-ui-draw-disc-dispatches-by-skin ()
  "Normal: each skin draws its signature shapes through the one entry point."
  (dolist (case '((terra . ((radialGradient . 0) (polygon . 0)))))
    (let ((takuzu-coin-skin (car case))
          (svg (svg-create 100 100)))
      (takuzu--draw-disc svg 50 50 33 0 nil)
      (dolist (want (cdr case))
        (should (= (length (dom-by-tag svg (car want))) (cdr want)))))))

(ert-deftest test-takuzu-ui-cursor-bezel-metal-matches-skin ()
  "Normal: the cursor ring is brass on the terra set, iron on every other set."
  (dolist (case '((terra . :cursor-bezel-hi)
                  (wood . :cursor-iron-hi)))
    (let ((takuzu-coin-skin (car case))
          (svg (svg-create 100 100)))
      (takuzu--draw-cursor-bezel svg 10 10 50)
      (let ((stops (dom-by-tag svg 'stop)))
        (should stops)
        (should (equal (dom-attr (car stops) 'stop-color)
                       (takuzu--c (cdr case))))))))

(ert-deftest test-takuzu-ui-skin-selector-shows-counter ()
  "Normal: the skin selector shows the tape-counter index and never a name."
  (test-takuzu-ui--with-buffer
    (test-takuzu-ui--setup-4)
    (dolist (case '((wood . "01") (terra . "02")))
      (let* ((takuzu-coin-skin (car case))
             (texts (mapcar #'dom-texts (dom-by-tag (takuzu--svg) 'text))))
        (should (member (cdr case) texts))
        (should (member "COIN" texts))
        ;; the drum shows only the index -- no skin is named on the plate
        (should-not (member (upcase (symbol-name (car case))) texts))))))

;; --- raster coin skins (gestell, collegiate) ---

(ert-deftest test-takuzu-ui-gestell-raster-faces ()
  "Normal: gestell draws value 0 as the taijitu sprite and value 1 as the
gear -- each embedded once as an <image> in <defs> and referenced by a
<use>, and the two values carry visibly different sprite data."
  (let ((takuzu-coin-skin 'gestell)
        (v0 (svg-create 100 100)) (v1 (svg-create 100 100)))
    (takuzu--draw-disc v0 50 50 33 0 nil)
    (takuzu--draw-disc v1 50 50 33 1 nil)
    (let ((img0 (car (dom-by-id v0 "^gest-0$")))
          (use0 (car (dom-by-tag v0 'use)))
          (img1 (car (dom-by-id v1 "^gest-1$")))
          (use1 (car (dom-by-tag v1 'use))))
      (should img0)
      (should img1)
      ;; the sprite is embedded, not referenced from disk
      (should (string-prefix-p "data:image/png;base64,"
                               (dom-attr img0 'xlink:href)))
      ;; the cell references the embedded sprite by id
      (should (equal (dom-attr use0 'xlink:href) "#gest-0"))
      (should (equal (dom-attr use1 'xlink:href) "#gest-1"))
      ;; value carries the face: the two sprites differ
      (should-not (equal (dom-attr img0 'xlink:href)
                         (dom-attr img1 'xlink:href))))))

(ert-deftest test-takuzu-ui-collegiate-raster-faces ()
  "Normal: collegiate draws value 0 as the Cal coin and value 1 as the
Stanford coin, each embedded once and referenced by a <use>; the two
school faces differ."
  (let ((takuzu-coin-skin 'collegiate)
        (v0 (svg-create 100 100)) (v1 (svg-create 100 100)))
    (takuzu--draw-disc v0 50 50 33 0 nil)
    (takuzu--draw-disc v1 50 50 33 1 nil)
    (let ((img0 (car (dom-by-id v0 "^col-0$")))
          (img1 (car (dom-by-id v1 "^col-1$"))))
      (should img0)
      (should img1)
      (should (string-prefix-p "data:image/png;base64,"
                               (dom-attr img0 'xlink:href)))
      (should (equal (dom-attr (car (dom-by-tag v0 'use)) 'xlink:href) "#col-0"))
      (should (equal (dom-attr (car (dom-by-tag v1 'use)) 'xlink:href) "#col-1"))
      (should-not (equal (dom-attr img0 'xlink:href)
                         (dom-attr img1 'xlink:href))))))

(ert-deftest test-takuzu-ui-raster-embeds-sprite-once ()
  "Boundary: a raster face repeated across many cells embeds the heavy
<image> a single time and references it with one lightweight <use> per
cell -- the define-once property that keeps a full board's SVG small."
  (let ((takuzu-coin-skin 'gestell)
        (svg (svg-create 400 400)))
    (dotimes (i 8)
      (takuzu--draw-disc svg (+ 20 (* i 40)) 50 16 0 nil))
    (should (= (length (dom-by-id svg "^gest-0$")) 1))
    (should (= (length (dom-by-tag svg 'use)) 8))))

(ert-deftest test-takuzu-ui-raster-skins-ignore-given ()
  "Normal: the raster themes carry no separate fixed-cell marking, so a
given cell and a placed cell of the same value render identically."
  (dolist (skin '(gestell collegiate))
    (let ((takuzu-coin-skin skin)
          (placed (svg-create 100 100)) (given (svg-create 100 100)))
      (takuzu--draw-disc placed 50 50 33 0 nil)
      (takuzu--draw-disc given  50 50 33 0 t)
      (should (equal (dom-attr (car (dom-by-tag placed 'use)) 'xlink:href)
                     (dom-attr (car (dom-by-tag given 'use)) 'xlink:href)))
      (should (= (length (dom-children placed))
                 (length (dom-children given)))))))

;; --- themed board plates ---

(ert-deftest test-takuzu-ui-skin-board-lookup ()
  "Normal: gestell carries a board plate for every size; other skins don't."
  (dolist (sz '(4 6 8 10 12))
    (let ((b (takuzu--skin-board 'gestell sz)))
      (should b)
      (should (stringp (nth 0 b)))
      (should (numberp (nth 1 b)))
      (should (numberp (nth 2 b)))))
  (dolist (skin '(wood terra collegiate))
    (should-not (takuzu--skin-board skin 8))))

(ert-deftest test-takuzu-ui-board-plate-vs-sockets-dispatch ()
  "Normal: a skin with a board plate draws one plate image at the plate block
span; a skin without draws recessed sockets and no board image."
  (test-takuzu-ui--with-buffer
    (test-takuzu-ui--setup-4)
    (let ((takuzu-coin-skin 'gestell) (svg (svg-create 600 600)))
      (takuzu--draw-board svg 0 0)
      ;; exactly one block-span image (the plate); sprite images live in defs
      (let ((plate (seq-filter (lambda (im)
                                 (equal (dom-attr im 'width) (takuzu--board-block-span 4)))
                               (dom-by-tag svg 'image))))
        (should (= (length plate) 1))
        (should (string-match-p "^data:image/[a-z]+;base64,"
                                (dom-attr (car plate) 'xlink:href)))))
    (let ((takuzu-coin-skin 'wood) (svg (svg-create 400 400)))
      (takuzu--draw-board svg 0 0)
      (should-not (dom-by-tag svg 'image))
      (should (dom-by-tag svg 'rect)))))

(ert-deftest test-takuzu-ui-board-plate-places-a-piece-per-filled-cell ()
  "Normal: in plate mode every filled cell gets a piece drawn via <use>."
  (test-takuzu-ui--with-buffer
    ;; a 4x4 with all cells filled
    (let ((cells (make-vector 16 0)))
      (test-takuzu-ui--setup-4 cells)
      (let ((takuzu-coin-skin 'gestell) (svg (svg-create 400 400)))
        (takuzu--draw-board svg 0 0)
        (should (= (length (dom-by-tag svg 'use)) 16))))))

(ert-deftest test-takuzu-ui-board-plate-cursor-and-error-cues ()
  "Normal: plate mode draws the cursor bezel on the cursor cell and rings a
flagged cell (assist), since there is no socket cup to stroke."
  (test-takuzu-ui--with-buffer
    (test-takuzu-ui--setup-4 (vector 0 0 0 nil nil nil nil nil
                                     nil nil nil nil nil nil nil nil))
    (setq takuzu--assist t takuzu--cursor '(0 . 3))
    (let ((takuzu-coin-skin 'gestell) (svg (svg-create 400 400)))
      (takuzu--draw-board svg 0 0)
      ;; the three same-colour cells in row 0 are a rule break -> a fail-stroked ring
      (should (seq-find (lambda (n) (equal (dom-attr n 'stroke) (takuzu--c :fail)))
                        (dom-by-tag svg 'rect)))
      ;; the cursor bezel emits its gradient stops
      (should (dom-by-tag svg 'stop)))))

(ert-deftest test-takuzu-ui-solving-lamp-breathes ()
  "Normal/Boundary: while solving, the SOLVING lamp breathes a smooth cosine
between a dim floor and full glow, riding wall-clock time.  It bottoms out at
`takuzu--breath-floor' and peaks at 1.0 -- never fully dark -- and it goes to
zero once the game ends and SOLVED takes over."
  (test-takuzu-ui--with-buffer
    (test-takuzu-ui--setup-4)
    (setq takuzu--armed nil takuzu--won nil takuzu--proven nil)
    ;; trough at phase 0, crest at half a cycle
    (cl-letf (((symbol-function 'float-time) (lambda (&optional _) 0.0)))
      (should (= (takuzu--solving-intensity) takuzu--breath-floor)))
    (cl-letf (((symbol-function 'float-time)
               (lambda (&optional _) (/ takuzu--breath-period 2.0))))
      (should (< (abs (- (takuzu--solving-intensity) 1.0)) 1e-6)))
    ;; sampled across a cycle, the lamp stays within [floor, 1] -- always lit
    (dolist (now '(0.0 0.4 0.9 1.3 2.0 2.6))
      (cl-letf (((symbol-function 'float-time) (lambda (&optional _) now)))
        (let ((k (nth 2 (nth 1 (takuzu--state-lamps)))))
          (should (<= takuzu--breath-floor k 1.0)))))
    ;; a finished game shows SOLVED; the SOLVING lamp is spent, not breathing
    (setq takuzu--won t)
    (should (= (nth 2 (nth 1 (takuzu--state-lamps))) 0.0))))

(ert-deftest test-takuzu-ui-tiles-partition-the-faceplate ()
  "Normal/Boundary: the three display tiles partition the faceplate exactly.
Left and right tiles share the top band and abut at the seam; the bottom tile
spans full width below them.  Their sizes sum to the single-image faceplate,
so the split introduces no gap or overhang at any board size."
  (with-temp-buffer
    (dolist (n '(4 8 12))
      (setq takuzu--size n takuzu--board (takuzu-make-board n))
      (let ((w (takuzu--faceplate-width)) (h (takuzu--faceplate-height))
            (split (takuzu--tile-split-y))
            (left (takuzu--svg-left)) (right (takuzu--svg-right))
            (bottom (takuzu--svg-bottom)))
        (should (= (+ (dom-attr left 'width) (dom-attr right 'width)) w))
        (should (= (dom-attr left 'height) split))
        (should (= (dom-attr right 'height) split))
        (should (= (dom-attr bottom 'width) w))
        (should (= (+ split (dom-attr bottom 'height)) h))))))

(ert-deftest test-takuzu-ui-left-tile-stable-while-right-breathes ()
  "Normal: the left tile (title and board) is byte-identical between two
breathe frames while the right tile (clock and lamps) differs.  That gap is
the whole point of the split -- Emacs serves the costly plate-bearing left
tile from its image cache and re-rasterises only the cheap right tile."
  (test-takuzu-ui--with-buffer
    (test-takuzu-ui--setup-4)
    (setq takuzu--armed nil takuzu--won nil takuzu--proven nil)
    (cl-flet ((dump (svg) (with-temp-buffer (svg-print svg) (buffer-string))))
      (let (l0 l1 r0 r1)
        (cl-letf (((symbol-function 'float-time) (lambda (&optional _) 0.4)))
          (setq l0 (dump (takuzu--svg-left)) r0 (dump (takuzu--svg-right))))
        (cl-letf (((symbol-function 'float-time)
                   (lambda (&optional _) (/ takuzu--breath-period 2.0))))
          (setq l1 (dump (takuzu--svg-left)) r1 (dump (takuzu--svg-right))))
        (should (string= l0 l1))
        (should-not (string= r0 r1))))))

(provide 'test-takuzu-ui)
;;; test-takuzu-ui.el ends here
