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
  "Normal: the skin defcustom defaults to pierced; favourites lead the cycle."
  (should (eq (eval (car (get 'takuzu-coin-skin 'standard-value))) 'pierced))
  (should (equal takuzu--coin-skins
                 '(sovereign pierced machined cash gems lamp jewel compass
                   guilloche runic scallop bimetal matrix split rosette
                   filigree))))

(ert-deftest test-takuzu-ui-filigree-wheel-gems-and-metals ()
  "Normal: the filigree wheel is silver for 0, dark pewter for 1, with six
pierced lights, six spokes, multicolour stones at the felloes, and a
diamond at the hub."
  (let ((takuzu-coin-skin 'filigree))
    (let ((c0 (svg-create 100 100)) (c1 (svg-create 100 100)))
      (takuzu--draw-disc c0 50 50 33 0 nil)
      (takuzu--draw-disc c1 50 50 33 1 nil)
      (should (dom-by-id c0 "^m-silver-fill$"))
      (should (dom-by-id c1 "^m-pewter-fill$"))
      ;; the multicolour setting: every spoke cut defined, diamond at hub
      (dolist (gem '(ruby sapphire emerald amethyst topaz aqua diamond))
        (should (dom-by-id c0 (format "^takuzu-gem-%s$" gem))))
      ;; six spokes and six pierced lights
      (should (= (length (dom-by-tag c0 'line)) 6))
      (should (>= (length (seq-filter
                           (lambda (n)
                             (equal (dom-attr n 'fill) (takuzu--c :socket)))
                           (dom-by-tag c0 'circle)))
                  6)))))

(ert-deftest test-takuzu-ui-sovereign-solid-user-two-tone-fixed ()
  "Normal: a placed sovereign coin is solid one-tone wood -- all coal for
0, all beech for 1, no hole and no pin; a fixed coin is the two-tone --
the ring holding a heart of the other wood, with no centre dot."
  (let ((takuzu-coin-skin 'sovereign))
    (let ((c0 (svg-create 100 100)) (c1 (svg-create 100 100))
          (f0 (svg-create 100 100)) (f1 (svg-create 100 100)))
      (takuzu--draw-disc c0 50 50 33 0 nil)
      (takuzu--draw-disc c1 50 50 33 1 nil)
      (takuzu--draw-disc f0 50 50 33 0 t)
      (takuzu--draw-disc f1 50 50 33 1 t)
      ;; user coins: one wood only
      (should (dom-by-id c0 "^m-coal-fill$"))
      (should-not (dom-by-id c0 "^m-beech-fill$"))
      (should (dom-by-id c1 "^m-beech-fill$"))
      (should-not (dom-by-id c1 "^m-coal-fill$"))
      ;; fixed coins: both woods, ring + heart
      (should (dom-by-id f0 "^m-coal-fill$"))
      (should (dom-by-id f0 "^m-beech-fill$"))
      (should (dom-by-id f1 "^m-beech-fill$"))
      (should (dom-by-id f1 "^m-coal-fill$"))
      ;; the heart stays small -- well inside the dotted band
      (let ((heart (seq-find (lambda (n)
                               (equal (dom-attr n 'fill) "url(#m-beech-fill)"))
                             (dom-by-tag f0 'circle))))
        (should heart)
        (should (<= (dom-attr heart 'r) (* 33 0.32))))
      ;; no hole anywhere, no pin anywhere
      (dolist (svg (list c0 c1 f0 f1))
        (should-not (seq-find (lambda (n)
                                (equal (dom-attr n 'fill) (takuzu--c :socket)))
                              (dom-by-tag svg 'circle)))
        (should-not (seq-find (lambda (n)
                                (or (equal (dom-attr n 'fill) (takuzu--metal 'coal 1))
                                    (equal (dom-attr n 'fill) (takuzu--metal 'sunflower 1))))
                              (dom-by-tag svg 'circle)))))))

(ert-deftest test-takuzu-ui-runic-carves-wood ()
  "Normal: the runic coin is oak for 0, walnut for 1, with carved rune lines."
  (let ((takuzu-coin-skin 'runic))
    (let ((c0 (svg-create 100 100)) (c1 (svg-create 100 100)))
      (takuzu--draw-disc c0 50 50 33 0 nil)
      (takuzu--draw-disc c1 50 50 33 1 nil)
      (should (dom-by-id c0 "^m-oak-fill$"))
      (should (dom-by-id c1 "^m-walnut-fill$"))
      (should (> (length (dom-by-tag c0 'line)) 10)))))

(ert-deftest test-takuzu-ui-runic-lod-band-drops-at-board-scale ()
  "Boundary: the futhorc band carves at 2x; board scale keeps the centre rune."
  (let ((takuzu-coin-skin 'runic))
    (let ((big (svg-create 100 100)) (small (svg-create 100 100)))
      (takuzu--draw-disc big 50 50 33 0 nil)
      (takuzu--draw-disc small 50 50 16 0 nil)
      (should (> (length (dom-by-tag big 'line))
                 (length (dom-by-tag small 'line))))
      (should (> (length (dom-by-tag small 'line)) 0)))))

(ert-deftest test-takuzu-ui-runic-given-rings-in-contrast ()
  "Normal: a fixed oak coin rings in iron; a fixed walnut coin in silver."
  (let ((takuzu-coin-skin 'runic))
    (let ((oak (svg-create 100 100)) (wal (svg-create 100 100)))
      (takuzu--draw-disc oak 50 50 33 0 t)
      (takuzu--draw-disc wal 50 50 33 1 t)
      (should (seq-find (lambda (n)
                          (equal (dom-attr n 'stroke) (takuzu--c :rim-iron)))
                        (dom-by-tag oak 'circle)))
      (should (seq-find (lambda (n)
                          (equal (dom-attr n 'stroke) (takuzu--c :rim-silver)))
                        (dom-by-tag wal 'circle))))))

(ert-deftest test-takuzu-ui-cycle-skin-cycles ()
  "Normal: the skin command walks the whole list and wraps back around."
  (test-takuzu-ui--with-buffer
    (test-takuzu-ui--setup-4)
    (let ((takuzu-coin-skin 'pierced))
      (takuzu-cycle-skin)
      (should (eq takuzu-coin-skin 'machined))
      (dotimes (_ (1- (length takuzu--coin-skins)))
        (takuzu-cycle-skin))
      (should (eq takuzu-coin-skin 'pierced)))))

(ert-deftest test-takuzu-ui-cycle-skin-back-walks-and-wraps ()
  "Normal/Boundary: W walks the drum backward and wraps past the head."
  (test-takuzu-ui--with-buffer
    (test-takuzu-ui--setup-4)
    (should (eq (keymap-lookup takuzu-mode-map "w") 'takuzu-cycle-skin))
    (should (eq (keymap-lookup takuzu-mode-map "W") 'takuzu-cycle-skin-back))
    (let ((takuzu-coin-skin 'pierced))
      (takuzu-cycle-skin-back)
      (should (eq takuzu-coin-skin 'sovereign))
      (takuzu-cycle-skin-back)
      (should (eq takuzu-coin-skin 'filigree))
      (takuzu-cycle-skin-back)
      (should (eq takuzu-coin-skin 'rosette)))))

(ert-deftest test-takuzu-ui-every-skin-has-a-drawer ()
  "Normal: every skin in the cycle list resolves to a draw function.
A skin added to the list without a drawer would silently fall back to lamp."
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

(ert-deftest test-takuzu-ui-metal-defs-shared-per-metal ()
  "Boundary: a board of bimetal coins defines each metal's two defs once.
The Dupre bimetal uses four metals (blue+silver, terracotta+gold), so four
coins define exactly four fills and four edges -- shared, not per-coin."
  (let ((takuzu-coin-skin 'bimetal)
        (svg (svg-create 200 100)))
    (dotimes (i 4)
      (takuzu--draw-disc svg (+ 30 (* i 40)) 50 16 (mod i 2) nil))
    (should (= (length (dom-by-tag svg 'radialGradient)) 4))
    (should (= (length (dom-by-tag svg 'linearGradient)) 4))))

(ert-deftest test-takuzu-ui-pierced-wears-college-colours ()
  "Normal: the pierced pair in school colours -- Berkeley blue with gold
accents for 0, Stanford cardinal with silver accents for 1.  Only a FIXED
coin is pierced; user coins are flat faces with no centre hole."
  (let ((takuzu-coin-skin 'pierced))
    (let ((c0 (svg-create 100 100)) (c1 (svg-create 100 100))
          (fx (svg-create 100 100)))
      (takuzu--draw-disc c0 50 50 33 0 nil)
      (takuzu--draw-disc c1 50 50 33 1 nil)
      (takuzu--draw-disc fx 50 50 33 0 t)
      (should (dom-by-id c0 "^m-berkeley-fill$"))
      (should (dom-by-id c1 "^m-cardinal-fill$"))
      ;; the radial engraves in each school's accent
      (should (seq-find (lambda (n)
                          (equal (dom-attr n 'stroke) (takuzu--metal 'sunflower 2)))
                        (dom-by-tag c0 'path)))
      (should (seq-find (lambda (n)
                          (equal (dom-attr n 'stroke) (takuzu--metal 'silver 2)))
                        (dom-by-tag c1 'path)))
      ;; user coins are flat; only the fixed coin is pierced
      (dolist (svg (list c0 c1))
        (should-not (seq-find (lambda (n)
                                (equal (dom-attr n 'fill) (takuzu--c :socket)))
                              (dom-by-tag svg 'circle))))
      (should (seq-find (lambda (n)
                          (equal (dom-attr n 'fill) (takuzu--c :socket)))
                        (dom-by-tag fx 'circle))))))

(ert-deftest test-takuzu-ui-bimetal-wears-dupre-colours ()
  "Normal: the bimetal coin strikes the Dupre palette -- a blue ring with a
silver core and olive accents for 0, a terracotta ring with a gold core and
regal accents for 1."
  (let ((takuzu-coin-skin 'bimetal))
    (let ((c0 (svg-create 100 100)) (c1 (svg-create 100 100)))
      (takuzu--draw-disc c0 50 50 33 0 nil)
      (takuzu--draw-disc c1 50 50 33 1 nil)
      (should (dom-by-id c0 "^m-blue-fill$"))
      (should (dom-by-id c0 "^m-silver-fill$"))
      (should (dom-by-id c1 "^m-copper-fill$"))
      (should (dom-by-id c1 "^m-gold-fill$"))
      (should (seq-find (lambda (n)
                          (equal (dom-attr n 'stroke) (takuzu--metal 'olive 2)))
                        (dom-by-tag c0 'circle)))
      (should (seq-find (lambda (n)
                          (equal (dom-attr n 'stroke) (takuzu--metal 'regal 2)))
                        (dom-by-tag c1 'circle))))))

(ert-deftest test-takuzu-ui-cash-has-square-hole ()
  "Normal: the cash coin carries its square hole at both scales."
  (let ((takuzu-coin-skin 'cash))
    (dolist (r '(16 33))
      (let ((svg (svg-create 100 100)))
        (takuzu--draw-disc svg 50 50 r 0 nil)
        (should (>= (length (dom-by-tag svg 'rect)) 2))))))

(ert-deftest test-takuzu-ui-matrix-keeps-its-pellets ()
  "Normal: the original dot-matrix coin still draws its pellet grid."
  (let ((takuzu-coin-skin 'matrix)
        (svg (svg-create 100 100)))
    (takuzu--draw-disc svg 50 50 33 0 nil)
    (should (dom-by-id svg "^m-bronze-fill$"))
    (should (> (length (dom-by-tag svg 'circle)) 16))))

(ert-deftest test-takuzu-ui-gems-pellets-are-jeweled ()
  "Normal: the gems coin encrusts multicolour gems in a precious metal --
a silver body for 0, gold for 1, with at least four gem cuts on the face."
  (let ((takuzu-coin-skin 'gems))
    (let ((c0 (svg-create 100 100)) (c1 (svg-create 100 100)))
      (takuzu--draw-disc c0 50 50 33 0 nil)
      (takuzu--draw-disc c1 50 50 33 1 nil)
      (should (dom-by-id c0 "^m-silver-fill$"))
      (should (dom-by-id c1 "^m-gold-fill$"))
      (dolist (gem '(ruby sapphire emerald diamond))
        (should (dom-by-id c0 (format "^takuzu-gem-%s$" gem))))
      ;; 16 grid positions plus the blank's circles
      (should (> (length (dom-by-tag c0 'circle)) 16)))))

(ert-deftest test-takuzu-ui-machined-given-knurls ()
  "Normal: a fixed machined coin adds the knurled (dashed) rim."
  (let ((takuzu-coin-skin 'machined))
    (let ((plain (svg-create 100 100)) (fixed (svg-create 100 100)))
      (takuzu--draw-disc plain 50 50 33 0 nil)
      (takuzu--draw-disc fixed 50 50 33 0 t)
      (should (seq-find (lambda (node) (dom-attr node 'stroke-dasharray))
                        (dom-by-tag fixed 'circle)))
      (should (> (length (dom-by-tag fixed 'circle))
                 (length (dom-by-tag plain 'circle)))))))

(ert-deftest test-takuzu-ui-draw-disc-dispatches-by-skin ()
  "Normal: each skin draws its signature shapes through the one entry point."
  (dolist (case '((lamp . ((radialGradient . 0) (polygon . 0)))
                  (jewel . ((radialGradient . 1) (ellipse . 1)))
                  (compass . ((radialGradient . 1) (polygon . 17)))))
    (let ((takuzu-coin-skin (car case))
          (svg (svg-create 100 100)))
      (takuzu--draw-disc svg 50 50 33 0 nil)
      (dolist (want (cdr case))
        (should (= (length (dom-by-tag svg (car want))) (cdr want)))))))

(ert-deftest test-takuzu-ui-jewel-given-wears-collar ()
  "Normal: a fixed jewel adds the two brass collar rings."
  (let ((takuzu-coin-skin 'jewel))
    (let ((plain (svg-create 100 100)) (fixed (svg-create 100 100)))
      (takuzu--draw-disc plain 50 50 33 1 nil)
      (takuzu--draw-disc fixed 50 50 33 1 t)
      (should (= (- (length (dom-by-tag fixed 'circle))
                    (length (dom-by-tag plain 'circle)))
                 2))
      (should (seq-find (lambda (node)
                          (equal (dom-attr node 'stroke) (takuzu--c :gold)))
                        (dom-by-tag fixed 'circle))))))

(ert-deftest test-takuzu-ui-cursor-bezel-metal-matches-skin ()
  "Normal: the cursor ring is brass on the original lamp set, iron on the
jewel and compass sets."
  (dolist (case '((lamp . :cursor-bezel-hi)
                  (jewel . :cursor-iron-hi)
                  (compass . :cursor-iron-hi)))
    (let ((takuzu-coin-skin (car case))
          (svg (svg-create 100 100)))
      (takuzu--draw-cursor-bezel svg 10 10 50)
      (let ((stops (dom-by-tag svg 'stop)))
        (should stops)
        (should (equal (dom-attr (car stops) 'stop-color)
                       (takuzu--c (cdr case))))))))

(ert-deftest test-takuzu-ui-compass-vals-are-different-instruments ()
  "Normal: colour 0 is the ray-rose medallion; colour 1 a needle dial.
The two pieces must differ in kind, not just palette: the rose is all
polygons with no tick lines, the dial carries tick lines, an N, and a
two-piece needle instead of the sixteen rays."
  (let ((takuzu-coin-skin 'compass))
    (let ((rose (svg-create 100 100)) (dial (svg-create 100 100)))
      (takuzu--draw-disc rose 50 50 33 0 nil)
      (takuzu--draw-disc dial 50 50 33 1 nil)
      (should (>= (length (dom-by-tag rose 'polygon)) 16))
      (should (= (length (dom-by-tag rose 'line)) 0))
      (should (> (length (dom-by-tag dial 'line)) 8))
      (should (<= (length (dom-by-tag dial 'polygon)) 4))
      (should (member "N" (mapcar #'dom-texts (dom-by-tag dial 'text)))))))

(ert-deftest test-takuzu-ui-compass-given-wears-silver-rim ()
  "Normal: a fixed compass medallion rings in bright silver."
  (let ((takuzu-coin-skin 'compass))
    (let ((fixed (svg-create 100 100)))
      (takuzu--draw-disc fixed 50 50 33 1 t)
      (should (seq-find (lambda (node)
                          (equal (dom-attr node 'stroke)
                                 (takuzu--c :rim-silver)))
                        (dom-by-tag fixed 'circle))))))

(ert-deftest test-takuzu-ui-compass-lod-drops-dentate-at-board-scale ()
  "Boundary: the compass dentate border draws at 2x but not below r=20."
  (let ((takuzu-coin-skin 'compass))
    (let ((big (svg-create 100 100)) (small (svg-create 100 100)))
      (takuzu--draw-disc big 50 50 33 0 nil)
      (takuzu--draw-disc small 50 50 16 0 nil)
      (should (= (length (dom-by-tag big 'polygon)) 17))
      (should (= (length (dom-by-tag small 'polygon)) 16)))))

(ert-deftest test-takuzu-ui-shared-coin-gradients-defined-once ()
  "Boundary: two coins of both colours share one gradient def per colour."
  (let ((takuzu-coin-skin 'jewel)
        (svg (svg-create 200 100)))
    (takuzu--draw-disc svg 40 50 16 0 nil)
    (takuzu--draw-disc svg 80 50 16 0 nil)
    (takuzu--draw-disc svg 120 50 16 1 nil)
    (takuzu--draw-disc svg 160 50 16 1 nil)
    (should (= (length (dom-by-tag svg 'radialGradient)) 2))))

(ert-deftest test-takuzu-ui-skin-selector-shows-counter ()
  "Normal: the skin selector shows the tape-counter index and never a name."
  (test-takuzu-ui--with-buffer
    (test-takuzu-ui--setup-4)
    (dolist (case '((sovereign . "01") (pierced . "02") (machined . "03")
                    (cash . "04") (filigree . "16")))
      (let* ((takuzu-coin-skin (car case))
             (texts (mapcar #'dom-texts (dom-by-tag (takuzu--svg) 'text))))
        (should (member (cdr case) texts))
        (should (member "COIN" texts))
        ;; the drum shows only the index -- no skin is named on the plate
        (should-not (member (upcase (symbol-name (car case))) texts))))))

(provide 'test-takuzu-ui)
;;; test-takuzu-ui.el ends here
