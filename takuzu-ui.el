;;; takuzu-ui.el --- Interactive hi-fi console for Takuzu -*- lexical-binding: t -*-

;; Author: Craig Jennings <craigmartinjennings@gmail.com>
;; Keywords: games

;;; Commentary:
;; `takuzu-mode' renders the whole game as one state-to-SVG faceplate image,
;; regenerated on every state change, in the Dupré instrument-console style
;; (design of record: docs/prototypes/2026-07-11-takuzu-prototype-hifi.html).
;; The board is a panel of recessed lamp-sockets: empty cells are dark wells,
;; placed cells are matte colour discs, givens wear a silver bezel, the cursor
;; lights a gold ring.  A right instrument panel carries an analogue clock, the
;; grade and cells-left readouts, and a vertical VU meter for fill progress.
;;
;; Interaction is by keymap; the image regenerates per state change and once a
;; second (for the clock and the win pulse).  A plain-text board renders as a
;; fallback on terminals, where SVG is unavailable.

;;; Code:

(require 'cl-lib)
(require 'svg)
(require 'takuzu-board)
(require 'takuzu-solver)
(require 'takuzu-async)

;; Config knobs live in takuzu.el; forward-declare so this compiles clean.
(defvar takuzu-default-size)
(defvar takuzu-default-difficulty)
(defvar takuzu-sizes)
(defvar takuzu-flash-period)
(declare-function takuzu "takuzu" (&optional size difficulty))

;; --- palette (from the hi-fi prototype) ---

(defconst takuzu--colors
  '(:room "#0d0b09" :plate "#1b1710" :plate-edge "#2c2620"
    :well "#0a0c0d" :board-bg "#14110c" :socket "#0c0a07" :socket-edge "#05040a"
    :gold "#b99640" :gold-hi "#dcc061" :silver "#bfc4d0" :cream "#f3e7c5"
    :steel "#969385" :dim "#7c838a" :pass "#74932f" :fail "#cb6b4d"
    :disc0 "#4d5f75" :disc1 "#8f5236" :bezel "#a8aeb5" :meter-bg "#0d0f10"
    :disc0-edge "#6f839c" :disc1-edge "#b8734f"
    :slate "#424f5e" :wash "#2c2f32"
    :tag-red "#e24a2c" :tag-red-off "#3a1613"
    :tag-amber "#e0a12e" :tag-amber-off "#3a2c12"
    :tag-green "#86b437" :tag-green-off "#24311b")
  "Faceplate palette, matching the prototype.")

(defun takuzu--c (key)
  "The palette colour for KEY."
  (plist-get takuzu--colors key))

;; --- layout ---

(defconst takuzu--gap 6 "Pixels between board cells.")
(defconst takuzu--bpad 14 "Board inner padding.")
(defconst takuzu--ppad 24 "Faceplate padding.")
(defconst takuzu--stage-gap 16 "Gap between the board and the right panel.")
(defconst takuzu--panel-w 106 "Right panel width.")
(defconst takuzu--title-h 78 "Title band height.")
(defconst takuzu--legend-h 92 "Legend band height.")
(defconst takuzu--fill 0.85
  "Fraction of the window the faceplate is scaled to fill.
The SVG is vector, so this just rasterizes larger; 1.0 is edge-to-edge.")
(defconst takuzu--scale-step 0.2
  "Fraction of the remaining scale gap closed per animation frame.")
(defconst takuzu--scale-interval 0.02
  "Seconds between scale-animation frames.")

(defun takuzu--cell-size (n)
  "Pixel size of a cell for an N-wide board."
  (cond ((<= n 6) 50) ((<= n 8) 44) ((<= n 10) 38) (t 34)))

;; --- buffer-local state ---

(defvar-local takuzu--board nil "The puzzle board.")
(defvar-local takuzu--solution nil "The unique solution board.")
(defvar-local takuzu--grade nil "Difficulty grade of the current puzzle.")
(defvar-local takuzu--size 6 "Board size.")
(defvar-local takuzu--cursor '(0 . 0) "Cursor cell as (ROW . COL).")
(defvar-local takuzu--assist nil "Non-nil to highlight rule breaks live.")
(defvar-local takuzu--history nil "Undo stack of (INDEX . PREV-VALUE).")
(defvar-local takuzu--start-time nil "When the current puzzle began.")
(defvar-local takuzu--won nil "Non-nil once solved.")
(defvar-local takuzu--proven nil "Non-nil once the solution was shown.")
(defvar-local takuzu--won-elapsed 0 "Frozen elapsed seconds at win.")
(defvar-local takuzu--status "" "Persistent status line.")
(defvar-local takuzu--timer nil "The per-second refresh timer.")
(defvar-local takuzu--generating nil "Plist (:size :difficulty) while a puzzle is being generated.")
(defvar-local takuzu--spinner 0 "Spinner frame index shown while generating.")
(defvar-local takuzu--spinner-timer nil "Timer animating the generating spinner.")
(defvar-local takuzu--gen-process nil "The async generation process, if any.")
(defvar-local takuzu--scale nil "Currently displayed image scale, eased toward the fit target.")
(defvar-local takuzu--scale-timer nil "Timer easing the image scale on a resize.")
(defvar-local takuzu--armed nil "Plist (:size :difficulty) while waiting for the start keypress.")
(defvar-local takuzu--pending nil "A pre-generated result awaiting the start keypress.")
(defvar-local takuzu--pending-start nil "Non-nil if start was pressed before generation finished.")
(defvar-local takuzu--difficulty nil "The requested difficulty of the current or next puzzle.")
(defvar-local takuzu--clock-flash 0 "Remaining half-flashes for the quick clock-ring cue.")
(defvar-local takuzu--clock-flash-timer nil "Timer for the quick clock-ring cue.")

;; --- helpers ---

(defun takuzu--elapsed ()
  "Elapsed seconds since the puzzle began (0 until started, frozen once finished)."
  (cond ((or takuzu--won takuzu--proven) takuzu--won-elapsed)
        (takuzu--start-time
         (floor (float-time (time-subtract (current-time) takuzu--start-time))))
        (t 0)))

(defun takuzu--flash-on-p ()
  "Non-nil during the lit half of the current flash cycle (`takuzu-flash-period')."
  (< (mod (float-time) takuzu-flash-period) (* 0.5 takuzu-flash-period)))

(defun takuzu--refresh-interval ()
  "Redraw interval that keeps flashing visible without over-drawing."
  (max 0.2 (min 1.0 (/ takuzu-flash-period 2.0))))

(defun takuzu--fmt-time (s)
  "Format S seconds as M:SS."
  (format "%d:%02d" (/ s 60) (% s 60)))

(defun takuzu--curp (r c)
  "Non-nil if the cursor is on cell R, C."
  (and (= r (car takuzu--cursor)) (= c (cdr takuzu--cursor))))

(defun takuzu--line-bad-p (line n lines)
  "Non-nil if LINE (width N) breaks a rule among sibling LINES."
  (or (takuzu--line-has-triple-p line)
      (not (takuzu--line-count-legal-p line n))
      (and (takuzu--line-complete-p line)
           (> (cl-count-if (lambda (l)
                             (and (takuzu--line-complete-p l) (equal l line)))
                           lines)
              1))))

(defun takuzu--error-vector ()
  "Vector marking each board index that lies in a rule-breaking line, or nil.
Returns nil when assist is off."
  (when takuzu--assist
    (let* ((n takuzu--size)
           (bad (make-vector (* n n) nil))
           (rows (takuzu-board-rows takuzu--board))
           (cols (takuzu-board-cols takuzu--board)))
      (dotimes (r n)
        (when (takuzu--line-bad-p (nth r rows) n rows)
          (dotimes (c n) (aset bad (+ (* r n) c) t))))
      (dotimes (c n)
        (when (takuzu--line-bad-p (nth c cols) n cols)
          (dotimes (r n) (aset bad (+ (* r n) c) t))))
      bad)))

(defun takuzu--txt (svg x y str size color &optional anchor weight)
  "Draw STR on SVG at X,Y in monospace SIZE and COLOR.
ANCHOR is start/middle/end; WEIGHT normal/bold."
  (svg-text svg str :x x :y y :font-family "monospace"
            :font-size size :fill color
            :font-weight (or weight "normal")
            :text-anchor (or anchor "start")))

;; --- SVG regions ---

(defun takuzu--draw-screw (svg x y)
  "Draw a faceplate screw on SVG at X,Y."
  (svg-circle svg x y 5 :fill "#2b271f" :stroke "#0c0a07")
  (svg-line svg (- x 2) (- y 2) (+ x 2) (+ y 2) :stroke "#0c0a07" :stroke-width 1))

(defun takuzu--draw-title (svg x y)
  "Draw the TAKUZU / aliases title on SVG anchored at X,Y (top-left)."
  (svg-text svg "TAKUZU" :x x :y (+ y 31) :font-family "monospace" :font-size 30
            :fill (takuzu--c :gold) :font-weight "bold" :font-style "italic"
            :text-anchor "start")
  (takuzu--txt svg (+ x 112) (+ y 31) "/" 30 (takuzu--c :gold-hi) "start")
  (let ((ax (+ x 134)))
    (takuzu--txt svg ax (+ y 14) "BINAIRO" 10 (takuzu--c :dim))
    (takuzu--txt svg ax (+ y 28) "TOHU WA-VOHU" 10 (takuzu--c :dim))
    (takuzu--txt svg ax (+ y 42) "BINARY LOGIC" 10 (takuzu--c :dim))))

(defun takuzu--fill-pct ()
  "Percent of the board that is filled, 0-100."
  (let* ((n takuzu--size) (total (* n n))
         (filled (- total (cl-count nil (append (takuzu-board-cells takuzu--board) nil)))))
    (* 100 (/ filled (float total)))))

(defun takuzu--draw-led (svg cx cy r color)
  "Draw an LED at CX,CY radius R.  COLOR is the lit colour, nil for unlit.
Unlit reads as a dark recessed dome with a faint rim; lit gets a glossy cap."
  (svg-circle svg cx cy (+ r 1.5) :fill "#08070a"
              :stroke (takuzu--c :plate-edge) :stroke-width 1)
  (svg-circle svg cx cy r :fill (or color "#1b1f24")
              :stroke (if color "#00000066" (takuzu--c :slate)) :stroke-width 1)
  (svg-circle svg (- cx (* r 0.32)) (- cy (* r 0.32)) (* r 0.34)
              :fill "#ffffff" :fill-opacity (if color 0.55 0.14)))

(defun takuzu--draw-lamp (svg cx cy r)
  "Draw the status LED at CX,CY radius R, reflecting progress and win state.
Off under 50%, amber to 80%, green above; flashing green on a solve, red on a
reveal."
  (let* ((pct (takuzu--fill-pct))
         (color (cond (takuzu--proven (takuzu--c :fail))
                      (takuzu--won (takuzu--c :pass))
                      ((< pct 50) nil)
                      ((< pct 80) (takuzu--c :gold))
                      (t (takuzu--c :pass))))
         (blink (or takuzu--won takuzu--proven))
         (on (or (not blink) (takuzu--flash-on-p))))
    (takuzu--draw-led svg cx cy r (and on color))))

(defun takuzu--draw-ring (svg cx cy r pct value)
  "Draw the LEFT gauge on SVG at CX,CY radius R: arc fills by PCT (0-100),
VALUE centred with a LEFT label beneath it.  Arc red under 50%, amber to 80%,
green above."
  (let* ((sw 7) (circ (* 2 float-pi r))
         (filled (* (/ pct 100.0) circ))
         (color (cond (takuzu--proven (takuzu--c :fail))
                      ((< pct 50) (takuzu--c :fail))
                      ((< pct 80) (takuzu--c :gold))
                      (t (takuzu--c :pass)))))
    (svg-circle svg cx cy r :fill "none" :stroke (takuzu--c :meter-bg) :stroke-width sw)
    (when (> pct 0)
      (svg-circle svg cx cy r :fill "none" :stroke color :stroke-width sw
                  :stroke-linecap "round"
                  :stroke-dasharray (format "%s %s" filled (- circ filled))
                  :transform (format "rotate(-90 %s %s)" cx cy)))
    (takuzu--txt svg cx (+ cy 3) (number-to-string value) 18 (takuzu--c :cream) "middle" "bold")
    (takuzu--txt svg cx (+ cy 15) "LEFT" 8 (takuzu--c :steel) "middle")))

(defun takuzu--draw-disc (svg cx cy r val given)
  "Draw a disc of VAL on SVG at CX,CY radius R.
Givens wear a thin dulled-silver bezel; placed discs a thin lighter lip of their
own colour."
  (let ((fill (if (eql val 0) (takuzu--c :disc0) (takuzu--c :disc1)))
        (edge (if (eql val 0) (takuzu--c :disc0-edge) (takuzu--c :disc1-edge))))
    (if given
        (progn
          (svg-circle svg cx cy (+ r 1) :fill "none" :stroke "#000" :stroke-width 1)
          (svg-circle svg cx cy r :fill fill :stroke (takuzu--c :bezel) :stroke-width 1.2))
      (svg-circle svg cx cy r :fill fill :stroke edge :stroke-width 1.2))))

(defun takuzu--draw-board (svg x y)
  "Draw the board on SVG with its top-left at X,Y."
  (let* ((n takuzu--size) (cell (takuzu--cell-size n))
         (gap takuzu--gap) (bpad takuzu--bpad)
         (span (+ (* 2 bpad) (* n cell) (* (1- n) gap)))
         (errs (takuzu--error-vector)))
    (svg-rectangle svg x y span span :rx 12
                   :fill (takuzu--c :board-bg) :stroke "#000")
    (dotimes (r n)
      (dotimes (c n)
        (let* ((sx (+ x bpad (* c (+ cell gap))))
               (sy (+ y bpad (* r (+ cell gap))))
               (idx (+ (* r n) c))
               (val (takuzu-board-ref takuzu--board r c))
               (given (takuzu-board-given-p takuzu--board r c)))
          (svg-rectangle svg sx sy cell cell :rx 9
                         :fill (takuzu--c :socket)
                         :stroke (if (and errs (aref errs idx))
                                     (takuzu--c :fail) (takuzu--c :socket-edge))
                         :stroke-width (if (and errs (aref errs idx)) 2 1))
          (when val
            (takuzu--draw-disc svg (+ sx (/ cell 2)) (+ sy (/ cell 2))
                               (round (* cell 0.28)) val given))
          (when (takuzu--curp r c)
            (svg-rectangle svg (+ sx 2) (+ sy 2) (- cell 4) (- cell 4) :rx 7
                           :fill "none" :stroke (takuzu--c :gold) :stroke-width 2)))))
    span))

(defun takuzu--start-clock-flash (buf)
  "Flash the clock ring twice quickly in BUF as a start/stop cue."
  (with-current-buffer buf
    (setq takuzu--clock-flash 4)
    (when (timerp takuzu--clock-flash-timer) (cancel-timer takuzu--clock-flash-timer))
    (setq takuzu--clock-flash-timer
          (run-at-time
           0.11 0.11
           (lambda ()
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (setq takuzu--clock-flash (1- takuzu--clock-flash))
                 (when (<= takuzu--clock-flash 0)
                   (setq takuzu--clock-flash 0)
                   (when (timerp takuzu--clock-flash-timer)
                     (cancel-timer takuzu--clock-flash-timer))
                   (setq takuzu--clock-flash-timer nil))
                 (takuzu--redraw buf))))))))

(defun takuzu--dial-glow (br)
  "Warm ember hex for the dial backlight at brightness BR (0..1)."
  (format "#%02x%02x%02x"
          (round (+ 34 (* 66 br))) (round (+ 24 (* 46 br))) (round (+ 13 (* 19 br)))))

(defun takuzu--draw-clock (svg cx cy rad)
  "Draw the analogue clock on SVG centred at CX,CY radius RAD.
A warm radial backlight breathes gently behind the dial, like an internal lamp
peeking out from behind the hands."
  (let* ((br (+ 0.5 (* 0.22 (sin (* 2 float-pi (/ (float-time) 5.0))))))
         (flashing (and (> takuzu--clock-flash 0) (cl-oddp takuzu--clock-flash))))
    (svg-gradient svg "takuzu-dial" 'radial
                  (list (cons 0 (takuzu--dial-glow br))
                        (cons 52 "#17110a") (cons 100 "#0a0908")))
    (svg-circle svg cx cy rad :gradient "takuzu-dial"
                :stroke (if flashing (takuzu--c :gold) (takuzu--c :plate-edge))
                :stroke-width (if flashing 1 2)))
  ;; hour markers: cardinals (12/3/6/9) long and bright, the rest short ticks
  (dotimes (i 12)
    (let* ((a (* i (/ float-pi 6)))
           (s (sin a)) (co (cos a))
           (cardinal (zerop (mod i 3)))
           (inner (- rad (if cardinal 9 5))))
      (svg-line svg (+ cx (* inner s)) (- cy (* inner co))
                (+ cx (* rad s)) (- cy (* rad co))
                :stroke (takuzu--c (if cardinal :silver :steel))
                :stroke-width (if cardinal 2 1))))
  (let* ((s (takuzu--elapsed))
         (ha (* (/ float-pi 6) (/ s 3600.0)))
         (ma (* (/ float-pi 30) (/ s 60.0)))
         (sa (* (/ float-pi 30) (mod s 60))))
    ;; hour hand: short and stout
    (svg-line svg cx cy (+ cx (* (* rad 0.45) (sin ha))) (- cy (* (* rad 0.45) (cos ha)))
              :stroke (takuzu--c :gold) :stroke-width 3 :stroke-linecap "round")
    ;; minute hand: longer and lighter
    (svg-line svg cx cy (+ cx (* (* rad 0.68) (sin ma))) (- cy (* (* rad 0.68) (cos ma)))
              :stroke (takuzu--c :gold-hi) :stroke-width 2 :stroke-linecap "round")
    ;; second hand: thin, sweeping, with a short counterweight tail
    (svg-line svg (- cx (* (* rad 0.2) (sin sa))) (+ cy (* (* rad 0.2) (cos sa)))
              (+ cx (* (* rad 0.82) (sin sa))) (- cy (* (* rad 0.82) (cos sa)))
              :stroke (takuzu--c :steel) :stroke-width 1 :stroke-linecap "round"))
  (svg-circle svg cx cy 2.6 :fill (takuzu--c :gold) :stroke "#0a0908" :stroke-width 0.6))

(defun takuzu--draw-readout (svg cx y value unit)
  "Draw a readout on SVG centred at CX, baseline Y: big VALUE, small UNIT below."
  (takuzu--txt svg cx y value 20 (takuzu--c :cream) "middle" "bold")
  (takuzu--txt svg cx (+ y 12) unit 9 (takuzu--c :steel) "middle"))

(defun takuzu--draw-tag (svg x y w h lit-col off-col lit label)
  "Draw one grade tag rect at X,Y size W,H with LABEL centred in bold black.
Lit: bright LIT-COL with a soft glow and sheen; unlit: the dark OFF-COL."
  (if lit
      (progn
        (svg-rectangle svg (- x 3) (- y 3) (+ w 6) (+ h 6) :rx 6
                       :fill lit-col :fill-opacity 0.28)
        (svg-rectangle svg x y w h :rx 4 :fill lit-col
                       :stroke "#00000055" :stroke-width 1)
        (svg-rectangle svg (+ x 2) (+ y 2) (- w 4) (* h 0.42) :rx 3
                       :fill "#ffffff" :fill-opacity 0.16))
    (svg-rectangle svg x y w h :rx 4 :fill off-col :stroke "#000000" :stroke-width 1))
  (takuzu--txt svg (+ x (/ w 2)) (+ y (round (* h 0.5)) 3) label 10 "#000000" "middle" "bold"))

(defun takuzu--draw-grade (svg cx cy)
  "Draw the stacked red/amber/green grade tags centred vertically at CY.
The tag matching the current grade (or armed difficulty) lights up.  An engraved
frame surrounds the stack, broken at the bottom by the GRADE label."
  (let* ((tw 64) (th 14) (g 6) (pad 7)
         (cur (or takuzu--grade takuzu--difficulty))
         (tags '((easy :tag-green :tag-green-off)
                 (medium :tag-amber :tag-amber-off)
                 (hard :tag-red :tag-red-off)))
         (stackh (+ (* 3 th) (* 2 g)))
         (x (- cx (/ tw 2)))
         (top (round (- cy (/ stackh 2))))
         (fx (- x pad)) (fy (- top pad))
         (fw (+ tw (* 2 pad))) (fh (+ stackh (* 2 pad))))
    (svg-rectangle svg fx fy fw fh :rx 6 :fill "none"
                   :stroke (takuzu--c :wash) :stroke-width 1)
    (cl-loop for (grd lit-key off-key) in tags for i from 0 do
             (takuzu--draw-tag svg x (+ top (* i (+ th g))) tw th
                               (takuzu--c lit-key) (takuzu--c off-key)
                               (eq grd cur) (upcase (symbol-name grd))))
    (let ((ly (+ fy fh)))
      (svg-rectangle svg (- cx 22) (- ly 6) 44 12 :fill (takuzu--c :well))
      (takuzu--txt svg cx (+ ly 3) "GRADE" 9 (takuzu--c :steel) "middle"))))

(defun takuzu--draw-size-cell (svg x y w h num lit)
  "Draw a digital size cell at X,Y size W,H showing NUM; lit glows gold, else dim."
  (if lit
      (progn
        (svg-rectangle svg (- x 2) (- y 2) (+ w 4) (+ h 4) :rx 5
                       :fill (takuzu--c :gold) :fill-opacity 0.22)
        (svg-rectangle svg x y w h :rx 3 :fill "#241d10"
                       :stroke (takuzu--c :gold) :stroke-width 1)
        (takuzu--txt svg (+ x (/ w 2)) (+ y (round (* h 0.5)) 4)
                     (number-to-string num) 13 (takuzu--c :gold-hi) "middle" "bold"))
    (progn
      (svg-rectangle svg x y w h :rx 3 :fill (takuzu--c :well) :stroke "#1c1f24" :stroke-width 1)
      (takuzu--txt svg (+ x (/ w 2)) (+ y (round (* h 0.5)) 4)
                   (number-to-string num) 13 (takuzu--c :slate) "middle" "bold"))))

(defun takuzu--draw-size (svg cx cy)
  "Draw the even sizes 4-12 as a framed digital grid centred at CX,CY.
The current board size lights up; the rest are dark.  Layout: 4 6 / 8 10 / 12.
An engraved frame surrounds the grid, broken at the bottom by the SIZE label."
  (let* ((cw 26) (ch 15) (hgap 10) (vgap 5) (pad 7)
         (dx (/ (+ cw hgap) 2.0))
         (rows '((4 6) (8 10) (12)))
         (gridh (+ (* 3 ch) (* 2 vgap)))
         (top (round (- cy (/ gridh 2))))
         (fw (+ (* 2 cw) hgap (* 2 pad)))
         (fx (round (- cx (/ fw 2))) ) (fy (- top pad))
         (fh (+ gridh (* 2 pad))))
    (svg-rectangle svg fx fy fw fh :rx 6 :fill "none"
                   :stroke (takuzu--c :wash) :stroke-width 1)
    (cl-loop for row in rows for r from 0 do
             (let ((ry (+ top (* r (+ ch vgap)) (/ ch 2)))
                   (xs (if (= (length row) 1) (list cx) (list (- cx dx) (+ cx dx)))))
               (cl-loop for num in row for xc in xs do
                        (takuzu--draw-size-cell svg (round (- xc (/ cw 2)))
                                                (round (- ry (/ ch 2)))
                                                cw ch num (= num takuzu--size)))))
    (let ((ly (+ fy fh)))
      (svg-rectangle svg (- cx 18) (- ly 6) 36 12 :fill (takuzu--c :well))
      (takuzu--txt svg cx (+ ly 3) "SIZE" 9 (takuzu--c :steel) "middle"))))

(defun takuzu--draw-panel (svg x y h)
  "Draw the right instrument panel on SVG at X,Y with height H.
Clock, size, grade, and the LEFT gauge (a ring carrying its own status LED in
the upper-left corner) spread evenly down the panel."
  (let* ((w takuzu--panel-w) (cx (+ x (/ w 2)))
         (empty (cl-count nil (append (takuzu-board-cells takuzu--board) nil)))
         (m 44) (step (/ (- h (* 2 m)) 3.0)) (ringr 30))
    (svg-rectangle svg x y w h :rx 10 :fill (takuzu--c :well) :stroke "#201d17")
    (takuzu--draw-clock svg cx (+ y m) 28)
    (takuzu--txt svg cx (+ y m 40) "TIME" 9 (takuzu--c :steel) "middle")
    (takuzu--draw-size svg cx (round (+ y m step)))
    (takuzu--draw-grade svg cx (round (+ y m (* 2 step))))
    (let ((ry (round (+ y m (* 3 step)))))
      (takuzu--draw-ring svg cx ry ringr (takuzu--fill-pct) empty)
      (takuzu--draw-lamp svg (+ (- cx ringr) 5) (+ (- ry ringr) 5) 3.2))))

(defun takuzu--legend-glyph (svg cx y size key color &optional underline)
  "Draw KEY on SVG at CX,Y in monospace SIZE and COLOR; underline when UNDERLINE."
  (svg-text svg key :x cx :y y :font-family "monospace" :font-size size :fill color
            :font-weight (if underline "bold" "normal")
            :text-decoration (if underline "underline" "none")))

(defun takuzu--draw-switch (svg x yc on)
  "Draw a toggle switch on SVG with left edge X, vertical centre YC, state ON."
  (let* ((w 32) (hh 16) (r (- (/ hh 2) 2)))
    (svg-rectangle svg x (- yc (/ hh 2)) w hh :rx (/ hh 2)
                   :fill (if on (takuzu--c :slate) (takuzu--c :well))
                   :stroke (if on (takuzu--c :gold) (takuzu--c :slate)) :stroke-width 1)
    (svg-circle svg (if on (- (+ x w) (/ hh 2)) (+ x (/ hh 2))) yc r
                :fill (if on (takuzu--c :gold) (takuzu--c :slate)))))

(defun takuzu--legend-item-width (it charw kgap)
  "Estimated pixel width of legend item IT with CHARW and key-gap KGAP."
  (pcase (car it)
    ((or 'word 'flashword) (* (length (nth 1 it)) charw))
    ('toggle (+ (* (length (nth 1 it)) charw) 10 32))
    (_ (+ (* (length (nth 1 it)) charw) kgap (* (length (nth 2 it)) charw)))))

(defun takuzu--draw-legend-line (svg x y width size items)
  "Draw legend ITEMS justified across WIDTH on SVG from X at baseline Y.
Each item is (word WORD) -- word with its first letter gold-underlined -- or
(keyed KEY LABEL) -- KEY gold-underlined then LABEL dim."
  (let* ((charw (* size 0.62)) (kgap (* size 0.4))
         (widths (mapcar (lambda (it) (takuzu--legend-item-width it charw kgap)) items))
         (n (length items))
         (inter (if (> n 1)
                    (max (* size 0.9) (/ (- width (apply #'+ widths)) (1- n)))
                  0))
         (cx x))
    (cl-loop for it in items for iw in widths do
             (cond
              ((eq (car it) 'word)
               (let ((w (nth 1 it)))
                 (takuzu--legend-glyph svg cx y size (substring w 0 1) (takuzu--c :gold) t)
                 (takuzu--legend-glyph svg (+ cx charw) y size (substring w 1) (takuzu--c :dim))))
              ((eq (car it) 'flashword)
               (let ((w (nth 1 it)) (on (nth 2 it)))
                 (takuzu--legend-glyph svg cx y size (substring w 0 1)
                                       (if on (takuzu--c :gold-hi) (takuzu--c :steel)) on)
                 (takuzu--legend-glyph svg (+ cx charw) y size (substring w 1) (takuzu--c :dim))))
              ((eq (car it) 'keyed)
               (let ((k (nth 1 it)) (l (nth 2 it)))
                 (takuzu--legend-glyph svg cx y size k (takuzu--c :gold) t)
                 (takuzu--legend-glyph svg (+ cx (* (length k) charw) kgap) y size l (takuzu--c :dim))))
              ((eq (car it) 'toggle)
               (let ((w (nth 1 it)))
                 (takuzu--legend-glyph svg cx y size (substring w 0 1) (takuzu--c :gold) t)
                 (takuzu--legend-glyph svg (+ cx charw) y size (substring w 1) (takuzu--c :dim))
                 (takuzu--draw-switch svg (+ cx (* (length w) charw) 10) (- y 4) (nth 2 it)))))
             (setq cx (+ cx iw inter)))))

(defun takuzu--draw-engrave (svg x y width label)
  "Draw an engraved section LABEL on SVG at X,Y with a hairline across WIDTH."
  (takuzu--txt svg x y label 9 (takuzu--c :steel))
  (svg-line svg (+ x (* (length label) 7) 12) (- y 3) (+ x width) (- y 3)
            :stroke (takuzu--c :wash) :stroke-width 1))

(defun takuzu--draw-legend (svg x y width)
  "Draw the two engraved control sections and status on SVG at X,Y across WIDTH.
GAME (session keys) sits on top, PLAY (solving keys, with the Assist toggle)
below.  While armed, the New key flashes to prompt the start."
  (let ((new-item (if takuzu--armed
                      (list 'flashword "new" (takuzu--flash-on-p))
                    '(word "new"))))
    (takuzu--draw-engrave svg x y width "GAME")
    (takuzu--draw-legend-line
     svg x (+ y 16) width 11
     `(,new-item (word "reset") (word "size") (word "diff")
       (word "prove") (word "quit")))
    (takuzu--draw-engrave svg x (+ y 36) width "PLAY")
    (takuzu--draw-legend-line
     svg x (+ y 52) width 11
     `((keyed "SPC" "cycle") (word "undo") (keyed "?" "hint")
       (word "check") (toggle "assist" ,takuzu--assist)))
    (unless (string-empty-p takuzu--status)
      (takuzu--txt svg x (+ y 74) takuzu--status 12
                   (cond (takuzu--won (takuzu--c :gold))
                         (t (takuzu--c :silver)))
                   "start" (if takuzu--won "bold" "normal")))))

(defun takuzu--faceplate-width ()
  "Pixel width of the faceplate for the current board size."
  (let* ((n takuzu--size) (cell (takuzu--cell-size n))
         (boardw (+ (* 2 takuzu--bpad) (* n cell) (* (1- n) takuzu--gap)))
         (contentw (+ boardw takuzu--stage-gap takuzu--panel-w)))
    (+ (* 2 takuzu--ppad) (max contentw 380))))

(defun takuzu--faceplate-height ()
  "Pixel height of the faceplate for the current board size."
  (let* ((n takuzu--size) (cell (takuzu--cell-size n))
         (boardw (+ (* 2 takuzu--bpad) (* n cell) (* (1- n) takuzu--gap))))
    (+ (* 2 takuzu--ppad) takuzu--title-h boardw takuzu--legend-h)))

(defun takuzu--svg ()
  "Build the faceplate SVG for the current state."
  (let* ((n takuzu--size) (cell (takuzu--cell-size n))
         (boardw (+ (* 2 takuzu--bpad) (* n cell) (* (1- n) takuzu--gap)))
         (ppad takuzu--ppad)
         (w (takuzu--faceplate-width))
         (stagey (+ ppad takuzu--title-h))
         (h (takuzu--faceplate-height))
         (svg (svg-create w h)))
    (svg-rectangle svg 0 0 w h :rx 16 :fill (takuzu--c :plate) :stroke (takuzu--c :plate-edge))
    (dolist (p (list (cons 12 12) (cons (- w 12) 12)
                     (cons 12 (- h 12)) (cons (- w 12) (- h 12))))
      (takuzu--draw-screw svg (car p) (cdr p)))
    (takuzu--draw-title svg ppad ppad)
    (takuzu--draw-board svg ppad stagey)
    (let* ((px (+ ppad boardw takuzu--stage-gap))
           (ptop (+ ppad 6))
           (ph (- (+ stagey boardw) ptop)))
      (takuzu--draw-panel svg px ptop ph))
    (takuzu--draw-legend svg ppad (+ stagey boardw 22) (- w (* 2 ppad)))
    svg))

;; --- text fallback (tty) ---

(defun takuzu--glyph (val)
  "Glyph for cell VAL."
  (cond ((eql val 0) "O") ((eql val 1) "X") (t ".")))

(defun takuzu--render-text ()
  "Return a plain-text board for terminals."
  (let ((n takuzu--size) (out ""))
    (dotimes (r n)
      (dotimes (c n)
        (let ((v (takuzu-board-ref takuzu--board r c)))
          (setq out (concat out (if (takuzu--curp r c) "[" " ")
                            (takuzu--glyph v)
                            (if (takuzu--curp r c) "]" " ")))))
      (setq out (concat out "\n")))
    out))

;; --- rendering ---

(defun takuzu--graphical-p ()
  "Non-nil when the SVG board can be shown."
  (and (display-graphic-p) (image-type-available-p 'svg)))

(defconst takuzu--spinner-frames
  ["⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"]
  "Braille frames for the generating spinner.")

(defun takuzu--stop-spinner ()
  "Cancel the generating spinner timer if running."
  (when (timerp takuzu--spinner-timer) (cancel-timer takuzu--spinner-timer))
  (setq takuzu--spinner-timer nil))

(defun takuzu--spin (buffer)
  "Advance the spinner frame and redraw BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq takuzu--spinner (1+ takuzu--spinner))
      (takuzu--redraw buffer))))

(defun takuzu--svg-generating ()
  "Build a minimal faceplate showing the generating spinner and message."
  (let* ((w (takuzu--faceplate-width)) (h (takuzu--faceplate-height))
         (svg (svg-create w h))
         (diff (plist-get takuzu--generating :difficulty))
         (n (plist-get takuzu--generating :size))
         (frame (aref takuzu--spinner-frames
                      (mod takuzu--spinner (length takuzu--spinner-frames)))))
    (svg-rectangle svg 0 0 w h :rx 16 :fill (takuzu--c :plate) :stroke (takuzu--c :plate-edge))
    (dolist (p (list (cons 12 12) (cons (- w 12) 12)
                     (cons 12 (- h 12)) (cons (- w 12) (- h 12))))
      (takuzu--draw-screw svg (car p) (cdr p)))
    (takuzu--draw-title svg takuzu--ppad takuzu--ppad)
    (takuzu--txt svg (/ w 2) (- (/ h 2) 4) frame 48 (takuzu--c :gold) "middle")
    (takuzu--txt svg (/ w 2) (+ (/ h 2) 36)
                 (format "generating a %s %d×%d puzzle…" diff n n)
                 13 (takuzu--c :dim) "middle")
    svg))

(defun takuzu--fit-scale (win)
  "Image scale that fits the faceplate into WIN at `takuzu--fill'."
  (let ((fw (takuzu--faceplate-width)) (fh (takuzu--faceplate-height)))
    (if (and win (> (window-body-width win t) 0) (> (window-body-height win t) 0))
        (max 0.4 (* takuzu--fill (min (/ (float (window-body-width win t)) fw)
                                      (/ (float (window-body-height win t)) fh))))
      1.0)))

(defun takuzu--ease-scale (buffer)
  "Ease `takuzu--scale' one frame toward the fit target and redraw BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((win (get-buffer-window buffer t)))
        (if (null win)
            (progn (when (timerp takuzu--scale-timer) (cancel-timer takuzu--scale-timer))
                   (setq takuzu--scale-timer nil))
          (let* ((target (takuzu--fit-scale win))
                 (cur (or takuzu--scale target)))
            (if (< (abs (- target cur)) 0.01)
                (progn (setq takuzu--scale target)
                       (when (timerp takuzu--scale-timer) (cancel-timer takuzu--scale-timer))
                       (setq takuzu--scale-timer nil))
              (setq takuzu--scale (+ cur (* takuzu--scale-step (- target cur)))))
            (takuzu--redraw buffer)))))))

(defun takuzu--redraw (&optional buffer)
  "Redraw BUFFER (or the current buffer) from state."
  (with-current-buffer (or buffer (current-buffer))
    (when (derived-mode-p 'takuzu-mode)
      (let ((inhibit-read-only t) (pt (point)))
        (erase-buffer)
        (if (takuzu--graphical-p)
            (let* ((win (get-buffer-window (current-buffer) t))
                   (cw (max 1 (frame-char-width)))
                   (ch (max 1 (frame-char-height)))
                   (winw (if win (window-body-width win t) 0))
                   (winh (if win (window-body-height win t) 0))
                   (fw (takuzu--faceplate-width))
                   (fh (takuzu--faceplate-height))
                   (target (takuzu--fit-scale win))
                   (scale (or takuzu--scale (setq takuzu--scale target)))
                   (sw (* fw scale)) (sh (* fh scale))
                   (hpad (max 0 (floor (/ (- winw sw) 2 cw))))
                   (toplines (max 0 (floor (/ (- winh sh) 2 ch)))))
              (when (and win (> (abs (- target scale)) 0.01)
                         (not (timerp takuzu--scale-timer)))
                (setq takuzu--scale-timer
                      (run-at-time 0 takuzu--scale-interval #'takuzu--ease-scale (current-buffer))))
              (insert (make-string toplines ?\n))
              (insert (make-string hpad ?\s))
              (insert-image (svg-image (if takuzu--generating
                                           (takuzu--svg-generating)
                                         (takuzu--svg))
                                       :scale scale)))
          (if takuzu--generating
              (insert (format "Generating a %s %dx%d puzzle…\n"
                              (plist-get takuzu--generating :difficulty)
                              takuzu--size takuzu--size))
            (insert (format "Takuzu  %dx%d  %s\n\n" takuzu--size takuzu--size takuzu--grade)
                    (takuzu--render-text)
                    (format "\n%s\n\nhjkl move  SPC cycle  U undo  N new  R reset  C check  P prove  q quit\n"
                            takuzu--status))))
        (goto-char (min pt (point-max)))))))

;; --- game actions ---

(defun takuzu--set-status (msg) "Set the status line to MSG." (setq takuzu--status msg))

(defmacro takuzu--playing-only (&rest body)
  "Run BODY unless a puzzle is generating; otherwise nudge and do nothing.
Guards board-dereferencing commands so a mid-generation keypress is inert."
  (declare (indent 0))
  `(cond (takuzu--armed (message "Press n to begin."))
         (takuzu--generating (message "Still generating a puzzle…"))
         (t ,@body)))

(defun takuzu--check-win ()
  "Note a win if the board is solved."
  (when (and (not takuzu--won) (takuzu-board-solved-p takuzu--board))
    (setq takuzu--won t takuzu--won-elapsed (takuzu--elapsed))
    (takuzu--set-status (format "Solved in %s -- nicely done" (takuzu--fmt-time takuzu--won-elapsed)))
    (takuzu--start-clock-flash (current-buffer))))

(defun takuzu--current-given-p ()
  "Non-nil if the cursor is on a given."
  (takuzu-board-given-p takuzu--board (car takuzu--cursor) (cdr takuzu--cursor)))

(defun takuzu--set-current (val)
  "Set the cursor cell to VAL, recording history, unless it is a given."
  (cond
   ((takuzu--current-given-p) (takuzu--set-status "That cell is a given -- it can't change."))
   ((or takuzu--won takuzu--proven) (takuzu--set-status "The puzzle is finished."))
   (t (let* ((r (car takuzu--cursor)) (c (cdr takuzu--cursor))
             (idx (+ (* r takuzu--size) c)))
        (push (cons idx (takuzu-board-ref takuzu--board r c)) takuzu--history)
        (takuzu-board-set takuzu--board r c val)
        (takuzu--set-status "")
        (takuzu--check-win))))
  (takuzu--redraw))

(defun takuzu--move (dr dc)
  "Move the cursor by DR rows and DC columns, clamped.
Clears any transient status message; the win/reveal note persists."
  (let ((n takuzu--size))
    (setq takuzu--cursor
          (cons (max 0 (min (1- n) (+ (car takuzu--cursor) dr)))
                (max 0 (min (1- n) (+ (cdr takuzu--cursor) dc)))))
    (unless (or takuzu--won takuzu--proven) (setq takuzu--status ""))
    (takuzu--redraw)))

(defun takuzu-up ()    "Move up."    (interactive) (takuzu--move -1 0))
(defun takuzu-down ()  "Move down."  (interactive) (takuzu--move 1 0))
(defun takuzu-left ()  "Move left."  (interactive) (takuzu--move 0 -1))
(defun takuzu-right () "Move right." (interactive) (takuzu--move 0 1))

(defun takuzu-cycle ()
  "Cycle the cursor cell empty -> 0 -> 1 -> empty."
  (interactive)
  (takuzu--playing-only
    (let ((v (takuzu-board-ref takuzu--board (car takuzu--cursor) (cdr takuzu--cursor))))
      (takuzu--set-current (cond ((null v) 0) ((eql v 0) 1) (t nil))))))

(defun takuzu-undo ()
  "Undo the last placement."
  (interactive)
  (takuzu--playing-only
  (cond
   ((or takuzu--won takuzu--proven) (takuzu--set-status "The puzzle is finished."))
   ((null takuzu--history) (takuzu--set-status "Nothing to undo."))
   (t (let* ((last (pop takuzu--history))
             (idx (car last)) (n takuzu--size))
        (takuzu-board-set takuzu--board (/ idx n) (mod idx n) (cdr last))
        (setq takuzu--cursor (cons (/ idx n) (mod idx n)))
        (takuzu--set-status ""))))
  (takuzu--redraw)))

(defun takuzu-hint ()
  "Fill one cell that current logic forces."
  (interactive)
  (takuzu--playing-only
  (if (or takuzu--won takuzu--proven)
      (takuzu--set-status "The puzzle is finished.")
    (let ((n takuzu--size) (found nil))
      (cl-block scan
        (dotimes (r n)
          (dotimes (c n)
            (when (null (takuzu-board-ref takuzu--board r c))
              (let ((vals (takuzu--legal-values takuzu--board r c)))
                (when (and vals (null (cdr vals)))
                  (setq found (list r c (car vals)))
                  (cl-return-from scan)))))))
      (if (not found)
          (takuzu--set-status "No cell is forced right now -- reason further.")
        (setq takuzu--cursor (cons (nth 0 found) (nth 1 found)))
        (takuzu--set-current (nth 2 found))
        (takuzu--set-status "Filled a forced cell."))))
  (takuzu--redraw)))

(defun takuzu-check ()
  "Report solved / full-but-wrong / unfinished."
  (interactive)
  (takuzu--playing-only
  (takuzu--check-win)
  (unless takuzu--won
    (takuzu--set-status
     (if (takuzu-board-full-p takuzu--board)
         "The board is full but a rule is broken."
       (format "Not finished -- %d cells left."
               (cl-count nil (append (takuzu-board-cells takuzu--board) nil))))))
  (takuzu--redraw)))

(defun takuzu-prove ()
  "Give up and show the full solution."
  (interactive)
  (takuzu--playing-only
  (when (yes-or-no-p "Show the full solution? ")
    (setf (takuzu-board-cells takuzu--board)
          (copy-sequence (takuzu-board-cells takuzu--solution)))
    (setq takuzu--proven t takuzu--won-elapsed (takuzu--elapsed))
    (takuzu--set-status "Solution shown.")
    (takuzu--start-clock-flash (current-buffer))
    (takuzu--redraw))))

(defun takuzu-reset ()
  "Clear every non-given cell."
  (interactive)
  (takuzu--playing-only
  (let ((n takuzu--size))
    (dotimes (r n)
      (dotimes (c n)
        (unless (takuzu-board-given-p takuzu--board r c)
          (takuzu-board-set takuzu--board r c nil))))
    (setq takuzu--won nil takuzu--proven nil takuzu--history nil
          takuzu--start-time (current-time))
    (takuzu--set-status "")
    (takuzu--redraw))))

(defun takuzu-toggle-assist ()
  "Toggle live rule-break highlighting."
  (interactive)
  (setq takuzu--assist (not takuzu--assist))
  (takuzu--set-status (if takuzu--assist "Assist on -- rule breaks highlight." "Assist off."))
  (takuzu--redraw))

(defun takuzu-new ()
  "Start the armed puzzle, or arm a fresh game (blank board, flashing New)."
  (interactive)
  (cond
   ((and takuzu--armed takuzu--pending)
    (takuzu--begin-play (current-buffer) takuzu--pending))
   (takuzu--armed
    ;; not generated yet: show the spinner and start as soon as it arrives
    (setq takuzu--pending-start t takuzu--spinner 0
          takuzu--generating (list :size (plist-get takuzu--armed :size)
                                   :difficulty (plist-get takuzu--armed :difficulty)))
    (takuzu--stop-timer)
    (setq takuzu--spinner-timer (run-at-time 0 0.1 #'takuzu--spin (current-buffer)))
    (takuzu--redraw))
   (t (takuzu-ui-arm takuzu--size (or takuzu--difficulty takuzu-default-difficulty)))))

(defun takuzu-cycle-size ()
  "Cycle to the next board size and arm a fresh game at that size."
  (interactive)
  (let* ((sizes takuzu-sizes)
         (next (nth (mod (1+ (or (cl-position takuzu--size sizes) 0)) (length sizes)) sizes)))
    (takuzu-ui-arm next (or takuzu--difficulty takuzu-default-difficulty))))

(defun takuzu-cycle-difficulty ()
  "Cycle to the next difficulty and arm a fresh game at that difficulty."
  (interactive)
  (let* ((all '(easy medium hard))
         (cur (or (cl-position (or takuzu--difficulty takuzu-default-difficulty) all) 0))
         (next (nth (mod (1+ cur) 3) all)))
    (takuzu-ui-arm takuzu--size next)))

;; --- mode ---

(defvar-keymap takuzu-mode-map
  :doc "Keymap for `takuzu-mode'."
  :parent special-mode-map
  "<up>" #'takuzu-up "<down>" #'takuzu-down "<left>" #'takuzu-left "<right>" #'takuzu-right
  "k" #'takuzu-up "j" #'takuzu-down "h" #'takuzu-left "l" #'takuzu-right
  "SPC" #'takuzu-cycle
  "u" #'takuzu-undo
  "?" #'takuzu-hint
  "c" #'takuzu-check
  "p" #'takuzu-prove
  "r" #'takuzu-reset
  "a" #'takuzu-toggle-assist
  "s" #'takuzu-cycle-size
  "d" #'takuzu-cycle-difficulty
  "n" #'takuzu-new)

(defun takuzu--stop-timer ()
  "Cancel the refresh timer if running."
  (when (timerp takuzu--timer) (cancel-timer takuzu--timer))
  (setq takuzu--timer nil))

(defun takuzu--cleanup ()
  "Cancel timers and any in-flight generation when the buffer goes away."
  (takuzu--stop-timer)
  (takuzu--stop-spinner)
  (when (timerp takuzu--scale-timer) (cancel-timer takuzu--scale-timer))
  (setq takuzu--scale-timer nil)
  (when (timerp takuzu--clock-flash-timer) (cancel-timer takuzu--clock-flash-timer))
  (setq takuzu--clock-flash-timer nil)
  (when (process-live-p takuzu--gen-process) (delete-process takuzu--gen-process)))

(defun takuzu--on-window-change (&rest _)
  "Re-center the board when the window is resized."
  (when (derived-mode-p 'takuzu-mode) (takuzu--redraw)))

(define-derived-mode takuzu-mode special-mode "Takuzu"
  "Major mode for playing Takuzu (Binairo)."
  (setq-local cursor-type nil)
  (setq-local truncate-lines t)
  (buffer-disable-undo)
  (add-hook 'kill-buffer-hook #'takuzu--cleanup nil t)
  (add-hook 'window-configuration-change-hook #'takuzu--on-window-change nil t))

(defun takuzu--begin-play (buf result)
  "Populate BUF from RESULT, start the clock, and begin play."
  (with-current-buffer buf
    (let ((board (plist-get result :board)))
      (setq takuzu--board board takuzu--solution (plist-get result :solution)
            takuzu--grade (plist-get result :grade)
            takuzu--size (takuzu-board-size board)
            takuzu--armed nil takuzu--pending nil takuzu--pending-start nil
            takuzu--generating nil takuzu--cursor '(0 . 0) takuzu--assist nil
            takuzu--history nil takuzu--start-time (current-time)
            takuzu--won nil takuzu--proven nil takuzu--won-elapsed 0 takuzu--status ""))
    (takuzu--stop-spinner)
    (takuzu--stop-timer)
    (let ((iv (takuzu--refresh-interval)))
      (setq takuzu--timer (run-at-time iv iv (lambda () (takuzu--redraw buf)))))
    (takuzu--start-clock-flash buf)
    (takuzu--redraw buf)))

(defun takuzu-ui-arm (size difficulty)
  "Open the game buffer blank at SIZE, ready to start DIFFICULTY on the New key.
The clock stays stopped and the New key flashes until the user starts; the
puzzle pre-generates in the background so starting is instant."
  (let ((buf (get-buffer-create "*Takuzu*")))
    (with-current-buffer buf
      (takuzu-mode)
      (takuzu--stop-timer)
      (takuzu--stop-spinner)
      (when (process-live-p takuzu--gen-process) (delete-process takuzu--gen-process))
      (setq takuzu--size size takuzu--difficulty difficulty
            takuzu--board (takuzu-make-board size)
            takuzu--solution nil takuzu--grade nil
            takuzu--armed (list :size size :difficulty difficulty)
            takuzu--pending nil takuzu--pending-start nil takuzu--generating nil
            takuzu--cursor '(0 . 0) takuzu--assist nil takuzu--history nil
            takuzu--start-time nil takuzu--won nil takuzu--proven nil
            takuzu--won-elapsed 0 takuzu--status "")
      (let ((iv (takuzu--refresh-interval)))
        (setq takuzu--timer (run-at-time iv iv (lambda () (takuzu--redraw buf)))))
      (setq takuzu--gen-process
            (takuzu-generate-async
             size difficulty
             (lambda (result)
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (setq takuzu--gen-process nil)
                   (cond
                    ((null result)
                     (takuzu--stop-spinner)
                     (setq takuzu--generating nil)
                     (takuzu--set-status "Generation failed -- press n to retry.")
                     (takuzu--redraw buf))
                    (takuzu--pending-start
                     (takuzu--stop-spinner)
                     (takuzu--begin-play buf result))
                    (t (setq takuzu--pending result)))))))))
    (switch-to-buffer buf)
    (takuzu--redraw buf)))

(provide 'takuzu-ui)
;;; takuzu-ui.el ends here
