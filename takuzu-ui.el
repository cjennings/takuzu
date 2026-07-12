;;; takuzu-ui.el --- Interactive hi-fi console for Takuzu -*- lexical-binding: t -*-

;; Author: Craig Jennings <craigmartinjennings@gmail.com>
;; Keywords: games

;;; Commentary:
;; `takuzu-mode' renders the whole game as one state-to-SVG faceplate image,
;; regenerated on every state change, in the Dupré instrument-console style
;; (design of record: docs/prototypes/takuzu-instruments-prototype-2.html).
;; The board is a panel of recessed lamp-sockets: empty cells are dark wells,
;; placed cells are matte colour discs, givens wear a silver bezel, the cursor
;; marks its cell with gold corner brackets.  A right instrument panel carries
;; nixie-tube TIME and SIZE readouts, a rotary LEVEL selector, a cells-LEFT
;; needle gauge, and a framed group of jewel STATE lamps; an event annunciator
;; strip under the board pulses momentary events (FIXED, HINT, INVALID, ...).
;;
;; Interaction is by keymap; the image regenerates per state change and on a
;; refresh interval (for the nixie clock, the flashing New key, and the event
;; pulse).  A plain-text board renders as a fallback on terminals, where SVG
;; is unavailable.

;;; Code:

(require 'cl-lib)
(require 'svg)
(require 'takuzu-board)
(require 'takuzu-solver)
(require 'takuzu-async)

;; Config knobs live in takuzu.el; forward-declare so this compiles clean.
(defvar takuzu-default-difficulty)
(defvar takuzu-sizes)
(defvar takuzu-flash-period)
(declare-function takuzu "takuzu" (&optional size difficulty))

;; --- palette (from the hi-fi prototype) ---

(defconst takuzu--colors
  '(:plate "#1b1710" :plate-edge "#2c2620"
    :well "#0a0c0d" :board-bg "#14110c" :socket "#0c0a07" :socket-edge "#05040a"
    :gold "#b99640" :gold-hi "#dcc061" :cream "#f3e7c5"
    :steel "#969385" :dim "#7c838a" :fail "#cb6b4d"
    :amber "#ff9a3c" :amber-off "#3a2a1c" :amber-hi "#ffbf7a"
    :disc0 "#4d5f75" :disc1 "#8f5236" :bezel "#a8aeb5"
    :disc0-edge "#6f839c" :disc1-edge "#b8734f"
    :slate "#424f5e" :wash "#2c2f32")
  "Faceplate palette, matching the prototype.")

(defun takuzu--c (key)
  "The palette colour for KEY."
  (plist-get takuzu--colors key))

;; --- layout ---

(defconst takuzu--gap 6 "Pixels between board cells.")
(defconst takuzu--bpad 14 "Board inner padding.")
(defconst takuzu--ppad 24 "Faceplate padding.")
(defconst takuzu--stage-gap 16 "Gap between the board and the right panel.")
(defconst takuzu--panel-w 88 "Right panel width.")
(defconst takuzu--panel-min-h 406
  "Minimum height of the instrument panel.
Below this the five framed instruments overprint each other, so small
boards stretch the stage to keep it.")
(defconst takuzu--title-h 78 "Title band height.")
(defconst takuzu--legend-h 64 "Legend (control rows) band height.")
(defconst takuzu--event-h 30 "Height of the event annunciator strip below the board.")
(defconst takuzu--event-tick 0.08 "Seconds between event-lamp pulse frames.")
(defconst takuzu--event-breath 2.8 "Seconds per breathing cycle of a pulsing event lamp.")
(defconst takuzu--event-dur (* 2 takuzu--event-breath)
  "Seconds an event lamp pulses before it goes dark.
A whole number of breathing cycles, so the pulse fades out instead of
snapping off mid-breath.")
(defconst takuzu--fill 0.85
  "Fraction of the window the faceplate is scaled to fill.
The SVG is vector, so this just rasterizes larger; 1.0 is edge-to-edge.")
(defconst takuzu--scale-step 0.2
  "Fraction of the remaining scale gap closed per animation frame.")
(defconst takuzu--scale-interval 0.02
  "Seconds between scale-animation frames.")

(defun takuzu--board-span (n)
  "Pixel span of the board block at size N: padding, cells, and gaps."
  (let ((cell (takuzu--cell-size n)))
    (+ (* 2 takuzu--bpad) (* n cell) (* (1- n) takuzu--gap))))

(defun takuzu--cell-size (n)
  "Pixel size of a cell on a board N cells wide."
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
(defvar-local takuzu--status "" "The current status message, shown in the text fallback.")
(defvar-local takuzu--event nil "Current momentary event symbol on the annunciator, or nil.")
(defvar-local takuzu--event-time nil "When the current event fired, for the pulse.")
(defvar-local takuzu--event-timer nil "Timer redrawing the event-lamp pulse.")
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
(defvar-local takuzu--help nil "Non-nil while the rules/help overlay is shown.")

;; --- helpers ---

(defmacro takuzu--cancel-timer (var)
  "Cancel the timer held in VAR if it is running, and set VAR to nil."
  `(progn
     (when (timerp ,var) (cancel-timer ,var))
     (setq ,var nil)))

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
  "Redraw interval fast enough to show flashing without over-drawing."
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

(defun takuzu--draw-faceplate-shell (svg w h &optional no-title)
  "Draw the plate background and corner screws on SVG sized W by H.
Also draw the TAKUZU title, unless NO-TITLE is non-nil (the instructions overlay
omits it)."
  (svg-rectangle svg 0 0 w h :rx 16 :fill (takuzu--c :plate) :stroke (takuzu--c :plate-edge))
  (dolist (p (list (cons 12 12) (cons (- w 12) 12)
                   (cons 12 (- h 12)) (cons (- w 12) (- h 12))))
    (takuzu--draw-screw svg (car p) (cdr p)))
  (unless no-title (takuzu--draw-title svg takuzu--ppad takuzu--ppad)))

(defun takuzu--empty-count ()
  "Number of still-empty cells on the current board."
  (seq-count #'null (takuzu-board-cells takuzu--board)))

(defun takuzu--fill-pct ()
  "Percent of the board that is filled, 0-100."
  (let* ((total (* takuzu--size takuzu--size))
         (filled (- total (takuzu--empty-count))))
    (* 100 (/ filled (float total)))))

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

(defun takuzu--draw-cursor (svg sx sy cell)
  "Draw the cursor on SVG as four corner brackets at SX,SY, cell size CELL.
Only the corners of the ring are drawn -- half the perimeter -- so the cursor
reads clearly with far fewer pixels than a full ring."
  (let* ((m 3)                          ; inset from the cell edge
         (a (round (* cell 0.3)))       ; arm length
         (gold (takuzu--c :gold))
         (x0 (+ sx m)) (y0 (+ sy m))
         (x1 (- (+ sx cell) m)) (y1 (- (+ sy cell) m)))
    (dolist (corner (list (list x0 y0 1 1)      ; top-left
                          (list x1 y0 -1 1)     ; top-right
                          (list x0 y1 1 -1)     ; bottom-left
                          (list x1 y1 -1 -1)))  ; bottom-right
      (pcase-let ((`(,cx ,cy ,dx ,dy) corner))
        (svg-line svg cx cy (+ cx (* dx a)) cy
                  :stroke gold :stroke-width 2 :stroke-linecap "round")
        (svg-line svg cx cy cx (+ cy (* dy a))
                  :stroke gold :stroke-width 2 :stroke-linecap "round")))))

(defun takuzu--draw-board (svg x y)
  "Draw the board on SVG with its top-left at X,Y."
  (let* ((n takuzu--size) (cell (takuzu--cell-size n))
         (gap takuzu--gap) (bpad takuzu--bpad)
         (span (takuzu--board-span n))
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
            (takuzu--draw-cursor svg sx sy cell)))))
    span))

(defun takuzu--draw-nixie-tube (svg x y w h ch lit)
  "Draw a nixie tube on SVG at X,Y size W,H showing string CH.
Glowing amber when LIT, a dim ember when not.  The digit scales with H."
  (svg-rectangle svg x y w h :rx 4 :fill "#0b0807" :stroke "#2c261d")
  (let ((fs (round (* h 0.62))))
    (when lit
      (svg-ellipse svg (+ x (/ w 2.0)) (+ y (* h 0.52)) (* w 0.42) (* h 0.34)
                   :fill "#ff8c32" :fill-opacity 0.16)
      (takuzu--txt svg (+ x (/ w 2)) (round (+ y (* h 0.72))) ch (+ fs 2)
                   (takuzu--c :amber-hi) "middle"))
    (takuzu--txt svg (+ x (/ w 2)) (round (+ y (* h 0.72))) ch fs
                 (if lit (takuzu--c :amber) (takuzu--c :amber-off)) "middle"))
  (svg-rectangle svg (+ x 1.5) (+ y 1.5) (- w 3) (* h 0.28) :rx 3
                 :fill "#ffffff" :fill-opacity 0.05))

(defun takuzu--draw-frame (svg x y w h label)
  "Draw an engraved instrument frame on SVG at X,Y size W,H.
LABEL is etched into a break in the frame's top edge, the shared caption
convention for every panel instrument."
  (svg-rectangle svg x y w h :rx 6 :fill "none"
                 :stroke (takuzu--c :wash) :stroke-width 1)
  (let* ((cx (+ x (/ w 2.0)))
         (lw (+ (* (length label) 7) 8)))
    (svg-rectangle svg (round (- cx (/ lw 2.0))) (- y 6) (round lw) 12
                   :fill (takuzu--c :well))
    (takuzu--txt svg (round cx) (+ y 3) label 9 (takuzu--c :steel) "middle")))

(defun takuzu--draw-nixie-size (svg cx y size)
  "Draw board SIZE on SVG as two nixie tubes centred at CX with their top at Y.
A single-digit size shows a ghost 0 in the tens tube so it reads as an idle
tube rather than a dead socket.  Up/down chevrons to the right hint that the
size is adjustable (the s key)."
  (let* ((tw 16) (th 23) (g 4)
         (s (format "%02d" size))
         (x0 (round (- cx 23)))
         (ax (+ x0 (* 2 tw) g 7)))
    (takuzu--draw-nixie-tube svg x0 y tw th (substring s 0 1) (>= size 10))
    (takuzu--draw-nixie-tube svg (+ x0 tw g) y tw th (substring s 1 2) t)
    (svg-polygon svg (list (cons ax (+ y 3)) (cons (+ ax 3) (+ y 9)) (cons (- ax 3) (+ y 9)))
                 :fill (takuzu--c :steel))
    (svg-polygon svg (list (cons ax (+ y th -1)) (cons (+ ax 3) (+ y th -7)) (cons (- ax 3) (+ y th -7)))
                 :fill (takuzu--c :steel))))

(defun takuzu--draw-nixie-time (svg cx y)
  "Draw elapsed time on SVG as M:SS nixie tubes centred at CX, top at Y."
  (let* ((str (takuzu--fmt-time (takuzu--elapsed)))
         (tw 15) (th 23) (g 3) (colw 7) (total 0))
    (dotimes (i (length str))
      (setq total (+ total (if (eq (aref str i) ?:) colw tw) (if (> i 0) g 0))))
    (let ((x (- cx (/ total 2))))
      (dotimes (i (length str))
        (when (> i 0) (setq x (+ x g)))
        (let ((ch (aref str i)))
          (if (eq ch ?:)
              (progn
                (takuzu--txt svg (round (+ x (/ colw 2))) (round (+ y (* th 0.64)))
                             ":" 15 "#ff9a3c" "middle")
                (setq x (+ x colw)))
            (takuzu--draw-nixie-tube svg (round x) y tw th (char-to-string ch) t)
            (setq x (+ x tw))))))))

(defun takuzu--draw-rotary-level (svg cx cy)
  "Draw the LEVEL rotary selector on SVG at CX,CY, pointer at the level."
  (let* ((r 19) (level (or takuzu--grade takuzu--difficulty))
         (specs '((easy "EASY" -46) (medium "MED" 0) (hard "HARD" 46)))
         (ang (or (nth 2 (assq level specs)) 0))
         (rad (lambda (a) (* (- a 90) (/ float-pi 180)))))
    (svg-circle svg cx cy r :fill "#1f1b17" :stroke "#3a352c" :stroke-width 1)
    (svg-circle svg (round (- cx (* r 0.3))) (round (- cy (* r 0.3))) (round (* r 0.5))
                :fill "#2a2622" :fill-opacity 0.45)
    (let* ((a (funcall rad ang))
           (px (+ cx (* (- r 5) (cos a)))) (py (+ cy (* (- r 5) (sin a)))))
      (svg-line svg cx cy (round px) (round py)
                :stroke (takuzu--c :gold-hi) :stroke-width 2 :stroke-linecap "round"))
    (dolist (spec specs)
      (let* ((lv (nth 0 spec)) (lab (nth 1 spec)) (la (funcall rad (nth 2 spec)))
             (lx (+ cx (* (+ r 12) (cos la)))) (ly (+ cy (* (+ r 12) (sin la))))
             (on (eq lv level)))
        (takuzu--txt svg (round lx) (round (+ ly 3)) lab 7
                     (if on (takuzu--c :gold-hi) (takuzu--c :steel)) "middle" (if on "bold" nil))))))

(defun takuzu--draw-needle-gauge (svg cx cy r pct value)
  "Draw an analog needle gauge on SVG with pivot at CX,CY radius R.
The needle sweeps left-to-right across the top by PCT (0-100); VALUE sits below."
  (dom-append-child svg
    (dom-node 'path (list (cons 'd (format "M %d %d A %d %d 0 0 1 %d %d"
                                           (- cx r) cy r r (+ cx r) cy))
                          (cons 'fill "none") (cons 'stroke (takuzu--c :wash))
                          (cons 'stroke-width "2"))))
  (cl-loop for i from 0 to 6 do
           (let* ((th (* (- 180 (* i 30)) (/ float-pi 180)))
                  (x1 (+ cx (* r (cos th)))) (y1 (- cy (* r (sin th))))
                  (x2 (+ cx (* (- r 5) (cos th)))) (y2 (- cy (* (- r 5) (sin th)))))
             (svg-line svg (round x1) (round y1) (round x2) (round y2)
                       :stroke (takuzu--c :steel) :stroke-width 1)))
  (let* ((th (* (- 180 (* (/ pct 100.0) 180)) (/ float-pi 180)))
         (nx (+ cx (* (- r 4) (cos th)))) (ny (- cy (* (- r 4) (sin th)))))
    (svg-line svg cx cy (round nx) (round ny)
              :stroke (takuzu--c :gold-hi) :stroke-width 2 :stroke-linecap "round"))
  (svg-circle svg cx cy 4 :fill (takuzu--c :gold))
  (takuzu--txt svg cx (+ cy 15) (number-to-string value) 12 (takuzu--c :cream) "middle" "bold"))

(defun takuzu--draw-panel (svg x y h)
  "Draw the right instrument panel on SVG at X,Y with height H.
Each instrument sits in an engraved frame with its caption etched into the
frame's top break.  The frames have fixed heights sized to their contents;
whatever height is left spreads as even gaps between them."
  (let* ((w takuzu--panel-w) (cx (+ x (/ w 2)))
         (fx (+ x 8)) (fw (- w 16))
         (time-h 44) (size-h 46) (level-h 84) (left-h 62) (state-h 110)
         (gap (/ (- h 12 8 time-h size-h level-h left-h state-h) 4.0))
         (fy (+ y 12.0)))
    (svg-rectangle svg x y w h :rx 10 :fill (takuzu--c :well) :stroke "#201d17")
    (takuzu--draw-frame svg fx (round fy) fw time-h "TIME")
    (takuzu--draw-nixie-time svg cx (+ (round fy) 10))
    (setq fy (+ fy time-h gap))
    (takuzu--draw-frame svg fx (round fy) fw size-h "SIZE")
    (takuzu--draw-nixie-size svg cx (+ (round fy) 11) takuzu--size)
    (setq fy (+ fy size-h gap))
    (takuzu--draw-frame svg fx (round fy) fw level-h "LEVEL")
    (takuzu--draw-rotary-level svg cx (+ (round fy) 48))
    (setq fy (+ fy level-h gap))
    (takuzu--draw-frame svg fx (round fy) fw left-h "LEFT")
    (takuzu--draw-needle-gauge svg cx (+ (round fy) 38) 26
                               (takuzu--fill-pct) (takuzu--empty-count))
    (setq fy (+ fy left-h gap))
    (takuzu--draw-state-lamps svg fx (round fy) fw (- (+ y h) 8 (round fy)))))

(defun takuzu--legend-glyph (svg cx y size key color &optional underline)
  "Draw KEY on SVG at CX,Y in monospace SIZE and COLOR; underline when UNDERLINE."
  (svg-text svg key :x cx :y y :font-family "monospace" :font-size size :fill color
            :font-weight (if underline "bold" "normal")
            :text-decoration (if underline "underline" "none")))

(defun takuzu--legend-item-width (it charw kgap)
  "Estimated pixel width of legend item IT with CHARW and key-gap KGAP."
  (pcase (car it)
    ((or 'word 'flashword) (* (length (nth 1 it)) charw))
    (_ (+ (* (length (nth 1 it)) charw) kgap (* (length (nth 2 it)) charw)))))

(defun takuzu--draw-legend-line (svg x y width size items)
  "Draw legend ITEMS justified across WIDTH on SVG from X at baseline Y.
Each item is (word WORD) -- word with its first letter gold-underlined -- or
\\(keyed KEY LABEL) -- KEY gold-underlined then LABEL dim."
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
                 (takuzu--legend-glyph svg (+ cx (* (length k) charw) kgap) y size l (takuzu--c :dim)))))
             (setq cx (+ cx iw inter)))))

(defun takuzu--draw-engrave (svg x y width label)
  "Draw an engraved section LABEL on SVG at X,Y with a hairline across WIDTH."
  (takuzu--txt svg x y label 9 (takuzu--c :steel))
  (svg-line svg (+ x (* (length label) 7) 12) (- y 3) (+ x width) (- y 3)
            :stroke (takuzu--c :wash) :stroke-width 1))

(defun takuzu--strip-width ()
  "Width of the strip under the board: the board plus the right panel."
  (+ (takuzu--board-span takuzu--size) takuzu--stage-gap takuzu--panel-w))

(defun takuzu--lerp-color (a b k)
  "Blend hex colours A and B by K in [0,1], returning a hex string."
  (cl-flet ((chan (s i) (string-to-number (substring s (1+ (* i 2)) (+ 3 (* i 2))) 16)))
    (format "#%02x%02x%02x"
            (max 0 (min 255 (round (+ (chan a 0) (* k (- (chan b 0) (chan a 0)))))))
            (max 0 (min 255 (round (+ (chan a 1) (* k (- (chan b 1) (chan a 1)))))))
            (max 0 (min 255 (round (+ (chan a 2) (* k (- (chan b 2) (chan a 2))))))))))

(defun takuzu--event-intensity ()
  "Pulse intensity 0..1 of the current event, a slow breathing over elapsed time."
  (if (null takuzu--event-time) 0
    (let ((e (float-time (time-subtract (current-time) takuzu--event-time))))
      (* 0.5 (- 1 (cos (/ (* 2 float-pi e) takuzu--event-breath)))))))

(defun takuzu--draw-event-annunciator (svg x y w h)
  "Draw the EVENT annunciator strip on SVG at X,Y size W,H.
Six momentary-event legends; the active one (`takuzu--event') pulses like a warm
incandescent lamp, then fades."
  (svg-rectangle svg x y w h :rx 10 :fill (takuzu--c :well) :stroke "#201d17")
  (let* ((events '((fixed . "FIXED") (hint . "HINT") (no-hint . "NO HINT")
                   (invalid . "INVALID") (nothing . "NOTHING") (gen-fail . "GEN FAIL")))
         (n (length events)) (pad 12) (gap 4)
         (cw (/ (- w (* 2 pad) (* (1- n) gap)) (float n)))
         (ch (- h 16)) (cy (+ y 8))
         (k (takuzu--event-intensity)))
    (cl-loop for cell in events for i from 0 do
             (let* ((ev (car cell)) (lab (cdr cell))
                    (cx (+ x pad (* i (+ cw gap))))
                    (lit (if (eq ev takuzu--event) k 0))
                    (bg (takuzu--lerp-color "#141210" "#a8843a" lit))
                    (fg (takuzu--lerp-color (takuzu--c :dim) "#1c1710" lit)))
               (svg-rectangle svg (round cx) cy (round cw) (round ch) :rx 3
                              :fill bg :stroke "#262320")
               (takuzu--txt svg (round (+ cx (/ cw 2))) (round (+ cy (/ ch 2) 3))
                            lab 8.5 fg "middle" "bold")))))

(defun takuzu--draw-jewel (svg cx cy r color on)
  "Draw a jewel pilot lamp on SVG at CX,CY radius R in COLOR; dim when ON is nil."
  (if on
      (progn
        (svg-circle svg cx cy (+ r 3) :fill color :fill-opacity 0.25)
        (svg-circle svg cx cy r :fill color :stroke "#00000055" :stroke-width 0.6)
        (svg-circle svg (round (- cx (* r 0.35))) (round (- cy (* r 0.35))) (round (* r 0.4))
                    :fill "#ffffff" :fill-opacity 0.6))
    (svg-circle svg cx cy r :fill "#241f1b" :stroke "#0e0c0a" :stroke-width 0.5)))

(defun takuzu--game-state ()
  "The current game STATE symbol: ready, solving, solved, or shown."
  (cond (takuzu--armed 'ready) (takuzu--won 'solved)
        (takuzu--proven 'shown) (t 'solving)))

(defun takuzu--draw-state-lamps (svg x y w h)
  "Draw the framed STATE lamp group on SVG at X,Y size W,H.
READY/SOLVING/SOLVED/SHOWN track the game state; ASSIST is its own mode lamp."
  (takuzu--draw-frame svg x y w h "STATE")
  (let* ((state (takuzu--game-state))
         (lamps `((ready "READY" "#6fce33" ,(eq state 'ready))
                  (solving "SOLVING" "#ffb43a" ,(eq state 'solving))
                  (solved "SOLVED" "#6fce33" ,(eq state 'solved))
                  (shown "SHOWN" "#cb6b4d" ,(eq state 'shown))
                  (assist "ASSIST" "#63e6c8" ,takuzu--assist)))
         (n (length lamps)) (top (+ y 16)) (bot (- (+ y h) 12))
         (rowstep (/ (- bot top) (float (1- n)))) (jx (+ x 22)))
    (cl-loop for lamp in lamps for i from 0 do
             (let ((ly (round (+ top (* i rowstep)))) (lab (nth 1 lamp))
                   (col (nth 2 lamp)) (on (nth 3 lamp)))
               (takuzu--draw-jewel svg jx ly 6 col on)
               (takuzu--txt svg (+ jx 13) (+ ly 3) lab 7
                            (if on (takuzu--c :cream) (takuzu--c :steel)) "start")))))

(defun takuzu--draw-legend (svg x y width)
  "Draw the two engraved control sections on SVG at X,Y across WIDTH.
GAME (session keys) sits on top, PLAY (solving keys) below.  While armed, the
New key flashes to prompt the start."
  (let ((new-item (if takuzu--armed
                      (list 'flashword "NEW" (takuzu--flash-on-p))
                    '(word "NEW"))))
    (takuzu--draw-engrave svg x y width "GAME")
    (takuzu--draw-legend-line
     svg x (+ y 16) width 10
     `(,new-item (word "RESET") (word "SIZE") (word "LEVEL")
       (word "PROVE") (word "INSTRUCTIONS") (word "QUIT")))
    (takuzu--draw-engrave svg x (+ y 36) width "PLAY")
    (takuzu--draw-legend-line
     svg x (+ y 52) width 10
     `((keyed "SPC" "CYCLE") (word "UNDO") (keyed "?" "HINT")
       (word "CHECK") (word "ASSIST")))))

(defun takuzu--faceplate-width ()
  "Pixel width of the faceplate for the current board size."
  (+ (* 2 takuzu--ppad) (max (takuzu--strip-width) 380)))

(defun takuzu--panel-top ()
  "Y of the panel's top edge.
At sizes 8+ the board is tall enough that the panel can start at the board's
top edge; smaller boards start it up in the title band so the instruments
get more room."
  (if (>= takuzu--size 8)
      (+ takuzu--ppad takuzu--title-h)
    (+ takuzu--ppad 6)))

(defun takuzu--stage-bottom ()
  "Y of the stage's bottom edge: the lower of the board and panel bottoms.
A small board is shorter than the instrument stack, so the stage stretches
to keep the panel at `takuzu--panel-min-h' and the board centres in the
extra room."
  (max (+ takuzu--ppad takuzu--title-h (takuzu--board-span takuzu--size))
       (+ (takuzu--panel-top) takuzu--panel-min-h)))

(defun takuzu--faceplate-height ()
  "Pixel height of the faceplate for the current board size."
  (+ (takuzu--stage-bottom) 12 takuzu--event-h 14 takuzu--legend-h takuzu--ppad))

(defun takuzu--svg ()
  "Build the faceplate SVG for the current state."
  (let* ((n takuzu--size)
         (boardw (takuzu--board-span n))
         (ppad takuzu--ppad)
         (w (takuzu--faceplate-width))
         (stagey (+ ppad takuzu--title-h))
         (bottom (takuzu--stage-bottom))
         (boardy (+ stagey (/ (- bottom stagey boardw) 2)))
         (h (takuzu--faceplate-height))
         (svg (svg-create w h)))
    (takuzu--draw-faceplate-shell svg w h)
    (takuzu--draw-board svg ppad boardy)
    (let* ((px (+ ppad boardw takuzu--stage-gap))
           (ptop (takuzu--panel-top)))
      (takuzu--draw-panel svg px ptop (- bottom ptop)))
    (let ((evy (+ bottom 12)))
      (takuzu--draw-event-annunciator svg ppad evy (takuzu--strip-width) takuzu--event-h)
      (takuzu--draw-legend svg ppad (+ evy takuzu--event-h 14) (- w (* 2 ppad))))
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
  (takuzu--cancel-timer takuzu--spinner-timer))

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
    (takuzu--draw-faceplate-shell svg w h)
    (takuzu--txt svg (/ w 2) (- (/ h 2) 4) frame 48 (takuzu--c :gold) "middle")
    (takuzu--txt svg (/ w 2) (+ (/ h 2) 36)
                 (format "generating a %s %d×%d puzzle…" diff n n)
                 13 (takuzu--c :dim) "middle")
    svg))

(defconst takuzu--rules
  '(("No three cells of the same colour"
     "may sit adjacent in a row or column.")
    ("Each row and each column must hold"
     "an equal number of both colours.")
    ("No two rows may be identical, and"
     "no two columns may be identical."))
  "The three Takuzu rules, each a list of two wrapped lines, for the overlay.")

(defun takuzu--help-etch (svg x y str size anchor ink hi &optional weight)
  "Draw etched STR on SVG at X,Y: a light lower highlight then dark INK on top.
ANCHOR is the text anchor, HI the emboss highlight, WEIGHT the font weight."
  (svg-text svg str :x x :y (1+ y) :font-family "monospace" :font-size size
            :fill hi :fill-opacity 0.5 :text-anchor anchor :font-weight (or weight "normal"))
  (svg-text svg str :x x :y y :font-family "monospace" :font-size size
            :fill ink :text-anchor anchor :font-weight (or weight "normal")))

(defun takuzu--help-print (svg x y str size anchor &optional weight)
  "Draw STR on SVG at X,Y as flat printed black text, no etch."
  (svg-text svg str :x x :y y :font-family "monospace" :font-size size
            :fill "#000000" :text-anchor anchor :font-weight (or weight "normal")))

(defun takuzu--help-divider (svg x1 x2 y ink hi)
  "Draw an engraved divider on SVG from X1 to X2 at Y, dark INK over highlight HI."
  (svg-line svg x1 (1+ y) x2 (1+ y) :stroke hi :stroke-opacity 0.5)
  (svg-line svg x1 y x2 y :stroke ink :stroke-opacity 0.55))

(defun takuzu--help-plate (svg x y w h)
  "Draw a brushed-silver spec plate on SVG at X,Y size W,H, with sheen and rivets."
  (let ((base "#74787d") (light "#909498") (dark "#52565b") (edge (takuzu--c :plate-edge)))
    (svg-rectangle svg x y w h :rx 9 :fill base :stroke edge :stroke-width 1.4)
    ;; soft brushed-metal sheen: a light wash fading down from the top and a
    ;; shadow deepening toward the bottom, replacing the old hard reflection bands
    (let* ((bands 26) (bh (+ 1.0 (/ h bands))))
      (dotimes (i bands)
        (let* ((frac (/ i (float bands))) (by (+ y (* frac h))))
          (svg-rectangle svg (+ x 3) by (- w 6) bh :fill light :fill-opacity (* 0.45 (- 1 frac)))
          (svg-rectangle svg (+ x 3) by (- w 6) bh :fill dark :fill-opacity (* 0.40 frac)))))
    (cl-loop for yy from (+ y 6) to (- (+ y h) 6) by 2 do
             (svg-line svg (+ x 6) yy (- (+ x w) 6) yy :stroke "#ffffff" :stroke-opacity 0.04))
    (svg-rectangle svg (+ x 8) (+ y 8) (- w 16) (- h 16) :rx 6 :fill "none"
                   :stroke edge :stroke-opacity 0.4)
    (dolist (p (list (cons (+ x 16) (+ y 16)) (cons (- (+ x w) 16) (+ y 16))
                     (cons (+ x 16) (- (+ y h) 16)) (cons (- (+ x w) 16) (- (+ y h) 16))))
      (svg-circle svg (car p) (cdr p) 3.4 :fill light :stroke edge :stroke-width 0.9)
      (svg-circle svg (- (car p) 1) (- (cdr p) 1) 1.2 :fill "#ffffff" :fill-opacity 0.7))))

(defun takuzu--help-emblem (svg cx cy kind ink hi)
  "Draw a faux-compliance emblem of KIND on SVG at CX,CY, etched in INK/HI."
  (pcase kind
    ('ce (takuzu--help-etch svg cx (+ cy 4) "CE" 13 "middle" ink hi "bold"))
    ('class2
     (svg-rectangle svg (- cx 9) (- cy 9) 18 18 :fill "none" :stroke ink :stroke-opacity 0.7)
     (svg-rectangle svg (- cx 6) (- cy 6) 12 12 :fill "none" :stroke ink :stroke-opacity 0.7)
     (takuzu--help-etch svg cx (+ cy 4) "II" 10 "middle" ink hi "bold"))
    ('warn
     (svg-polygon svg (list (cons cx (- cy 9)) (cons (+ cx 9) (+ cy 7)) (cons (- cx 9) (+ cy 7)))
                  :fill "none" :stroke ink :stroke-opacity 0.7 :stroke-linejoin "round")
     (takuzu--help-etch svg cx (+ cy 6) "!" 11 "middle" ink hi "bold"))))

(defun takuzu--help-disc (svg cx cy r fill)
  "Draw a small filled disc on SVG at CX,CY radius R in FILL."
  (svg-circle svg cx cy r :fill fill :stroke "#00000055" :stroke-width 0.8))

(defun takuzu--help-rule-widget (svg x y kind d0 d1 fail)
  "Draw the diagram for rule KIND on SVG at left edge X, vertical centre Y.
D0, D1 and FAIL are the two disc colours and the strike colour.  The widget
stays in a narrow left column so it never overlaps the rule text."
  (let ((r 5) (g 13))
    (pcase kind
      (1 (dotimes (i 3) (takuzu--help-disc svg (+ x (* i g)) y r d1))
         (svg-line svg (- x 7) (+ y 7) (+ x (* 2 g) 7) (- y 7)
                   :stroke fail :stroke-width 2.4 :stroke-linecap "round"))
      ;; a legal row with equal counts and no triple (three of each colour),
      ;; so the diagram itself does not break rule 1 above it
      (2 (let ((row (list d0 d0 d1 d1 d0 d1)) (r2 4) (g2 10))
           (dotimes (i 6) (takuzu--help-disc svg (+ x (* i g2)) y r2 (nth i row)))))
      (3 (let ((top (list d1 d0 d1)) (bot (list d0 d1 d0)))
           (dotimes (i 3) (takuzu--help-disc svg (+ x (* i g)) (- y 7) r (nth i top)))
           (dotimes (i 3) (takuzu--help-disc svg (+ x (* i g)) (+ y 7) r (nth i bot))))))))

(defun takuzu--draw-help-dedication (svg cx y)
  "Draw the dedication on SVG centred at CX, baseline Y.
Printed in flat black; only the name is embossed silver, raised off the plate.
The line is sized down so the full name fits within the plate."
  (let* ((size 10) (a (* size 0.6))
         (pre "Dedicated to ") (name "Christine Ciarmello")
         (post ", with thanks for the inspiration.")
         (total (+ (length pre) (length name) (length post)))
         (x0 (- cx (/ (* total a) 2)))
         (nx (+ x0 (* (length pre) a)))
         (ne (+ nx (* (length name) a)))
         (silver "#9ba0a8"))
    (takuzu--help-print svg x0 y pre size "start")
    ;; the name, embossed silver: dark shadow below, bright highlight above, face on top
    (svg-text svg name :x nx :y (+ y 1) :font-family "monospace" :font-size size
              :fill "#000000" :fill-opacity 0.6 :text-anchor "start" :font-weight "bold")
    (svg-text svg name :x nx :y (- y 1) :font-family "monospace" :font-size size
              :fill "#ffffff" :fill-opacity 0.85 :text-anchor "start" :font-weight "bold")
    (svg-text svg name :x nx :y y :font-family "monospace" :font-size size
              :fill silver :text-anchor "start" :font-weight "bold")
    (takuzu--help-print svg ne y post size "start")))

(defun takuzu--draw-help-card (svg x y w h ink hi d0 d1 fail)
  "Draw the spec-plate card into SVG filling the box X,Y,W,H.
Frame, brushed plate, header (title + model/serial + aliases), the three rules
each with a diagram, the compliance footer, and the dedication.  Callers scale
and position the card with a group transform, so all offsets here are relative
to the box.  INK/HI are the etch colours; D0/D1/FAIL the rule-diagram colours."
  (let* ((lx (+ x 26)) (rx (- (+ x w) 26)) (cx (+ x (/ w 2)))
         (wx (+ lx 20)) (tx (+ lx 80)))
    ;; faceplating: an engraved frame the plate is set into
    (svg-rectangle svg (- x 6) (- y 6) (+ w 12) (+ h 12) :rx 13 :fill "none"
                   :stroke (takuzu--c :plate-edge) :stroke-width 1.2)
    (takuzu--help-plate svg x y w h)
    ;; header
    (takuzu--help-etch svg lx (+ y 40) "TAKUZU" 26 "start" ink hi "bold")
    (takuzu--help-etch svg rx (+ y 45) "MODEL TKZ-06" 11 "end" ink hi)
    (takuzu--help-etch svg rx (+ y 60) "SERIAL 2026-0711-CJ" 10 "end" ink hi)
    (takuzu--help-etch svg lx (+ y 60) "BINAIRO  -  TOHU WA-VOHU  -  BINARY LOGIC" 10 "start" ink hi)
    (takuzu--help-divider svg lx rx (+ y 76) ink hi)
    ;; footer, anchored to the bottom
    (takuzu--help-divider svg lx rx (+ y h -100) ink hi)
    (let ((ey (+ y h -78)))
      (takuzu--help-emblem svg (+ lx 12) ey 'ce "#000000" "#000000")
      (takuzu--help-emblem svg (+ lx 52) ey 'class2 "#000000" "#000000")
      (takuzu--help-emblem svg (+ lx 92) ey 'warn "#000000" "#000000")
      (takuzu--help-print svg rx (- ey 4) "CONFORMS TO BINARY-LOGIC STANDARD 3" 10 "end")
      (takuzu--help-print svg rx (+ ey 10) "CLASS II  -  SIZES 4 TO 12  -  ONE SOLUTION" 10 "end"))
    (takuzu--help-print svg lx (+ y h -50) "MADE IN NEW ORLEANS, LA, USA  -  (c) CRAIG JENNINGS 2026" 10 "start")
    (takuzu--draw-help-dedication svg cx (+ y h -26))
    ;; rules block, centred between the two dividers
    (let* ((rtop (+ y 92)) (rbot (+ y h -112)) (blockh 165)
           (y0 (+ rtop (max 0 (/ (- (- rbot rtop) blockh) 2)))))
      (takuzu--help-print svg lx y0 "HOW TO PLAY" 12 "start" "bold")
      (takuzu--help-print svg lx (+ y0 18) "Fill the grid with two colours so that:" 11 "start")
      (let ((ry (+ y0 50)))
        (cl-loop for rule in takuzu--rules for n from 1 do
                 (takuzu--help-rule-widget svg wx (+ ry 5) n d0 d1 fail)
                 (takuzu--help-print svg tx ry (nth 0 rule) 12 "start")
                 (takuzu--help-print svg tx (+ ry 17) (nth 1 rule) 12 "start")
                 (setq ry (+ ry 48)))))))

(defconst takuzu--help-card-w 452 "Natural width of the spec-plate card before scaling.")
(defconst takuzu--help-card-h 438 "Natural height of the spec-plate card before scaling.")
(defconst takuzu--help-card-frac 0.8
  "Card width as a fraction of the faceplate width; the plate is centred.")

(defun takuzu--svg-help ()
  "Build the instructions overlay at the game faceplate's size.
Border and screws match the board view so they never move; the board,
instruments, and title give way to a large plate centred in the faceplate."
  (let* ((w (takuzu--faceplate-width)) (h (takuzu--faceplate-height))
         (svg (svg-create w h))
         (ink "#16181c") (hi "#ffffff")
         (d0 (takuzu--c :disc0)) (d1 (takuzu--c :disc1)) (fail (takuzu--c :fail))
         (cw takuzu--help-card-w) (ch takuzu--help-card-h)
         (s (/ (* takuzu--help-card-frac w) cw))
         (gx (round (/ (- w (* cw s)) 2)))
         (gy (round (/ (- h (* ch s)) 2)))
         (card (dom-node 'g (list (cons 'transform
                                        (format "translate(%d %d) scale(%s)" gx gy s))))))
    (takuzu--draw-faceplate-shell svg w h t)
    (takuzu--draw-help-card card 0 0 cw ch ink hi d0 d1 fail)
    (dom-append-child svg card)
    svg))

(defun takuzu--view-width ()
  "Pixel width of the display; overlay matches the faceplate to avoid a jump."
  (takuzu--faceplate-width))

(defun takuzu--view-height ()
  "Pixel height of the display; overlay matches the faceplate to avoid a jump."
  (takuzu--faceplate-height))

(defun takuzu--fit-scale (win)
  "Image scale that fits the current view into WIN at `takuzu--fill'."
  (let ((fw (takuzu--view-width)) (fh (takuzu--view-height)))
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
            (takuzu--cancel-timer takuzu--scale-timer)
          (let* ((target (takuzu--fit-scale win))
                 (cur (or takuzu--scale target)))
            (if (< (abs (- target cur)) 0.01)
                (progn (setq takuzu--scale target)
                       (takuzu--cancel-timer takuzu--scale-timer))
              (setq takuzu--scale (+ cur (* takuzu--scale-step (- target cur)))))
            (takuzu--redraw buffer)))))))

(defun takuzu--redraw-graphical ()
  "Insert the faceplate image, centred and scaled to the current window.
Kicks off the scale-easing timer when the displayed scale is off target."
  (let* ((win (get-buffer-window (current-buffer) t))
         (cw (max 1 (frame-char-width)))
         (ch (max 1 (frame-char-height)))
         (winw (if win (window-body-width win t) 0))
         (winh (if win (window-body-height win t) 0))
         (target (takuzu--fit-scale win))
         (scale (or takuzu--scale (setq takuzu--scale target)))
         (sw (* (takuzu--view-width) scale)) (sh (* (takuzu--view-height) scale))
         (hpad (max 0 (floor (/ (- winw sw) 2 cw))))
         (toplines (max 0 (floor (/ (- winh sh) 2 ch)))))
    (when (and win (> (abs (- target scale)) 0.01)
               (not (timerp takuzu--scale-timer)))
      (setq takuzu--scale-timer
            (run-at-time 0 takuzu--scale-interval #'takuzu--ease-scale (current-buffer))))
    (insert (make-string toplines ?\n))
    (insert (make-string hpad ?\s))
    (insert-image (svg-image (cond (takuzu--help (takuzu--svg-help))
                                   (takuzu--generating (takuzu--svg-generating))
                                   (t (takuzu--svg)))
                             :scale scale))))

(defun takuzu--redraw-textual ()
  "Insert the plain-text fallback for the current state (help/generating/board)."
  (cond
   (takuzu--help
    (insert "TAKUZU -- HOW TO PLAY\n\nFill the grid with two colours so that:\n\n")
    (cl-loop for rule in takuzu--rules for n from 1 do
             (insert (format "  %d. %s\n" n (string-join rule " "))))
    (insert "\n(c) Craig Jennings, 2026\nDedicated to Christine, with thanks for the inspiration.\n"))
   (takuzu--generating
    (insert (format "Generating a %s %dx%d puzzle…\n"
                    (plist-get takuzu--generating :difficulty)
                    takuzu--size takuzu--size)))
   (t
    (insert (format "Takuzu  %dx%d  %s\n\n" takuzu--size takuzu--size takuzu--grade)
            (takuzu--render-text)
            (format "\n%s\n\narrows move  SPC cycle  u undo  ? hint  c check  a assist  n new  r reset  s size  l level  p prove  i instructions  q quit\n"
                    takuzu--status)))))

(defun takuzu--redraw (&optional buffer)
  "Redraw BUFFER (or the current buffer) from state."
  (with-current-buffer (or buffer (current-buffer))
    (when (derived-mode-p 'takuzu-mode)
      (let ((inhibit-read-only t) (pt (point)))
        (erase-buffer)
        (if (takuzu--graphical-p)
            (takuzu--redraw-graphical)
          (takuzu--redraw-textual))
        (goto-char (min pt (point-max)))))))

;; --- game actions ---

(defun takuzu--event-of (msg)
  "Map a status MSG to an annunciator event symbol, or nil for a state message."
  (cond ((string-prefix-p "That cell is a given" msg) 'fixed)
        ((string-prefix-p "Filled a forced" msg) 'hint)
        ((string-prefix-p "No cell is forced" msg) 'no-hint)
        ((string-prefix-p "The board is full" msg) 'invalid)
        ((string-prefix-p "Nothing to undo" msg) 'nothing)
        ((string-prefix-p "Generation failed" msg) 'gen-fail)))

(defun takuzu--set-status (msg)
  "Set the status MSG and flash its event lamp.
A non-empty MSG with no lamp of its own is echoed instead, so the keypress
still gives visible feedback in the graphical UI."
  (setq takuzu--status msg)
  (let ((event (takuzu--event-of msg)))
    (takuzu--signal-event event)
    (when (and (null event) (not (string-empty-p msg)))
      (message "%s" msg))))

(defun takuzu--signal-event (event)
  "Light the EVENT annunciator lamp for EVENT and pulse it; nil clears the strip.
The pulse timer runs even when the buffer is undisplayed -- it is also the
expiry mechanism, and a lamp lit in a buried buffer must still go dark."
  (takuzu--cancel-timer takuzu--event-timer)
  (if (null event)
      (setq takuzu--event nil takuzu--event-time nil)
    (setq takuzu--event event takuzu--event-time (current-time))
    (setq takuzu--event-timer
          (run-at-time 0 takuzu--event-tick #'takuzu--event-pulse (current-buffer)))))

(defun takuzu--event-pulse (buf)
  "Redraw the event-lamp pulse in BUF; clear and stop once the pulse elapses."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (if (or (null takuzu--event-time)
              (> (float-time (time-subtract (current-time) takuzu--event-time))
                 takuzu--event-dur))
          (progn (setq takuzu--event nil takuzu--event-time nil)
                 (takuzu--cancel-timer takuzu--event-timer)
                 (takuzu--redraw buf))
        (takuzu--redraw buf)))))

(defun takuzu--close-help ()
  "Dismiss the help overlay and redraw."
  (setq takuzu--help nil)
  (takuzu--redraw))

(defconst takuzu--msg-finished "The puzzle is finished."
  "Status message for a keypress on an already-finished puzzle.")

(defun takuzu--current-difficulty ()
  "The difficulty in effect: the requested one, falling back to the default."
  (or takuzu--difficulty takuzu-default-difficulty))

(defmacro takuzu--playing-only (&rest body)
  "Run BODY unless a puzzle is generating; otherwise nudge and do nothing.
Guards board-dereferencing commands so a mid-generation keypress is inert.
While the help overlay is up, a game key just dismisses it."
  (declare (indent 0))
  `(cond (takuzu--help (takuzu--close-help))
         (takuzu--armed (message "Press n to begin."))
         (takuzu--generating (message "Still generating a puzzle…"))
         (t ,@body)))

(defun takuzu--check-win ()
  "Note a win if the board is solved.
The elapsed time is read before the won flag flips: once the flag is set,
`takuzu--elapsed' returns the frozen value, so reading it after would record
the stale zero instead of the solve time."
  (when (and (not takuzu--won) (takuzu-board-solved-p takuzu--board))
    (let ((elapsed (takuzu--elapsed)))
      (setq takuzu--won t takuzu--won-elapsed elapsed))
    (takuzu--set-status (format "Solved in %s -- nicely done" (takuzu--fmt-time takuzu--won-elapsed)))))

(defun takuzu--current-given-p ()
  "Non-nil if the cursor is on a given."
  (takuzu-board-given-p takuzu--board (car takuzu--cursor) (cdr takuzu--cursor)))

(defun takuzu--set-current (val)
  "Set the cursor cell to VAL, recording history, unless it is a given."
  (cond
   ((takuzu--current-given-p) (takuzu--set-status "That cell is a given -- it can't change."))
   ((or takuzu--won takuzu--proven) (takuzu--set-status takuzu--msg-finished))
   (t (let* ((r (car takuzu--cursor)) (c (cdr takuzu--cursor))
             (idx (+ (* r takuzu--size) c)))
        (push (cons idx (takuzu-board-ref takuzu--board r c)) takuzu--history)
        (takuzu-board-set takuzu--board r c val)
        (takuzu--set-status "")
        (takuzu--check-win))))
  (takuzu--redraw))

(defun takuzu--move (dr dc)
  "Move the cursor by DR rows and DC columns, clamped.
Clears any transient status message; the win/reveal note persists.
While the help overlay is up, a movement key dismisses it instead of moving."
  (if takuzu--help
      (takuzu--close-help)
    (let ((n takuzu--size))
      (setq takuzu--cursor
            (cons (max 0 (min (1- n) (+ (car takuzu--cursor) dr)))
                  (max 0 (min (1- n) (+ (cdr takuzu--cursor) dc)))))
      (unless (or takuzu--won takuzu--proven) (setq takuzu--status ""))
      (takuzu--redraw))))

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
   ((or takuzu--won takuzu--proven) (takuzu--set-status takuzu--msg-finished))
   ((null takuzu--history) (takuzu--set-status "Nothing to undo."))
   (t (let* ((last (pop takuzu--history))
             (idx (car last)) (n takuzu--size))
        (takuzu-board-set takuzu--board (/ idx n) (mod idx n) (cdr last))
        (setq takuzu--cursor (cons (/ idx n) (mod idx n)))
        (takuzu--set-status ""))))
  (takuzu--redraw)))

(defun takuzu--forced-cell ()
  "First empty cell whose value is forced to a single legal option.
Return (ROW COL VALUE) or nil when no cell is forced."
  (let ((n takuzu--size) (found nil))
    (cl-block scan
      (dotimes (r n)
        (dotimes (c n)
          (when (null (takuzu-board-ref takuzu--board r c))
            (let ((vals (takuzu--legal-values takuzu--board r c)))
              (when (and vals (null (cdr vals)))
                (setq found (list r c (car vals)))
                (cl-return-from scan)))))))
    found))

(defun takuzu-hint ()
  "Fill the first cell whose value current logic pins down."
  (interactive)
  (takuzu--playing-only
  (if (or takuzu--won takuzu--proven)
      (takuzu--set-status takuzu--msg-finished)
    (let ((found (takuzu--forced-cell)))
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
       (format "Not finished -- %d cells left." (takuzu--empty-count)))))
  (takuzu--redraw)))

(defun takuzu-prove ()
  "Give up and show the full solution."
  (interactive)
  (takuzu--playing-only
  (when (yes-or-no-p "Show the full solution? ")
    (setf (takuzu-board-cells takuzu--board)
          (copy-sequence (takuzu-board-cells takuzu--solution)))
    ;; read the clock before the proven flag freezes `takuzu--elapsed'
    (let ((elapsed (takuzu--elapsed)))
      (setq takuzu--proven t takuzu--won-elapsed elapsed))
    (takuzu--set-status "Solution shown.")
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

(defun takuzu-help ()
  "Toggle the rules/help overlay.
Shows the three rules over the console shell; any key returns to the game."
  (interactive)
  (setq takuzu--help (not takuzu--help))
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
   (t (takuzu-ui-arm takuzu--size (takuzu--current-difficulty)))))

(defun takuzu-cycle-size ()
  "Cycle to the next board size and arm a fresh game at that size."
  (interactive)
  (let* ((sizes takuzu-sizes)
         (next (nth (mod (1+ (or (cl-position takuzu--size sizes) 0)) (length sizes)) sizes)))
    (takuzu-ui-arm next (takuzu--current-difficulty))))

(defun takuzu-cycle-level ()
  "Cycle to the next level (difficulty) and arm a fresh game at it."
  (interactive)
  (let* ((all '(easy medium hard))
         (cur (or (cl-position (takuzu--current-difficulty) all) 0))
         (next (nth (mod (1+ cur) 3) all)))
    (takuzu-ui-arm takuzu--size next)))

;; --- mode ---

(defvar-keymap takuzu-mode-map
  :doc "Keymap for `takuzu-mode'."
  :parent special-mode-map
  "<up>" #'takuzu-up "<down>" #'takuzu-down "<left>" #'takuzu-left "<right>" #'takuzu-right
  "SPC" #'takuzu-cycle
  "u" #'takuzu-undo
  "?" #'takuzu-hint
  "c" #'takuzu-check
  "p" #'takuzu-prove
  "r" #'takuzu-reset
  "a" #'takuzu-toggle-assist
  "s" #'takuzu-cycle-size
  "l" #'takuzu-cycle-level
  "n" #'takuzu-new
  "i" #'takuzu-help)

(defun takuzu--stop-timer ()
  "Cancel the refresh timer if running."
  (takuzu--cancel-timer takuzu--timer))

(defun takuzu--start-refresh-timer (buf)
  "Start the per-redraw-interval refresh timer for BUF."
  (let ((iv (takuzu--refresh-interval)))
    (setq takuzu--timer (run-at-time iv iv (lambda () (takuzu--redraw buf))))))

(defun takuzu--cleanup ()
  "Cancel timers and any in-flight generation when the buffer is killed."
  (takuzu--stop-timer)
  (takuzu--stop-spinner)
  (takuzu--cancel-timer takuzu--scale-timer)
  (takuzu--cancel-timer takuzu--event-timer)
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
            takuzu--won nil takuzu--proven nil takuzu--won-elapsed 0 takuzu--status ""
            takuzu--event nil takuzu--help nil))
    (takuzu--stop-spinner)
    (takuzu--stop-timer)
    (takuzu--cancel-timer takuzu--event-timer)
    (takuzu--start-refresh-timer buf)
    (takuzu--redraw buf)))

(defun takuzu--on-generated (buf result)
  "Handle a finished background generation for BUF.
A nil RESULT reports the failure; otherwise begin play at once if the start
key was already pressed, or hold the RESULT pending the start key."
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
       (t (setq takuzu--pending result))))))

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
            takuzu--won-elapsed 0 takuzu--status "" takuzu--event nil takuzu--help nil)
      (takuzu--cancel-timer takuzu--event-timer)
      (takuzu--start-refresh-timer buf)
      (setq takuzu--gen-process
            (takuzu-generate-async
             size difficulty
             (lambda (result) (takuzu--on-generated buf result)))))
    (switch-to-buffer buf)
    (takuzu--redraw buf)))

(provide 'takuzu-ui)
;;; takuzu-ui.el ends here
