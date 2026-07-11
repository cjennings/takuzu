;;; takuzu.el --- Binairo / Takuzu binary logic puzzle -*- lexical-binding: t -*-

;; Author: Craig Jennings <craigmartinjennings@gmail.com>
;; Version: 0.7.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: games
;; URL: https://github.com/cjennings/takuzu

;;; Commentary:
;; Takuzu (also called Binairo or Binary Puzzle) is a two-color logic puzzle on
;; an even square grid.  Fill every cell one of two colors so that: no three
;; same-color cells are adjacent in a line; each row and column holds equal
;; counts; and no two rows or columns are identical.  Every puzzle has a single
;; solution reachable by logic.
;;
;; Entry point:  M-x takuzu

;;; Code:

(require 'takuzu-board)
(require 'takuzu-solver)
(require 'takuzu-generator)
(require 'takuzu-async)
(require 'takuzu-ui)

(defgroup takuzu nil
  "Binairo/Takuzu binary logic puzzle."
  :group 'games
  :prefix "takuzu-")

(defcustom takuzu-sizes '(4 6 8 10 12)
  "The board sizes offered when starting a game.  Each must be even."
  :type '(repeat integer)
  :group 'takuzu)

(defcustom takuzu-default-size 6
  "The board size proposed by default when starting a game."
  :type 'integer
  :group 'takuzu)

(defcustom takuzu-default-difficulty 'easy
  "The difficulty proposed by default when starting a game."
  :type '(choice (const easy) (const medium) (const hard))
  :group 'takuzu)

(defcustom takuzu-show-glyphs t
  "Non-nil to draw a glyph inside each cell (○ for 0, ● for 1, · for empty).
Glyphs are the reliable signal in terminals and for color-blind play; colors
enhance them.  Toggle in a game with \\`g'."
  :type 'boolean
  :group 'takuzu)

(defcustom takuzu-flash-period 1.0
  "Seconds for one on/off cycle of the slow flashing indicators.
Governs the status LED (win/reveal) and the flashing New prompt.  Increase for
slower flashing, decrease for faster; takes effect on the next game (or redraw
for the LED).  The clock-ring start/stop cue is a separate quick double-flash."
  :type 'number
  :group 'takuzu)

(defun takuzu--read-size ()
  "Prompt for a board size from `takuzu-sizes'."
  (string-to-number
   (completing-read (format "Board size (default %d): " takuzu-default-size)
                    (mapcar #'number-to-string takuzu-sizes)
                    nil t nil nil (number-to-string takuzu-default-size))))

(defun takuzu--read-difficulty ()
  "Prompt for a difficulty."
  (intern
   (completing-read (format "Difficulty (default %s): " takuzu-default-difficulty)
                    '("easy" "medium" "hard")
                    nil t nil nil (symbol-name takuzu-default-difficulty))))

;;;###autoload
(defun takuzu (&optional size difficulty)
  "Start a new Takuzu game of SIZE and DIFFICULTY.
Interactively, prompt for both."
  (interactive (list (takuzu--read-size) (takuzu--read-difficulty)))
  (let ((size (or size takuzu-default-size))
        (difficulty (or difficulty takuzu-default-difficulty)))
    (takuzu-ui-arm size difficulty)))

(provide 'takuzu)
;;; takuzu.el ends here
