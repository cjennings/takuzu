;;; takuzu-stats.el --- Win/loss and best-time records for Takuzu -*- lexical-binding: t -*-

;; Author: Craig Jennings <craigmartinjennings@gmail.com>
;; Keywords: games
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Persistent game statistics: wins, losses, and best solve times, tallied
;; per (SIZE . GRADE) so a 4x4 easy record never pads the 12x12 hard one.
;; A proven board counts as a loss; an abandoned game counts as nothing.
;;
;; Stats live as a printed alist in `takuzu-stats-file':
;;   (((SIZE . GRADE) :wins N :losses N :best SECONDS) ...)
;; :best is absent until the first win.  The file is read with `read' and
;; a corrupt or missing file degrades to empty stats rather than an error.

;;; Code:

(defcustom takuzu-stats-file (locate-user-emacs-file "takuzu-stats.eld")
  "File persisting Takuzu win/loss tallies and best times."
  :type 'file
  :group 'takuzu)

(defun takuzu-stats-load ()
  "Read stats from `takuzu-stats-file'; nil when missing or unreadable."
  (when (file-readable-p takuzu-stats-file)
    (with-temp-buffer
      (insert-file-contents takuzu-stats-file)
      (condition-case nil
          (read (current-buffer))
        (error nil)))))

(defun takuzu-stats-save (stats)
  "Write STATS to `takuzu-stats-file' as a printed form."
  (with-temp-file takuzu-stats-file
    (let ((print-length nil) (print-level nil))
      (prin1 stats (current-buffer))
      (insert "\n"))))

(defun takuzu-stats-entry (stats size grade)
  "Return the (:wins N :losses N [:best SECS]) entry in STATS for SIZE/GRADE."
  (cdr (assoc (cons size grade) stats)))

(defun takuzu-stats-record (size grade result elapsed)
  "Record RESULT (`win' or `loss') for a SIZE/GRADE game solved in ELAPSED seconds.
Loads, updates, and saves the stats file; returns the updated entry.
A win updates the best time when ELAPSED beats it; a loss never touches it."
  (let* ((stats (takuzu-stats-load))
         (key (cons size grade))
         ;; fresh list, not a quoted literal: `plist-put' mutates in place,
         ;; and a shared constant would leak counts across calls
         (entry (or (cdr (assoc key stats)) (list :wins 0 :losses 0))))
    (if (eq result 'win)
        (progn
          (setq entry (plist-put entry :wins (1+ (plist-get entry :wins))))
          (let ((best (plist-get entry :best)))
            (when (or (null best) (< elapsed best))
              (setq entry (plist-put entry :best elapsed)))))
      (setq entry (plist-put entry :losses (1+ (plist-get entry :losses)))))
    (setq stats (cons (cons key entry)
                      (assoc-delete-all key stats)))
    (takuzu-stats-save stats)
    entry))

(defun takuzu-stats-totals (stats)
  "Return aggregate (WINS . LOSSES) across every entry in STATS."
  (let ((wins 0) (losses 0))
    (dolist (item stats)
      (setq wins (+ wins (or (plist-get (cdr item) :wins) 0))
            losses (+ losses (or (plist-get (cdr item) :losses) 0))))
    (cons wins losses)))

(provide 'takuzu-stats)
;;; takuzu-stats.el ends here
