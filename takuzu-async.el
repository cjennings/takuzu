;;; takuzu-async.el --- Off-thread puzzle generation for Takuzu -*- lexical-binding: t -*-

;; Author: Craig Jennings <craigmartinjennings@gmail.com>
;; Keywords: games
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Generation is CPU-bound and, on a large hard board, can run for many seconds.
;; Rather than freeze Emacs, run it in a throwaway child Emacs (`emacs -Q
;; --batch') that prints the result as a `read'-able plist; the parent reads it
;; in the process sentinel and hands it back through a callback.  The UI shows a
;; spinner meanwhile.  See `takuzu--encode-result' / `takuzu--decode-result' for
;; the wire format.

;;; Code:

(require 'takuzu-generator)

(defun takuzu--lib-dir ()
  "Directory holding the takuzu source, for the child's load path."
  (file-name-directory (locate-library "takuzu")))

(defun takuzu--emacs-binary ()
  "Absolute path to the running Emacs executable."
  (expand-file-name invocation-name invocation-directory))

(defun takuzu-generate-async (size difficulty callback)
  "Generate a SIZE by DIFFICULTY puzzle in a child Emacs.
Call CALLBACK with the decoded result plist (:board :solution :grade) on
success, or with nil if the child failed.  Return the process."
  (let* ((dir (takuzu--lib-dir))
         (out (generate-new-buffer " *takuzu-gen*"))
         (expr (format "(progn (random t) (prin1 (takuzu--encode-result (takuzu-generate %d '%s))))"
                       size difficulty)))
    (make-process
     :name "takuzu-gen"
     :buffer out
     :noquery t
     :connection-type 'pipe
     :command (list (takuzu--emacs-binary) "-Q" "--batch" "-L" dir "-l" "takuzu"
                    "--eval" expr)
     :sentinel
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (let ((result nil))
           (unwind-protect
               (when (and (eq (process-status proc) 'exit)
                          (= 0 (process-exit-status proc)))
                 (with-current-buffer (process-buffer proc)
                   (goto-char (point-min))
                   (let ((data (ignore-errors (read (current-buffer)))))
                     (when data (setq result (takuzu--decode-result data))))))
             (kill-buffer (process-buffer proc)))
           (funcall callback result)))))))

(provide 'takuzu-async)
;;; takuzu-async.el ends here
