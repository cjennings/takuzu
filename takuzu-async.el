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

(defun takuzu--read-last-sexp (buffer)
  "Read and return the last complete sexp in BUFFER, or nil if none.
The child prints its result plist last, but stray output (load messages,
warnings) can precede it on stdout, so reading from `point-min' would choke
on the noise instead of the result."
  (with-current-buffer buffer
    (ignore-errors
      (goto-char (point-max))
      (forward-sexp -1)
      (read (current-buffer)))))

(defun takuzu-generate-async (size difficulty callback)
  "Generate a SIZE by DIFFICULTY puzzle in a child Emacs.
Call CALLBACK with the decoded result plist (:board :solution :grade) on
success, with the symbol `cancelled' when the process was deliberately
killed (a size/level cycle or a buffer kill), or with nil if the child
genuinely failed.  Return the process.

The child's stderr collects in a hidden buffer that is deleted on success
or cancel; on failure it is renamed to *takuzu-gen-stderr* and kept for
post-mortem."
  (let* ((dir (takuzu--lib-dir))
         (out (generate-new-buffer " *takuzu-gen*"))
         (err (generate-new-buffer " *takuzu-gen-stderr*"))
         (expr (format "(progn (random t) (prin1 (takuzu--encode-result (takuzu-generate %d '%s))))"
                       size difficulty))
         (proc
          (make-process
           :name "takuzu-gen"
           :buffer out
           :stderr err
           :noquery t
           :connection-type 'pipe
           :command (list (takuzu--emacs-binary) "-Q" "--batch" "-L" dir
                          "-l" "takuzu-generator" "--eval" expr)
           :sentinel
           (lambda (proc _event)
             (when (memq (process-status proc) '(exit signal))
               (let ((result (unless (eq (process-status proc) 'exit)
                               ;; a signal is a deliberate cancel (size/level
                               ;; cycling, buffer kill), not a failure
                               'cancelled)))
                 (unwind-protect
                     (when (and (eq (process-status proc) 'exit)
                                (= 0 (process-exit-status proc)))
                       (let ((data (takuzu--read-last-sexp (process-buffer proc))))
                         (when (and (consp data) (plist-get data :size))
                           (setq result (takuzu--decode-result data)))))
                   (kill-buffer (process-buffer proc))
                   (cond
                    ;; success or cancel: no post-mortem to keep
                    (result (kill-buffer err))
                    (t
                     (when-let ((old (get-buffer "*takuzu-gen-stderr*")))
                       (kill-buffer old))
                     (with-current-buffer err
                       (rename-buffer "*takuzu-gen-stderr*")))))
                 (funcall callback result)))))))
    (when-let ((errproc (get-buffer-process err)))
      (set-process-query-on-exit-flag errproc nil))
    proc))

(provide 'takuzu-async)
;;; takuzu-async.el ends here
