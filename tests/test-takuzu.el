;;; test-takuzu.el --- Tests for the takuzu entry point -*- lexical-binding: t -*-

;;; Commentary:
;; The customization defaults, the size/difficulty readers (prompts mocked), and
;; the entry command's arming behaviour.

;;; Code:

(require 'ert)
(require 'takuzu)

(ert-deftest test-takuzu-defcustoms ()
  "Normal: the game defcustoms carry sane defaults."
  (should (memq takuzu-default-size takuzu-sizes))
  (should (cl-every #'cl-evenp takuzu-sizes))
  (should (memq takuzu-default-difficulty '(easy medium hard)))
  (should (numberp takuzu-flash-period)))

(ert-deftest test-takuzu-read-size ()
  "Normal: read-size returns the chosen size as a number."
  (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "8")))
    (should (= (takuzu--read-size) 8))))

(ert-deftest test-takuzu-read-difficulty ()
  "Normal: read-difficulty returns the chosen difficulty as a symbol."
  (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "hard")))
    (should (eq (takuzu--read-difficulty) 'hard))))

(ert-deftest test-takuzu-command-arms-buffer ()
  "Integration: `takuzu' arms a blank *Takuzu* buffer with the clock stopped.

Components integrated:
- takuzu (entry point)
- takuzu-ui-arm (real; starts a background generation process and a blink timer,
  both torn down here)

`switch-to-buffer' is stubbed so the command runs headless."
  (let ((buf nil))
    (unwind-protect
        (cl-letf (((symbol-function 'switch-to-buffer) #'ignore)
                  ((symbol-function 'takuzu--read-size) (lambda () 4))
                  ((symbol-function 'takuzu--read-difficulty) (lambda () 'easy)))
          (call-interactively #'takuzu)
          (setq buf (get-buffer "*Takuzu*"))
          (should buf)
          (with-current-buffer buf
            (should takuzu--armed)
            (should (null takuzu--start-time))
            (should (= takuzu--size 4))
            (should (cl-every #'null (append (takuzu-board-cells takuzu--board) nil)))))
      (when buf
        (with-current-buffer buf (ignore-errors (takuzu--cleanup)))
        (ignore-errors (kill-buffer buf))))))

(ert-deftest test-takuzu-command-uses-defaults ()
  "Normal: `takuzu' with no args arms at the configured default size."
  (cl-letf (((symbol-function 'switch-to-buffer) #'ignore))
    (unwind-protect
        (progn
          (takuzu)
          (with-current-buffer "*Takuzu*"
            (should (= takuzu--size takuzu-default-size))))
      (let ((b (get-buffer "*Takuzu*")))
        (when b
          (with-current-buffer b (ignore-errors (takuzu--cleanup)))
          (ignore-errors (kill-buffer b)))))))

;; Redirect stats writes for the whole batch: tests that win or prove a board
;; record a result, and without this every suite and hook run would write the
;; developer's real stats file.
(setq takuzu-stats-file (make-temp-file "takuzu-stats-suite-" nil ".eld"))

(ert-deftest test-takuzu-suite-stats-file-redirected ()
  "Error: the suite must never write the developer's real stats file.
Tests that win or prove a board record a result through `takuzu-stats-file';
left at its default, every suite and hook run pollutes the real record."
  (should-not (equal (expand-file-name takuzu-stats-file)
                     (expand-file-name (locate-user-emacs-file "takuzu-stats.eld")))))

(ert-deftest test-takuzu-stats-file-default-absolute ()
  "Error: the stats-file default is absolute, never cwd-relative.
A bare filename would resolve against `default-directory' and fragment the
record across every directory Emacs happens to be in."
  (should (file-name-absolute-p
           (eval (car (get 'takuzu-stats-file 'standard-value))))))

(ert-deftest test-takuzu-make-test-prefers-newer-source ()
  "Error: test targets load newer source instead of stale bytecode."
  (let* ((root (locate-dominating-file default-directory "Makefile"))
         (makefile (with-temp-buffer
                     (insert-file-contents (expand-file-name "Makefile" root))
                     (buffer-string))))
    (dolist (target '("test" "test-file"))
      (should (string-match-p
               (format "^%s:\n\\(?:\t.*\n\\)*?\t.*load-prefer-newer t" target)
               makefile)))))

(provide 'test-takuzu)
;;; test-takuzu.el ends here
