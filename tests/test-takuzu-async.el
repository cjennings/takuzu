;;; test-takuzu-async.el --- Tests for takuzu-async -*- lexical-binding: t -*-

;;; Commentary:
;; The path helpers are unit-tested; generation-in-a-child is an integration
;; test that spawns a real child Emacs and reads the result back.

;;; Code:

(require 'ert)
(require 'takuzu)

(ert-deftest test-takuzu-async-lib-dir ()
  "Normal: lib-dir is the directory that holds the takuzu source."
  (let ((d (takuzu--lib-dir)))
    (should (stringp d))
    (should (file-exists-p (expand-file-name "takuzu.el" d)))))

(ert-deftest test-takuzu-async-emacs-binary ()
  "Normal: the child Emacs path resolves to an existing executable."
  (let ((e (takuzu--emacs-binary)))
    (should (stringp e))
    (should (file-exists-p e))))

(ert-deftest test-takuzu-integration-generate-async ()
  "Integration: async generation returns a unique board via a child Emacs.

Components integrated:
- takuzu-generate-async (real child process)
- the process sentinel + `takuzu--decode-result' (real)
- takuzu-generate in the child (real)

Validates the result is a same-size, uniquely-solvable board with a valid grade."
  (let ((done 'pending) (deadline (+ (float-time) 30)))
    (takuzu-generate-async 4 'easy (lambda (r) (setq done r)))
    (while (and (eq done 'pending) (< (float-time) deadline))
      (accept-process-output nil 0.2))
    (should (not (eq done 'pending)))
    (should done)
    (should (= (takuzu-board-size (plist-get done :board)) 4))
    (should (takuzu-unique-p (plist-get done :board)))
    (should (takuzu-board-solved-p (plist-get done :solution)))
    (should (memq (plist-get done :grade) '(easy medium hard)))))

(provide 'test-takuzu-async)
;;; test-takuzu-async.el ends here
