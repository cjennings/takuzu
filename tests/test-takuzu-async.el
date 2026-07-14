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

(ert-deftest test-takuzu-async-read-last-sexp ()
  "Normal/Boundary: the last complete sexp wins; an empty buffer reads nil.
Stray child output (load messages, warnings) can precede the result plist
on stdout, so the reader must not anchor at `point-min'."
  (with-temp-buffer
    (insert "Loading /tmp/foo.el (source)...\n(1 2) (:grade easy)\n")
    (should (equal (takuzu--read-last-sexp (current-buffer)) '(:grade easy))))
  (with-temp-buffer
    (should-not (takuzu--read-last-sexp (current-buffer)))))

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

(ert-deftest test-takuzu-integration-generate-async-failure ()
  "Integration: a failing child reports nil and keeps its stderr for post-mortem.

Components integrated:
- takuzu-generate-async (real child process, forced to die with an odd size)
- the process sentinel (real)

Validates: the callback receives nil, and the child's stderr survives in a
visible buffer holding the error output."
  (when-let ((stale (get-buffer "*takuzu-gen-stderr*")))
    (kill-buffer stale))
  (let ((done 'pending) (deadline (+ (float-time) 30)))
    (takuzu-generate-async 5 'easy (lambda (r) (setq done r)))
    (while (and (eq done 'pending) (< (float-time) deadline))
      (accept-process-output nil 0.2))
    (should (not (eq done 'pending)))
    (should-not done)
    (let ((err (get-buffer "*takuzu-gen-stderr*")))
      (unwind-protect
          (progn
            (should err)
            ;; stderr arrives on its own pipe; keep pumping until it lands
            (while (and (= (buffer-size err) 0) (< (float-time) deadline))
              (accept-process-output nil 0.1))
            (should (> (buffer-size err) 0)))
        (when err (kill-buffer err))))))

(ert-deftest test-takuzu-integration-generate-async-cancelled ()
  "Integration: a cancelled generation keeps no stderr buffer.

Components integrated:
- takuzu-generate-async (real child process, killed mid-flight)
- the process sentinel (real)

Validates: `delete-process' on an in-flight generation (size/level cycling
does this) reports the distinct `cancelled' marker -- not nil, which means
failure -- and litters no *takuzu-gen-stderr* buffer."
  (when-let ((stale (get-buffer "*takuzu-gen-stderr*")))
    (kill-buffer stale))
  (let ((done 'pending) (deadline (+ (float-time) 30)))
    (delete-process (takuzu-generate-async 12 'hard (lambda (r) (setq done r))))
    (while (and (eq done 'pending) (< (float-time) deadline))
      (accept-process-output nil 0.1))
    (should (eq done 'cancelled))
    (should-not (get-buffer "*takuzu-gen-stderr*"))))

(provide 'test-takuzu-async)
;;; test-takuzu-async.el ends here
