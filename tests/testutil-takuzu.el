;;; testutil-takuzu.el --- Shared test fixtures for takuzu -*- lexical-binding: t -*-

;;; Commentary:
;; Cross-file helpers.  Currently one: a scoped stats-file sandbox, needed by
;; every test that records or reads game results.

;;; Code:

(require 'takuzu-stats)

(defmacro takuzu-testutil-with-stats-file (&rest body)
  "Run BODY with `takuzu-stats-file' bound to a fresh temp path, cleaned after."
  (declare (indent 0))
  `(let ((takuzu-stats-file (make-temp-file "takuzu-stats-" nil ".eld")))
     (unwind-protect
         (progn (delete-file takuzu-stats-file) ,@body)
       (ignore-errors (delete-file takuzu-stats-file)))))

(provide 'testutil-takuzu)
;;; testutil-takuzu.el ends here
