;;; nsmacs-flycheck.el --- flycheck 関連のコマンド
;;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'flycheck)
(require 'dash)
(require 'komunan-lisp-library)

;;;###autoload
(defun e:flycheck-copy-error-ids ()
  "カーソル箇所のエラーの ID をコピーする."
  (interactive)
  (let ((messages (->> (flycheck-overlay-errors-at (point))
                       (-map #'flycheck-error-id)
                       (-uniq)
                       (-non-nil))))
    (when messages
      (kill-new (string-join messages ", "))
      (message (string-join messages ", ")))))

;;;###autoload
(defun ad:flycheck-start-command-checker@with-cwd (fn &rest args)
  "プロジェクトルートで `flycheck-start-command-checker' を実行する.
FN, ARGS はアドバイス対象の関数とその引数."
  (let* ((project-root (kllib:project-root default-directory))
         (default-directory (or project-root default-directory)))
    (apply fn args)))

(provide 'nsmacs-flycheck)
;;; nsmacs-flycheck.el ends here
