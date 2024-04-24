;;; nsmacs-flycheck.el --- flycheck 関連のコマンド
;;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'flycheck)
(require 'dash)

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

(provide 'nsmacs-flycheck)
;;; nsmacs-flycheck.el ends here
