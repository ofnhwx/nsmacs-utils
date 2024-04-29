;;; nsmacs-orderless.el --- orderless 関連のコマンド
;;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'orderless)

;;;###autoload
(defun orderless-migemo (component)
  "`orderless' で `migemo' を使うための関数.
COMPONENT は入力されたクエリ."
  (when (fboundp 'migemo-get-pattern)
    (let ((pattern (migemo-get-pattern component)))
      (condition-case nil
          (progn (string-match-p pattern "") pattern)
        (invalid-regexp nil)))))

(provide 'nsmacs-orderless)
;;; nsmacs-orderless.el ends here
