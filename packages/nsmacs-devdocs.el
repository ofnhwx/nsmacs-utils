;;; nsmacs-devdocs.el --- devdocs 関連のコマンド
;;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'devdocs)

;;;###autoload
(defun e:devdocs-ruby ()
  "Ruby 用の `devdocs' を設定."
  (setq-local devdocs-current-docs '("rails~7.1" "ruby~3")))

(provide 'nsmacs-devdocs)
;;; nsmacs-devdocs.el ends here
