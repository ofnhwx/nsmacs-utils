;;; nsmacs-haml-mode.el --- haml-mode 関連のコマンド
;;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'haml-mode)
(require 'flycheck)

;;;###autoload
(defun e:setup-haml-mode ()
  "`haml-mode' 用の設定を行う."
  (when (flycheck-may-enable-checker 'haml-lint)
    (flycheck-select-checker 'haml-lint)
    (setq-local flycheck-command-wrapper-function
                (lambda (command)
                  (append '("bundle" "exec") command)))))

(provide 'nsmacs-haml-mode)
;;; nsmacs-haml-mode.el ends here
