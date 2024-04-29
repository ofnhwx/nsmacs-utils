;;; nsmacs-shell-pop.el --- shell-pop 関連のコマンド
;;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'shell-pop)

;;;###autoload
(defun ad:shell-pop@auto-session-name (fn &rest args)
  "`shell-pop' でセッション名を自動で設定する.
FN, ARGS はアドバイス対象の関数とその引数."
  (let* ((tab (tab-bar--current-tab))
         (identifier (if (alist-get 'explicit-name tab)
                         (alist-get 'name tab)
                       "default"))
         (shell-pop-internal-mode-buffer (format "*vterm-%s*" identifier))
         (vterm-shell (format "tmux new -A -s emacs-%s" identifier)))
    (apply fn args)))

(provide 'nsmacs-shell-pop)
;;; nsmacs-shell-pop.el ends here
