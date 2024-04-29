;;; nsmacs-shackle.el --- shackle 関連のコマンド
;;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'shackle)
(require 'dash)

(defvar e:shackle-auto-close-windows nil
  "`shackle' で管理している自動で閉じるウィンドウのリスト.")

;;;###autoload
(defun ad:shackle-display-buffer-action@save-windows (&rest _)
  "`shackle' で表示したウィンドウを自動で閉じるように管理する."
  (setq e:shackle-auto-close-windows (-filter #'window-live-p e:shackle-auto-close-windows))
  (add-to-list 'e:shackle-auto-close-windows shackle-last-window))

;;;###autoload
(defun ad:keyboard-quit@shackle-auto-close (&rest _)
  "`keyboard-quit' で `shackle' で表示したウィンドウを自動で閉じる."
  (ignore-errors
    (-each e:shackle-auto-close-windows 'delete-window))
  (setq e:shackle-auto-close-windows nil))

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

(provide 'nsmacs-shackle)
;;; nsmacs-shackle.el ends here
