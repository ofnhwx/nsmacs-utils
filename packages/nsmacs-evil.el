;;; nsmacs-evil.el --- evil 関連のコマンド
;;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'evil)

;;;###autoload
(defun evil-shift-right-visual ()
  "選択領域を維持する `evil-shift-right'."
  (interactive)
  (call-interactively 'evil-shift-right)
  (evil-normal-state)
  (evil-visual-restore))

;;;###autoload
(defun evil-shift-left-visual ()
  "選択領域を維持する `evil-shift-left'."
  (interactive)
  (call-interactively 'evil-shift-left)
  (evil-normal-state)
  (evil-visual-restore))

(provide 'nsmacs-evil)
;;; nsmacs-evil.el ends here
