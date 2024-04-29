;;; nsmacs-copilot.el --- copilot 関連のコマンド
;;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'copilot)

;;;###autoload
(defun ad:copilot-accept-completion (&rest _)
  "他の処理の前に `copilot-accept-completion' を呼び出す."
  (copilot-accept-completion))

;;;###autoload
(defun ad:copilot-clear-overlay (&rest _)
  "他の処理の前に `copilot-clear-overlay' を呼び出す."
  (copilot-clear-overlay))

(provide 'nsmacs-copilot)
;;; nsmacs-copilot.el ends here
