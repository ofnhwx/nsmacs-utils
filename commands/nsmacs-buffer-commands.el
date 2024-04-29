;;; nsmacs-buffer-commands.el --- バッファー関連のコマンド
;;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;;###autoload
(defun e:switch-to-last-buffer ()
  "直前に表示していたバッファーに切り替える."
  (interactive)
  (if-let ((buffer (caar (window-prev-buffers))))
      (switch-to-buffer buffer)))

;;;###autoload
(defun e:switch-to-messages-buffer ()
  "メッセージバッファーに切り替える."
  (interactive)
  (switch-to-buffer (messages-buffer)))

;;;###autoload
(defun e:indent-buffer ()
  "バッファー全体をインデントする."
  (interactive)
  (indent-region (point-min) (point-max)))

(provide 'nsmacs-buffer-commands)
;;; nsmacs-buffer-commands.el ends here
