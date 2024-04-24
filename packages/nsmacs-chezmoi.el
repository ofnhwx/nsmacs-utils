;;; nsmacs-chezmoi.el --- chezmoi 関連のコマンド
;;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'chezmoi)

;;;###autoload
(defun e:reopen-in-chezmoi-mode ()
  "`chezmoi' の管理対象のファイルを開いたら自動で有効化する."
  (let* ((source (buffer-file-name))
         (target (chezmoi-target-file (or source ""))))
    (when (and target
               (not chezmoi-mode))
      (chezmoi-find target))))

(provide 'nsmacs-chezmoi)
;;; nsmacs-chezmoi.el ends here
