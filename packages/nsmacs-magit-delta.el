;;; nsmacs-magit-delta.el --- magit-delta 関連のコマンド
;;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'magit-delta)

(defvar e:nth/magit-delta-point-max 50000
  "magit-delta を有効にするバッファの最大サイズ.")

;;;###autoload
(defun ad:magit-delta-call-delta-and-convert-ansi-escape-sequences@auto-disable (fn &rest args)
  "`magit-delta' を自動で無効にする.
FN, ARGS はアドバイス対象の関数とその引数."
  (if (<= (point-max) e:nth/magit-delta-point-max)
      (apply fn args)
    (magit-delta-mode -1)))

;;;###autoload
(defun e:nth/magit-delta-auto-enable (&rest _)
  "`magit-delta' を自動で有効にする."
  (when (and (not magit-delta-mode)
             (<= (point-max) e:nth/magit-delta-point-max))
    (magit-delta-mode +1)))

(provide 'nsmacs-magit-delta)
;;; nsmacs-magit-delta.el ends here
