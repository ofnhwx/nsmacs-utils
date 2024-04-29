;;; nsmacs-skk.el --- skk 関連のコマンド
;;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'skk)
(require 'dash)
(require 'evil)

;;;###autoload
(defun ad:evil-refresh-cursor@with-skk (fn &rest args)
  "`evil' でのカーソル表示を `skk' に対応させる.
FN, ARGS はアドバイス対象の関数とその引数."
  (unless (and (eq evil-state 'insert)
               (bound-and-true-p skk-mode))
    (apply fn args)))

;;;###autoload
(defun e:skk-mode ()
  "`skk' の有効化で半角英数入力にする."
  (interactive)
  (unless (derived-mode-p 'vterm-mode)
    (if (bound-and-true-p skk-mode)
        (skk-latin-mode-on)
      (let ((skk-mode-hook (-union skk-mode-hook '(skk-latin-mode-on))))
        (skk-mode)))))

(provide 'nsmacs-skk)
;;; nsmacs-skk.el ends here
