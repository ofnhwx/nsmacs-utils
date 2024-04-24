;;; nsmacs-scratch.el --- scratch 関連のコマンド
;;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'scratch)

;;;###autoload
(defun e:scratch-buffer-p ()
  "バッファーが *scratch* かをチェックする."
  (or (eq (current-buffer) (get-scratch-buffer-create))
      (bound-and-true-p scratch-buffer)))

(provide 'nsmacs-scratch)
;;; nsmacs-scratch.el ends here
