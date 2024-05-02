;;; nsmacs-vterm.el --- vterm 関連のコマンド
;;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'vterm)
(require 's)

;;;###autoload
(defun e:vterm-exec (name program &rest args)
  "NAME, PROGRAM および ARGS を指定して `vterm' を起動する."
  (let ((vterm-buffer-name (format "*vterm-%s*" name))
        (vterm-shell (s-join " " (cons program args))))
    (vterm--internal #'ignore)))

(provide 'nsmacs-vterm)
;;; nsmacs-vterm.el ends here
