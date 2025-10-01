;;; nsmacs-misc-commands.el --- ちょっと便利なコマンド
;;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;;###autoload
(defun ad:ignore-errors (fn &rest args)
  "Call FN with ARGS, ignoring errors."
  (ignore-errors (apply fn args)))

(provide 'nsmacs-misc-commands)
;;; nsmacs-misc-commands.el ends here
