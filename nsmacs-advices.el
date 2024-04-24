;;; nsmacs-advices.el --- アドバイス用の関数定義
;;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;;###autoload
(defun ad:consult-line@with-orderless (fn &rest args)
  "`consult-line' を `orderless' の保管スタイルで実行する.
FN, ARGS はアドバイス対象の関数とその引数."
  (let ((completion-styles '(orderless)))
    (apply fn args)))

(provide 'nsmacs-advices)
;;; nsmacs-advices.el ends here
