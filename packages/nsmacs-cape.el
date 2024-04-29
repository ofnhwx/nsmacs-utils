;;; nsmacs-cape.el --- cape 関連のコマンド
;;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'nsmacs-macros)
(require 'dash)
(require 'cape)
(require 'codeium)
(require 'company-org-block)
(require 'lsp-completion)

;;;###autoload
(defalias 'cape-codeium (cape-capf-interactive #'codeium-completion-at-point))

;;;###autoload
(defalias 'cape-org-block (cape-capf-interactive (cape-company-to-capf #'company-org-block)))

(defun e:capf-function (name &rest capfs)
  "補完の設定を行う関数を定義して返す.
NAME は定義する関数の識別に使用し、CAPFS はこの関数でメインに使う補完用の関数を指定する."
  (let ((fun (intern (format "e:cape-%s" name)))
        (capfs (-concat capfs '(cape-dabbrev codeium-completion-at-point))))
    (defalias fun
      (cape-capf-interactive
       (cape-capf-buster
        (apply #'cape-capf-super capfs))))
    (list #'cape-file fun)))

;;;###autoload
(defun e:setup-capf/default ()
  "標準の補完を設定する."
  (interactive)
  (e:local! completion-at-point-functions
            (e:capf-function major-mode (car completion-at-point-functions))))

;;;###autoload
(defun e:setup-capf/org ()
  "`org-mode' 用の補完を設定する."
  (interactive)
  (e:local! completion-at-point-functions
            (e:capf-function "org" #'cape-elisp-block #'cape-org-block)))

;;;###autoload
(defun e:setup-capf/lsp ()
  "`lsp-mode' 用の補完を設定する."
  (interactive)
  (e:local! completion-at-point-functions
            (e:capf-function "lsp" #'lsp-completion-at-point)))

(provide 'nsmacs-cape)
;;; nsmacs-cape.el ends here
