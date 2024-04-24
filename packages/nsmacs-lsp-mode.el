;;; nsmacs-lsp-mode.el --- lsp-mode 関連のコマンド
;;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'lsp-mode)
(require 'nsmacs-bundler)

;;;###autoload
(defun ad:lsp-rubocop--build-command@auto-detect ()
  "`lsp-rubocop--build-command' の実行時に `bundler' 経由でコマンドを実行するかを自動で切り替える."
  (setq-local lsp-rubocop-use-bundler (e:bundle-exists "rubocop")))

;;;###autoload
(defun ad:lsp-solargraph--build-command@auto-detect ()
  "`lsp-solargraph--build-command' の実行時に `bundler' 経由でコマンドを実行するかを自動で切り替える."
  (setq-local lsp-solargraph-use-bundler (e:bundle-exists "solargraph")))

;;;###autoload
(defun e:set-lsp-client-add-on (client value)
  "対象の CLIENT の `add-on' の設定を VALUE に変更する."
  (let ((ls (gethash client lsp-clients)))
    (setf (lsp--client-add-on? ls) value)))

(provide 'nsmacs-lsp-mode)
;;; nsmacs-lsp-mode.el ends here
