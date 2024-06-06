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
(defun ad:lsp-resolve-final-command@with-lsp-booster (fn command &optional test)
  "`emacs-lsp-booster' を使って `lsp-server' を起動する.
FN, COMMAND, TEST はアドバイス対象の関数とその引数."
  (let ((result (funcall fn command test)))
    (if (and (not test)
             (not (file-remote-p default-directory))
             (bound-and-true-p lsp-use-plists)
             (not (functionp 'json-rpc-connection))
             (executable-find "emacs-lsp-booster"))
        (progn
          (message "Using emacs-lsp-booster for %s!" result)
          (append '("emacs-lsp-booster" "--json-false-value" ":json-false" "--")
                  result))
      result)))

;;;###autoload
(defun ad:json-read@with-lsp-booster (fn &rest args)
  "`emacs-lsp-booster' が返すバイトコードを JSON の代わりに読み込む処理.
FN, ARGS はアドバイス対象の関数とその引数."
  (or (when (equal (following-char) ?#)
        (let ((bytecode (read (current-buffer))))
          (when (byte-code-function-p bytecode)
            (funcall bytecode))))
      (apply fn args)))

;;;###autoload
(defun e:set-lsp-client-add-on (client value)
  "対象の CLIENT の `add-on' の設定を VALUE に変更する."
  (let ((ls (gethash client lsp-clients)))
    (setf (lsp--client-add-on? ls) value)))

(provide 'nsmacs-lsp-mode)
;;; nsmacs-lsp-mode.el ends here
