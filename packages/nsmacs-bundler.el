;;; nsmacs-bundler.el --- bundler 関連のコマンド
;;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'bundler)
(require 'komunan-lisp-library)
(require 'ht)

(defvar e:bundle-exists-cache (ht-create 'equal)
  "`bundle exists' の結果をキャッシュしておく変数.")

;;;###autoload
(defun e:clear-bundle-exists-cache ()
  "`e:bundle-exists-cache'をクリアする."
  (interactive)
  (ht-clear! e:bundle-exists-cache))

(defun e:bundle-exists (name)
  "NAME で指定した gem が bundler でインストールされているかをチェックする."
  (let ((key (format "%s@%s" name (or (kllib:project-root) (buffer-name)))))
    (unless (ht-get e:bundle-exists-cache key)
      (ht-set e:bundle-exists-cache key (call-process-shell-command (format "bundle info %s" name))))
    (zerop (ht-get e:bundle-exists-cache key))))

(provide 'nsmacs-bundler)
;;; nsmacs-bundler.el ends here
