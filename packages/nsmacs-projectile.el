;;; nsmacs-projectile.el --- projectile 関連のコマンド
;;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'projectile)
(require 'komunan-lisp-library)

;;;###autoload
(defun e:setup-projectile-known-projects ()
  "`ghq' で管理しているリポジトリを `projectile' で選択できるように設定する."
  (when (executable-find "ghq")
    (setq projectile-known-projects
          (->> projectile-known-projects
               (--remove (eq (projectile-project-vcs it) 'none))
               (-union (-map 'f-short (kllib:shell-command-to-list "ghq list --full-path")))
               (-map 'file-name-as-directory)
               (-sort 's-less?)
               (-distinct)))))

(provide 'nsmacs-projectile)
;;; nsmacs-projectile.el ends here
