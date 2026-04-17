;;; nsmacs-project.el --- project.el 関連のコマンド -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'project)
(require 'komunan-lisp-library)

;;;###autoload
(defun e:setup-project-known-projects ()
  "`ghq' で管理しているリポジトリを `project.el' で選択できるように設定する."
  (when (executable-find "ghq")
    (setq project--list
          (->> (kllib:shell-command-to-list "ghq list --full-path")
               (-map 'f-short)
               (-map 'file-name-as-directory)
               (-sort 's-less?)
               (-map #'list)))))

;;;###autoload
(defun e:project-edit-dir-locals ()
  "現在のプロジェクトの `.dir-locals.el' を開く."
  (interactive)
  (let ((project (project-current t)))
    (find-file (expand-file-name ".dir-locals.el" (project-root project)))))

(provide 'nsmacs-project)
;;; nsmacs-project.el ends here
