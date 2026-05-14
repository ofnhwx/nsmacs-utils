;;; nsmacs-project.el --- project.el 関連のコマンド -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'project)
(require 'komunan-lisp-library)

(defun e:list-known-projects ()
  "`ghq' で管理しているリポジトリと worktree の一覧を返す."
  (->> (kllib:shell-command-to-list "ghq list --full-path")
       (--map (file-name-as-directory (f-short it)))
       (--map (cons it
                    (->> (file-expand-wildcards (expand-file-name ".git/worktrees/*/gitdir" it))
                         (--map (string-trim (f-read-text it)))
                         (--map (file-name-directory it))
                         (--map (file-name-as-directory (f-short it))))))
       (-flatten)
       (-sort 's-less?)))

;;;###autoload
(defun ad:project-prompt-project-dir@update (&rest _)
  "`project-prompt-project-dir' の直前に `project--list' を更新する."
  (when (executable-find "ghq")
    (project--ensure-read-project-list)
    (setq project--list
          (-uniq (-concat project--list (-map #'list (e:list-known-projects)))))))

;;;###autoload
(defun e:project-edit-dir-locals ()
  "現在のプロジェクトの `.dir-locals.el' を開く."
  (interactive)
  (let ((project (project-current t)))
    (find-file (expand-file-name ".dir-locals.el" (project-root project)))))

(provide 'nsmacs-project)
;;; nsmacs-project.el ends here
