;;; nsmacs-magit.el --- magit 関連のコマンド
;;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'magit)
(require 'komunan-lisp-library)
(require 'dash)
(require 'f)
(require 'marginalia)

;;;###autoload
(defun ad:magit-repos-alist@override (&rest _)
  "`magit' のリポジトリの一覧表示にパスを追加する."
  (magit-list-repos-uniquify
   (--map (cons (f-short it) it)
          (magit-list-repos))))

;;;###autoload
(defun e:setup-magit-repository-directories ()
  "`magit' でリポジトリを探すパスを `ghq` をもとに設定する."
  (when (executable-find "ghq")
    (setopt magit-repository-directories
            (->> (kllib:shell-command-to-list "ghq root --all")
                 (--map (cons it 5))))))

;;;###autoload
(defun e:setup-marginalia-magit ()
  "`magit' 用の `marginalia' の設定を有効にする."
  (--each '(magit-checkout
            magit-branch-and-checkout
            magit-branch-checkout
            magit-branch-configure
            magit-branch-create
            magit-branch-delete
            magit-branch-or-checkout
            magit-branch-orphan
            magit-branch-rename
            magit-branch-reset
            magit-branch-shelve
            magit-merge-absorb
            magit-merge-editmsg
            magit-merge-into
            magit-merge-nocommit
            magit-merge-plain
            magit-merge-squash
            magit-reset-quickly
            magit-worktree-checkout)
    (add-to-list 'marginalia-command-categories `(,it . magit-branch)))
  (add-to-list 'marginalia-annotator-registry '(magit-branch marginalia-annotate-magit-branch builtin none)))

(defun marginalia-annotate-magit-branch (cand)
  "`magit-branch' 用の `marginalia' の設定.
CAND の詳細は `marginalia を参照."
  (pcase-let* (;;
               (command1 (format "git log -1 --pretty='format:%%cd%%s' --date=human %s" cand))
               (`(,committer-date ,subject) (s-split "" (kllib:shell-command-to-string command1)))
               ;;
               (command2 (format "git config --get 'branch.%s.description'" cand))
               (description (kllib:shell-command-to-string command2)))
    (marginalia--fields
     (committer-date :face 'marginalia-date)
     (description    :face 'marginalia-documentation)
     (subject        :face 'marginalia-lighter))))

(provide 'nsmacs-magit)
;;; nsmacs-magit.el ends here
