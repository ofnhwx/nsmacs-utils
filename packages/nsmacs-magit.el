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
            magit-branch-or-checkout
            magit-branch-and-checkout
            magit-merge-plain
            magit-merge-editmsg
            magit-merge-nocommit
            magit-merge-absorb
            magit-merge-squash
            magit-merge-into)
    (add-to-list 'marginalia-command-categories `(,it . magit-branch)))
  (add-to-list 'marginalia-annotator-registry '(magit-branch marginalia-annotate-magit-branch builtin none)))

(defun marginalia-annotate-magit-branch (cand)
  "`magit-branch' 用の `marginalia' の設定.
CAND の詳細は `marginalia を参照."
  (let* ((command (format "git log -1 --pretty=format:%%s%%ad --date=short %s" cand))
         (log (s-split "" (kllib:shell-command-to-string command))))
    (marginalia--fields
     ((nth 0 log) :face 'marginalia-documentation)
     ((nth 1 log) :face 'marginalia-date))))

(provide 'nsmacs-magit)
;;; nsmacs-magit.el ends here
