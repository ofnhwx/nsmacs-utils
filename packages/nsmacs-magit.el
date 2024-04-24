;;; nsmacs-magit.el --- magit 関連のコマンド
;;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'magit)
(require 'komunan-lisp-library)
(require 'dash)

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

(provide 'nsmacs-magit)
;;; nsmacs-magit.el ends here
