;;; nsmacs-project-rails.el --- project-rails 関連のコマンド -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'project-rails)

;;;###autoload
(defun ad:project-rails--choices@override (dirs)
  "`project-rails--choices' を dir 相対マッチで再実装する.
upstream はプロジェクトルート相対パスに正規表現を適用するため、
キーに dir プレフィックスが混入したり、prefix 指定エントリ
(`environment' 等) が正しく扱えない.
本実装は projectile-rails 相当で、正規表現は dir 相対パスに適用し、
ハッシュの値はルート相対パスで保存する."
  (let ((hash (make-hash-table :test 'equal)))
    (dolist (entry dirs)
      (let* ((dir (nth 0 entry))
             (re (nth 1 entry))
             (prefix (nth 2 entry))
             (full-dir (project-rails-expand-root dir)))
        (when (file-directory-p full-dir)
          (dolist (file (directory-files-recursively full-dir "." nil))
            (let ((dir-rel (file-relative-name file full-dir)))
              (when (string-match re dir-rel)
                (puthash (concat (or prefix "") (match-string 1 dir-rel))
                         (file-relative-name file (project-rails-root))
                         hash)))))))
    hash))

;;;###autoload
(defun e:setup-project-rails-views-re ()
  "Rails のビューファイルとして認識される拡張子を設定する."
  (let ((exts '("html" "erb" "haml" "slim"
                "js" "coffee" "ts"
                "css" "scss" "sass" "less"
                "json" "builder" "jbuilder" "rabl"
                "csb" "csvbuilder" "axlsx")))
    (setopt project-rails-views-re (concat "\\." (regexp-opt exts)))))

;;;###autoload
(defun e:project-rails-find-decorator ()
  "Find a decorator."
  (interactive)
  (project-rails--completing-read
   "decorator: "
   (project-rails--choices '(("app/decorators/" "\\(.+\\)\\.rb$")))
   "app/decorators/%s.rb"))

;;;###autoload
(defun e:project-rails-find-form ()
  "Find a form."
  (interactive)
  (project-rails--completing-read
   "form: "
   (project-rails--choices '(("app/forms/" "\\(.+\\)\\.rb$")))
   "app/forms/%s.rb"))

;;;###autoload
(defun e:project-rails-find-view-components ()
  "Find a View component."
  (interactive)
  (project-rails--completing-read
   "components: "
   (project-rails--choices
    `(("app/components/" "\\(.+\\)\\.rb$")
      ("app/components/" ,(concat "\\(.+\\)" project-rails-views-re))))
   "app/components/%s.rb"))

;;;###autoload
(defun e:project-rails-find-graphql ()
  "Find a GraphQL."
  (interactive)
  (project-rails--completing-read
   "graphql: "
   (project-rails--choices '(("app/graphql/" "\\(.+\\)\\.rb$")))
   "app/graphql/%s.rb"))

;;;###autoload
(defun e:project-rails-find-route ()
  "Find a route."
  (interactive)
  (project-rails--completing-read
   "routes: "
   (project-rails--choices '(("config/" "\\(routes\\)\\.rb$")
                             ("config/" "\\(routes/.+\\)\\.rb$")))))

(provide 'nsmacs-project-rails)
;;; nsmacs-project-rails.el ends here
