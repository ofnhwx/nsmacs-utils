;;; nsmacs-projectile-rails.el --- projectile-rails 関連のコマンド
;;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'projectile-rails)

;;;###autoload
(defun e:setup-projectile-rails-views-re ()
  "Rails のビューファイルとして認識される拡張子を設定する."
  (let ((exts '("html" "erb" "haml" "slim"
                "js" "coffee" "ts"
                "css" "scss" "sass" "less"
                "json" "builder" "jbuilder" "rabl"
                "csb" "csvbuilder" "axlsx")))
    (setopt projectile-rails-views-re (concat "\\." (regexp-opt exts)))))

;;;###autoload
(defun e:projectile-rails-find-view-components ()
  "Find a View component."
  (interactive)
  (projectile-rails-find-resource
   "components: "
   `(("app/components/" "\\(.+\\)\\.rb$")
     ("app/components/" ,(concat "\\(.+\\)" projectile-rails-views-re)))
   "app/components/${filename}"))

;;;###autoload
(defun e:projectile-rails-find-graphql ()
  "Find a GraphQL."
  (interactive)
  (projectile-rails-find-resource
   "graphql: "
   '(("app/graphql/" "\\(.+\\)\\.rb$"))
   "app/graphql/${filename}"))

(provide 'nsmacs-projectile-rails)
;;; nsmacs-projectile-rails.el ends here
