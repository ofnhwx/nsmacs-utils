;;; nsmacs-apheleia.el --- apheleia 関連のコマンド
;;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'apheleia)
(require 'dash)
(require 's)

;;;###autoload
(defun e:apheleia-inhibit-unnecesary-major-mode ()
  "特定のメジャーモード以外では `apheleia' を実行しない"
  (--none? (derived-mode-p it)
           '(
             ruby-base-mode
             )))

;;;###autoload
(defun e:apheleia-inhibit-rubocop-excludes ()
  "Rubocop で対象外としたいファイルを `apheleia' の対象からも除外する."
  (--any? (s-ends-with? it buffer-file-name)
          '(
            "/config/application.rb"
            "/config/environments/development.rb"
            "/config/environments/production.rb"
            "/config/environments/staging.rb"
            "/config/environments/test.rb"
            "/db/schema.rb"
            )))

(provide 'nsmacs-apheleia)
;;; nsmacs-apheleia.el ends here
