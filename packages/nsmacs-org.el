;;; nsmacs-org.el --- org 関連のコマンド
;;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'org)

;;;###autoload
(defun e:org-archive-file ()
  "アーカイブ用のファイルのパスを返す."
  (require 'org)
  (expand-file-name (format-time-string "archives/%Y.org") org-directory))

;;;###autoload
(defun e:org-tasks-file ()
  "タスク管理用のファイルのパスを返す."
  (require 'org)
  (expand-file-name "denote/20000101T000000--タスク__task.org" org-directory))

;;;###autoload
(defun e:org-note-file ()
  "メモ用ファイルのパスを返す."
  (expand-file-name "denote/20000101T000001--メモ__memo.org" org-directory))

;;;###autoload
(defun org-support/popup-tasks ()
  "タスク管理用のファイルをポップアップで表示."
  (interactive)
  (display-buffer (find-file-noselect (e:org-tasks-file))))

;;;###autoload
(defun org-support/popup-note ()
  "メモ用のファイルをポップアップで表示."
  (interactive)
  (display-buffer (find-file-noselect (e:org-note-file))))

(provide 'nsmacs-org)
;;; nsmacs-org.el ends here
