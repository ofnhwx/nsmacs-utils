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
  (expand-file-name "denote/TASK.org" org-directory))

;;;###autoload
(defun e:org-note-file ()
  "メモ用ファイルのパスを返す."
  (expand-file-name "denote/NOTE.org" org-directory))

;;;###autoload
(defun e:org-popup-tasks ()
  "タスク管理用のファイルをポップアップで表示."
  (interactive)
  (display-buffer (find-file-noselect (e:org-tasks-file))))

;;;###autoload
(defun e:org-popup-note ()
  "メモ用のファイルをポップアップで表示."
  (interactive)
  (display-buffer (find-file-noselect (e:org-note-file))))

(provide 'nsmacs-org)
;;; nsmacs-org.el ends here
