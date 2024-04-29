;;; nsmacs-borg.el --- borg 関連のコマンド
;;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'borg)
(require 'f)

;;;###autoload
(defun e:borg-prune ()
  "`borg' で不要となったディレクトリを掃除する."
  (interactive)
  (let ((drones  (borg-drones)))
    (--each (->> (f-directories (f-expand ".git/modules" borg-user-emacs-directory))
                 (--reject (-contains? drones (f-filename it))))
      (message "remove: %s" it)
      (f-delete it t))))

(provide 'nsmacs-borg)
;;; nsmacs-borg.el ends here
