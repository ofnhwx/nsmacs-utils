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
  (let ((drones (borg-drones)))
    (--each (->> (f-directories (f-expand ".git/modules" borg-user-emacs-directory))
                 (--reject (-contains? drones (f-filename it))))
      (message "remove: %s" it)
      (f-delete it t))))

;;;###autoload
(defun e:borg-check-removable-drones ()
  "依存関係をチェックして削除可能なドローンを表示する."
  (interactive)
  (message "================ start ================")
  (let ((drones (borg-drones)))
    (-each drones
      (lambda (drone)
        (and-let* ((package (epkg drone))
                   (reverse-dependencies (-map #'car (epkg-reverse-dependencies package))))
          (unless (-intersection reverse-dependencies drones)
            (message "has no reverse dependencies: %s => %s" drone reverse-dependencies))))))
  (message "================ done. ================"))

(provide 'nsmacs-borg)
;;; nsmacs-borg.el ends here
