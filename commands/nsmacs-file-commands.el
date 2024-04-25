;;; nsmacs-file-commands.el --- ファイル関連のコマンド
;;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'f)
(require 'borg)

;;;###autoload
(defun e:file/find-user-init-file ()
  "`user-init-file' を開く."
  (interactive)
  (find-file-existing user-init-file))

;;;###autoload
(defun e:file/find-early-init-file ()
  "`early-init-file' を開く."
  (interactive)
  (find-file-existing early-init-file))

;;;###autoload
(defun e:file/find-config-file ()
  "`user-emacs-directory' にある `init.org' を開く."
  (interactive)
  (find-file-existing (f-expand "init.org" user-emacs-directory)))

;;;###autoload
(defun e:file/find-utils-directory ()
  "`nsmacs-utils' のディレクトリを開く."
  (interactive)
  (find-file-existing (f-expand "nsmacs-utils" borg-drones-directory)))

;;;###autoload
(defun e:make-config ()
  "`init.org' から `init.el', `early-init.el' を生成してコンパイルする."
  (interactive)
  (let ((default-directory user-emacs-directory)
        (buffer (get-buffer-create "*Make*")))
    (display-buffer buffer)
    (set-process-sentinel
     (start-process-shell-command "e:make-config" buffer "make init-tangle && make init-build")
     (lambda (process event)
       (let* ((message (s-trim-right (format "%s %s" process event)))
              (command (format "notify-send '%s'" message)))
         (call-process-shell-command command))))))

(provide 'nsmacs-file-commands)
;;; nsmacs-file-commands.el ends here
