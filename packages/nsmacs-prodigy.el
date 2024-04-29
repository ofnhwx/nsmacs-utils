;;; nsmacs-prodigy.el --- prodigy 関連のコマンド
;;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'prodigy)
(require 'nsmacs-vterm)
(require 'skk)

;;;###autoload
(defun ad:start-process@with-vterm (name buffer program &rest args)
  "`prodigy' のプロセスを `vterm' で起動するためのアドバイス.
NAME, BUFFER, PROGRAM, ARGS は `start-process' と同じ."
  (let* ((cwd (plist-get (prodigy-find-service name) :cwd))
         (sock-file (f-expand ".overmind.sock" cwd)))
    (when (f-exists? sock-file)
      (message "delete: %s" sock-file)
      (delete-file sock-file)))
  (save-window-excursion
    (with-current-buffer (apply #'e:vterm-exec name program args)
      vterm--process)))

;;;###autoload
(defun ad:prodigy-start-service@with-vterm (fn &rest args)
  "`prodigy' のプロセスを `vterm' で起動する.
FN, ARGS はアドバイス対象の関数とその引数."
  (advice-add 'start-process :override #'ad:start-process@with-vterm)
  (prog1 (ignore-errors (apply fn args))
    (advice-remove 'start-process #'ad:start-process@with-vterm)))

;;;###autoload
(defvar yaskkserv2-dictionary (expand-file-name "~/dictionary.yaskkserv2")
  "`yaskkserv2' の辞書ファイルのパス.")

;;;###autoload
(defun e:prodigy-yaskkserv2 ()
  "`yaskkserv2' を `prodigy' で起動する."
  (interactive)
  (let ((service "yaskkserv2"))
    (require 'prodigy)
    (unless (prodigy-find-service service)
      (prodigy-define-service
        :name service
        :command skk-server-prog
        :args `("--no-daemonize" "--google-suggest" ,yaskkserv2-dictionary)
        :tags '(general)
        :stop-signal 'int))
    (prodigy-start-service (prodigy-find-service service))))

(provide 'nsmacs-prodigy)
;;; nsmacs-prodigy.el ends here
