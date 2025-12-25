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

;;;###autoload
(defun e:prodigy-define-rails-service (project-dir)
  "Rails プロジェクトを Prodigy のサービスとして定義する.
PROJECT-DIR はプロジェクトのルートディレクトリのパス."
  (unless (f-exists? project-dir)
    (error "対象のディレクトリが見つかりません: %s" project-dir))
  (let ((procfile (e:find-procfile project-dir))
        (service  (f-filename      project-dir)))
    (prodigy-define-service
      :name (format "rails/%s" service)
      :command "omind"
      :args (list "start" "--procfile" procfile)
      :cwd project-dir
      :tags '(rails)
      :url (format "https://%s.test" service)
      :stop-signal 'int)))

(defun e:find-procfile (project-dir)
  "PROJECT-DIR にある Procfile を探す."
  (let ((procfile (->> '("Procfile.dev" "Procfile.local" "Procfile")
                       (--map (f-expand it project-dir))
                       (-filter #'f-exists?)
                       (-map #'f-filename)
                       (-first-item))))
    (unless procfile
      (error "Procfile が見つかりません: %s" project-dir))
    procfile))

(provide 'nsmacs-prodigy)
;;; nsmacs-prodigy.el ends here
