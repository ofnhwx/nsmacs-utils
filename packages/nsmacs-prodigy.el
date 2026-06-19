;;; nsmacs-prodigy.el --- prodigy 関連のコマンド -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'prodigy)
(require 'skk)

;;;###autoload
(defun ad:start-process@with-pty (name buffer program &rest args)
  "`prodigy' のプロセスを pty で起動するためのアドバイス.
NAME, BUFFER, PROGRAM, ARGS は `start-process' と同じ.
pty の winsize を明示的に設定する (デフォルトの 0x0 だと `overmind' 内
tmux の `refresh -C 0,0' が弾かれて 2 つ目以降の neww が実行されない)."
  (let ((proc (make-process :name name
                            :buffer buffer
                            :command (cons program args)
                            :connection-type 'pty)))
    (set-process-window-size proc 24 80)
    proc))

;;;###autoload
(defun ad:prodigy-start-service@with-pty (fn &rest args)
  "`prodigy' のプロセスを pty で起動する.
FN, ARGS はアドバイス対象の関数とその引数."
  (advice-add 'start-process :override #'ad:start-process@with-pty)
  (prog1 (ignore-errors (apply fn args))
    (advice-remove 'start-process #'ad:start-process@with-pty)))

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
