;;; nsmacs-consult.el --- consult 関連のコマンド
;;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'consult)
(require 'dash)

;;;###autoload
(defun ad:consult-line@with-orderless (fn &rest args)
  "`consult-line' を `orderless' の補完スタイルで実行する.
FN, ARGS はアドバイス対象の関数とその引数."
  (let ((completion-styles '(orderless)))
    (apply fn args)))

;;;###autoload
(defun e:consult-faces ()
  "Face の一覧を `consult' で表示する."
  (interactive)
  (consult--read (--map (format "%s" it) (face-list))
                 :prompt "Face: "))

;;;###autoload
(defun e:consult-line-dwim ()
  "カーソルの要素で `consult-line' を実行する."
  (interactive)
  (consult-line (thing-at-point 'symbol)))

;;;###autoload
(defun e:consult-ripgrep-dwim ()
  "カーソルの要素で `consult-ripgrep' を実行する."
  (interactive)
  (consult-ripgrep nil (thing-at-point 'symbol)))

;;;###autoload
(defun e:consult-ripgrep-cwd (&optional initial)
  "現在のディレクトリで `consult-ripgrep' を実行する.
検索文字列の初期値は INITIAL で指定する."
  (interactive)
  (consult-ripgrep default-directory initial))

;;;###autoload
(defun e:consult-ripgrep-cwd-dwim ()
  "現在のディレクトリ, カーソルの要素で `consult-ripgrep' を実行する."
  (interactive)
  (consult-ripgrep default-directory (thing-at-point 'symbol)))

;;;###autoload
(defun e:consult-dunst-history ()
  "通知の履歴から候補を選択して通知を再表示する."
  (interactive)
  (and-let* ((notifications (e:consult-dunst--parse-history))
             (candidates (mapcar (lambda (notification)
                                   (cons (e:consult-dunst--format-notification notification) notification))
                                 (sort notifications (lambda (a b) (> (plist-get a :id) (plist-get b :id))))))
             (selected-string (consult--read candidates :prompt "Dunst History: " :category 'dunst-notification :require-match t :sort nil))
             (selected-notification (cdr (assoc selected-string candidates))))
    (e:consult-dunst--history-pop selected-notification)))

(defun e:consult-dunst--parse-history ()
  "通知の履歴を `dunstctl history' から plist 形式で取得する."
  (when-let* ((boot-time (time-convert (date-to-time (kllib:shell-command-to-string "uptime -s")) 'integer))
              (command "dunstctl history | jq -r '.data[0][] | [.id.data, .appname.data, .summary.data, (.body.data | gsub(\"\\n\"; \" \")), .timestamp.data] | @tsv'")
              (jq-output (shell-command-to-string command))
              ((not (string-empty-p jq-output))))
    (mapcar (lambda (line)
              (when-let* ((fields (split-string line "\t"))
                          ((>= (length fields) 5)))
                (list :id (string-to-number (nth 0 fields))
                      :appname (nth 1 fields)
                      :summary (nth 2 fields)
                      :body (nth 3 fields)
                      :timestamp (+ (/ (string-to-number (nth 4 fields)) 1000000) boot-time))))
            (split-string jq-output "\n" t))))

(defun e:consult-dunst--format-notification (notification)
  "通知を consult 用の文字列にフォーマットする.
NOTIFICATION は :id, :summary, :appname, :timestamp を含む plist."
  (let ((id (plist-get notification :id))
        (appname (plist-get notification :appname))
        (summary (plist-get notification :summary))
        (body (plist-get notification :body))
        (timestamp (plist-get notification :timestamp)))
    (format "%s %s %-15s %s %s"
            (propertize (format "%3d" id) 'face 'font-lock-constant-face)
            (propertize (format-time-string "%m/%d %H:%M" timestamp) 'face 'font-lock-type-face)
            (if (> (length appname) 15) (substring appname 0 15) appname)
            summary
            (propertize body 'face 'font-lock-comment-face))))

(defun e:consult-dunst--history-pop (notification)
  "選択した通知を再表示する.
NOTIFICATION は :id, :summary, :appname を含む plist."
  (when-let ((id (plist-get notification :id)))
    (shell-command-to-string (format "dunstctl history-pop %d" id))))

(provide 'nsmacs-consult)
;;; nsmacs-consult.el ends here
