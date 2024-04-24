;;; nsmacs-consult.el --- consult 関連のコマンド
;;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'consult)
(require 'dash)

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

(provide 'nsmacs-consult)
;;; nsmacs-consult.el ends here
