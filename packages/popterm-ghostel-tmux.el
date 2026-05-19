;;; popterm-ghostel-tmux.el --- popterm ghostel + tmux integration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'ghostel)
(require 'popterm)
(require 'no-littering)

;;;###autoload
(defun popterm-ghostel-tmux ()
  "popterm の ghostel バックエンド経由で tmux セッションをトグルする."
  (interactive)
  (let ((ghostel-shell (no-littering-expand-etc-file-name "ghostel/ghostel-tmux")))
    (popterm-ghostel)))

(provide 'popterm-ghostel-tmux)
;;; popterm-ghostel-tmux.el ends here
