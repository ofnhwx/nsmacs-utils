;;; nsmacs-ghostel.el --- ghostel 関連のコマンド -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'ghostel)
(require 's)

;;;###autoload
(defun e:ghostel-exec (name program &rest args)
  "NAME, PROGRAM および ARGS を指定して `ghostel' を起動する."
  (let ((buffer (get-buffer-create (format "*ghostel-%s*" name))))
    (ghostel-exec buffer program args)
    buffer))

(defun e:ghostel--dir-to-path (dir)
  "`ghostel--update-directory' の dir → path 変換を再現する."
  (if (string-prefix-p "file://" dir)
      (let* ((url (url-generic-parse-url dir))
             (host (url-host url))
             (filename (url-filename url)))
        (if (ghostel--local-host-p host)
            filename
          (let ((prefix (file-remote-p default-directory)))
            (if prefix
                (concat prefix filename)
              (format "/%s:%s:%s"
                      (or ghostel-tramp-default-method tramp-default-method)
                      host filename)))))
    dir))

(defun e:ghostel-update-directory-guard (orig dir)
  "ローカル shell から remote への遷移を拒否する advice."
  (when (or (file-remote-p default-directory)
            (not (stringp dir))
            (not (file-remote-p (e:ghostel--dir-to-path dir))))
    (funcall orig dir)))

(provide 'nsmacs-ghostel)
;;; nsmacs-ghostel.el ends here
