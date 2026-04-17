;;; nsmacs-forge.el --- forge 関連のカスタマイズ -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'eieio)
(require 'forge-repo)

(defun e:forge--expand-author-clause (clause)
  "CLAUSE が `(= topic:author \"a,b,...\")' のときに OR 句へ展開する."
  (if (and (consp clause)
           (eq (car clause) '=)
           (eq (cadr clause) 'topic:author)
           (stringp (caddr clause))
           (string-search "," (caddr clause)))
      `(or ,@(mapcar (lambda (a) `(= topic:author ,a))
                     (split-string (caddr clause) "," t)))
    clause))

;;;###autoload
(defun ad:forge--list-topics-2@multi-author (query)
  "QUERY 内のカンマ区切り author 指定を OR 句に展開する.
想定外の構造や例外が起きた場合は、元の QUERY をそのまま返し `message' で通知する."
  (condition-case err
      (if-let* (((vectorp query))
                (pos (seq-position query :where))
                ((< (1+ pos) (length query)))
                (where (aref query (1+ pos)))
                ((and (consp where) (eq (car where) 'and))))
          (let ((new-query (copy-sequence query)))
            (aset new-query (1+ pos)
                  (cons 'and (mapcar #'e:forge--expand-author-clause (cdr where))))
            new-query)
        query)
    (error
     (message "ad:forge--list-topics-2@multi-author: %S" err)
     query)))

;;;###autoload
(defun e:setup-forge-multi-author-filter ()
  "`forge-topics-filter-author' の reader を複数人対応に差し替える."
  (if-let* ((suffix (get 'forge-topics-filter-author 'transient--suffix)))
      (oset suffix reader
            (lambda ()
              (condition-case err
                  (let* ((repo (forge-get-repository :tracked))
                         (choices (and repo (mapcar #'cadr (oref repo assignees))))
                         (input (completing-read-multiple "Author: " choices)))
                    (and input (string-join input ",")))
                (error
                 (message "forge-topics-filter-author: %S" err)
                 nil))))
    (message "e:setup-forge-multi-author-filter: suffix が見つかりません")))

(provide 'nsmacs-forge)
;;; nsmacs-forge.el ends here
