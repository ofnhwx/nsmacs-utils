;;; nsmacs-patch.el --- 関数定義パッチ機構 -*- lexical-binding: t; -*-

;;; Commentary:
;; 他パッケージの関数定義を再定義しつつ、元定義の変更を検知する仕組み。
;; パッチ作成時の drone コミットハッシュと元定義 sexp の sha256 を記録し、
;; `e:patch-validate-all' で上流の変更を検出する。

;;; Code:

(require 'find-func)
(require 'subr-x)

(defvar e:patch--registry nil
  "登録済み patch の alist. (SYMBOL . PLIST) 形式.
PLIST は :commit :checksum :file :drone を含む.")

(defun e:patch--git-head (dir)
  "DIR の git HEAD commit hash を返す.
submodule (.git がファイル) も解決する."
  (let ((gitdir (expand-file-name ".git" dir)))
    (when (file-regular-p gitdir)
      (with-temp-buffer
        (insert-file-contents gitdir)
        (when (re-search-forward "^gitdir: \\(.*\\)" nil t)
          (setq gitdir (expand-file-name
                        (string-trim (match-string 1)) dir)))))
    (let ((head (expand-file-name "HEAD" gitdir)))
      (when (file-readable-p head)
        (with-temp-buffer
          (insert-file-contents head)
          (let ((s (string-trim (buffer-string))))
            (if (string-prefix-p "ref: " s)
                (let ((ref (expand-file-name (substring s 5) gitdir)))
                  (when (file-readable-p ref)
                    (with-temp-buffer
                      (insert-file-contents ref)
                      (string-trim (buffer-string)))))
              s)))))))

(defun e:patch--file-checksum (file symbol)
  "FILE 内の SYMBOL 定義 sexp の sha256 を返す.
見つからない場合は nil."
  (when (and file (file-readable-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (when (re-search-forward
             (concat "^(\\(?:cl-\\)?def\\(?:un\\|macro\\|subst\\)\\*?\\s-+"
                     (regexp-quote (symbol-name symbol))
                     "\\_>")
             nil t)
        (goto-char (match-beginning 0))
        (secure-hash 'sha256 (prin1-to-string (read (current-buffer))))))))

(defun e:patch--locate (symbol)
  "SYMBOL の (FILE . DRONE-DIR) を返す.
SYMBOL は既にロード済みである必要がある."
  (unless (fboundp symbol)
    (error "e:patch: %s is not defined; require upstream first" symbol))
  (let* ((loc (find-function-noselect symbol t))
         (file (and loc (buffer-file-name (car loc))))
         (drone (and file (locate-dominating-file file ".git"))))
    (unless file
      (error "e:patch: cannot locate source for %s" symbol))
    (cons file (and drone (directory-file-name drone)))))

(defun e:patch--register (symbol commit checksum)
  "SYMBOL を registry に登録する.
この時点で元定義の :file と :drone を捕捉する。
よって `defun' で上書きする前に呼ぶこと."
  (pcase-let ((`(,file . ,drone) (e:patch--locate symbol)))
    (setf (alist-get symbol e:patch--registry)
          (list :commit   commit
                :checksum checksum
                :file     file
                :drone    drone))))

;;;###autoload
(defun e:patch-validate (symbol)
  "SYMBOL のパッチを検証する.
戻り値は :ok / :commit-changed / :checksum-mismatch /
:drone-missing / :source-missing のいずれか."
  (let ((info (alist-get symbol e:patch--registry)))
    (unless info
      (error "e:patch: %s is not registered" symbol))
    (let ((file     (plist-get info :file))
          (drone    (plist-get info :drone))
          (e-commit (plist-get info :commit))
          (e-csum   (plist-get info :checksum)))
      (cond
       ((not (and drone (file-directory-p drone))) :drone-missing)
       ((not (and file  (file-readable-p file)))   :source-missing)
       ((equal (e:patch--git-head drone) e-commit) :ok)
       ((equal (e:patch--file-checksum file symbol) e-csum)
        :commit-changed)
       (t :checksum-mismatch)))))

;;;###autoload
(defun e:patch-validate-all ()
  "登録された全 patch を検証する.
問題があれば *e:patch* バッファに一覧表示する."
  (interactive)
  (let (problems)
    (dolist (entry e:patch--registry)
      (let ((result (e:patch-validate (car entry))))
        (unless (eq result :ok)
          (push (cons (car entry) result) problems))))
    (if (null problems)
        (message "e:patch: all %d patches OK" (length e:patch--registry))
      (with-current-buffer (get-buffer-create "*e:patch*")
        (erase-buffer)
        (insert (format "%d patch(es) need attention:\n\n" (length problems)))
        (dolist (p (nreverse problems))
          (let ((info (alist-get (car p) e:patch--registry)))
            (insert (format "%-50s %s\n" (car p) (cdr p)))
            (insert (format "  file:   %s\n"     (plist-get info :file)))
            (insert (format "  drone:  %s\n"     (plist-get info :drone)))
            (insert (format "  commit: %s → %s\n"
                            (plist-get info :commit)
                            (e:patch--git-head (plist-get info :drone))))
            (insert "\n")))
        (goto-char (point-min))
        (display-buffer (current-buffer))))))

;;;###autoload
(defun e:patch-metadata (symbol)
  "SYMBOL の現在の :commit / :checksum を表示し kill-ring に積む.
パッチを書き起こすときの補助コマンド."
  (interactive
   (list (intern
          (completing-read
           "Function: " obarray #'fboundp t
           (when-let* ((s (function-called-at-point))) (symbol-name s))))))
  (pcase-let* ((`(,file . ,drone) (e:patch--locate symbol))
               (commit   (and drone (e:patch--git-head drone)))
               (checksum (e:patch--file-checksum file symbol))
               (text     (format ":commit   %S\n  :checksum %S" commit checksum)))
    (kill-new text)
    (message "%s" text)))

;;;###autoload
(defmacro e:patch-defun (name &rest rest)
  "上流の変更を検知できる形で NAME を再定義する.

NAME の直後にキーワードペアを続けて、最後に通常の `defun' と
同じ ARGLIST 以降を書く:

  (e:patch-defun some-package-foo
      :commit   \"a1b2c3d\"
      :checksum \"3f1c...c9\"
    (x)
    \"docstring\"
    (new-impl x))

:commit   パッチ作成時の drone HEAD commit hash
:checksum パッチ作成時の元定義 sexp の sha256

NAME を含むパッケージは事前にロードされている必要がある."
  (declare (indent defun) (doc-string 4))
  (let (props)
    (while (keywordp (car rest))
      (push (pop rest) props)
      (push (pop rest) props))
    (let* ((plist    (nreverse props))
           (commit   (plist-get plist :commit))
           (checksum (plist-get plist :checksum))
           (arglist  (car rest))
           (body     (cdr rest)))
      (unless (stringp commit)
        (error "e:patch-defun: :commit is required"))
      (unless (stringp checksum)
        (error "e:patch-defun: :checksum is required"))
      (unless (listp arglist)
        (error "e:patch-defun: ARGLIST missing for %s" name))
      `(progn
         (e:patch--register ',name ,commit ,checksum)
         (defun ,name ,arglist ,@body)))))

(provide 'nsmacs-patch)
;;; nsmacs-patch.el ends here
