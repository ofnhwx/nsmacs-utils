;;; nsmacs-macros.el --- マクロ集 -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'evil-core))

(require 'cl-lib)
(require 's)
(require 'general)
(require 'bind-map)
(require 'marginalia)

(defmacro e:default! (variable default)
  "VARIABLE のデフォルト値を DEFAULT に設定する."
  `(setq-default ,variable ,default))

(defmacro e:local! (variable value)
  "VARIABLE のバッファーローカルな値を VALUE に設定する."
  `(setq-local ,variable ,value))

(defmacro e:var! (variable value)
  "VARIABLE で指定したファイルを `no-littering' で管理する.
ファイル名は VALUE に設定する."
  `(setopt ,variable (no-littering-expand-var-file-name ,value)))

(defmacro e:if! (condition &rest body)
  "CONDITION を満たす場合のみ BODY の内容を展開する."
  (declare (indent defun))
  (if (eval condition)
      `(progn ,@body)))

(defmacro e:eval! (&rest body)
  "BODY を評価する(コンパイル時の警告回避用)."
  (declare (indent defun))
  `(eval '(progn ,@body)))

(defmacro e:major-mode-key-def (modes key def &rest bindings)
  "MODES で指定したメジャーモード用のキーバイドを設定する.
KEY, DEF の組み合わせをを一つのペアとし、BINDINGS をして任意の回数繰り返し指定できる."
  (declare (indent defun))
  `(e:mode-key-def :major-modes ,modes ,key ,def ,@bindings))

(defmacro e:minor-mode-key-def (modes key def &rest bindings)
  "MODES で指定したマイナーモード用のキーバイドを設定する.
KEY, DEF の組み合わせをを一つのペアとし、BINDINGS をして任意の回数繰り返し指定できる."
  (declare (indent defun))
  `(e:mode-key-def :minor-modes ,modes ,key ,def ,@bindings))

(defmacro e:mode-key-def (mode-key modes key def &rest bindings)
  "MODES で指定したメジャー/マイナーモード用のキーバインドを設定する.
メジャーモード or マイナーモードは MODE-KEY の値で判断する.
KEY, DEF の組み合わせをを一つのペアとし、BINDINGS をして任意の回数繰り返し指定できる."
  (declare (indent defun))
  (let* ((modes (if (listp modes) modes (list modes)))
         (mode (or (car-safe modes) modes))
         (command (intern (format "e:%s-command" mode)))
         (map (intern (format "e:%s-command-map" mode)))
         (prefix-bindings nil)
         (general-bindings nil))
    (while key
      (if (stringp def)
          (setq prefix-bindings (append prefix-bindings (list key def)))
        (setq general-bindings (append general-bindings (list key def))))
      (setq key (pop bindings)
            def (pop bindings)))
    `(progn
       (bind-map ,map
         :prefix-cmd ,command
         ,mode-key ,modes
         :keys ("M-<return>" "M-m m")
         :evil-keys ("," "SPC m")
         :evil-states (motion normal visual))
       (general-def ,map ,@general-bindings)
       ,(when prefix-bindings
          `(which-key-add-keymap-based-replacements ,map
             ,@prefix-bindings)))))

(defmacro e:define-minor-mode-switch (minor-mode)
  "MINOR-MODE の ON/OFF を切り替える関数を定義する."
  `(progn
     (defun ,(intern (format "%s-on" minor-mode)) ()
       ,(format "[generated] Turn on `%s'" minor-mode)
       (interactive)
       (,minor-mode 1))
     (defun ,(intern (format "%s-off" minor-mode)) ()
       ,(format "[generated] Turn off `%s'" minor-mode)
       (interactive)
       (,minor-mode 0))))

(defmacro e:key-def (name prefix &rest body)
  "NAME, PREFIX で指定したキーマップのキーバインドを設定する.
キーバインドの詳細は BODY で指定する."
  (declare (indent defun))
  `(general-def '(motion normal visual insert emacs) 'override
     :prefix ,(s-trim-right (format "SPC %s" (or prefix "")))
     :non-normal-prefix ,(s-trim-right (format "M-m %s" (or prefix "")))
     :prefix-command ',(intern (format "e:%s-command" name))
     :prefix-map ',(intern (format "e:%s-command-map" name))
     ,@body))

(defmacro e:define-vterm-command (name &optional command)
  "COMMAND で指定したコマンドを `vterm` で起動するコマンドを定義する.
COMMAND が指定されていない場合は NAME をそのままコマンドとして使用する."
  `(defun ,name ()
     (interactive)
     (pop-to-buffer-same-window
      (e:vterm-exec ,(format "%s" name) ,(format "%s" (or command name))))))

(defmacro e:load-env-file (path &optional base-path)
  "PATH の .env をバイトコンパイル時に読んで setenv 列に展開する.
BASE-PATH は $PATH 展開時のベースとなるシステム PATH 文字列."
  (let* ((file (expand-file-name path))
         (base-path (or base-path "/usr/local/bin:/usr/bin:/usr/local/sbin"))
         (process-environment
          (cons (concat "PATH=" base-path) process-environment))
         forms has-path)
    (unless (file-readable-p file)
      (warn "e:load-env-file: %s not readable at compile time" file))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (re-search-forward
                "^\\s-*\\([A-Za-z_][A-Za-z0-9_]*\\)=\\(.*\\)$" nil t)
          (let ((key (match-string 1))
                (val (match-string 2)))
            (when (string-match "\\`\\([\"']\\)\\(.*\\)\\1\\'" val)
              (setq val (match-string 2 val)))
            (let ((expanded (substitute-env-vars val)))
              (when (equal key "PATH")
                (setq expanded
                      (mapconcat #'directory-file-name
                                 (delete-dups (parse-colon-path expanded))
                                 ":")))
              (push `(setenv ,key ,expanded) forms))
            (when (equal key "PATH") (setq has-path t)))))
      (when has-path
        (push `(setq exec-path
                     (append (parse-colon-path (getenv "PATH"))
                             (list exec-directory)))
              forms)))
    `(progn ,@(nreverse forms))))

(defmacro e:setup-project-rails-annotation (type dir &optional suffix command)
  "`project-rails-find-{TYPE}' で使用する `marginalia' の設定を用意する.
DIR は文字列または文字列のリスト、SUFFIX はよい感じに設定してください.
COMMAND を指定した場合は `project-rails-find-{TYPE}' の代わりにそのコマンドを対象とする."
  (let ((command   (or command (intern (format "project-rails-find-%s" type))))
        (category  (intern (format "rails-%s-file" type)))
        (annotator (intern (format "marginalia-annotate-rails-%s-file" type))))
    `(progn
       (defun ,annotator (cand)
         (when-let* ((root (marginalia--project-root)))
           (cl-loop for d in (if (listp ,dir) ,dir (list ,dir))
                    for pattern = (format "%s/%s%s*" (f-expand d root) cand ,(or suffix ""))
                    for file = (car (file-expand-wildcards pattern))
                    when file return (marginalia-annotate-file file))))
       (add-to-list 'marginalia-command-categories '(,command . ,category))
       (add-to-list 'marginalia-annotators '(,category ,annotator builtin none)))))

(provide 'nsmacs-macros)
;;; nsmacs-macros.el ends here
