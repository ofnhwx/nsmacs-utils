;;; nsmacs-macros.el --- マクロ集
;;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 's)
(require 'general)
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

(defvar e:high-priority-config-queue nil
  "起動時に優先度高めで実行する設定.")
(defvar e:low-priority-config-queue nil
  "起動時に優先度低めで実行する設定.")
(defvar e:high-priority-config-queue-timer nil
  "起動時に優先度高めで実行する設定の制御用タイマー.")
(defvar e:low-priority-config-queue-timer nil
  "起動時に優先度低めで実行する設定の制御用タイマー.")
(defvar e:deferred-config-loaded nil
  "起動時の設定を実行したかを保存しておく.")

(defmacro e:deferred-config! (package &rest body)
  "PACKAGE の設定(BODY)を遅延評価する."
  (declare (indent defun))
  (let* ((priority (or (plist-get body :priority) :low))
         (queue (if (eq priority :high) 'e:high-priority-config-queue 'e:low-priority-config-queue))
         (fn-name (intern (format "e:package-config-%s!" package))))
    `(let ((fn (defun ,fn-name () (leaf ,package ,@body))))
       (if e:deferred-config-loaded
           (funcall fn)
         (setq ,queue (append ,queue (list fn)))))))

;;;###autoload
(defun e:process-high-priority-config-queue ()
  "高優先度の設定を実行する."
  (setq e:high-priority-config-queue-timer
        (run-with-timer
         0.1 0.001
         (lambda ()
           (if e:high-priority-config-queue
               (let ((inhibit-message t))
                 (funcall (pop e:high-priority-config-queue)))
             (cancel-timer e:high-priority-config-queue-timer))))))

;;;###autoload
(defun e:process-low-priority-config-queue ()
  "低優先度の設定を実行する."
  (setq e:low-priority-config-queue-timer
        (run-with-timer
         0.5 0.005
         (lambda ()
           (if e:low-priority-config-queue
               (let ((inhibit-message t))
                 (funcall (pop e:low-priority-config-queue)))
             (cancel-timer e:low-priority-config-queue-timer))))))

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

(defmacro e:setup-projectile-rails-annotation (type dir &optional suffix)
  "`projectile-rails-find-{TYPE}' で使用する `marginalia' の設定を用意する.
DIR, SUFFIX はよい感じに設定してください."
  (let ((command   (intern (format "projectile-rails-find-%s" type)))
        (category  (intern (format "rails-%s-file" type)))
        (annotator (intern (format "marginalia-annotate-rails-%s-file" type))))
    `(progn
       (defun ,annotator (cand)
         (when-let* ((root    (marginalia--project-root))
                     (dir     (f-expand ,dir root))
                     (pattern (format "%s/%s%s.*" dir cand ,(or suffix "")))
                     (file    (car (file-expand-wildcards pattern))))
           (marginalia-annotate-file file)))
       (add-to-list 'marginalia-command-categories '(,command . ,category))
       (add-to-list 'marginalia-annotator-registry '(,category ,annotator builtin none)))))

(provide 'nsmacs-macros)
;;; nsmacs-macros.el ends here
