;;; nsmacs-deferred.el --- 遅延設定ロード機構
;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; パッケージ設定を優先度付きで遅延ロードする仕組みを提供します。

;;; Code:

(require 'cl-lib)

;; キュー設定を構造体で管理
(cl-defstruct e:deferred-queue
  name           ; キュー名（表示用）
  items          ; 実行する関数のリスト
  initial-delay  ; 初回遅延時間
  start-time     ; 開始時刻
  end-time)      ; 終了時刻

;; キューの定義
(defvar e:high-priority-queue
  (make-e:deferred-queue
   :name "High priority"
   :items nil
   :initial-delay 0)
  "高優先度の遅延設定キュー.")

(defvar e:low-priority-queue
  (make-e:deferred-queue
   :name "Low priority"
   :items nil
   :initial-delay 0.1)
  "低優先度の遅延設定キュー.")

(defvar e:deferred-config-loaded nil
  "起動時の設定を実行したかを保存しておく.")

(defun e:emacs-uptime ()
  "Emacs の起動からの時間を取得する."
  (float-time (time-since before-init-time)))

(defun e:debug-log (fmt &rest args)
  "FMT と ARGS を使ってメッセージを表示する."
  (when init-file-debug
    (apply #'message (concat "[%f] " fmt) (e:emacs-uptime) args)))

(defun e:process-config-queue--process-item (queue)
  "QUEUE から1つの設定を取り出して実行する."
  (let ((inhibit-message t)
        (name (e:deferred-queue-name queue)))
    (if (e:deferred-queue-items queue)
        (let ((func (pop (e:deferred-queue-items queue)))
              (time (current-time)))
          (condition-case err
              (funcall func)
            (error
             (message "[ERROR] Failed to load config %s: %s" func err)))
          (let ((elapsed (float-time (time-subtract (current-time) time))))
            (when (or (> elapsed 0.01) init-file-debug)
              (message "[%f] %s config: %s (%f)"
                       (e:emacs-uptime) name func elapsed)))
          (run-with-timer 0 nil #'e:process-config-queue--process-item queue))
      (setf (e:deferred-queue-end-time queue) (current-time))
      (message "[%f] %s config queue finished." (e:emacs-uptime) name))))

(defun e:process-config-queue (queue)
  "QUEUE の設定を実行する処理."
  (let ((name (e:deferred-queue-name queue)))
    (setf (e:deferred-queue-start-time queue) (current-time))
    (message "[%f] %s config queue started." (e:emacs-uptime) name)
    (e:process-config-queue--process-item queue)))

;;;###autoload
(defun e:process-high-priority-config-queue ()
  "高優先度の設定を実行する."
  (run-with-timer (e:deferred-queue-initial-delay e:high-priority-queue)
                  nil #'e:process-config-queue e:high-priority-queue))

;;;###autoload
(defun e:process-low-priority-config-queue ()
  "低優先度の設定を実行する."
  (run-with-timer (e:deferred-queue-initial-delay e:low-priority-queue)
                  nil #'e:process-config-queue e:low-priority-queue))

;;;###autoload
(defun e:deferred-config-stats ()
  "遅延設定のロード統計を表示する."
  (interactive)
  (let ((high-time (when (e:deferred-queue-end-time e:high-priority-queue)
                     (float-time
                      (time-subtract (e:deferred-queue-end-time e:high-priority-queue)
                                     (e:deferred-queue-start-time e:high-priority-queue)))))
        (low-time (when (e:deferred-queue-end-time e:low-priority-queue)
                    (float-time
                     (time-subtract (e:deferred-queue-end-time e:low-priority-queue)
                                    (e:deferred-queue-start-time e:low-priority-queue))))))
    (message "Emacs init: %s, High priority: %.3fs, Low priority: %.3fs"
             (emacs-init-time) (or high-time 0) (or low-time 0))))

;;;###autoload
(defmacro e:deferred-config! (package &rest body)
  "PACKAGE の設定(BODY)を遅延評価する.
BODYは leaf マクロに渡される.
:priority キーワードで :high または :low (デフォルト) を指定可能."
  (declare (indent defun))
  (let* ((priority (or (plist-get body :priority) :low))
         (queue (if (eq priority :high)
                    'e:high-priority-queue
                  'e:low-priority-queue))
         (fn-name (intern (format "e:package-config-%s!" package))))
    `(let ((fn (defun ,fn-name () (leaf ,package ,@body))))
       (when init-file-debug
         (message "[%f] Queued config: %s" (e:emacs-uptime) ',package))
       (if e:deferred-config-loaded
           (funcall fn)
         (setf (e:deferred-queue-items ,queue)
               (nconc (e:deferred-queue-items ,queue) (list fn)))))))

(provide 'nsmacs-deferred)
;;; nsmacs-deferred.el ends here
