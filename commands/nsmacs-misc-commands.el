;;; nsmacs-misc-commands.el --- ちょっと便利なコマンド
;;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;;###autoload
(defun ad:ignore-errors (fn &rest args)
  "Call FN with ARGS, ignoring errors."
  (ignore-errors (apply fn args)))

;;;###autoload
(defun e:display-feature-load-time ()
  "機能のロードにかかった時間をメッセージに表示する."
  (interactive)
  (message "================================================================
起動    : %s
高優先度: %f seconds
低優先度: %f seconds
================================================================"
           (emacs-init-time)
           (float-time (time-subtract e:high-priority-config-queue-end-time
                                      e:high-priority-config-queue-start-time))
           (float-time (time-subtract e:low-priority-config-queue-end-time
                                      e:low-priority-config-queue-start-time))))

(provide 'nsmacs-misc-commands)
;;; nsmacs-misc-commands.el ends here
