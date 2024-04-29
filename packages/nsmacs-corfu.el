;;; nsmacs-corfu.el --- corfu 関連のコマンド
;;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'corfu)

;; https://github.com/minad/corfu?tab=readme-ov-file#completing-in-the-minibuffer
;;;###autoload
(defun e:corfu-enable-always-in-minibuffer ()
  "Enable Corfu in the minibuffer if Vertico/Mct are not active."
  (unless (or (bound-and-true-p mct--active)
              (bound-and-true-p vertico--input)
              (eq (current-local-map) read-passwd-map))
    ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
    (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                corfu-popupinfo-delay nil)
    (corfu-mode 1)))

(provide 'nsmacs-corfu)
;;; nsmacs-corfu.el ends here
