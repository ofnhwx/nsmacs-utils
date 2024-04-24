;;; nsmacs-layout-commands.el --- レイアウト関連のコマンド
;;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;;###autoload
(defun e:tab-bar-select-tab-1 () "1番目のタブに切り替える." (interactive) (tab-bar-select-tab 1))
;;;###autoload
(defun e:tab-bar-select-tab-2 () "2番目のタブに切り替える." (interactive) (tab-bar-select-tab 2))
;;;###autoload
(defun e:tab-bar-select-tab-3 () "3番目のタブに切り替える." (interactive) (tab-bar-select-tab 3))
;;;###autoload
(defun e:tab-bar-select-tab-4 () "4番目のタブに切り替える." (interactive) (tab-bar-select-tab 4))
;;;###autoload
(defun e:tab-bar-select-tab-5 () "5番目のタブに切り替える." (interactive) (tab-bar-select-tab 5))
;;;###autoload
(defun e:tab-bar-select-tab-6 () "6番目のタブに切り替える." (interactive) (tab-bar-select-tab 6))
;;;###autoload
(defun e:tab-bar-select-tab-7 () "7番目のタブに切り替える." (interactive) (tab-bar-select-tab 7))
;;;###autoload
(defun e:tab-bar-select-tab-8 () "8番目のタブに切り替える." (interactive) (tab-bar-select-tab 8))
;;;###autoload
(defun e:tab-bar-select-tab-9 () "9番目のタブに切り替える." (interactive) (tab-bar-select-tab 9))

;;;###autoload
(defun e:tab-switch-last ()
  "最後に表示していたタブに切り替える."
  (interactive)
  (if-let* ((tab (car (tab-bar--tabs-recent)))
            (name (alist-get 'name tab)))
      (tab-bar-switch-to-tab name)))

(provide 'nsmacs-layout-commands)
;;; nsmacs-layout-commands.el ends here
