;;; nsmacs-rails-routes.el --- rails-routes 関連のコマンド
;;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'rails-routes)
(require 'komunan-lisp-library)
(require 'marginalia)

;;;###autoload
(defun e:rails-routes--generate-command-result ()
  "Rails のルーティングを `--expand' オプションを使用して取得する."
  (->> (kllib:shell-command-to-list "bundle exec rails routes --expanded")
       (--filter (or (s-starts-with? "--\[ Route " it)
                     (s-starts-with? "Prefix            | " it)
                     (s-starts-with? "Verb              | " it)
                     (s-starts-with? "URI               | " it)
                     (s-starts-with? "Controller#Action | " it)))
       (-partition 5)
       (--map (list (string-trim-left (nth 1 it) "^Prefix            | ")
                    (string-trim-left (nth 2 it) "^Verb              | ")
                    (string-trim-left (nth 3 it) "^URI               | ")
                    (string-trim-left (nth 4 it) "^Controller#Action | ")))))

;;;###autoload
(defun ad:rails-routes--run-command@override ()
  "`rails-routes--run-command' のオーバーライド."
  (message "Fetching routes.  Please wait.")
  (let ((command-result (e:rails-routes--generate-command-result)))
    (rails-routes--set-cache command-result)
    (rails-routes--set-cache-validations t)
    (rails-routes--save-cache)
    command-result))

;;;###autoload
(defun ad:rails-routes-insert@override ()
  "`rails-routes-insert' のオーバーライド."
  (rails-routes--load-cache)
  (let* ((routes (rails-routes--get-routes-cached))
         (selected-value (completing-read "Route: " routes))
         (selected (--find (s-equals? (car it) selected-value) routes))
         (selected-route (nth 2 selected)))
    ;; (when (not (rails-routes--guess-ignore-class)) (insert rails-routes-class-name))
    ;; (rails-routes--insert-value selected)
    (insert selected-value rails-routes-insert-after-path)
    (when (or (string-match-p ":id" selected-route)
              (string-match-p ":[a-zA-Z0-9]+_id" selected-route))
      (progn (insert "()") (backward-char)))))

;;;###autoload
(defun e:setup-marginalia-annotate-rails-routes ()
  "`rails-routes' 用の `marginalia' の設定を有効にする."
  (add-to-list 'marginalia-command-categories '(rails-routes-insert . rails-routes))
  (add-to-list 'marginalia-command-categories '(rails-routes-insert-no-cache . rails-routes))
  (add-to-list 'marginalia-annotator-registry '(rails-routes marginalia-annotate-rails-routes builtin none)))

(defun marginalia-annotate-rails-routes (cand)
  "`rails-routes' 用の `marginalia' の設定.
CAND の詳細は `marginalia を参照."
  (when-let ((route (--find (s-equals? cand (car it)) (rails-routes--get-routes-cached))))
    (marginalia--fields
     ((nth 1 route) :face 'marginalia-type)
     ((nth 2 route) :face 'marginalia-value)
     ((nth 3 route) :face 'marginalia-documentation))))

(provide 'nsmacs-rails-routes)
;;; nsmacs-rails-routes.el ends here
