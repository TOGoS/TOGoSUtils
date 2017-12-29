(defun insert-name-in-database-template ()
  (interactive)
  (insert "name in database @ \"\"")
  (backward-char))
(global-set-key (kbd "C-x a d") 'insert-name-in-database-template)

(defun insert-open-angle-quote () (interactive) (insert "‹"))
(global-set-key (kbd "C-x a ,") 'insert-open-angle-quote)

(defun insert-close-angle-quote () (interactive) (insert "›"))
(global-set-key (kbd "C-x a .") 'insert-close-angle-quote)

(defun insert-open-angle-double-quote () (interactive) (insert "«"))
(global-set-key (kbd "C-x a <") 'insert-open-angle-double-quote)

(defun insert-close-angle-double-quote () (interactive) (insert "»"))
(global-set-key (kbd "C-x a >") 'insert-close-angle-double-quote)

(defun replace-last-sexp ()
  (interactive)
  (let ((value (eval (preceding-sexp))))
    (kill-sexp -1)
    (insert (format "%S" value))))

(defun insert-timestamp ()
  (interactive)
  (insert (shell-command-to-string "echo -n $(date +'%Y-%m-%dT%H:%M:%S%:z')")))

(defun insert-date ()
  (interactive)
  (insert (shell-command-to-string "echo -n $(date +'%Y-%m-%d')")))

(defun tog-tabs ()
  (interactive)
  (setq tab-width 3)
  (setq c-basic-offset 3)
  (setq indent-tabs-mode t))

(defun tog-tabs-harder ()
  (interactive)
  (tog-tabs)
  (local-set-key "\t" 'self-insert-command))

(defun infer-indentation-style ()
  ;; if our source file uses tabs, we use tabs, if spaces spaces, and if
  ;; neither, we use the current indent-tabs-mode
  (let ((space-count (how-many "^  " (point-min) (point-max)))
        (tab-count (how-many "^\t" (point-min) (point-max))))
    (if (> space-count tab-count) (setq indent-tabs-mode nil))
    (if (> tab-count space-count) (setq indent-tabs-mode t))))
