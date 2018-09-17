(defun insert-name-in-database-template ()
  (interactive)
  (insert "name in database @ \"\"")
  (backward-char))
(global-set-key (kbd "C-x a n") 'insert-name-in-database-template)

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
(global-set-key (kbd "C-x r e") 'replace-last-sexp)

(defun insert-timestamp ()
  (interactive)
  (insert (format-time-string "%Y-%m-%dT%H:%M:%S%:z")))

(global-set-key (kbd "C-x a t") 'insert-timestamp)

(defun insert-date ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(global-set-key (kbd "C-x a d") 'insert-date)

(defun tog-tabs ()
  (interactive)
  (setq tab-width 3)
  (setq c-basic-offset 3)
  (setq indent-tabs-mode t))

(global-set-key (kbd "C-x t t t") 'tog-tabs)

(defun real-tabs () (interactive) (local-set-key "\t" 'self-insert-command))

(defun tog-tabs-harder ()
  (interactive)
  (tog-tabs)
  (local-set-key "\t" 'self-insert-command))

(global-set-key (kbd "C-x t t h") 'tog-tabs-harder)


(defun infer-indentation-style ()
  ;; if our source file uses tabs, we use tabs, if spaces spaces, and if
  ;; neither, we use the current indent-tabs-mode
  (let ((space-count (how-many "^  " (point-min) (point-max)))
        (tab-count (how-many "^\t" (point-min) (point-max))))
    (if (> space-count tab-count) (setq indent-tabs-mode nil))
    (if (> tab-count space-count) (setq indent-tabs-mode t))))

(if (fboundp 'org-add-link-type)
    (progn
      (org-add-link-type "factorio-git-commit" 'org-factorio-git-commit-open)
      (defun org-factorio-git-commit-open (commit-hash)
	"Visit the commit on GitHub"
	(org-open-link-from-string (concat "https://github.com/Wube/Factorio/commit/" commit-hash)))))


;; Minor mode to help track time while staining
;; pieces of wood so I don't get as many keys stained
(define-minor-mode
  stain-log-mode
  "Stain logging mode

Hit 's' to indicate that you started staining a board.
Hit 'f' to indicate that you finished staining a board (presumably the one you last started).
Hit 'w' when you start wiping the stain off a board (presumably the first unsiped one).
Hit 'c' to 'cancel' the last thing by inserting an \"oops!\",
which will remind you to sort things out later after you clean off your hands.
"
  :init-value nil
  :lighter " Stain Log [s,f,c]"
  :keymap '(
	    ("s" . (lambda () (interactive)
		     (insert (format-time-string "%Y-%m-%dT%H:%M"))))
	    ("f" . (lambda () (interactive)
		     (insert "-" (format-time-string "%H:%M\n"))))
	    ("c" . (lambda () (interactive)
		     (insert "Oops!\n")))
	    ("w" . (lambda () (interactive)
		     (insert (format-time-string "%Y-%m-%dT%H:%M") " - wiped\n")))
	    ("q" . (lambda () (interactive)
		     (insert (format-time-string "%Y-%m-%dT%H:%M") " - end stain log\n")
		     (stain-log-mode 'toggle)))
	    ))
