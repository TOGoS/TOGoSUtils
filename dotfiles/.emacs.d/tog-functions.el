(defun insert-name-in-database-template ()
  (interactive)
  (insert "name in database @ \"\"")
  (backward-char))
(global-set-key (kbd "C-x a n") 'insert-name-in-database-template)

(defun insert-picture-files-url-prefix ()
  (interactive)
  (insert "http://picture-files.nuke24.net/uri-res/raw/"))
(global-set-key (kbd "C-x p f") 'insert-picture-files-url-prefix)

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

;; Me learning elisp

(defun tog-parse-query-string-parts-to-alist (qs-parts)
  (if (eq 0 (length qs-parts)) '()
    (let ((part (car qs-parts)) (remaining-parts (cdr qs-parts)))
      (let ((asploded (split-string part "=")))
	(cons (cons (intern (car asploded)) (cadr asploded))
	      (tog-parse-query-string-parts-to-alist remaining-parts))))))

(tog-parse-query-string-to-alist "foo=bar&baz=quux")

(defun tog-parse-query-string-to-alist (qs)
  (if (or (eq nil qs) (string= "" qs))
      '()
    (tog-parse-query-string-parts-to-alist (split-string qs "&"))))

(defun tog-parse-x-git-commit-url-body (urlbody)
  (let ((splitq (split-string urlbody "?")))
    (cons (cons 'commit-id (car splitq)) (tog-parse-query-string-to-alist (cadr splitq)))))

;; Generate a www link, like for a person to look at
(defun tog-generate-git-commit-web-link (commit-id repository-url)
  (let ((repository-url (if (eq repository-url nil) "" repository-url)))
    (if (string-match "^\\(https?://github\\.com/.*?\\)\\(?:\\.git\\)?$" repository-url)
	(concat (match-string 1 repository-url) "/commit/" commit-id)
      (concat "http://wherever-files.nuke24.net/uri-res/brows/x-git-commit:" commit-id))))

;(tog-parse-x-git-commit-url-body "asdf?repository=Hello")
;(tog-parse-x-git-commit-url-body "asdf?no-repo=Hello")
;(tog-generate-git-commit-web-link
; "90a3ecbecb2a6a403c2b2439ea31fe4657fd1ceb"
; "https://github.com/Wube/Factorio.git")

(if (fboundp 'org-add-link-type)
    (progn
      (org-add-link-type "factorio-git-commit" 'org-factorio-git-commit-open)
      (defun org-factorio-git-commit-open (commit-hash)
	"Visit the commit on GitHub"
	(org-open-link-from-string
	 (tog-generate-git-commit-web-link
	  commit-hash
	  "https://github.com/Wube/Factorio")))
      (org-add-link-type "x-git-commit" 'org-x-git-commit-open)
      (defun org-x-git-commit-open (linkbody)
	"Visit the commit in some web interface"
	(org-open-link-from-string
	 (let ((link-info (tog-parse-x-git-commit-url-body linkbody)))
	   (tog-generate-git-commit-web-link
	    (alist-get 'commit-id link-info)
	    (alist-get 'repository link-info)))))
      ))

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
