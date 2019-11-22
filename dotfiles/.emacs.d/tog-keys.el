; Inserting stuff

(defun insert-name-in-database-template ()
  (interactive)
  (insert "name in database @ \"\"")
  (backward-char))
(global-set-key (kbd "C-x a n") 'insert-name-in-database-template)

(defun insert-picture-files-url-prefix ()
  (interactive)
  (insert "http://picture-files.nuke24.net/uri-res/raw/"))
(global-set-key (kbd "C-x p f") 'insert-picture-files-url-prefix)
(defun insert-factorio-github-commit-prefix ()
  (interactive)
  (insert "https://github.com/wube/Factorio/commit/"))
(global-set-key (kbd "C-x g f c") 'insert-factorio-github-commit-prefix)

(defun insert-github-factorio-commit-url-prefix ()
  (interactive)
  (insert "https://github.com/Wube/Factorio/commit/"))
(global-set-key (kbd "C-x g f c") 'insert-github-factorio-commit-url-prefix)

(defun insert-open-angle-quote () (interactive) (insert "‹"))
(global-set-key (kbd "C-x a ,") 'insert-open-angle-quote)

(defun insert-close-angle-quote () (interactive) (insert "›"))
(global-set-key (kbd "C-x a .") 'insert-close-angle-quote)

(defun insert-open-angle-double-quote () (interactive) (insert "«"))
(global-set-key (kbd "C-x a <") 'insert-open-angle-double-quote)

(defun insert-close-angle-double-quote () (interactive) (insert "»"))
(global-set-key (kbd "C-x a >") 'insert-close-angle-double-quote)

;

(global-set-key (kbd "C-x r e") 'replace-last-sexp)

(global-set-key (kbd "C-x t t t") 'tog-tabs)
(global-set-key (kbd "C-x t t h") 'tog-tabs-harder)

; Visiting common files

(global-set-key (kbd "M-g d t") 'visit-todays-doke-entry)
(global-set-key (kbd "M-g d e") 'visit-doke-entry)
(global-set-key (kbd "M-g e t") 'visit-eit-timelog)
(global-set-key (kbd "M-g j t") 'visit-todays-jht-notes)
