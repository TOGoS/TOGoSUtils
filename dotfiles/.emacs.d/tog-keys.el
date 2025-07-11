; Inserting stuff

;; > C-x reserved for Emacs native essential keybindings: buffer, window, frame, file, directory, etc...
;; > C-c reserved for user and major mode: C-c letter reserved for user. <F5>-<F9> reserved for user. C-c C-letter reserved for major mode.
;; > Don't rebind C-g , C-h and ESC .
;; -- https://www.google.com/search?client=firefox-b-1-d&q=emacs+key+conventions
;; So I guess these 'C-x ...'s should really be C-cs.

(global-set-key (kbd "C-x a t") 'insert-timestamp)
(global-set-key (kbd "C-x a d") 'insert-date)

(defun insert-name-in-database-template ()
  (interactive)
  (insert "name in database @ \"\"")
  (backward-char))
(global-set-key (kbd "C-x a n") 'insert-name-in-database-template)

(defun insert-picture-files-url-prefix ()
  (interactive)
  (insert "http://picture-files.nuke24.net/uri-res/raw/"))
(global-set-key (kbd "C-x p f") 'insert-picture-files-url-prefix)

(defun insert-wherever-files-url-prefix ()
  (interactive)
  (insert "http://wherever-files.nuke24.net/uri-res/raw/"))
(global-set-key (kbd "C-x w f") 'insert-wherever-files-url-prefix)

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

(global-set-key (kbd "C-x a u") 'tog-insert-new-uuid-urn)

; I like to use em-dashes for quotes, since org-mode things I'm making a list if I just use a hyphen.
(defun insert-emdash () (interactive) (insert "—"))
(global-set-key (kbd "C-x a m") 'insert-emdash)

;

(global-set-key (kbd "C-x r e") 'replace-last-sexp)

; In newer versions of emacs, 'C-x t t' means something else,
; so I can't use it as a prefix.  Oh well.
; (global-set-key (kbd "C-x t t t") 'tog-tabs)
; (global-set-key (kbd "C-x t t h") 'tog-tabs-harder)

; Visiting common files

(global-set-key (kbd "M-g a s") 'visit-archived-stuff)
(global-set-key (kbd "M-g d e") 'visit-doke-entry)
(global-set-key (kbd "M-g d l") 'visit-latest-doke-entry)
(global-set-key (kbd "M-g d t") 'visit-todays-doke-entry)
(global-set-key (kbd "M-g e t") 'visit-eit-timelog)
(global-set-key (kbd "M-g e p") 'visit-tog-weekly-plan)
(global-set-key (kbd "M-g t t") 'visit-tog-tasks)
(global-set-key (kbd "M-g e b") 'visit-bodylog)
(global-set-key (kbd "M-g e h") 'visit-self-help)
(global-set-key (kbd "M-g a l") 'visit-alvin-log)
(global-set-key (kbd "M-g k l") 'visit-kia-log)
(global-set-key (kbd "M-g l n") 'visit-love-notes)
(global-set-key (kbd "M-g l c") 'visit-love-complaints)
(global-set-key (kbd "M-g j t") 'visit-todays-jht-notes)
(global-set-key (kbd "M-g j l") 'visit-latest-jht-notes)
(global-set-key (kbd "M-g j h") 'visit-jht-help)
(global-set-key (kbd "M-g j b") 'visit-todays-jht-bullshit)
(global-set-key (kbd "M-g 9 t") 'visit-4909-transfers)
(global-set-key (kbd "M-g m m") 'visit-togos-music-metadata)
(global-set-key (kbd "M-g m n") 'visit-togos-music-notes)
(global-set-key (kbd "M-g m t") 'visit-togos-music-tasks)
(global-set-key (kbd "M-g s t") 'visit-todays-sa-meeting-notes)
(global-set-key (kbd "M-g s g d") 'visit-synthgen2100-devlog)
(global-set-key (kbd "M-g w p") 'visit-workshop-parts-file)
(global-set-key (kbd "M-g w i a") 'visit-workshop-available-ids-file)
(global-set-key (kbd "M-g 3 p d") 'visit-3d-printing-data)
(global-set-key (kbd "M-g 3 p l") 'visit-3d-printing-log)
(global-set-key (kbd "M-g w o") 'visit-workshop-orders)
(global-set-key (kbd "M-g w r") 'visit-workshop-readme)
(global-set-key (kbd "M-g w s") 'visit-workshop-stained-items-file)
(global-set-key (kbd "M-g w t") 'visit-workshop-tasks-file)
(global-set-key (kbd "M-g j o e") 'visit-joe-log)
