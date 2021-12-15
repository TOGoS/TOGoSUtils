;; org mode stuff?

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)

(defun insert-picture-files-url-prefix ()
  (interactive)
  (insert "http://picture-files.nuke24.net/uri-res/raw/"))
(global-set-key "\C-xpf" 'insert-picture-files-url-prefix)

(setq org-default-notes-file "~/job/EarthIT/timelog/notes.org")
; (setq org-default-notes-file (concat org-directory "/notes.org"))

;; C-c {t|n} to add a todo or a note:

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/job/EarthIT/timelog/tog.todo.org" "To be prioritized")
	 "** %?\n\n%U")
	("n" "Note" entry (file "~/job/EarthIT/timelog/notes.org")
	 "* %?\nEntered on %U from file:%F")))

;; Seems necessary with newer org-mode to include this so that
;; "<e" + TAB etc works:
(if (string-version-lessp "9.2" (org-version))
    (progn
      (require 'org-tempo)
      ;; https://emacs.stackexchange.com/questions/52441/how-to-modify-org-structure-template-alist-after-org-mode-9-2
      ;; Haven'tyet tested with org mode >= 9.2!

      ;; Fix to get old uppercase BEGIN / END blocks back:
      (defun org-tempo-add-block (entry)
	"Add block entry from `org-structure-template-alist'."
	(let* ((key (format "<%s" (car entry)))
	       (name (cdr entry))
	       (special (member name '("src" "export"))))
	  (tempo-define-template (format "org-%s" (replace-regexp-in-string " " "-" name))
				 `(,(format "#+BEGIN_%s%s" name (if special " " ""))
				   ,(when special 'p) '> n ,(unless special 'p) n
				   ,(format "#+END_%s" (car (split-string name " ")))
				   >)
				 key
				 (format "Insert a %s block" name)
				 'org-tempo-tags)))
      (add-to-list 'org-structure-template-alist '("s" . "SRC\n"))
      (add-to-list 'org-structure-template-alist '("q" . "QUOTE\n"))
      (add-to-list 'org-structure-template-alist '("e" . "EXAMPLE\n"))
      ))



;; Maybe this should be set in mode customization somehow?
(setq org-adapt-indentation nil)
