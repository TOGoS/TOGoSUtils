(defun replace-last-sexp ()
  (interactive)
  (let ((value (eval (preceding-sexp))))
    (kill-sexp -1)
    (insert (format "%S" value))))

(defun insert-timestamp ()
  (interactive)
  (insert (format-time-string "%Y-%m-%dT%H:%M:%S%:z")))

(defun insert-date ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun tog-tabs ()
  (interactive)
  (setq tab-width 3)
  (setq c-basic-offset tab-width)
  (setq js-indent-level tab-width)
  (setq nxml-child-indent tab-width)
  (setq indent-tabs-mode t))

(defun tog-tabs-8 ()
  (interactive)
  (setq tab-width 8)
  (setq indent-tabs-mode t)
  (local-set-key "\t" 'self-insert-command))

(defun real-tabs () (interactive) (local-set-key "\t" 'self-insert-command))

(defun tog-tabs-harder ()
  (interactive)
  (tog-tabs)
  (local-set-key "\t" 'self-insert-command))

(defun looks-like-indentation-is-tabs ()
  (let ((space-count (how-many "^  " (point-min) (point-max)))
        (tab-count (how-many "^\t" (point-min) (point-max))))
    (> tab-count space-count)))

(defun infer-indentation-style ()
  ;; if our source file uses tabs, we use tabs, if spaces spaces, and if
  ;; neither, we use the current indent-tabs-mode
  (if (looks-like-indentation-is-tabs)
      (tog-tabs)
    (setq indent-tabs-mode nil)))

;; Me learning elisp

(defun tog-parse-query-string-parts-to-alist (qs-parts)
  (if (eq 0 (length qs-parts)) '()
    (let ((part (car qs-parts)) (remaining-parts (cdr qs-parts)))
      (let ((asploded (split-string part "=")))
	(cons (cons (intern (car asploded)) (cadr asploded))
	      (tog-parse-query-string-parts-to-alist remaining-parts))))))

(defun tog-parse-query-string-to-alist (qs)
  (if (or (eq nil qs) (string= "" qs))
      '()
    (tog-parse-query-string-parts-to-alist (split-string qs "&"))))

;(tog-parse-query-string-to-alist "foo=bar&baz=quux")

(defun tog-parse-x-git-commit-url-body (urlbody)
  (let ((splitq (split-string urlbody "?")))
    (cons (cons 'commit-id (car splitq)) (tog-parse-query-string-to-alist (cadr splitq)))))

;; Generate a www link, like for a person to look at
(defun tog-generate-git-commit-web-link (commit-id repository-url)
  (let ((repository-url (if (eq repository-url nil) "" repository-url)))
    (if (string-match "^\\(https?://github\\.com/.*?\\)\\(?:\\.git\\)?$" repository-url)
	(concat (match-string 1 repository-url) "/commit/" commit-id)
      (concat "http://wherever-files.nuke24.net/uri-res/brows/x-git-commit:" commit-id))))


;; "Warning: Don't use this function in Lisp programs!"
;; -- https://www.gnu.org/software/emacs/manual/html_node/elisp/Buffer-End-Motion.html, regarding end-of-buffer
;; So I'll write my own:
(defun goto-end-of-buffer ()
  (goto-char (point-max)))


;; Functions to find and visit files

(defun first-matching (things matcher)
  (let ((result nil))
    (progn
      (while (and things (not result))
	(let ((thing (car things)))
	  (setq result (funcall matcher thing)))
	(setq things (cdr things)))
      result)))

(defun latest-file-in (dirs filter)
  (if (eq '() dirs)
      nil
    (first-matching dirs
		    (lambda (dir)
		      (if (not (file-directory-p dir))
			  (if (funcall filter dir) dir)
			(let ((sorted-subdirs (mapcar (lambda (filename) (concat dir "/" filename))
						      (let ((filenames (directory-files dir)))
							(sort (seq-filter (lambda (a) (not (= ?. (string-to-char a)))) filenames)
							      (lambda (a b) (string> a b)))))))
			  (latest-file-in sorted-subdirs filter)))))))

(defun find-tog-proj-dir-in (projname stuffdirlist)
  (if stuffdirlist
      (let ((potentialprojdir (concat (car stuffdirlist) "/" projname)))
	(if (file-directory-p potentialprojdir) potentialprojdir
	  (find-tog-proj-dir-in projname (cdr stuffdirlist))))
    (error (concat "Couln't find proj dir: " projname))))

(defun find-tog-proj-dir (projname)
  (find-tog-proj-dir-in projname
			(list (getenv "HOME")
			      (getenv "USERPROFILE")
			      (concat (getenv "USERPROFILE") "/stuff"))))

(defun find-tog-proj-file (projname file)
  (let ((dir (find-tog-proj-dir projname)))
    (and dir (concat dir "/" file))))

(defun find-eit-timelog-file ()
  (latest-file-in (list (find-tog-proj-dir "job/EarthIT/timelog"))
		  (lambda (f) (string-match "-timelog\\.txt$" f))))

(defun find-todays-jht-notes-file ()
  (let ((jhtnotesdir (find-tog-proj-dir "job/JHT/notes"))
	(datestr (format-time-string "%Y/%m/%Y%m%d")))
    (if jhtnotesdir (concat jhtnotesdir "/" datestr "-jht-notes.org") nil)))

(defun find-latest-jht-notes-file ()
  (latest-file-in (list (find-tog-proj-dir "job/JHT/notes"))
		  (lambda (f) (string-match "-jht-notes\\.org$" f))))

(defun find-latest-doke-file ()
  (latest-file-in (list (find-tog-proj-dir "docs/doke/entries"))
		  ; Only look at year-based for lack of a natural string comparison function
		  (lambda (f) (string-match "^[0-9]\\{4\\}" (file-name-nondirectory f)))))

(defun visit-tog-proj-file (projname file)
  (interactive "MProject name:\nMFile:")
  (let ((fullpath (find-tog-proj-file projname file)))
    (if fullpath (find-file fullpath)
      (error (concat "Could not find project '" projname "'")))))

(defun visit-doke-entry (entry-id)
  (interactive "MVisit Doke entry with ID:")
  (visit-tog-proj-file "docs/doke" (concat "entries/" entry-id)))

(defun visit-todays-doke-entry ()
  (interactive)
  (visit-doke-entry (format-time-string "%Y-%m-%d"))
  (if (= (buffer-size) 0)
      (progn
	(insert "date: " (format-time-string "%Y-%m-%d") "\nstatus: draft\n\n"))))
(defun visit-latest-doke-entry ()
  (interactive)
  (find-file (find-latest-doke-file)))

(defun visit-4909-transfers ()
  (interactive)
  (visit-tog-proj-file "docs/financial/4909" "transfers.txt"))

(defun visit-togos-music-metadata ()
  (interactive)
  (visit-tog-proj-file "music/TOGoSMusicMetadata" "music.txt"))
(defun visit-togos-music-notes ()
  (interactive)
  (visit-tog-proj-file "music/TOGoSMusicMetadata" "notes.tef"))
(defun visit-togos-music-todo ()
  (interactive)
  (visit-tog-proj-file "music/TOGoSMusicMetadata" "todo.org"))

(defun visit-eit-timelog ()
  (interactive)
  (find-file (find-eit-timelog-file))
  (goto-end-of-buffer)
  (search-backward-regexp "^= "))
(defun visit-tog-todo ()
  (interactive)
  (find-file (find-tog-proj-file "job/EarthIT/timelog" "tog.todo.org")))
(defun visit-bodylog ()
  (interactive)
  (find-file (find-tog-proj-file "job/EarthIT/timelog" "bodylog.txt"))
  (goto-end-of-buffer)
  (call-interactively 'eval-last-sexp)
  (search-backward-regexp "^= "))
(defun visit-love-notes ()
  (interactive)
  (find-file (find-tog-proj-file "docs/LoveNotes" "2019/20191031-poly-thoughts.org"))
  (goto-end-of-buffer)
  (search-backward-regexp "^\*\* "))
(defun visit-workshop-available-ids-file ()
  (interactive)
  (find-file (find-tog-proj-file "docs/ProjectNotes2" "available-ids.org")))
(defun visit-workshop-parts-file ()
  (interactive)
  (find-file (find-tog-proj-file "docs/ProjectNotes2" "2019/Parts.tef"))
  (goto-end-of-buffer)
  (search-backward-regexp "^="))
(defun visit-workshop-stained-items-file ()
  (interactive)
  (find-file (find-tog-proj-file "docs/ProjectNotes2" "2018/StainTest/StainedItems.tef"))
  (goto-end-of-buffer)
  (search-backward-regexp "^="))

(defun visit-jht-help ()
  (interactive)
  (find-file (find-tog-proj-file "job/JHT/notes" "help.org")))
(defun visit-todays-jht-bullshit ()
  (interactive)
  (find-file (find-tog-proj-file "job/JHT/notes" (format-time-string "%Y/%m/%Y%m-bullshit.org")))
  (if (= (buffer-size) 0)
      (progn
	(insert "#TITLE: " (format-time-string "%Y-%m-%d") " JHT Bullshit\n\n** Any vaguely notable contributions")
	(goto-end-of-buffer))))
(defun visit-todays-jht-notes ()
  (interactive)
  (find-file (find-todays-jht-notes-file))
  (if (= (buffer-size) 0)
      (progn
	(insert "#TITLE: " (format-time-string "%Y-%m-%d") " JHT Notes\n\n")
	(goto-end-of-buffer))))
(defun visit-latest-jht-notes ()
  (interactive)
  (find-file (find-latest-jht-notes-file)))

(defun visit-todays-sa-meeting-notes ()
  (interactive)
  (find-file (find-tog-proj-file "job/SA" (format-time-string "notes/%Y/%m/%Y%m%d-sa-meeting-notes.org")))
  (if (= (buffer-size) 0)
      (progn
	(insert "#TITLE: " (format-time-string "%Y-%m-%d") " SA meeting notes\n\n")
	(goto-end-of-buffer))))

(defun visit-synthgen2100-devlog ()
  (interactive)
  (find-file (find-tog-proj-file "proj/SynthGen2100" "P0011/DEVLOG.org"))
  (goto-end-of-buffer))

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
