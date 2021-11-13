(setq-default buffer-file-coding-system 'utf-8-unix)
(setq delete-selection-mode t) ; So I can overwrite stuff without polluting the clipboard

(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))

(load "tog-functions")
(load "tog-keys")
(load "tog-hooks")
(load "tog-org-stuff")

(autoload 'typescript-mode "typescript-mode" "Edit TypeScript source" t)
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

(autoload 'powershell-mode "powershell-mode" "Edit PowerShell scripts" t)
(add-to-list 'auto-mode-alist '("\\.ps1\\'" . powershell-mode))

;; In timelog files, color '###...' lines even if part of content.
;; This keyword list is prefixed onto tef-mode's,
;; so we don't need to override.
(setq timelog-mode-comment-regex "^##+\\(?:$\\|\\([^#][^\n]*?\\)?##$\\)")
(setq timelog-mode-extra-font-lock-keywords
      `(
	(,timelog-mode-comment-regex 0 'font-lock-comment-delimiter-face nil t)
	(,timelog-mode-comment-regex 1 'font-lock-comment-face t t) ;; other order without override didn't seem to work.  *shrug*
	))

(define-derived-mode timelog-mode tef-mode "timelog"
  "major mode for editing timelog.txt or similar, based on tef-mode"
  (font-lock-add-keywords nil timelog-mode-extra-font-lock-keywords))

(let ((tef-elisp-dir (find-tog-proj-file "proj/TEF" "src/editor-integration/elisp")))
  (if tef-elisp-dir
      (progn
	(add-to-list 'load-path tef-elisp-dir)
	(autoload 'tef-mode "tef-mode" "Edit TEF files" t)
	(add-to-list 'auto-mode-alist '("\\.tef\\'" . tef-mode))
	(add-to-list 'auto-mode-alist '("timelog\\.txt\\'" . timelog-mode))
	(add-to-list 'auto-mode-alist '("bodylog\\.txt\\'" . tef-mode)))))
