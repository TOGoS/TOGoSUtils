;; Make UTf-8 the default interpretation of files so that unicode
;; characters don't become strings of garbage upon loading:
(set-language-environment "UTF-8")

(setq-default buffer-file-coding-system 'utf-8-unix)
(setq delete-selection-mode t) ; So I can overwrite stuff without polluting the clipboard
;; Some stuff from https://github.com/susam/emfy/blob/main/.emacs
(column-number-mode)

(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))

(load "togtabs")

(load "tog-functions")
(load "tog-keys")
(load "tog-hooks")

(autoload 'typescript-mode "typescript-mode" "Edit TypeScript source" t)
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

(autoload 'funda-haxe-mode "funda-haxe-mode" "Edit Haxe source" t)
(add-to-list 'auto-mode-alist '("\\.hx\\'" . funda-haxe-mode))

(autoload 'powershell-mode "powershell-mode" "Edit PowerShell scripts" t)
(add-to-list 'auto-mode-alist '("\\.ps1\\'" . powershell-mode))

(autoload 'uuid-urn "uuid" "Prepend a UUID string with 'urn:uuid:'" t)
(autoload 'uuid-4 "uuid" "Generate a version 4 UUID string" t)

(autoload 'unisonlang-mode "unisonlang-mode" "Mode fore editing Unison source code" t)
(add-to-list 'auto-mode-alist '("\\.u\\'" . unisonlang-mode))

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
  (tog-tabs-8)
  (font-lock-add-keywords nil timelog-mode-extra-font-lock-keywords))

(let ((tef-elisp-dir (condition-case err (find-tog-proj-file "proj/TEF" "src/editor-integration/elisp") (error nil))))
  (if tef-elisp-dir
      (add-to-list 'load-path tef-elisp-dir)))

;; Set up TEF mode regardless!
;; Because there's a copy of tef-mode.el here in TOGoSUtils/dotfiles/.emacs.d/
(progn
  (autoload 'tef-mode "tef-mode" "Edit TEF files" t)
  (add-to-list 'auto-mode-alist '("\\.tef\\'" . tef-mode))
  (add-to-list 'auto-mode-alist '("\\.ino\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("music\\.txt\\'" . tef-mode))
  (add-to-list 'auto-mode-alist '("timelog\\.txt\\'" . timelog-mode))
  (add-to-list 'auto-mode-alist '("alvinlog\\.txt\\'" . timelog-mode))
  (add-to-list 'auto-mode-alist '("bodylog\\.txt\\'" . tef-mode)))
