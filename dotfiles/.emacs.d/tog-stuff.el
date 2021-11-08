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
