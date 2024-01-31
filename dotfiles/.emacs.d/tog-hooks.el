(add-hook 'c++-mode-hook 'togtabs-infer-indentation-style)
(add-hook 'c-mode-hook 'togtabs-infer-indentation-style)
(add-hook 'text-mode-hook 'tog-tabs-8)
(add-hook 'php-mode-hook 'togtabs-infer-indentation-style)
(add-hook 'js-mode-hook 'togtabs-infer-indentation-style)
(add-hook 'typescript-mode-hook 'togtabs-infer-indentation-style)
(add-hook 'powershell-mode-hook 'togtabs-infer-indentation-style)
(add-hook 'lua-mode-hook 'togtabs-infer-indentation-style)

;; Seems like this should be default!
;; Maybe one of my other scripts messes it up?
;; But this seems to fix the problem:
(defun tog-org-mode-hook ()
  (local-set-key "\t" 'org-cycle)
;; Careful!!  Emacs might think you mean Shift + "r" followed by "e" "t" "u" 'r" "n"!
;; I need to figure out how that all works.
;;(local-set-key "S-RET" 'org-table-copy-down)
  (load "tog-org-stuff")
)
(add-hook 'org-mode-hook 'tog-org-mode-hook)

(defun tog-scad-mode-hook ()
  (togtabs-infer-indentation-style)
  (tog-disable-electric-indent-mode))
(add-hook 'scad-mode-hook 'tog-scad-mode-hook)

(defun tog-cargo-build ()
  (interactive)
  (compile "cargo build"))
(defun tog-cargo-run ()
  (interactive)
  (compile "cargo run"))
(defun tog-cargo-run-default ()
  (interactive)
  (compile "cargo run-default"))
(defun tog-cargo-test ()
  (interactive)
  (compile "cargo test"))

(defun tog-rust-mode-hook ()
  (interactive) ; sometimes it doesn't seem to run?
  (setq indent-tabs-mode nil) ; RUST USES SPACES STOP TURNING THEM INTO TABS
  (local-set-key (kbd "C-c C-c") 'tog-cargo-build)
  (local-set-key (kbd "C-c C-r") 'tog-cargo-run)
  (local-set-key (kbd "C-c r d") 'tog-cargo-run-default)
  (local-set-key (kbd "C-c C-t") 'tog-cargo-test))
(add-hook 'rust-mode-hook 'tog-rust-mode-hook)

(defun tog-nxml-mode-hook ()
  (local-set-key (kbd "C-e") 'end-of-line) ; nxml seems to override 'move-end-of-line' to do something weird
  (if (togtabs-looks-like-indentation-is-tabs)
      (progn
	(setq indent-tabs-mode t)
	(setq tab-width 3)
	(setq nxml-child-indent tab-width))))
(add-hook 'nxml-mode-hook 'tog-nxml-mode-hook)
