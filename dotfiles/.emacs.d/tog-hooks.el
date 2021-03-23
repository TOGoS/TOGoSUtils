(add-hook 'c++-mode-hook 'infer-indentation-style)
(add-hook 'c-mode-hook 'infer-indentation-style)
(add-hook 'text-mode-hook 'tog-tabs-8)

;; Seems like this should be default!
;; Maybe one of my other scripts messes it up?
;; But this seems to fix the problem:
(defun tog-org-mode-hook ()
  (local-set-key "\t" 'org-cycle)
;; Careful!!  Emacs might think you mean Shift + "r" followed by "e" "t" "u" 'r" "n"!
;; I need to figure out how that all works.
;  (local-set-key "S-RET" 'org-table-copy-down)
  )
(add-hook 'org-mode-hook 'tog-org-mode-hook)

(defun tog-php-mode-hook ()
  (infer-indentation-style))
(add-hook 'php-mode-hook 'tog-php-mode-hook)

(defun tog-js-mode-hook ()
  (infer-indentation-style))
(add-hook 'js-mode-hook 'tog-js-mode-hook)

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
