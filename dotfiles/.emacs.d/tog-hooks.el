(add-hook 'c++-mode-hook 'infer-indentation-style)
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
