(add-hook 'c++-mode-hook 'infer-indentation-style)
(add-hook 'text-mode-hook 'tog-tabs-8)

;; Seems like this should be default!
;; Maybe one of my other scripts messes it up?
;; But this seems to fix the problem:
(defun tog-org-mode-hook ()
  (local-set-key "\t" 'org-cycle))
(add-hook 'org-mode-hook 'tog-org-mode-hook)
