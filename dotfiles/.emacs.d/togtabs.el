(setq togtabs-default-tab-width 3)

(setq togtabs-indent-level-variables '(
	c-basic-offset ;; used by several modes other than C[++], including php-mode
	js-indent-level
	typescript-indent-level
	powershell-indent
	nxml-child-indent
))

(defun togtabs-enable (&optional width)
  (interactive)
  (or width (setq width togtabs-default-tab-width))
  (dolist
    (indent-level-var (cons 'tab-width togtabs-indent-level-variables))
    (set indent-level-var width))
  (setq indent-tabs-mode t))

(defun togtabs-looks-like-indentation-is-tabs ()
  (let ((space-count (how-many "^  " (point-min) (point-max)))
        (tab-count (how-many "^\t" (point-min) (point-max))))
    (>= tab-count space-count))) ;; bias towards using tabs kekeke

(defun togtabs-infer-indentation-style ()
  ;; if our source file uses tabs, switch to tog-tabs.
  ;; Otherwise turn indent-tabs-mode off.
  (if (togtabs-looks-like-indentation-is-tabs)
      (togtabs-enable)
    (setq indent-tabs-mode nil)))

;; Add hooks for modes as needed, e.g.:
;;
;;  (add-hook 'c++-mode-hook 'togtabs-infer-indentation-style)
;;  (add-hook 'c-mode-hook 'togtabs-infer-indentation-style)
;;  (add-hook 'text-mode-hook 'tog-tabs-8)
;;  (add-hook 'php-mode-hook 'togtabs-infer-indentation-style)
;;  (add-hook 'js-mode-hook 'togtabs-infer-indentation-style)
;;  (add-hook 'typescript-mode-hook 'togtabs-infer-indentation-style)
;;  (add-hook 'powershell-mode-hook 'togtabs-infer-indentation-style)
