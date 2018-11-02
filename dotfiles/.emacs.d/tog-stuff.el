(setq default-buffer-file-coding-system 'utf-8-unix)

(let ((lode (lambda (filename) (load-file (concat (file-name-directory (or load-file-name buffer-file-name)) "/" filename)))))
  (funcall lode "tog-functions.el")
  (funcall lode "tog-hooks.el")
  (funcall lode "tog-org-stuff.el"))


