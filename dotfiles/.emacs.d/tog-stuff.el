(let ((lode (lambda (filename) (load-file (concat (file-name-directory (or load-file-name buffer-file-name)) "/" filename)))))
  (funcall lode "tog-functions.el")
  (funcall lode "tog-hooks.el")
  (funcall lode "tog-org-stuff.el"))


