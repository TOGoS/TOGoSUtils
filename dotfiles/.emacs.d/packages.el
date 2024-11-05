;; Actually loading this stuff up takes a bunch of time and I don't really need to every time I start emacs,
(package-initialize)
(setq package-list '(php-mode markdown-mode paredit rainbow-delimiters))
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
