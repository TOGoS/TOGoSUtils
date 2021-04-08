;; Assuming you have linked TOGoSUtils/dotfiles/* into your .emacs.d.
;; But actually just load ...path-to-emacs.d/tog-stuff.el, instead.

(load "~/.emacs.d/tog-functions")
(load "~/.emacs.d/tog-hooks")

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
