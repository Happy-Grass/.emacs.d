
(add-to-list 'load-path "~/.emacs.d/lisp/")
(setq custom-file "~/.emacs.d/lisp/custom.el")
(require 'ui)
(require 'setting)
(require 'font)
(require 'init-package)
(require 'load-package)
(require 'keybind)
(load custom-file)
;;; init.el ends here
(put 'narrow-to-region 'disabled nil)
