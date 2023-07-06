;;; Package --- Summary
;;; Commentary:
;;; Code:
(defun xfw/org-mode-setup()
  (org-indent-mode)
  ;;(variable-pitch-mode 1)成比例字体，但现在还不太会用，先禁止
  (auto-fill-mode 1)
  (visual-line-mode 1)
  (setq evil-auto-indent nil)
  (setq-local line-spacing 0.45);设置显示行距
)
(setq-default prettify-symbols-alist '(("#+BEGIN_SRC" . "»")
                                       ("#+END_SRC" . "«")
                                       ("#+begin_src" . "»")
                                       ("#+end_src" . "«")
				       ("#+title:" . "♏")
				       ("#+filetags:" . "⚓")
				       ("TODO" . "")
				       ("WAIT" . "")
				       ("NOPE" . "")
				       ("DONE" . "")
				       ("[#A]" . "")
				       ("[#B]" . "")
				       ("[#C]" . "")
				       ("[ ]" . "")
				       ("[X]" . "")
				       ("[-]" . "")
				       ("#+STARTUP:" . "")
				       ("#+RESULTS:" . "")
				       ("#+NAME:" . "")
				       ("#+ROAM_TAGS:" . "")
				       ("#+HTML_HEAD:" . "")
				       ("#+SUBTITLE:" . "")
				       ("#+AUTHOR:" . "")
				       (":Effort:" . "")
				       ("SCHEDULED:" . "")
				       ("DEADLINE:" . "")
                                       (">=" . "≥")
                                       ("=>" . "⇨")))
(setq prettify-symbols-unprettify-at-point 'right-edge)
(setq org-ellipsis "▼"
	org-hide-emphasis-markers t)
(setq org-agenda-files
	'("D:/Gitlocal/Notes/tasks.org"))
(setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
	  (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)"
		    "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))
(add-hook 'org-mode-hook 'prettify-symbols-mode)
(add-hook 'org-mode-hook 'xfw/org-mode-setup)
(provide 'xfw-org)
;;; xfw-org.el ends here
