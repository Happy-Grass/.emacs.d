;;; Package --- Summary
;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'use-package))
(use-package benchmark-init
  :ensure t
  :defer t
  :init
  (benchmark-init/activate)
  :hook
  (after-init . benchmark-init/deactivate))

(use-package rime ;; 输入法
  :ensure t
  :defer t
  :init
  (setq default-input-method "rime"
        rime-show-candidate 'posframe
	rime-title "ℋ")
  :config
  (setq rime-user-data-dir "~/.emacs.d/rime/"))

(use-package evil ;; vim模式
  :ensure t
  :init (evil-mode)
  :config
  (evil-define-key 'normal global-map "s" 'avy-goto-char)
)

(use-package general ;; leader键配置
  :ensure t
  :demand t
  :init
  (defvar global-leader ",")
  :config
  (general-create-definer global-leader-def
    :prefix global-leader)
  )

(use-package hydra
  :hook (emacs-lisp-mode . hydra-add-imenu))


(use-package major-mode-hydra
  :ensure t
  :demand t
  :bind
  ("<M-SPC>" . major-mode-hydra)
  ;:config
  ;(leader-def
    ;:keymaps 'normal
    ;"m" 'major-mode-hydra)
  )

;;需要处理一下加载顺序，必须先加载major-mode-hydra，有报错，暂时不管吧
;;这是为主模式配置单独按键的例子，还有一个问题是major-mode-hydra的键位绑定问题
(use-package emacs-lisp-mode
  :ensure nil
  :mode "\\.el\\'"
  :mode-hydra
  (emacs-lisp-mode
   (:title "Elisp Commands")
   ("Eval"
    (("b" eval-buffer "buffer")
     ("e" eval-defun "defun")
     ("r" eval-region "region" :color green))
    "REPL"
    (("I" ielm "ielm"))
    "Test"
    (("t" ert "prompt")
     ("T" (ert t) "all")
     ("F" (ert :failed) "failed"))
    "Doc"
    (("d" describe-foo-at-point "thing-at-pt")
     ("f" describe-function "function")
     ("v" describe-variable "variable")
     ("i" info-lookup-symbol "info lookup")))
   ))
;; pretty-hydra的用法
(pretty-hydra-define xfw-toggles
  (:title "Toggles" :color amaranth :quit-key "q" )
  ("Basic"
   (("n" display-line-numbers-mode "line number" :toggle t)
    ("w" whitespace-mode "whitespace" :toggle t)
    ("W" whitespace-cleanup-mode "whitespace cleanup" :toggle t)
    ("r" rainbow-mode "rainbow" :toggle t)
    ("L" page-break-lines-mode "page break lines" :toggle t))
   "Highlight"
   (("s" symbol-overlay-mode "symbol" :toggle t)
    ("l" hl-line-mode "line" :toggle t)
    ("x" highlight-sexp-mode "sexp" :toggle t)
    ("t" hl-todo-mode "todo" :toggle t))
   "UI"
   (("d" jp-themes-toggle-light-dark "dark theme" :toggle xfw-current-theme-dark-p))
   "Coding"
   (("p" smartparens-mode "smartparens" :toggle t)
    ("P" smartparens-strict-mode "smartparens strict" :toggle t)
    ("S" show-smartparens-mode "show smartparens" :toggle t)
    ("f" flycheck-mode "flycheck" :toggle t))
   "Emacs"
   (("D" toggle-debug-on-error "debug on error" :toggle (default-value 'debug-on-error))
    ("X" toggle-debug-on-quit "debug on quit" :toggle (default-value 'debug-on-quit)))))
(global-set-key (kbd "<f4>") 'xfw-toggles/body)

;(use-package hydra-posframe
  ;:el-get "Ladicle/hydra-posframe"
  ;:hook (after-init . hydra-posframe-mode))

(use-package rainbow-mode ;; 颜色显示
  :ensure t
  :defer t
  :hook (progmode . rainbow-mode))

(use-package good-scroll
  :ensure t
  :defer t
  :if window-system ;;在图形化界面时才使用这个插件
  :init (good-scroll-mode))

(use-package smartparens ;括号配对
  :ensure t
  :defer t
  :hook
  (prog-mode . smartparens-mode)
  )

(use-package rainbow-delimiters ;;括号匹配彩色
  :ensure t
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package counsel
  :defer t
  :ensure t)

(use-package swiper
  :defer t
  :ensure t)

(use-package ivy
  :ensure t
  :defer t
  :init
  (ivy-mode 1)
  (counsel-mode 1)
  :config
  (setq ivy-initial-inputs-alist nil
	ivy-use-virtual-buffers t
	search-default-mode #'char-fold-to-regexp
	ivy-count-format "(%d/%d) ")
  :bind
  (("C-s" . 'swiper)
   ("C-x b" . 'ivy-switch-buffer))
  )

(use-package undo-tree
  :ensure t
  :defer t
  :init (global-undo-tree-mode)
  :custom
  (undo-tree-auto-save-history nil))

(use-package avy ;类似于easymotion
  :defer t
  :ensure t)

;;;让avy和ivy支持拼音搜索
(use-package ace-pinyin
  :after avy
  :ensure t
  :config
  (ace-pinyin-global-mode +1))

(use-package company
  :defer t
  :ensure t
  :hook (prog-mode . company-mode)
  :config
  (setq company-minimum-prefix-length 1 ; 只需敲 1 个字母就开始进行自动补全
	company-tooltip-align-annotations t
	company-idle-delay 0.0
	company-show-quick-access t ;; 给选项编号 (按快捷键 M-1、M-2 等等来进行选择).
	company-selection-wrap-around t
	company-tooltip-limit 7
	company-tooltip-minimum 4; 当底下行数小于4时显示在上方
	company-tooltip-offset-display 'lines
	company-icon-size '(auto-scale . 20)
	company-icon-margin 3
	company-transformers '(company-sort-by-occurrence)) ; 根据选择的频率进行排序
)

(use-package flycheck;;语法检查工具
  :ensure t
  :defer t
  :config
  (setq truncate-lines nil) ; 如果单行信息很长会自动换行
  :hook
  (prog-mode . flycheck-mode))

(use-package magit
  :ensure t
  :defer t)

(use-package projectile
  :ensure t
  :init
  (projectile-mode t)
  :config
  (setq projectile-indexing-method 'native
	projectile-sort-order 'modification-time ;按最近修改时间排序
	projectile-completion-system 'ivy)
  )

(use-package ace-window
  :ensure t
  :defer t
  :bind (("C-x o" . 'ace-window)))

(use-package org
  :ensure t
  :defer t
  :config
  (require 'xfw-org)
  )

(use-package org-bullets
  :ensure t
  :defer t
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("󰟒" "󱐡" "󱤅" "󱤅" "󱠆" )))

(use-package org-roam
  :ensure t
  :defer t
  :custom
  (org-roam-directory (file-truename "D:/Gitlocal/Notes/roam/"))
  (org-roam-dailies-directory "daily/")
  :config
  (require 'xfw-orgroam)
  (setq org-roam-node-display-template (concat "${title:40} " (propertize "${tags:*}" 'face 'org-tag)))
  (setq org-roam-database-connector 'sqlite-builtin)
  (org-roam-db-autosync-mode)
  (setq org-roam-capture-templates
	'(
	  ("d" "default" plain "%?"
	   :target (file+head "%<%Y%m%d%H>-${slug}.org"
			      "#+title: ${title}\n#+filetags: \n")
	   :unnarrowed t)
	  ("b" "book notes" plain "%?"
	   :target (file+head "book/book%<%Y%m%d%H>-${slug}.org"
			      "#+title: ${title}\n#+filetags: :bookreading: \n\n")
	   :unnarrowed t)

	  ("a" "article notes" plain "%?"
	   :target (file+head "article/${citekey}.org"
			      "#+title: ${title}\n#+filetags: :articlereading: \n\n* 概要\n\n* 亮点\n\n* 想法\n\n* 不足\n\n* 结论\n\n"
			      )
	   :unnarrowed t)

	  ("e" "english notes" plain "%?"
	   :target (file+head "english/english%<%Y%m%d%H>-${slug}.org"
			      "#+title: ${title}\n#+filetags: :english: \n\n")
	   :unnarrowed t)

	  ("p" "technique" plain "%?"
	   :target (file+head "technique/technique%<%Y%m%d%H>-${slug}.org"
			      "#+title: ${title}\n#+filetags: :technique: \n\n")
	   :unnarrowed t)

	  ("g" "agenda" plain "%?"
	   :target (file+head "agenda/agenda%<%Y%m%d%H>-${slug}.org"
			      "#+title: ${title}\n#+filetags: :agenda: \n\n")
	   :unnarrowed t)

	  ("c" "concept" plain "%?"
	   :target (file+head "concept/concept%<%Y%m%d%H>-${slug}.org"
			      "#+title: ${title}\n#+filetags: :concept: \n\n")
	   :unnarrowed t)
	  ("s" "summary" plain "%?"
	   :target (file+head "summary/summary%<%Y%m%d%H>-${slug}.org"
			      "#+title: ${title}\n#+filetags: :summary: \n\n")
	   :unnarrowed t)
	  ("t" "tool" plain "%?"
	   :target (file+head "tool/tool%<%Y%m%d%H>-${slug}.org"
			      "#+title: ${title}\n#+filetags: :tool: \n\n")
	   :unnarrowed t)
	  ))
  (require 'org-roam-protocol))

(use-package org-roam-ui
  :ensure t
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
	org-roam-ui-follow t
	org-roam-ui-update-on-save t
	org-roam-ui-open-on-start t)
  )

(use-package org-download ;插入图片,基本只有拖拽有用了
  :init
  :ensure t
  :after org-roam
  :hook
  ((org-mode dired-mode) . org-download-enable)
  :config
  (setq-default org-download-image-dir (concat org-roam-directory "figures"))
  (setq org-download-annotate-function (lambda (_link) "")
        org-download-method 'directory
	org-download-screenshot-method "/d/GreenSoftware/Scoop/apps/irfanview/current/i_view64.exe /capture=4 /convert=\"%s\"")
  )

(use-package org-ref
  :ensure t
  :after ivy-bibtex
  :config
  (setq org-ref-default-citation-link "citep")
  )

(use-package ivy-bibtex;;搜索条目
  :ensure t
  :defer t
  :config
  (setq bibtex-completion-bibliography (concat org-roam-directory "article/library.bib"))
  (setq bibtex-completion-pdf-field "File")
  (setq bibtex-completion-notes-path "D:/Gitlocal/Notes/roam/article/");笔记位置寻找
  (setq bibtex-completion-additional-search-fields '(keywords)) ;;扩充搜索字段
  (setq bibtex-completion-display-formats
    '((article       . "${=has-pdf=:1}${=has-note=:1} ${=type=:8} ${year:4} ${author:10} ${title:*} ${journal:5}")
      (inbook        . "${=has-pdf=:1}${=has-note=:1} ${=type=:8} ${year:4} ${author:10} ${title:*} Chapter ${chapter:5}")
      (incollection  . "${=has-pdf=:1}${=has-note=:1} ${=type=:8} ${year:4} ${author:10} ${title:*} ${booktitle:5}")
      (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${=type=:8} ${year:4} ${author:10} ${title:*} ${booktitle:5}")
      (t             . "${=has-pdf=:1}${=has-note=:1} ${=type=:8} ${year:4} ${author:10} ${title:*}")))
  (setq bibtex-completion-pdf-open-function
	(lambda (fpath)
	  (call-process "evince" nil 0 nil fpath)))
  )

(use-package org-roam-bibtex
  :ensure t
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :custom
  (orb-insert-interface 'ivy-bibtex)
  (orb-note-actions-interface 'hydra)
  :config
  (setq orb-insert-link-description "(REF)")
  (setq orb-autokey-format "%a%y")
  (setq orb-preformat-keywords '("citekey" "author" "year"))
 )

(use-package org-alert
  :ensure t)

;; 管理可恶的弹窗
(use-package popper
  :ensure t ; or :straight t
  :bind (("C-`"   . popper-toggle-latest)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))                ; For echo area hints

(provide 'load-package)
;;; load-package.el ends here
