(eval-when-compile
  (require 'use-package))
;; 输入法
(use-package rime
      :ensure t
      :demand t
      :init
      (setq rime-postframe-properties
                  (list :background-color "#333333"
                        :foreground-color "#dcdccc"
                        :font "KaiTi"
                        :internal-border-width 1)
	    default-input-method "rime"
            rime-show-candidate 'posframe
	    rime-title "⚡")
      :config
      (setq rime-user-data-dir "~/.emacs.d/rime/")
      )

;; vim模式
(use-package evil
  :ensure t
  :demand t
  :init (evil-mode)
  :config
  (setq evil-emacs-state-cursor '("#87cefa" box)
	evil-normal-state-cursor '("#657b83" box)
	evil-visual-state-cursor '("#ffdab9" box)
	evil-insert-state-cursor '("#87cefa" bar)
	evil-replace-state-cursor '("#87cefa" bar)
	evil-operator-state-cursor '("#87cefa" hollow))
)

;; vim一样的leader按键
(use-package general
  :ensure t)
(use-package hydra
  :ensure t)
;; 颜色显示
(use-package rainbow-mode
  :ensure t
  :hook (progmode . rainbow-mode))

(use-package good-scroll
  :ensure t
  :defer t
  :if window-system          ; 在图形化界面时才使用这个插件
  :init (good-scroll-mode))

(use-package smartparens ;括号配对
  :ensure t
  :hook
  (prog-mode . smartparens-mode)
  )

(use-package rainbow-delimiters ;;括号匹配彩色
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package counsel
  :ensure t)

(use-package swiper
  :ensure t)

(use-package ivy
  :ensure t
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
  :init (global-undo-tree-mode)
  :custom
  (undo-tree-auto-save-history nil))

(use-package avy ;类似于easymotion
  :ensure t)

;;;让avy和ivy支持拼音搜索
(use-package ace-pinyin
  :init
  ;(setq ace-pinyin-use-avy nil)
  :ensure t
  :config
  (ace-pinyin-global-mode +1))
(use-package company
  :ensure t
  :init
  (global-company-mode)
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
  :config
  (setq truncate-lines nil) ; 如果单行信息很长会自动换行
  :hook
  (prog-mode . flycheck-mode))

(use-package magit
  :ensure t)

(use-package projectile
  :ensure t
  :config
  (setq projectile-indexing-method 'native
	projectile-sort-order 'modification-time ;按最近修改时间排序
	projectile-completion-system 'ivy)
  )

(use-package ace-window
  :ensure t
  :bind (("C-x o" . 'ace-window)))

(use-package org
  :ensure t
  :config
  (require 'xfw-org))

(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●")))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "D:/Gitlocal/Notes/roam/"))
  (org-roam-dailies-directory "daily/")
  :config
  (require 'xfw-orgroam)
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:15}" 'face 'org-tag)))
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
  (setq-default org-download-image-dir (concat org-roam-directory "figures"))
  :ensure t
  :hook
  ((org-mode dired-mode) . org-download-enable)
  :config
  (setq org-download-annotate-function (lambda (_link) "")
        org-download-method 'directory
	org-download-screenshot-method "/d/GreenSoftware/Scoop/apps/irfanview/current/i_view64.exe /capture=4 /convert=\"%s\"")
  )

(use-package org-ref
  :ensure t
  :config
  (setq org-ref-default-citation-link "citep")
  )

(use-package ivy-bibtex;;搜索条目
  :ensure t
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
  (setq orb-insert-link-description "(${author},${year})")
  (setq orb-autokey-format "%a%y")
  (setq orb-preformat-keywords '("citekey" "author" "year"))
 )
(provide 'load-package)
;;; load-package.el ends here
