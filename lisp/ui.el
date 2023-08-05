;;; Package --- Summary
;;; Commentary:
;;; code:
(load-theme 'modus-vivendi t)
(setq frame-title-format
     '((:eval (if (buffer-file-name)
		  (abbreviate-file-name (buffer-file-name))
		"%b"))))
(menu-bar-mode -1);; 禁用菜单栏，F10 开启关闭菜单
(tool-bar-mode -1);; 取消工具栏
(scroll-bar-mode -1);; 取消滚动栏
(setq column-number-mode t);; 显示行列号,它显示在minibuffer上面那个杠上
(setq line-number-mode t)
(setq-default  use-dialog-box nil)
(setq cursor-type 'bar) ;;更改光标样式
(global-hl-line-mode t) ;;高亮当前行
(blink-cursor-mode 0) ;;禁用光标闪烁
(setq inhibit-startup-screen t);;关闭emacs启动时的画面
(setq inhibit-startup-message t)
(setq  initial-scratch-message nil)
(setq visible-bell nil
      ring-bell-function 'ignore);;关闭出错时的提示声
(setq transient-mark-mode t) ;启用标记区高亮
(require 'vs-modeline)
(vs-modeline-mode)

;; Mouse & Smooth Scroll
;; Scroll one line at a time (less "jumpy" than defaults)
(when (display-graphic-p)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . hscroll))
        mouse-wheel-scroll-amount-horizontal 1
        mouse-wheel-progressive-speed nil))
(setq scroll-step 1
      scroll-margin 0
      scroll-conservatively 100000
      auto-window-vscroll nil
      scroll-preserve-screen-position t)

;; Good pixel line scrolling
(if (fboundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode t)
  (when (and emacs/>=27p (not sys/macp))
    (use-package good-scroll
      :hook (after-init . good-scroll-mode)
      :bind (([remap next] . good-scroll-up-full-screen)
             ([remap prior] . good-scroll-down-full-screen)))))

(use-package nerd-icons
  :ensure t
  :demand t
  )
    
;; Smooth scrolling over images
(use-package iscroll
  :ensure t
  :hook (image-mode . iscroll-mode))

;; Use fixed pitch where it's sensible
(use-package mixed-pitch
  :ensure t)

;; Show line numbers
(use-package display-line-numbers
  :ensure nil
  :hook ((prog-mode yaml-mode conf-mode) . display-line-numbers-mode)
  :init (setq display-line-numbers-width-start t))

(use-package time
  
  :ensure nil
  :init (setq display-time-24hr-format t
              display-time-day-and-date t))
(provide 'ui)
;;; ui.el ends here

