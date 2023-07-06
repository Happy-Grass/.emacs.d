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

;;显示行号
;;(setq display-line-numbers-current-absolute t)

;;(defun vmacs-change-line-number-abs()
 ;; (setq display-line-numbers 'absolute))
;;(defun vmacs-change-line-number-relative()
 ;; (setq display-line-numbers 'visual))
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
;;(add-hook 'evil-insert-state-entry-hook 'vmacs-change-line-number-abs)
;;(add-hook 'evil-normal-state-entry-hook 'vmacs-change-line-number-relative)
;;(add-hook 'evil-motion-state-entry-hook 'vmacs-change-line-number-relative)

(provide 'ui)
;;; ui.el ends here

