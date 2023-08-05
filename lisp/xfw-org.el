;;; Package --- Summary
;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'org)
  (require 'org-agenda))

(defun xfw/org-mode-font ()
  "Set font for Org-mode"
  (interactive)
  (setq buffer-face-mode-face '(:family "Times New Roman" :height 100))
  (buffer-face-mode))

(defun xfw/org-mode-setup()
  (org-indent-mode)
  ;;(variable-pitch-mode 1)成比例字体，但现在还不太会用，先禁止
  ;(auto-fill-mode 1)
  (visual-line-mode 1)
  (setq evil-auto-indent nil)
  (setq-local line-spacing 0.45);设置显示行距
)
;;配置org-latex-preview
(setq org-preview-latex-image-directory "D:/Gitlocal/Notes/roam/figures/lateximg/")

(setq-default prettify-symbols-alist '(("#+BEGIN_SRC" . "󰶻")
                                       ("#+END_SRC" . "󰶺")
                                       ("#+begin_src" . "󰶻")
                                       ("#+end_src" . "󰶺")
				       ("[ ]" . "󰝣")
				       ("[X]" . "󰝤")
				       ("[-]" . "󱔀")
				       ("#+AUTHOR:" . "")
				       ("SCHEDULED:" . "󰸘")
				       ("DEADLINE:" . "󱨲")
                                       (">=" . "≥")
                                       ("=>" . "⇨")))
(setq prettify-symbols-unprettify-at-point 'right-edge)
(setq org-ellipsis "▶"
	org-hide-emphasis-markers t)

(setq org-agenda-file-regexp "\\`[^.].*\\.org\\'")
(setq org-agenda-files
	'("D:/Gitlocal/Notes/roam/agenda/"))
(setq org-todo-keywords '((sequence "TODO(t!)" "PROCESSING(p!)" "WAIT(w!)" "|" "DONE(d!)" "CANCLED(a@/!)")))
(setq org-todo-keyword-faces
   '(("TODO" .   (:foreground "red" :weight bold))
    ("WAIT" .   (:foreground "red" :weight bold))
    ("PROCESSING" .      (:foreground "orange" :weight bold))
    ("DONE" .      (:foreground "green" :weight bold))
    ("CANCLED" .     (:foreground "grey" :weight bold))
))

;; 优先级范围和默认任务的优先级
(setq org-highest-priority ?A)
(setq org-lowest-priority  ?E)
(setq org-default-priority ?E)
;; 优先级醒目外观
(setq org-priority-faces
  '((?A . (:background "red" :foreground "white" :weight bold))
    (?B . (:background "DarkOrange" :foreground "white" :weight bold))
    (?C . (:background "yellow" :foreground "DarkGreen" :weight bold))
    (?D . (:background "DodgerBlue" :foreground "black" :weight bold))
    (?E . (:background "SkyBlue" :foreground "black" :weight bold))
))
(setq org-enforce-todo-dependencies t);;如果子任务没有完成，主任务不能设置为DONE
(setq org-agenda-time-grid (quote ((daily today require-timed)
                                   (300
                                    600
                                    900
                                    1200
                                    1500
                                    1800
                                    2100
                                    2400)
                                   "......"
                                   "-----------------------------------------------------"
                                   )))
(setq org-agenda-custom-commands
      '(("f" "查看TODO条目（按创建时间排序）" todo "TODO"
         ((org-agenda-sorting-strategy '(priority-down time-up))))))

;; agenda 里面时间块彩色显示
;; From: https://emacs-china.org/t/org-agenda/8679/3
(defun ljg/org-agenda-time-grid-spacing ()
  "Set different line spacing w.r.t. time duration."
  (save-excursion
    (let* ((background (alist-get 'background-mode (frame-parameters)))
           (background-dark-p (string= background "dark"))
           (colors (list "#1ABC9C" "#2ECC71" "#3498DB" "#9966ff"))
           pos
           duration)
      (nconc colors colors)
      (goto-char (point-min))
      (while (setq pos (next-single-property-change (point) 'duration))
        (goto-char pos)
        (when (and (not (equal pos (point-at-eol)))
                   (setq duration (org-get-at-bol 'duration)))
          (let ((line-height (if (< duration 30) 1.0 (+ 0.5 (/ duration 60))))
                (ov (make-overlay (point-at-bol) (1+ (point-at-eol)))))
            (overlay-put ov 'face `(:background ,(car colors)
                                                :foreground
                                                ,(if background-dark-p "black" "white")))
            (setq colors (cdr colors))
            (overlay-put ov 'line-height line-height)
            (overlay-put ov 'line-spacing (1- line-height))))))))
(defun xfw/clockin ()
  "Start the clock when status change to PROCESSING."
  (when (string= org-state "PROCESSING")
    (org-clock-in)))

(defun xfw/clockout ()
  "Stop the clock when status changes from PROCESSING."
  (when (and (string= org-last-state "PROCESSING")
             (not (string= org-state "PROCESSING")))
    (org-clock-out)))

(add-hook 'org-after-todo-state-change-hook 'xfw/clockin)
(add-hook 'org-after-todo-state-change-hook 'xfw/clockout)
(add-hook 'org-agenda-finalize-hook #'ljg/org-agenda-time-grid-spacing)
(add-hook 'org-mode-hook 'prettify-symbols-mode)
(add-hook 'org-mode-hook 'xfw/org-mode-setup)
;(add-hook 'org-mode-hook 'xfw/org-mode-font)
(provide 'xfw-org)
;;; xfw-org.el ends here
