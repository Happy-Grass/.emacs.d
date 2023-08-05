;;; Package --- Summary
;;; Commentary:
;配置文件--设置快捷键
;;; Code:
(general-create-definer leader-def
  ;; map a leader key
  :prefix ",");定义leader键

;; 快速打开配置文件
(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(defun open-init-dir()
  (interactive)
  (dired "~/.emacs.d/lisp/")
)
(defhydra hydra-init (:hint nil)
  "
_f_: init  _d_: directory
"
  ("f" open-init-file :exit t)
  ("d" open-init-dir :exit t)
)
(global-set-key (kbd "<f2>") 'hydra-init/body)
(global-set-key (kbd "<f12>") 'org-agenda)
(global-set-key (kbd "<f11>") 'org-clock-goto)
(global-set-key (kbd "C-<f11>") 'org-clock-in)

;;字体放大与缩小
(defhydra hydra-zoom (:hint nil)
  "
_i_: increase  _o_: decrease
"
  ("i" text-scale-increase)
  ("o" text-scale-decrease))
(global-set-key (kbd "<f3>") 'hydra-zoom/body)



;;avy,mark跳转等
(defhydra hydra-jump (:hint nil)
  "
_c_: char _w_: word
"
  ("c" avy-goto-char :exit t)
  ("w" avy-goto-word-1 :exit t)
)
(leader-def
  :keymaps 'normal
  "j" 'hydra-jump/body)


;; 书签的制作和删除


;; org-roam节点操作
(defhydra hydra-orgroam (:hint nil)
  "
_f_: find        _c_:capture      _i_: insert      _j_: id-create
_t_: tag-add     _a_: tag-remove  _s_: alias-add   _h_: alias-remove
_I_: ref-insert  _e_: ref-add     _m_: ref-remove  _x_: ref-find
_u_: toggle-ui   _r_: org-ref     _d_: dailies     _b_: buffer
_n_: xfw-refile
_q_: quit
"
  ("i" org-roam-node-insert :exit t)
  ("I" orb-insert-link :exit t)
  ("f" org-roam-node-find :exit t)
  ("b" org-roam-buffer-toggle :exit t)
  ("u" org-roam-ui-mode :exit t)
  ("c" org-roam-capture :exit t)
  ("x" org-roam-ref-find :exit t)
  ("d" org-roam-dailies-capture-today :exit t)
  ("m" org-roam-ref-remove :exit t)
  ("a" org-roam-tag-remove :exit t)
  ("t" org-roam-tag-add :exit t)
  ("e" org-roam-ref-add :exit t)
  ("s" org-roam-alias-add :exit t)
  ("h" org-roam-alias-remove :exit t)
  ("j" org-id-get-create :exit t)
  ("r" org-ref-insert-link-hydra/body :exit t)
  ("n" xfw/refile :exit t)
  ("q" nil)
)
(leader-def
  :keymaps 'normal
  "n" 'hydra-orgroam/body)


;; undo
(defhydra hydra-undo (:hint nil)
  "
_u_: undo _r_: redo _s_: save _l_: load
"
  ("u" undo-tree-undo)
  ("r" undo-tree-redo)
  ("s" undo-tree-save-history)
  ("l" undo-tree-load-history)
  ("v" undo-tree-visualize "visualize" :color blue)
  ("q" nil "quit" :color blue)
)
(leader-def
  :keymaps 'normal
  "u" 'hydra-undo/body)

;; project
(defhydra hydra-project (:hint nil)
  "
_f_: find-file        _d_: find-dir        _t_: find-tag       _g_: grep-in-project  _r_: replace
_a_: add-project      _p_: switch-project  _b_: switch-buffer  _s_: shell            _c_: async-command
_T_: regenerate-tags  _S_: save-project    _R_: remove-project
"
  ("f" projectile-find-file :exit t)
  ("d" projectile-find-dir :exit t)
  ("t" projectile-find-tag :exit t)
  ("g" projectile-grep :exit t)
  ("a" projectile-add-known-project :exit t)
  ("r" projectile-replace :exit t)
  ("p" projectile-switch-project :exit t)
  ("c" projectile-run-async-shell-command-in-root :exit t)
  ("s" projectile-run-shell :exit t)
  ("b" projectile-switch-to-buffer :exit t)
  ("T" projectile-regenerate-tags :exit t)
  ("S" projectile-save-project-buffers :exit t)
  ("R" projectile-remove-known-project :exit t)
  ("q" nil "quit" :color blue)
  )

(leader-def
  :keymaps 'normal
  "p" 'hydra-project/body)
;;; 一些自定义函数
;; 查找替换函数
(defun xfw/query-replace-region ()
  "Query replace selected region."
  (interactive)
  (query-replace (buffer-substring-no-properties
                  (region-beginning)
                  (region-end))
                 (completing-read "Replace to: " ())
                 ))

(defun xfw/query-replace-point ()
  "Query replace thing at point."
  (interactive)
  (let ((word (thing-at-point 'word t)))
    (query-replace word
                   (completing-read (format "Replace \"%s\" to: " word) ())
                   nil (beginning-of-line))))


(defun xfw/capture ()
  "截图工具，使用Irfanview注意安装."
  (interactive)
  (lower-frame) ;; 把emacs最小化
  (setq filename
     (concat
       (make-temp-name
         (concat "img"
              "_"
              (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
  (shell-command (concat " \"c:/Program Files/IrfanView/i_view64.exe\" /capture=4 /convert=" "\"c:/Users/xfwah/Documents/Orgnotes/figures/screenshots" (format "\\%s\"" filename)))  ;; IrfanView 截图
  (insert (concat "[[file:c:/Users/xfwah/Documents/Orgnotes/figures/screenshots/" filename "]]"))
  (org-display-inline-images)
)
;; tools
(defhydra hydra-tools (:hint nil)
  "
_p_: replace-word _r_: replace-region _c_: capture
"
  ("p" xfw/query-replace-point :exit t)
  ("r" xfw/query-replace-region :exit t)
  ("c" xfw/capture :exit t)
)

(leader-def
  :keymaps 'normal
  "t" 'hydra-tools/body)


(defhydra hydra-org-insert (:hint nil)
  "
_i_: link  _d_: drawer      _h_: heading    _s_: subheading
_e_: item  _t_: struc-temp  _o_: todo-head  _q_: quit
_O_: todo-head-res-cont     _S_: todo-subhead
"
  ("i" org-insert-link :exit t)
  ("d" org-insert-drawer :exit t)
  ("h" org-insert-heading :exit t)
  ("s" org-insert-subheading :exit t)
  ("e" org-insert-item :exit t)
  ("t" org-insert-structure-template :exit t)
  ("o" org-insert-todo-heading :exit t)
  ("O" org-insert-todo-heading-respect-content :exit t)
  ("S" org-insert-todo-subheading :exit t)
  ("q" nil :exit t)
  )

(defhydra hydra-orgmode (:hint nil)
  "
_a_: agenda     _d_: display-img   _i_: insert
_s_: schedule   _t_: todo          _o_: open
_n_: deadline   _c_: checkitem     _q_: exit "
  ("a" org-agenda :exit t)
  ("i" hydra-org-insert/body :exit t)
  ("d" org-display-inline-images :exit t)
  ("o" org-open-at-point :exit t)
  ("t" org-todo :exit t)
  ("s" org-schedule :exit t)
  ("n" org-deadline :exit t)
  ("c" org-toggle-checkbox :exit t )
  ("q" nil :exit t)
  )
(leader-def
  :keymaps 'normal
  "m" 'hydra-orgmode/body)
(provide 'keybind)
;;; keybind.el ends here

