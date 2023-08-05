;;; Package -- Summary
;;; Commentary: 
;;; Code:
(setq default-directory "D:/Gitlocal/Notes/")
(setq make-backup-files nil) ;; 禁用自动备份
(setq default-buffer-file-coding-system 'utf-8) ;;默认文件编码为utf-8
(prefer-coding-system 'utf-8) ;指定文件编码优先，此时buffer新建和读取默认都是utf-8
(setq-default auto-save-timeout 15); 15秒无动作，自动保存
(setq-default auto-save-interval 100); 100个字符间隔，自动保存
(fset 'yes-or-no-p 'y-or-n-p) ;; 把Yes用y代替
(setq-default fill-column 80) ;;当某行的长度大于70个字符时，再次输入文字会自动添加到下一行
(setq user-full-name "xfw")
(setq user-mail-address "xfwahss@qq.com")
(setq initial-major-mode 'fundamental-mode)
(setq inhibit-compacting-font-caches t);;缓存字体信息，提高启动速度
(provide 'setting)
;;; setting.el ends here
