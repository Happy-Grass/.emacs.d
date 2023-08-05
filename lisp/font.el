;;; Pakcage --- Summary
;;; Commentary:
;;; Code:
;;; This is a package for global font setting
;;Symbola
(set-fontset-font t nil "Symbola" nil 'prepend)
(set-face-attribute
 'default nil
 :font (font-spec :name "SauceCodePro NF"
                  :weight 'normal
                  :slant 'normal
                  :size 12.5)) ;;好像小数会有问题
(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font
   (frame-parameter nil 'font)
   charset
   (font-spec :name "KaiTi"
              :weight 'normal
              :slant 'normal
              :size 15.0)))
(provide 'font)
;;; font.el ends here
