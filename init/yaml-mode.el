;; yaml-mode
(el-get-bundle yaml-mode)

(setq auto-mode-alist
			(append  '(("\\.yml\\'" . yaml-mode)) auto-mode-alist))
