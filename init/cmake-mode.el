;; cmake-mode
(el-get-bundle cmake-mode)

(setq auto-mode-alist
			(append  '(("CMakeLists\\.txt\\'" . cmake-mode))
							 '(("\\.cmake\\'" . cmake-mode)) auto-mode-alist))
