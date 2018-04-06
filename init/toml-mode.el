;; toml-mode
(el-get-bundle toml-mode)

(setq auto-mode-alist
			(append  '(("\\.toml'" . toml-mode)) auto-mode-alist))
