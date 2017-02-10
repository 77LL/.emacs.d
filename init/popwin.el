;; popwin.el
(el-get-bundle popwin)

(popwin-mode 1)
;; helm
(push '("^\*helm .+\*$" :regexp t ) popwin:special-display-config)
(global-set-key (kbd "M-p") popwin:keymap)
