;; yasnippet
(el-get-bundle yasnippet)

;; directories
;; (setq yas-snippet-dirs
;;       '("~/.emacs.d/config/yasnippet/mysnippets/"
;; 				"~/.emacs.d/config/yasnippet/snippets/"
;;         ))

(yas-global-mode 1)

;; key bindings
(custom-set-variables '(yas-trigger-key "TAB"))

(define-key yas-minor-mode-map (kbd "C-'") 'yas-expand)
;; http://toot-fafsuhiro.hatenablog.com/entry/2013/12/06/133938
(define-key yas-minor-mode-map (kbd "C-x i i") 'yas-insert-snippet)
(define-key yas-minor-mode-map (kbd "C-x i n") 'yas-new-snippet)
(define-key yas-minor-mode-map (kbd "C-x i v") 'yas-visit-snippet-file)

(custom-set-variables '(yas-new-snippet-default "\
# -*- mode: snippet -*-
# name: $1
# key: ${2:${1:$(yas--key-from-desc yas-text)}}
# expand-env: ((yas/indent-line 'fixed) (yas/wrap-around-region 'nil))
# --
$0"))
