;; auto-complete
(el-get-bundle auto-complete)

(global-auto-complete-mode t)
(setq ac-auto-start 2)

(add-to-list 'ac-modes 'ess-mode)
(add-to-list 'ac-modes 'inferior-ess-mode)
(add-to-list 'ac-modes 'yatex-mode)
(add-to-list 'ac-modes 'debugger-mode)
