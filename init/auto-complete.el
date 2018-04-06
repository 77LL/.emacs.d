;; auto-complete
(el-get-bundle auto-complete)

;; (global-auto-complete-mode t)
;; (setq ac-auto-start 2)

(custom-set-variables
 `(ac-dictionary-directories ,(concat user-emacs-directory "ac-dict"))
 '(ac-use-fuzzy t)
 '(ac-auto-start 2)
 '(ac-use-menu-map t)
 '(ac-quick-help-delay 1.0))
(ac-config-default)

(add-to-list 'ac-modes 'yatex-mode)
;; (add-to-list 'ac-modes 'ess-mode)
;; (add-to-list 'ac-modes 'inferior-ess-mode)
;; (add-to-list 'ac-modes 'debugger-mode)

;; settings below requires yasnippet
(defun ac-yatex-mode-setup ()
  (setq ac-sources (append '(ac-source-yasnippet) ac-sources)))
(add-hook 'yatex-mode-hook 'ac-yatex-mode-setup)
