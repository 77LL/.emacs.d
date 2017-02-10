;; flycheck
(el-get-bundle flycheck)

(add-hook 'after-init-hook #'global-flycheck-mode)

(custom-set-variables
 '(flycheck-display-errors-delay 0.5)
 '(flycheck-idle-change-delay 1.0))

(defvar flycheck-c/c++-include-path
      (list "." ".." "../include" (expand-file-name "~/usr/include")))
(defun my/set-flycheck-c/c++-include-path ()
  (setq flycheck-gcc-include-path flycheck-c/c++-include-path))
(add-hook 'c-mode-hook 'my/set-flycheck-c/c++-include-path)
(add-hook 'c++-mode-hook 'my/set-flycheck-c/c++-include-path)
(define-key global-map (kbd "M-l") 'my/flycheck-list-errors)
