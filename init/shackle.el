;; shackle.el
(el-get-bundle shackle)

(setq shackle-rules
      '((compilation-mode :align below :ratio 0.2)
        ("*Help*" :align right)
        ("*Completions*" :align below :ratio 0.3)
        ;; ("^\*helm .+\*$" :regexp t :align below)
				("\\`\\*helm.*?\\*\\'" :regexp t :align below)
				))
(shackle-mode 1)
(setq shackle-lighter "")

;;; key bindings
;; C-z : undo windows
;; (winner-mode 1)
;; (global-set-key (kbd "C-z") 'winner-undo)
