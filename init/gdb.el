;; gdb-mode
;; open windows
(defvar gdb-many-windows t)

;; show value on mouse cursol
(add-hook 'gdb-mode-hook '(lambda () (gud-tooltip-mode t)))

;; show io buffer
(defvar gdb-use-separate-io-buffer t)

(defvar gud-tooltip-echo-area nil)
(put 'set-goal-column 'disabled nil)
