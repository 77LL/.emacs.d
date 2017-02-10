;; basic customizations
(custom-set-variables
 ;; inhibit startup screen
 '(inhibit-startup-screen t)
 ;; not create backup files
 '(backup-inhibited t)
 '(make-backup-files nil)
 ;; delete auto save files
 '(delete-auto-save-files t)
 ;; not beep
 '(visible-bell t)
 '(ring-bell-function 'ignore)
 ;; kill until the end of line
 '(kill-whole-line t)
 ;; remove text properties when yanking
 '(yank-excluded-properties t))

;; enable delete selection mode
(delete-selection-mode t)

;; yes-or-no-p
(defalias 'yes-or-no-p 'y-or-n-p)
(custom-set-variables
 '(use-dialog-box nil))


