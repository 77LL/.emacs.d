;; appearance customizations
;; scrooll-bar position
(set-scroll-bar-mode 'right)

(custom-set-variables
 ;; frame title
 '(frame-title-format (format "%%f")))

;; hide toolbar
(when window-system
  (tool-bar-mode 0))

;; show parenthesis visible
(show-paren-mode t)
(custom-set-variables
 '(show-paren-style 'mixed)
 '(show-paren-delay 0)       ; default: 0.15
 ;; '(show-paren-style 'parenthesis)
 ;; '(show-paren-delay 0.05) ;; default: 0.125
 )

;; enable linum mode
(line-number-mode t)
(column-number-mode t)
(global-linum-mode t)
(custom-set-variables
 '(linum-format "%4d"))

;; font setting
(add-to-list 'default-frame-alist '(font . "ricty-12"))

;; hl-line
(global-hl-line-mode)

;; fullscreen
(set-frame-parameter nil 'fullscreen 'maximized)

;; Color theme
;; hc-zenburn
;; (load-theme 'org-hc-zenburn t)
;; (enable-theme 'org-hc-zenburn)

;; original theme
(load-theme 'org-dark-blue t)
(enable-theme 'org-dark-blue)

(set-frame-parameter (selected-frame) 'alpha '(90 75))
(add-to-list 'default-frame-alist '(alpha 90 75))

;; mode line setting
(size-indication-mode t)
;; (setq display-time-day-and-date t)
;; (setq display-time-24hr-format t)
;; (display-time-mode t)
