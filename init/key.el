;; basic key bindings
;; meke C-q prefix key
(define-key global-map "\C-q" (make-sparse-keymap))
(global-set-key "\C-q\C-q" 'quoted-insert)

(global-set-key [home] 'beginning-of-buffer)
(global-set-key [end] 'end-of-buffer)

;; C-h : backspace
(global-set-key "\C-h" 'delete-backward-char)

;; C-o : delete to top
(defun backward-delete-line ()
	(interactive)
	(kill-region (point) (progn (beginning-of-line) (point)))
	)
(global-set-key "\C-o" 'backward-delete-line)

p;; C-t : other window
(defun other-window-or-split (val)
  (interactive)
  (when (one-window-p)
		(split-window-horizontally) ;split horizontally
    ;; (split-window-vertically) ;split vertically
  )
  (other-window val))

(global-set-key (kbd "<C-tab>") (lambda () (interactive) (other-window-or-split 1)))
(global-set-key "\C-t" (lambda () (interactive) (other-window-or-split 1)))
(global-set-key (kbd "<C-S-tab>") (lambda () (interactive) (other-window-or-split -1)))

(global-set-key "\C-cl" 'toggle-truncate-lines)
(global-set-key "\C-cr" 'revert-buffer)
