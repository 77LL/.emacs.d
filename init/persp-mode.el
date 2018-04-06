;; persp-mode
(el-get-bundle persp-mode)

(defvar persp-keymap-prefix (kbd "C-c p"))
(defvar persp-add-on-switch-or-display t)
(persp-mode 1)
(defun persp-register-buffers-on-create ()
  (interactive)
  (dolist (bufname (condition-case _
                       (helm-comp-read
                        "Buffers: "
                        (mapcar 'buffer-name (buffer-list))
                        :must-match t
                        :marked-candidates t)
                     (quit nil)))
    (persp-add-buffer (get-buffer bufname))))
(add-hook 'persp-activated-hook 'persp-register-buffers-on-create)
(defvar recentf-exclude '("~/.emacs.d/persp-confs/persp-auto-save"))
