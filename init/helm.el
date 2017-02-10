;;; helm
(el-get-bundle helm)

(define-key global-map (kbd "C-;")     'helm-mini)
(define-key global-map (kbd "M-x")     'helm-M-x)
(define-key global-map (kbd "C-x C-f") 'helm-find-files)
(define-key global-map (kbd "C-x C-r") 'helm-recentf)
(define-key global-map (kbd "M-y")     'helm-show-kill-ring)
(define-key global-map (kbd "C-c i")   'helm-imenu)
(define-key global-map (kbd "C-x b")   'helm-buffers-list)

(define-key helm-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)

;; http://d.hatena.ne.jp/a_bicky/20140104/1388822688
;; Emulate `kill-line' in helm minibuffer
(setq helm-delete-minibuffer-contents-from-point t)
(defadvice helm-delete-minibuffer-contents (before helm-emulate-kill-line activate)
	"Emulate `kill-line' in helm minibuffer"
	(kill-new (buffer-substring (point) (field-end))))

(defadvice helm-ff-kill-or-find-buffer-fname (around execute-only-if-exist activate)
	"Execute command only if CANDIDATE exists"
	(when (file-exists-p candidate)
		ad-do-it))

(defadvice helm-ff-transform-fname-for-completion (around my-transform activate)
	"Transform the pattern to reflect my intention"
	(let* ((pattern (ad-get-arg 0))
				 (input-pattern (file-name-nondirectory pattern))
				 (dirname (file-name-directory pattern)))
		(setq input-pattern (replace-regexp-in-string "\\." "\\\\." input-pattern))
		(setq ad-return-value
					(concat dirname
									(if (string-match "^\\^" input-pattern)
											;; '^' is a pattern for basename
											;; and not required because the directory name is prepended
											(substring input-pattern 1)
										(concat ".*" input-pattern))))))

;; (run-with-idle-timer 30 t '(lambda ()          ;; 30秒ごとに .recentf を保存
;;    (with-suppressed-message (recentf-save-list))))

;; ;; yasnippet
;; ;; http://d.hatena.ne.jp/syohex/20121207/1354885367
;; (defun my-yas/prompt (prompt choices &optional display-fn)
;;   (let* ((names (loop for choice in choices
;;                       collect (or (and display-fn (funcall display-fn choice))
;;                                   choice)))
;;          (selected (helm-other-buffer
;;                     `(((name . ,(format "%s" prompt))
;;                        (candidates . names)
;;                        (action . (("Insert snippet" . (lambda (arg) arg))))))
;;                     "*helm yas/prompt*")))
;;     (if selected
;;         (let ((n (position selected names :test 'equal)))
;;           (nth n choices))
;;       (signal 'quit "user quit!"))))
;; (custom-set-variables '(yas/prompt-functions '(my-yas/prompt)))

;; gtags
(el-get-bundle helm-gtags)

;;; Enable helm-gtags-mode
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)

;; customize
(custom-set-variables
 '(helm-gtags-path-style 'relative)
 '(helm-gtags-ignore-case t)
 '(helm-gtags-auto-update t))

;; key bindings
(add-hook 'helm-gtags-mode-hook
          '(lambda ()
						 (local-set-key (kbd "M-t") 'helm-gtags-find-tag)
						 (local-set-key (kbd "M-r") 'helm-gtags-find-rtag)
						 (local-set-key (kbd "M-s") 'helm-gtags-find-symbol)
						 (local-set-key (kbd "M-g M-p") 'helm-gtags-parse-file)
						 (local-set-key (kbd "C-c <") 'helm-gtags-previous-history)
						 (local-set-key (kbd "C-c >") 'helm-gtags-next-history)
						 (local-set-key (kbd "M-,") 'helm-gtags-pop-stack)))
