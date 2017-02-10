;; smartchr.el
(el-get-bundle smartchr)

(defun my-smartchr-braces ()
  "Insert a pair of braces like below.
\n {\n `!!'\n}"
  ;; foo {
  ;; `!!'
  ;; }
  (lexical-let (beg end)
    (smartchr-make-struct
     :insert-fn (lambda ()
                  (setq beg (point))
                  (insert "{\n\n}")
                  (indent-region beg (point))
                  (forward-line -1)
                  (indent-according-to-mode)
                  (goto-char (point-at-eol))
                  (setq end (save-excursion
                              (re-search-forward "[[:space:][:cntrl:]]+}" nil t))))
     :cleanup-fn (lambda ()
                   (delete-region beg end))
     )))

(defun my-smartchr-setting ()
  (local-set-key (kbd "=") (smartchr '("=" "==" "=")))
  (local-set-key (kbd "+") (smartchr '("+" "++" "+=" "+")))
  (local-set-key (kbd "-") (smartchr '("-" "--" "-=" "-")))

;;  (local-set-key (kbd "\"") (smartchr '("\"`!!'\"" "\"")))
  (local-set-key (kbd "'") (smartchr '("'`!!''" "'")))

  (local-set-key (kbd ">") (smartchr '(">" "->" ">>")))

	(local-set-key (kbd "[") (smartchr '("[`!!']" "[")))
  (local-set-key (kbd "{") (smartchr '(my-smartchr-braces "{`!!'}"  "{")))
	(local-set-key (kbd "(") (smartchr '("(`!!')" "(")))
  )

;; (add-hook 'c-mode-common-hook 'my-smartchr-setting)
(add-hook 'c-mode-hook 'my-smartchr-setting)
(add-hook 'objc-mode-hook 'my-smartchr-setting)
