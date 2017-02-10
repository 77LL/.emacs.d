(deftheme org-dark-blue
  "Original dark blue color theme")

(defvar darkblue-colors-alist
	'(("darkblue-fg"       . "#F0F8F0")
		("darkblue-fg2"      . "#D5F5D5")
    ("darkblue-bg"       . "#151520")
		("darkblue-bg05"     . "#303035")
		("darkblue-bg2"      . "#353545")
		("darkblue-bg3"      . "#454555")
    ("darkblue-red"      . "#D9A0A0")
    ("darkblue-orange"   . "#E67350")
    ("darkblue-yellow"   . "#FDECBC")
		("darkblue-yellow2"  . "#F8DC85")
    ("darkblue-green"    . "#9DD88C")
		("darkblue-green2"   . "#2C523C")
		("darkblue-cyan"     . "#A0EDF0")
    ("darkblue-lblue"    . "#52A2C5")
    ("darkblue-blue"     . "#2860A3")
		("darkblue-blue2"    . "#3560AE")
		("darkblue-blue3"    . "#9CA7E2")
    ("darkblue-magenta"  . "#E090C7")))

(defmacro darkblue-with-color-variables (&rest body)
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   darkblue-colors-alist))
     ,@body))

(darkblue-with-color-variables
 (custom-theme-set-faces
	'org-dark-blue
	;;;;; basic coloring
	`(cursor ((t (:foreground ,darkblue-fg))))
	`(default ((t (:background ,darkblue-bg :foreground ,darkblue-fg))))
	`(warning ((t (:foreground ,darkblue-orange :weight bold))))
	`(region ((t (:background ,darkblue-blue2))))

	;;;;isearch
	`(isearch ((t (:foreground ,darkblue-fg :weight bold :background ,darkblue-magenta))))
	`(isearch-fail ((t (:foreground ,darkblue-bg :background ,darkblue-yellow))))
	`(lazy-highlight ((t (:weight bold :background ,darkblue-orange))))
	;;;; menu bar
	`(menu ((t (:foreground ,darkblue-fg :background ,darkblue-bg))))
	`(minibuffer-prompt ((t (:foreground ,darkblue-yellow))))
	`(mode-line
		((,class (:foreground ,darkblue-fg2
													:background ,darkblue-green2
													:box (:line-width -1 :style released-button)))))
	`(mode-line-buffer-id ((t (:foreground ,darkblue-yellow :weight bold))))

	;;;;; compilation
	;;;;; font lock
   `(font-lock-builtin-face ((t (:foreground ,darkblue-green :weight bold))))
   `(font-lock-comment-face ((t (:foreground ,darkblue-orange))))
   `(font-lock-constant-face ((t (:foreground ,darkblue-green))))
   `(font-lock-doc-face ((t (:foreground ,darkblue-magenta))))
   `(font-lock-function-name-face ((t (:foreground ,darkblue-lblue))))
   `(font-lock-keyword-face ((t (:foreground ,darkblue-blue :weight bold))))
   `(font-lock-negation-char-face ((t (:foreground ,darkblue-yellow :weight bold))))
   `(font-lock-preprocessor-face ((t (:foreground ,darkblue-blue3))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,darkblue-yellow :weight bold))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,darkblue-green :weight bold))))
   `(font-lock-string-face ((t (:foreground ,darkblue-red))))
   `(font-lock-type-face ((t (:foreground ,darkblue-green))))
   `(font-lock-variable-name-face ((t (:foreground ,darkblue-yellow2))))
   `(font-lock-warning-face ((t (:foreground ,darkblue-red :weight bold))))

	 ;;;;; helm
   `(helm-header
     ((t (:foreground ,darkblue-green
                      :background ,darkblue-bg3
                      :underline nil
                      :box nil))))
   `(helm-source-header
     ((t (:foreground ,darkblue-yellow
                      :background ,darkblue-bg2
                      :underline nil
                      :weight bold
                      :box (:line-width -1 :style released-button)))))
	 `(helm-selection ((t (:background ,darkblue-green2 :underline nil))))
   `(helm-selection-line ((t (:background ,darkblue-bg))))
   `(helm-visible-mark ((t (:foreground ,darkblue-bg :background ,darkblue-yellow))))
   `(helm-candidate-number ((t (:foreground ,darkblue-green :background ,darkblue-bg))))
   `(helm-separator ((t (:foreground ,darkblue-red :background ,darkblue-bg))))
   `(helm-time-zone-current ((t (:foreground ,darkblue-green :background ,darkblue-bg))))
   `(helm-time-zone-home ((t (:foreground ,darkblue-red :background ,darkblue-bg))))
   `(helm-bookmark-addressbook ((t (:foreground ,darkblue-orange :background ,darkblue-bg))))
   `(helm-bookmark-directory ((t (:foreground nil :background nil :inherit helm-ff-directory))))
   `(helm-bookmark-file ((t (:foreground nil :background nil :inherit helm-ff-file))))
   `(helm-bookmark-gnus ((t (:foreground ,darkblue-magenta :background ,darkblue-bg))))
   `(helm-bookmark-info ((t (:foreground ,darkblue-green :background ,darkblue-bg))))
   `(helm-bookmark-man ((t (:foreground ,darkblue-yellow :background ,darkblue-bg))))
   `(helm-bookmark-w3m ((t (:foreground ,darkblue-magenta :background ,darkblue-bg))))
   `(helm-buffer-not-saved ((t (:foreground ,darkblue-red :background ,darkblue-bg))))
	 `(helm-buffer-process ((t (:foreground ,darkblue-orange :background ,darkblue-bg))))
   `(helm-buffer-saved-out ((t (:foreground ,darkblue-fg :background ,darkblue-bg))))
   `(helm-buffer-size ((t (:foreground ,darkblue-fg :background ,darkblue-bg))))
	 `(helm-ff-directory ((t (:foreground ,darkblue-lblue :background ,darkblue-bg :weight bold))))
   `(helm-ff-file ((t (:foreground ,darkblue-fg2 :background ,darkblue-bg :weight normal))))
   `(helm-ff-executable ((t (:foreground ,darkblue-green :background ,darkblue-bg :weight bold))))
   `(helm-ff-invalid-symlink ((t (:foreground ,darkblue-red :background ,darkblue-bg :weight bold))))
   `(helm-ff-symlink ((t (:foreground ,darkblue-cyan :background ,darkblue-bg :weight bold))))
   `(helm-ff-prefix ((t (:foreground ,darkblue-bg :background ,darkblue-yellow :weight normal))))
   `(helm-grep-cmd-line ((t (:foreground ,darkblue-cyan :background ,darkblue-bg))))
   `(helm-grep-file ((t (:foreground ,darkblue-fg :background ,darkblue-bg))))
   `(helm-grep-finish ((t (:foreground ,darkblue-green :background ,darkblue-bg))))
   `(helm-grep-lineno ((t (:foreground ,darkblue-fg :background ,darkblue-bg))))
   `(helm-grep-match ((t (:foreground nil :background nil :inherit helm-match))))
   `(helm-grep-running ((t (:foreground ,darkblue-red :background ,darkblue-bg))))
   `(helm-moccur-buffer ((t (:foreground ,darkblue-cyan :background ,darkblue-bg))))
   `(helm-mu-contacts-address-face ((t (:foreground ,darkblue-fg :background ,darkblue-bg))))
   `(helm-mu-contacts-name-face ((t (:foreground ,darkblue-fg :background ,darkblue-bg))))
;;;;; hl-line-mode
   `(hl-line ((,class (:background ,darkblue-bg05)) (t :weight bold)))
;;;;; linum-mode
   `(linum ((t (:foreground ,darkblue-red :background ,darkblue-bg))))
	))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'org-dark-blue)
