;;; yatex-mode
(el-get-bundle yatex)

(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
(setq auto-mode-alist
			(append  '(("\\.tex$" . yatex-mode)
								 ("\\.ltx$" . yatex-mode)
								 ("\\.cls$" . yatex-mode)
								 ("\\.sty$" . yatex-mode)
								 ("\\.clo$" . yatex-mode)
								 ("\\.bbl$" . yatex-mode)) auto-mode-alist))
(setq YaTeX-inhibit-prefix-letter t)
(setq YaTeX-kanji-code nil)
(defvar YaTeX-latex-message-code 'utf-8)
(defvar YaTeX-use-LaTeX2e t)
(defvar YaTeX-use-AMS-LaTeX t)
(defvar tex-command "platex")
(defvar dvi2-command "xdvi")

;;YaTeX old key bindings
(setq YaTeX-inhibit-prefix-letter nil)

;; enable RefTeX with YaTeX
(add-hook 'yatex-mode-hook '(lambda () (reftex-mode 1)))
(defvar reftex-enable-partial-scans t)
(defvar reftex-save-parse-info t)
(defvar reftex-use-multiple-selection-buffers t)
;; use \eqref for citation of formulas
(defvar reftex-label-alist '((nil ?e nil "~\\eqref{%s}" nil nil)))

;; Change key bindings
(add-hook 'reftex-mode-hook
 '(lambda ()
               (define-key reftex-mode-map (kbd "\C-cr") 'reftex-reference)
               (define-key reftex-mode-map (kbd "\C-cl") 'reftex-label)
               (define-key reftex-mode-map (kbd "\C-cc") 'reftex-citation)
))
