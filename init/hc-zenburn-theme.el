;;; hc-zenburn-theme.el --- An higher contrast version of the Zenburn theme.

;; Copyright (C)2016 Naoki Wakisaka

;; Author: Naoki Wakisaka

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; An higher contrast version of the Zenburn theme

;;; Credits:

;; Bozhidar Batsov <bozhidar@batsov.com> created the Zenburn theme
;; for emacs, which was a port of the vim theme made by Jani Nurminen.
;; His repository can be found at: https://github.com/bbatsov/zenburn-emacs

;; The original hc-zenburn-theme is at https:github.com/edran/hc-zenburn-emacs

;;; Code:

(deftheme org-hc-zenburn "An higher constrast Zenburn color theme")

;;; Color Palette

(defvar hc-zenburn-colors-alist
  '(("hc-zenburn-fg+1"     . "#FFFFEF")
    ("hc-zenburn-fg"       . "#ECECDC")
    ("hc-zenburn-fg-1"     . "#A0A08E")
    ("hc-zenburn-bg-2"     . "#000000")
    ("hc-zenburn-bg-1"     . "#050510")
    ("hc-zenburn-bg-05"    . "#101015")
    ("hc-zenburn-bg"       . "#151520")
    ("hc-zenburn-bg+05"    . "#202025")
    ("hc-zenburn-bg+1"     . "#252530")
    ("hc-zenburn-bg+2"     . "#303035")
    ("hc-zenburn-bg+3"     . "#353540")
		("hc-zenburn-bg+4"     . "#555560")
    ("hc-zenburn-red+1"    . "#E9B0B0")
    ("hc-zenburn-red"      . "#D9A0A0")
    ("hc-zenburn-red-1"    . "#C99090")
    ("hc-zenburn-red-2"    . "#B98080")
    ("hc-zenburn-red-3"    . "#A97070")
    ("hc-zenburn-red-4"    . "#996060")
    ("hc-zenburn-orange"   . "#ECBC9C")
    ("hc-zenburn-yellow"   . "#FDECBC")
    ("hc-zenburn-yellow-1" . "#EDDCAC")
    ("hc-zenburn-yellow-2" . "#AD9C6C")
    ("hc-zenburn-green-1"  . "#6C8C6C")
    ("hc-zenburn-green"    . "#8CAC8C")
    ("hc-zenburn-green+1"  . "#9CBF9C")
    ("hc-zenburn-green+2"  . "#ACD2AC")
    ("hc-zenburn-green+3"  . "#BCE5BC")
    ("hc-zenburn-green+4"  . "#CCF8CC")
    ("hc-zenburn-cyan"     . "#A0EDF0")
    ("hc-zenburn-blue+1"   . "#9CC7FB")
    ("hc-zenburn-blue"     . "#99DDE0")
    ("hc-zenburn-blue-1"   . "#89C5C8")
    ("hc-zenburn-blue-2"   . "#79ADB0")
    ("hc-zenburn-blue-3"   . "#699598")
    ("hc-zenburn-blue-4"   . "#597D80")
    ("hc-zenburn-blue-5"   . "#436D6D")
    ("hc-zenburn-magenta"  . "#E090C7"))
  "List of Hc-Zenburn colors.
Each element has the form (NAME . HEX).

`+N' suffixes indicate a color is lighter.
`-N' suffixes indicate a color is darker.")

(defmacro hc-zenburn-with-color-variables (&rest body)
  "`let' bind all colors defined in `hc-zenburn-colors-alist' around BODY.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   hc-zenburn-colors-alist))
     ,@body))

;;; Theme Faces
(hc-zenburn-with-color-variables
  (custom-theme-set-faces
   'org-hc-zenburn
;;;; Built-in
;;;;; basic coloring
   '(button ((t (:underline t))))
   `(default ((t (:foreground ,hc-zenburn-fg :background ,hc-zenburn-bg))))
   `(link ((t (:foreground ,hc-zenburn-yellow :underline t :weight bold))))
   `(link-visited ((t (:foreground ,hc-zenburn-yellow-2 :underline t :weight normal))))
   `(cursor ((t (:foreground ,hc-zenburn-fg :background ,hc-zenburn-fg+1))))
   `(escape-glyph ((t (:foreground ,hc-zenburn-yellow :bold t))))
   `(fringe ((t (:foreground ,hc-zenburn-fg :background ,hc-zenburn-bg+1))))
   `(header-line ((t (:foreground ,hc-zenburn-yellow
                                  :background ,hc-zenburn-bg-1
                                  :box (:line-width -1 :style released-button)))))
   `(highlight ((t (:background ,hc-zenburn-bg-05))))
   `(success ((t (:foreground ,hc-zenburn-green :weight bold))))
   `(warning ((t (:foreground ,hc-zenburn-orange :weight bold))))
;;;;; compilation
   `(compilation-column-face ((t (:foreground ,hc-zenburn-yellow))))
   `(compilation-enter-directory-face ((t (:foreground ,hc-zenburn-green))))
   `(compilation-error-face ((t (:foreground ,hc-zenburn-red-1 :weight bold :underline t))))
   `(compilation-face ((t (:foreground ,hc-zenburn-fg))))
   `(compilation-info-face ((t (:foreground ,hc-zenburn-blue))))
   `(compilation-info ((t (:foreground ,hc-zenburn-green+4 :underline t))))
   `(compilation-leave-directory-face ((t (:foreground ,hc-zenburn-green))))
   `(compilation-line-face ((t (:foreground ,hc-zenburn-yellow))))
   `(compilation-line-number ((t (:foreground ,hc-zenburn-yellow))))
   `(compilation-message-face ((t (:foreground ,hc-zenburn-blue))))
   `(compilation-warning-face ((t (:foreground ,hc-zenburn-orange :weight bold :underline t))))
   `(compilation-mode-line-exit ((t (:foreground ,hc-zenburn-green+2 :weight bold))))
   `(compilation-mode-line-fail ((t (:foreground ,hc-zenburn-red :weight bold))))
   `(compilation-mode-line-run ((t (:foreground ,hc-zenburn-yellow :weight bold))))
;;;;; grep
   `(grep-context-face ((t (:foreground ,hc-zenburn-fg))))
   `(grep-error-face ((t (:foreground ,hc-zenburn-red-1 :weight bold :underline t))))
   `(grep-hit-face ((t (:foreground ,hc-zenburn-blue))))
   `(grep-match-face ((t (:foreground ,hc-zenburn-orange :weight bold))))
   `(match ((t (:background ,hc-zenburn-bg-1 :foreground ,hc-zenburn-orange :weight bold))))
;;;;; neotree
   `(neo-file-link-face ((t (:background ,hc-zenburn-bg :foreground ,hc-zenburn-fg))))
   `(neo-root-dir-face ((t (:foreground ,hc-zenburn-blue :weight bold))))
   `(neo-dir-link-face ((t (:foreground ,hc-zenburn-green+1))))
;;;;; isearch
   `(isearch ((t (:foreground ,hc-zenburn-yellow-2 :weight bold :background ,hc-zenburn-bg+4))))
   `(isearch-fail ((t (:foreground ,hc-zenburn-fg :background ,hc-zenburn-red-4))))
   ;; `(lazy-highlight ((t (:foreground ,hc-zenburn-yellow-2 :weight bold :background ,hc-zenburn-bg-05))))
	 `(lazy-highlight ((t (:weight bold :background ,hc-zenburn-yellow-2))))

   `(menu ((t (:foreground ,hc-zenburn-fg :background ,hc-zenburn-bg))))
   `(minibuffer-prompt ((t (:foreground ,hc-zenburn-yellow))))
   `(mode-line
     ((,class (:foreground ,hc-zenburn-green+1
                           :background ,hc-zenburn-bg-1
                           :box (:line-width -1 :style released-button)))
      (t :inverse-video t)))
   `(mode-line-buffer-id ((t (:foreground ,hc-zenburn-yellow :weight bold))))
   `(mode-line-inactive
     ((t (:foreground ,hc-zenburn-green-1
                      :background ,hc-zenburn-bg-05
                      :box (:line-width -1 :style released-button)))))
   `(region ((,class (:background ,hc-zenburn-blue-5))
             (t :inverse-video t)))
   `(secondary-selection ((t (:background ,hc-zenburn-bg+2))))
   `(trailing-whitespace ((t (:background ,hc-zenburn-red))))
   `(vertical-border ((t (:foreground ,hc-zenburn-fg))))
;;;;; font lock
   `(font-lock-builtin-face ((t (:foreground ,hc-zenburn-green+2 :weight bold))))
	 ;; `(font-lock-builtin-face ((t (:foreground ,hc-zenburn-fg :weight bold))))
   `(font-lock-comment-face ((t (:foreground ,hc-zenburn-green))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,hc-zenburn-green-1))))
   `(font-lock-constant-face ((t (:foreground ,hc-zenburn-green+4))))
   `(font-lock-doc-face ((t (:foreground ,hc-zenburn-green+2))))
   `(font-lock-function-name-face ((t (:foreground ,hc-zenburn-cyan))))
   `(font-lock-keyword-face ((t (:foreground ,hc-zenburn-yellow :weight bold))))
   `(font-lock-negation-char-face ((t (:foreground ,hc-zenburn-yellow :weight bold))))
   `(font-lock-preprocessor-face ((t (:foreground ,hc-zenburn-blue+1))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,hc-zenburn-yellow :weight bold))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,hc-zenburn-green :weight bold))))
   `(font-lock-string-face ((t (:foreground ,hc-zenburn-red))))
   `(font-lock-type-face ((t (:foreground ,hc-zenburn-blue-1))))
   `(font-lock-variable-name-face ((t (:foreground ,hc-zenburn-orange))))
   `(font-lock-warning-face ((t (:foreground ,hc-zenburn-yellow-2 :weight bold))))

   `(c-annotation-face ((t (:inherit font-lock-constant-face))))
;;;;; newsticker
   `(newsticker-date-face ((t (:foreground ,hc-zenburn-fg))))
   `(newsticker-default-face ((t (:foreground ,hc-zenburn-fg))))
   `(newsticker-enclosure-face ((t (:foreground ,hc-zenburn-green+3))))
   `(newsticker-extra-face ((t (:foreground ,hc-zenburn-bg+2 :height 0.8))))
   `(newsticker-feed-face ((t (:foreground ,hc-zenburn-fg))))
   `(newsticker-immortal-item-face ((t (:foreground ,hc-zenburn-green))))
   `(newsticker-new-item-face ((t (:foreground ,hc-zenburn-blue))))
   `(newsticker-obsolete-item-face ((t (:foreground ,hc-zenburn-red))))
   `(newsticker-old-item-face ((t (:foreground ,hc-zenburn-bg+3))))
   `(newsticker-statistics-face ((t (:foreground ,hc-zenburn-fg))))
   `(newsticker-treeview-face ((t (:foreground ,hc-zenburn-fg))))
   `(newsticker-treeview-immortal-face ((t (:foreground ,hc-zenburn-green))))
   `(newsticker-treeview-listwindow-face ((t (:foreground ,hc-zenburn-fg))))
   `(newsticker-treeview-new-face ((t (:foreground ,hc-zenburn-blue :weight bold))))
   `(newsticker-treeview-obsolete-face ((t (:foreground ,hc-zenburn-red))))
   `(newsticker-treeview-old-face ((t (:foreground ,hc-zenburn-bg+3))))
   `(newsticker-treeview-selection-face ((t (:background ,hc-zenburn-bg-1 :foreground ,hc-zenburn-yellow))))
;;;; Third-party
;;;;; auto-complete
   `(ac-candidate-face ((t (:background ,hc-zenburn-bg+3 :foreground ,hc-zenburn-bg-2))))
   `(ac-selection-face ((t (:background ,hc-zenburn-blue-4 :foreground ,hc-zenburn-fg))))
   `(popup-tip-face ((t (:background ,hc-zenburn-yellow-2 :foreground ,hc-zenburn-bg-2))))
   `(popup-scroll-bar-foreground-face ((t (:background ,hc-zenburn-blue-5))))
   `(popup-scroll-bar-background-face ((t (:background ,hc-zenburn-bg-1))))
   `(popup-isearch-match ((t (:background ,hc-zenburn-bg :foreground ,hc-zenburn-fg))))
;;;;; diff
   `(diff-added ((,class (:foreground ,hc-zenburn-green+4 :background nil))
                 (t (:foreground ,hc-zenburn-green-1 :background nil))))
   `(diff-changed ((t (:foreground ,hc-zenburn-yellow))))
   `(diff-removed ((,class (:foreground ,hc-zenburn-red :background nil))
                   (t (:foreground ,hc-zenburn-red-3 :background nil))))
   `(diff-refine-added ((t (:inherit diff-added :weight bold))))
   `(diff-refine-change ((t (:inherit diff-changed :weight bold))))
   `(diff-refine-removed ((t (:inherit diff-removed :weight bold))))
   `(diff-header ((,class (:background ,hc-zenburn-bg+2))
                  (t (:background ,hc-zenburn-fg :foreground ,hc-zenburn-bg))))
   `(diff-file-header
     ((,class (:background ,hc-zenburn-bg+2 :foreground ,hc-zenburn-fg :bold t))
      (t (:background ,hc-zenburn-fg :foreground ,hc-zenburn-bg :bold t))))
;;;;; diff-hl
   `(diff-hl-change ((,class (:foreground ,hc-zenburn-blue-2 :background ,hc-zenburn-bg-05))))
   `(diff-hl-delete ((,class (:foreground ,hc-zenburn-red+1 :background ,hc-zenburn-bg-05))))
   `(diff-hl-insert ((,class (:foreground ,hc-zenburn-green+1 :background ,hc-zenburn-bg-05))))
   `(diff-hl-unknown ((,class (:foreground ,hc-zenburn-yellow :background ,hc-zenburn-bg-05))))
;;;;; dired+
   `(diredp-display-msg ((t (:foreground ,hc-zenburn-blue))))
   `(diredp-compressed-file-suffix ((t (:foreground ,hc-zenburn-orange))))
   `(diredp-date-time ((t (:foreground ,hc-zenburn-magenta))))
   `(diredp-deletion ((t (:foreground ,hc-zenburn-yellow))))
   `(diredp-deletion-file-name ((t (:foreground ,hc-zenburn-red))))
   `(diredp-dir-heading ((t (:foreground ,hc-zenburn-blue :background ,hc-zenburn-bg-1))))
   `(diredp-dir-priv ((t (:foreground ,hc-zenburn-cyan))))
   `(diredp-exec-priv ((t (:foreground ,hc-zenburn-red))))
   `(diredp-executable-tag ((t (:foreground ,hc-zenburn-green+1))))
   `(diredp-file-name ((t (:foreground ,hc-zenburn-blue))))
   `(diredp-file-suffix ((t (:foreground ,hc-zenburn-green))))
   `(diredp-flag-mark ((t (:foreground ,hc-zenburn-yellow))))
   `(diredp-flag-mark-line ((t (:foreground ,hc-zenburn-orange))))
   `(diredp-ignored-file-name ((t (:foreground ,hc-zenburn-red))))
   `(diredp-link-priv ((t (:foreground ,hc-zenburn-yellow))))
   `(diredp-mode-line-flagged ((t (:foreground ,hc-zenburn-yellow))))
   `(diredp-mode-line-marked ((t (:foreground ,hc-zenburn-orange))))
   `(diredp-no-priv ((t (:foreground ,hc-zenburn-fg))))
   `(diredp-number ((t (:foreground ,hc-zenburn-green+1))))
   `(diredp-other-priv ((t (:foreground ,hc-zenburn-yellow-1))))
   `(diredp-rare-priv ((t (:foreground ,hc-zenburn-red-1))))
   `(diredp-read-priv ((t (:foreground ,hc-zenburn-green-1))))
   `(diredp-symlink ((t (:foreground ,hc-zenburn-yellow))))
   `(diredp-write-priv ((t (:foreground ,hc-zenburn-magenta))))
;;;;; flycheck
   `(flycheck-error
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,hc-zenburn-red-1) :inherit unspecified))
      (t (:foreground ,hc-zenburn-red-1 :weight bold :underline t))))
   `(flycheck-warning
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,hc-zenburn-yellow) :inherit unspecified))
      (t (:foreground ,hc-zenburn-yellow :weight bold :underline t))))
   `(flycheck-info
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,hc-zenburn-cyan) :inherit unspecified))
      (t (:foreground ,hc-zenburn-cyan :weight bold :underline t))))
   `(flycheck-fringe-error ((t (:foreground ,hc-zenburn-red-1 :weight bold))))
   `(flycheck-fringe-warning ((t (:foreground ,hc-zenburn-yellow :weight bold))))
   `(flycheck-fringe-info ((t (:foreground ,hc-zenburn-cyan :weight bold))))
;;;;; gnus
   `(gnus-group-mail-1 ((t (:bold t :inherit gnus-group-mail-1-empty))))
   `(gnus-group-mail-1-empty ((t (:inherit gnus-group-news-1-empty))))
   `(gnus-group-mail-2 ((t (:bold t :inherit gnus-group-mail-2-empty))))
   `(gnus-group-mail-2-empty ((t (:inherit gnus-group-news-2-empty))))
   `(gnus-group-mail-3 ((t (:bold t :inherit gnus-group-mail-3-empty))))
   `(gnus-group-mail-3-empty ((t (:inherit gnus-group-news-3-empty))))
   `(gnus-group-mail-4 ((t (:bold t :inherit gnus-group-mail-4-empty))))
   `(gnus-group-mail-4-empty ((t (:inherit gnus-group-news-4-empty))))
   `(gnus-group-mail-5 ((t (:bold t :inherit gnus-group-mail-5-empty))))
   `(gnus-group-mail-5-empty ((t (:inherit gnus-group-news-5-empty))))
   `(gnus-group-mail-6 ((t (:bold t :inherit gnus-group-mail-6-empty))))
   `(gnus-group-mail-6-empty ((t (:inherit gnus-group-news-6-empty))))
   `(gnus-group-mail-low ((t (:bold t :inherit gnus-group-mail-low-empty))))
   `(gnus-group-mail-low-empty ((t (:inherit gnus-group-news-low-empty))))
   `(gnus-group-news-1 ((t (:bold t :inherit gnus-group-news-1-empty))))
   `(gnus-group-news-2 ((t (:bold t :inherit gnus-group-news-2-empty))))
   `(gnus-group-news-3 ((t (:bold t :inherit gnus-group-news-3-empty))))
   `(gnus-group-news-4 ((t (:bold t :inherit gnus-group-news-4-empty))))
   `(gnus-group-news-5 ((t (:bold t :inherit gnus-group-news-5-empty))))
   `(gnus-group-news-6 ((t (:bold t :inherit gnus-group-news-6-empty))))
   `(gnus-group-news-low ((t (:bold t :inherit gnus-group-news-low-empty))))
   `(gnus-header-content ((t (:inherit message-header-other))))
   `(gnus-header-from ((t (:inherit message-header-from))))
   `(gnus-header-name ((t (:inherit message-header-name))))
   `(gnus-header-newsgroups ((t (:inherit message-header-other))))
   `(gnus-header-subject ((t (:inherit message-header-subject))))
   `(gnus-summary-cancelled ((t (:foreground ,hc-zenburn-orange))))
   `(gnus-summary-high-ancient ((t (:foreground ,hc-zenburn-blue))))
   `(gnus-summary-high-read ((t (:foreground ,hc-zenburn-green :weight bold))))
   `(gnus-summary-high-ticked ((t (:foreground ,hc-zenburn-orange :weight bold))))
   `(gnus-summary-high-unread ((t (:foreground ,hc-zenburn-fg :weight bold))))
   `(gnus-summary-low-ancient ((t (:foreground ,hc-zenburn-blue))))
   `(gnus-summary-low-read ((t (:foreground ,hc-zenburn-green))))
   `(gnus-summary-low-ticked ((t (:foreground ,hc-zenburn-orange :weight bold))))
   `(gnus-summary-low-unread ((t (:foreground ,hc-zenburn-fg))))
   `(gnus-summary-normal-ancient ((t (:foreground ,hc-zenburn-blue))))
   `(gnus-summary-normal-read ((t (:foreground ,hc-zenburn-green))))
   `(gnus-summary-normal-ticked ((t (:foreground ,hc-zenburn-orange :weight bold))))
   `(gnus-summary-normal-unread ((t (:foreground ,hc-zenburn-fg))))
   `(gnus-summary-selected ((t (:foreground ,hc-zenburn-yellow :weight bold))))
   `(gnus-cite-1 ((t (:foreground ,hc-zenburn-blue))))
   `(gnus-cite-10 ((t (:foreground ,hc-zenburn-yellow-1))))
   `(gnus-cite-11 ((t (:foreground ,hc-zenburn-yellow))))
   `(gnus-cite-2 ((t (:foreground ,hc-zenburn-blue-1))))
   `(gnus-cite-3 ((t (:foreground ,hc-zenburn-blue-2))))
   `(gnus-cite-4 ((t (:foreground ,hc-zenburn-green+2))))
   `(gnus-cite-5 ((t (:foreground ,hc-zenburn-green+1))))
   `(gnus-cite-6 ((t (:foreground ,hc-zenburn-green))))
   `(gnus-cite-7 ((t (:foreground ,hc-zenburn-red))))
   `(gnus-cite-8 ((t (:foreground ,hc-zenburn-red-1))))
   `(gnus-cite-9 ((t (:foreground ,hc-zenburn-red-2))))
   `(gnus-group-news-1-empty ((t (:foreground ,hc-zenburn-yellow))))
   `(gnus-group-news-2-empty ((t (:foreground ,hc-zenburn-green+3))))
   `(gnus-group-news-3-empty ((t (:foreground ,hc-zenburn-green+1))))
   `(gnus-group-news-4-empty ((t (:foreground ,hc-zenburn-blue-2))))
   `(gnus-group-news-5-empty ((t (:foreground ,hc-zenburn-blue-3))))
   `(gnus-group-news-6-empty ((t (:foreground ,hc-zenburn-bg+2))))
   `(gnus-group-news-low-empty ((t (:foreground ,hc-zenburn-bg+2))))
   `(gnus-signature ((t (:foreground ,hc-zenburn-yellow))))
   `(gnus-x ((t (:background ,hc-zenburn-fg :foreground ,hc-zenburn-bg))))
;;;;; guide-key
   `(guide-key/highlight-command-face ((t (:foreground ,hc-zenburn-blue))))
   `(guide-key/key-face ((t (:foreground ,hc-zenburn-green))))
   `(guide-key/prefix-command-face ((t (:foreground ,hc-zenburn-green+1))))
;;;;; helm
   `(helm-header
     ((t (:foreground ,hc-zenburn-green
                      :background ,hc-zenburn-bg
                      :underline nil
                      :box nil))))
   `(helm-source-header
     ((t (:foreground ,hc-zenburn-yellow
                      :background ,hc-zenburn-bg-1
                      :underline nil
                      :weight bold
                      :box (:line-width -1 :style released-button)))))
   `(helm-selection ((t (:background ,hc-zenburn-bg+1 :underline nil))))
   `(helm-selection-line ((t (:background ,hc-zenburn-bg+1))))
   `(helm-visible-mark ((t (:foreground ,hc-zenburn-bg :background ,hc-zenburn-yellow-2))))
   `(helm-candidate-number ((t (:foreground ,hc-zenburn-green+4 :background ,hc-zenburn-bg-1))))
   `(helm-separator ((t (:foreground ,hc-zenburn-red :background ,hc-zenburn-bg))))
   `(helm-time-zone-current ((t (:foreground ,hc-zenburn-green+2 :background ,hc-zenburn-bg))))
   `(helm-time-zone-home ((t (:foreground ,hc-zenburn-red :background ,hc-zenburn-bg))))
   `(helm-bookmark-addressbook ((t (:foreground ,hc-zenburn-orange :background ,hc-zenburn-bg))))
   `(helm-bookmark-directory ((t (:foreground nil :background nil :inherit helm-ff-directory))))
   `(helm-bookmark-file ((t (:foreground nil :background nil :inherit helm-ff-file))))
   `(helm-bookmark-gnus ((t (:foreground ,hc-zenburn-magenta :background ,hc-zenburn-bg))))
   `(helm-bookmark-info ((t (:foreground ,hc-zenburn-green+2 :background ,hc-zenburn-bg))))
   `(helm-bookmark-man ((t (:foreground ,hc-zenburn-yellow :background ,hc-zenburn-bg))))
   `(helm-bookmark-w3m ((t (:foreground ,hc-zenburn-magenta :background ,hc-zenburn-bg))))
   `(helm-buffer-not-saved ((t (:foreground ,hc-zenburn-red :background ,hc-zenburn-bg))))
   `(helm-buffer-process ((t (:foreground ,hc-zenburn-cyan :background ,hc-zenburn-bg))))
   `(helm-buffer-saved-out ((t (:foreground ,hc-zenburn-fg :background ,hc-zenburn-bg))))
   `(helm-buffer-size ((t (:foreground ,hc-zenburn-fg-1 :background ,hc-zenburn-bg))))
   `(helm-ff-directory ((t (:foreground ,hc-zenburn-cyan :background ,hc-zenburn-bg :weight bold))))
   `(helm-ff-file ((t (:foreground ,hc-zenburn-fg :background ,hc-zenburn-bg :weight normal))))
   `(helm-ff-executable ((t (:foreground ,hc-zenburn-green+2 :background ,hc-zenburn-bg :weight normal))))
   `(helm-ff-invalid-symlink ((t (:foreground ,hc-zenburn-red :background ,hc-zenburn-bg :weight bold))))
   `(helm-ff-symlink ((t (:foreground ,hc-zenburn-yellow :background ,hc-zenburn-bg :weight bold))))
   `(helm-ff-prefix ((t (:foreground ,hc-zenburn-bg :background ,hc-zenburn-yellow :weight normal))))
   `(helm-grep-cmd-line ((t (:foreground ,hc-zenburn-cyan :background ,hc-zenburn-bg))))
   `(helm-grep-file ((t (:foreground ,hc-zenburn-fg :background ,hc-zenburn-bg))))
   `(helm-grep-finish ((t (:foreground ,hc-zenburn-green+2 :background ,hc-zenburn-bg))))
   `(helm-grep-lineno ((t (:foreground ,hc-zenburn-fg-1 :background ,hc-zenburn-bg))))
   `(helm-grep-match ((t (:foreground nil :background nil :inherit helm-match))))
   `(helm-grep-running ((t (:foreground ,hc-zenburn-red :background ,hc-zenburn-bg))))
   `(helm-moccur-buffer ((t (:foreground ,hc-zenburn-cyan :background ,hc-zenburn-bg))))
   `(helm-mu-contacts-address-face ((t (:foreground ,hc-zenburn-fg-1 :background ,hc-zenburn-bg))))
   `(helm-mu-contacts-name-face ((t (:foreground ,hc-zenburn-fg :background ,hc-zenburn-bg))))
;;;;; hl-line-mode
   `(hl-line-face ((,class (:background ,hc-zenburn-bg-05))
                   (t :weight bold)))
   `(hl-line ((,class (:background ,hc-zenburn-bg-05)) ; old emacsen
              (t :weight bold)))
;;;;; linum-mode
   `(linum ((t (:foreground ,hc-zenburn-green+2 :background ,hc-zenburn-bg))))
;;;;; magit
;;;;;; headings and diffs
   `(magit-section-highlight           ((t (:background ,hc-zenburn-bg+05))))
   `(magit-section-heading             ((t (:foreground ,hc-zenburn-yellow :weight bold))))
   `(magit-section-heading-selection   ((t (:foreground ,hc-zenburn-orange :weight bold))))
   `(magit-diff-file-heading           ((t (:weight bold))))
   `(magit-diff-file-heading-highlight ((t (:background ,hc-zenburn-bg+05  :weight bold))))
   `(magit-diff-file-heading-selection ((t (:background ,hc-zenburn-bg+05
                                            :foreground ,hc-zenburn-orange :weight bold))))
   `(magit-diff-hunk-heading           ((t (:background ,hc-zenburn-bg+1))))
   `(magit-diff-hunk-heading-highlight ((t (:background ,hc-zenburn-bg+2))))
   `(magit-diff-hunk-heading-selection ((t (:background ,hc-zenburn-bg+2
                                            :foreground ,hc-zenburn-orange))))
   `(magit-diff-lines-heading          ((t (:background ,hc-zenburn-orange
                                            :foreground ,hc-zenburn-bg+2))))
   `(magit-diff-context-highlight      ((t (:background ,hc-zenburn-bg+05
                                            :foreground "grey70"))))
   `(magit-diffstat-added   ((t (:foreground ,hc-zenburn-green+4))))
   `(magit-diffstat-removed ((t (:foreground ,hc-zenburn-red))))
;;;;;; popup
   `(magit-popup-heading             ((t (:foreground ,hc-zenburn-yellow  :weight bold))))
   `(magit-popup-key                 ((t (:foreground ,hc-zenburn-green-1 :weight bold))))
   `(magit-popup-argument            ((t (:foreground ,hc-zenburn-green   :weight bold))))
   `(magit-popup-disabled-argument   ((t (:foreground ,hc-zenburn-fg-1    :weight normal))))
   `(magit-popup-option-value        ((t (:foreground ,hc-zenburn-blue-2  :weight bold))))
;;;;;; process
   `(magit-process-ok    ((t (:foreground ,hc-zenburn-green  :weight bold))))
   `(magit-process-ng    ((t (:foreground ,hc-zenburn-red    :weight bold))))
;;;;;; log
   `(magit-log-author    ((t (:foreground ,hc-zenburn-orange))))
   `(magit-log-date      ((t (:foreground ,hc-zenburn-fg-1))))
   `(magit-log-graph     ((t (:foreground ,hc-zenburn-fg+1))))
;;;;;; sequence
   `(magit-sequence-pick ((t (:foreground ,hc-zenburn-yellow-2))))
   `(magit-sequence-stop ((t (:foreground ,hc-zenburn-green))))
   `(magit-sequence-part ((t (:foreground ,hc-zenburn-yellow))))
   `(magit-sequence-head ((t (:foreground ,hc-zenburn-blue))))
   `(magit-sequence-drop ((t (:foreground ,hc-zenburn-red))))
   `(magit-sequence-done ((t (:foreground ,hc-zenburn-fg-1))))
   `(magit-sequence-onto ((t (:foreground ,hc-zenburn-fg-1))))
;;;;;; bisect
   `(magit-bisect-good ((t (:foreground ,hc-zenburn-green))))
   `(magit-bisect-skip ((t (:foreground ,hc-zenburn-yellow))))
   `(magit-bisect-bad  ((t (:foreground ,hc-zenburn-red))))
;;;;;; blame
   `(magit-blame-heading ((t (:background ,hc-zenburn-bg-1 :foreground ,hc-zenburn-blue-2))))
   `(magit-blame-hash    ((t (:background ,hc-zenburn-bg-1 :foreground ,hc-zenburn-blue-2))))
   `(magit-blame-name    ((t (:background ,hc-zenburn-bg-1 :foreground ,hc-zenburn-orange))))
   `(magit-blame-date    ((t (:background ,hc-zenburn-bg-1 :foreground ,hc-zenburn-orange))))
   `(magit-blame-summary ((t (:background ,hc-zenburn-bg-1 :foreground ,hc-zenburn-blue-2
                                          :weight bold))))
;;;;;; references etc
   `(magit-dimmed         ((t (:foreground ,hc-zenburn-bg+3))))
   `(magit-hash           ((t (:foreground ,hc-zenburn-bg+3))))
   `(magit-tag            ((t (:foreground ,hc-zenburn-orange :weight bold))))
   `(magit-branch-remote  ((t (:foreground ,hc-zenburn-green  :weight bold))))
   `(magit-branch-local   ((t (:foreground ,hc-zenburn-blue   :weight bold))))
   `(magit-branch-current ((t (:foreground ,hc-zenburn-blue   :weight bold :box t))))
   `(magit-head           ((t (:foreground ,hc-zenburn-blue   :weight bold))))
   `(magit-refname        ((t (:background ,hc-zenburn-bg+2 :foreground ,hc-zenburn-fg :weight bold))))
   `(magit-refname-stash  ((t (:background ,hc-zenburn-bg+2 :foreground ,hc-zenburn-fg :weight bold))))
   `(magit-refname-wip    ((t (:background ,hc-zenburn-bg+2 :foreground ,hc-zenburn-fg :weight bold))))
   `(magit-signature-good      ((t (:foreground ,hc-zenburn-green))))
   `(magit-signature-bad       ((t (:foreground ,hc-zenburn-red))))
   `(magit-signature-untrusted ((t (:foreground ,hc-zenburn-yellow))))
   `(magit-cherry-unmatched    ((t (:foreground ,hc-zenburn-cyan))))
   `(magit-cherry-equivalent   ((t (:foreground ,hc-zenburn-magenta))))
   `(magit-reflog-commit       ((t (:foreground ,hc-zenburn-green))))
   `(magit-reflog-amend        ((t (:foreground ,hc-zenburn-magenta))))
   `(magit-reflog-merge        ((t (:foreground ,hc-zenburn-green))))
   `(magit-reflog-checkout     ((t (:foreground ,hc-zenburn-blue))))
   `(magit-reflog-reset        ((t (:foreground ,hc-zenburn-red))))
   `(magit-reflog-rebase       ((t (:foreground ,hc-zenburn-magenta))))
   `(magit-reflog-cherry-pick  ((t (:foreground ,hc-zenburn-green))))
   `(magit-reflog-remote       ((t (:foreground ,hc-zenburn-cyan))))
   `(magit-reflog-other        ((t (:foreground ,hc-zenburn-cyan))))
;;;;; org-mode
   `(org-agenda-date-today
     ((t (:foreground ,hc-zenburn-fg+1 :slant italic :weight bold))) t)
   `(org-agenda-structure
     ((t (:inherit font-lock-comment-face))))
   `(org-archived ((t (:foreground ,hc-zenburn-fg :weight bold))))
   `(org-checkbox ((t (:background ,hc-zenburn-bg+2 :foreground ,hc-zenburn-fg+1
                                   :box (:line-width 1 :style released-button)))))
   `(org-date ((t (:foreground ,hc-zenburn-blue :underline t))))
   `(org-deadline-announce ((t (:foreground ,hc-zenburn-red-1))))
   `(org-done ((t (:bold t :weight bold :foreground ,hc-zenburn-green+3))))
   `(org-formula ((t (:foreground ,hc-zenburn-yellow-2))))
   `(org-headline-done ((t (:foreground ,hc-zenburn-green+3))))
   `(org-hide ((t (:foreground ,hc-zenburn-bg-1))))
   `(org-level-1 ((t (:foreground ,hc-zenburn-orange))))
   `(org-level-2 ((t (:foreground ,hc-zenburn-green+4))))
   `(org-level-3 ((t (:foreground ,hc-zenburn-blue-1))))
   `(org-level-4 ((t (:foreground ,hc-zenburn-yellow-2))))
   `(org-level-5 ((t (:foreground ,hc-zenburn-cyan))))
   `(org-level-6 ((t (:foreground ,hc-zenburn-green+2))))
   `(org-level-7 ((t (:foreground ,hc-zenburn-red-4))))
   `(org-level-8 ((t (:foreground ,hc-zenburn-blue-4))))
   `(org-link ((t (:foreground ,hc-zenburn-yellow-2 :underline t))))
   `(org-scheduled ((t (:foreground ,hc-zenburn-green+4))))
   `(org-scheduled-previously ((t (:foreground ,hc-zenburn-red))))
   `(org-scheduled-today ((t (:foreground ,hc-zenburn-blue+1))))
   `(org-sexp-date ((t (:foreground ,hc-zenburn-blue+1 :underline t))))
   `(org-special-keyword ((t (:inherit font-lock-comment-face))))
   `(org-table ((t (:foreground ,hc-zenburn-green+2))))
   `(org-tag ((t (:bold t :weight bold))))
   `(org-time-grid ((t (:foreground ,hc-zenburn-orange))))
   `(org-todo ((t (:bold t :foreground ,hc-zenburn-red :weight bold))))
   `(org-upcoming-deadline ((t (:inherit font-lock-keyword-face))))
   `(org-warning ((t (:bold t :foreground ,hc-zenburn-red :weight bold :underline nil))))
   `(org-column ((t (:background ,hc-zenburn-bg-1))))
   `(org-column-title ((t (:background ,hc-zenburn-bg-1 :underline t :weight bold))))
   `(org-mode-line-clock ((t (:foreground ,hc-zenburn-fg :background ,hc-zenburn-bg-1))))
   `(org-mode-line-clock-overrun ((t (:foreground ,hc-zenburn-bg :background ,hc-zenburn-red-1))))
   `(org-ellipsis ((t (:foreground ,hc-zenburn-yellow-1 :underline t))))
   `(org-footnote ((t (:foreground ,hc-zenburn-cyan :underline t))))
;;;;; powerline
   `(powerline-active1 ((t (:background ,hc-zenburn-bg-05 :inherit mode-line))))
   `(powerline-active2 ((t (:background ,hc-zenburn-bg+2 :inherit mode-line))))
   `(powerline-inactive1 ((t (:background ,hc-zenburn-bg+1 :inherit mode-line-inactive))))
   `(powerline-inactive2 ((t (:background ,hc-zenburn-bg+3 :inherit mode-line-inactive))))
;;;;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,hc-zenburn-fg))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,hc-zenburn-green+4))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,hc-zenburn-yellow-2))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,hc-zenburn-cyan))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,hc-zenburn-green+2))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,hc-zenburn-blue+1))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,hc-zenburn-yellow-1))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,hc-zenburn-green+1))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,hc-zenburn-blue-2))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,hc-zenburn-orange))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground ,hc-zenburn-green))))
   `(rainbow-delimiters-depth-12-face ((t (:foreground ,hc-zenburn-blue-5))))
;;;;; term
   `(term-color-black ((t (:foreground ,hc-zenburn-bg
                                       :background ,hc-zenburn-bg-1))))
   `(term-color-red ((t (:foreground ,hc-zenburn-red-2
                                       :background ,hc-zenburn-red-4))))
   `(term-color-green ((t (:foreground ,hc-zenburn-green
                                       :background ,hc-zenburn-green+2))))
   `(term-color-yellow ((t (:foreground ,hc-zenburn-orange
                                       :background ,hc-zenburn-yellow))))
   `(term-color-blue ((t (:foreground ,hc-zenburn-blue-1
                                      :background ,hc-zenburn-blue-4))))
   `(term-color-magenta ((t (:foreground ,hc-zenburn-magenta
                                         :background ,hc-zenburn-red))))
   `(term-color-cyan ((t (:foreground ,hc-zenburn-cyan
                                       :background ,hc-zenburn-blue))))
   `(term-color-white ((t (:foreground ,hc-zenburn-fg
                                       :background ,hc-zenburn-fg-1))))
   '(term-default-fg-color ((t (:inherit term-color-white))))
   '(term-default-bg-color ((t (:inherit term-color-black))))
;;;;; whitespace-mode
   `(whitespace-space ((t (:background ,hc-zenburn-bg+1 :foreground ,hc-zenburn-bg+1))))
   `(whitespace-hspace ((t (:background ,hc-zenburn-bg+1 :foreground ,hc-zenburn-bg+1))))
   `(whitespace-tab ((t (:background ,hc-zenburn-red-1))))
   `(whitespace-newline ((t (:foreground ,hc-zenburn-bg+1))))
   `(whitespace-trailing ((t (:background ,hc-zenburn-red))))
   `(whitespace-line ((t (:background ,hc-zenburn-bg :foreground ,hc-zenburn-magenta))))
   `(whitespace-space-before-tab ((t (:background ,hc-zenburn-orange :foreground ,hc-zenburn-orange))))
   `(whitespace-indentation ((t (:background ,hc-zenburn-yellow :foreground ,hc-zenburn-red))))
   `(whitespace-empty ((t (:background ,hc-zenburn-yellow))))
   `(whitespace-space-after-tab ((t (:background ,hc-zenburn-yellow :foreground ,hc-zenburn-red))))
	 ))

;;; Theme Variables
(hc-zenburn-with-color-variables
  (custom-theme-set-variables
   'hc-zenburn
;;;;; ansi-color
   `(ansi-color-names-vector [,hc-zenburn-bg ,hc-zenburn-red ,hc-zenburn-green ,hc-zenburn-yellow
                                          ,hc-zenburn-blue ,hc-zenburn-magenta ,hc-zenburn-cyan ,hc-zenburn-fg])
;;;;; fill-column-indicator
   `(fci-rule-color ,hc-zenburn-bg+3))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'org-hc-zenburn)

;;; hc-zenburn-theme.el ends here
