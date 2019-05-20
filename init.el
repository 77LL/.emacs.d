;;; init.el --- Emacs initial setting

;; Copyright (C) 2016 by 77LL

;; Author: 77LL <natrusu.mi.lab@gmail.com>
;; Version: 0.01

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

;;; Code:
;; to use a other setting file
;; useage: emacs -l filename

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))
;; (add-to-list 'load-path user-emacs-directory)

;; locate-user-emacs-file gets absolute file name and if not, create it
(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path (locate-user-emacs-file "recipes"))
(el-get 'sync)

;; load init files
(el-get-bundle! init-loader
  ;; load
  (setq-default init-loader-show-log-after-init nil
                init-loader-byte-compile t)
  (init-loader-load (locate-user-emacs-file "init-loader"))

  ;; hide compilation results
  (let ((win (get-buffer-window "*Compile-Log*")))
    (when win (delete-window win))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-auto-start 2)
 '(ac-dictionary-directories "/home/wakisaka/.emacs.d/ac-dict")
 '(ac-quick-help-delay 1.0)
 '(ac-use-fuzzy t)
 '(ac-use-menu-map t)
 '(backup-inhibited t t)
 '(delete-auto-save-files t)
 '(flycheck-display-errors-delay 0.5)
 '(flycheck-idle-change-delay 1.0)
 '(frame-title-format (format "%%f") t)
 '(helm-gtags-auto-update t)
 '(helm-gtags-ignore-case t)
 '(helm-gtags-path-style (quote relative))
 '(inhibit-startup-screen t)
 '(kill-whole-line t)
 '(linum-format "%4d")
 '(make-backup-files nil)
 '(package-selected-packages (quote (mozc)))
 '(ring-bell-function (quote ignore))
 '(show-paren-delay 0)
 '(show-paren-style (quote mixed))
 '(use-dialog-box nil)
 '(visible-bell t)
 '(yank-excluded-properties t)
 '(yas-new-snippet-default
	 "# -*- mode: snippet -*-
# name: $1
# key: ${2:${1:$(yas--key-from-desc yas-text)}}
# expand-env: ((yas/indent-line 'fixed) (yas/wrap-around-region 'nil))
# --
$0")
 '(yas-trigger-key "TAB"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
