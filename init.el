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

