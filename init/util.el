;; utility
;; count the number of words
(defun word-count-region(start end)
  (interactive "r")
  (save-excursion
    (save-restriction
      (let ((c 0) (l 0) (w 0) (in-word nil) c-after)
        (goto-char start)
        (while (< (point) end)
          (setq c-after (char-after (point))) ; no need (point) if emacs 20
          (if (= c-after ?\n)
              (setq l (1+ l)))
          (if (or (= c-after ?\n) (= c-after ? ) (= c-after ?\C-i))
              (if in-word
                  (setq w (1+ w) in-word nil))
            (setq c (1+ c) in-word t))
          (forward-char))
        (message (format "%d lines, %d words, %d characters" l w c))))))
(global-set-key "\C-xe" 'word-count-region)

;; electric pair
;; complete the pair blacket automatically
(defun electric-pair ()
  (interactive)
  (let (parens-require-spaces)
    (insert-pair)))


;; triming whitespaces
(defun trim-whitespaces ()
  "Trim excess whitespaces."
  (interactive "*")
  (let ((key nil)
       (reg (and transient-mark-mode mark-active)))
    (save-excursion
      (save-restriction
        (if reg
            (narrow-to-region (region-beginning) (region-end)))
        (goto-char (point-min))
        (while (re-search-forward "[ \t]+$" nil t)
          (replace-match "" nil nil))
        (if reg nil
          (goto-char (point-max))
          (delete-blank-lines)))))
  (deactivate-mark))

;; display the number of colum and charactors in the region
;;http://d.hatena.ne.jp/sonota88/20110224/1298557375
(defun count-lines-and-chars ()
	(if mark-active
			(format "%d lines, %d chars %s"
							(count-lines (region-beginning) (region-end))
							(- (region-end) (region-beginning))
							(buffer-file-name))
		""))
(add-to-list 'default-mode-line-format
						 '(:eval (count-lines-and-chars)))

;; interactive window resizer
;;http://d.hatena.ne.jp/khiker/20100119/window_resize
(defun my-window-resizer ()
  "Control window size and position."
  (interactive)
  (let ((window-obj (selected-window))
        (current-width (window-width))
        (current-height (window-height))
        (dx (if (= (nth 0 (window-edges)) 0) 1
              -1))
        (dy (if (= (nth 1 (window-edges)) 0) 1
              -1))
        action c)
    (catch 'end-flag
      (while t
        (setq action
              (read-key-sequence-vector (format "size[%dx%d]"
                                                (window-width)
                                                (window-height))))
        (setq c (aref action 0))
        (cond ((= c ?f)
               (enlarge-window-horizontally dx))
              ((= c ?b)
               (shrink-window-horizontally dx))
              ((= c ?n)
               (enlarge-window dy))
              ((= c ?p)
               (shrink-window dy))
              ;; otherwise
              (t
               (let ((last-command-char (aref action 0))
                     (command (key-binding action)))
                 (when command
                   (call-interactively command)))
               (message "Quit")
               (throw 'end-flag t)))))))

(global-set-key "\C-qr" 'my-window-resizer)
