;; emacs-mozc
(el-get-bundle mozc)

(set-language-environment "Japanese")           ; 言語環境を"japanese"に
(setq default-input-method "japanese-mozc")     ; IMEをjapanes-mozcに
(prefer-coding-system 'utf-8)                   ; デフォルトの文字コードをUTF-8に

;; (global-set-key (kbd "C-SPC") 'toggle-input-method)
