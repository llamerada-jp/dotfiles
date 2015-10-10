(require 'cask)
(cask-initialize)

;; Load-pathの追加
(setq load-path (cons "~/.emacs.d/site-lisp" load-path))

;; より下に記述した物が PATH の先頭に追加されます
(dolist (dir (list
              "/sbin"
              "/usr/sbin"
              "/bin"
              "/usr/bin"
              "/usr/local/bin"
              (expand-file-name "~/bin")
              (expand-file-name "~/.emacs.d/bin")
              ))
  
;; PATH と exec-path に同じ物を追加します
(when (and (file-exists-p dir) (not (member dir exec-path)))
  (setenv "PATH" (concat dir ":" (getenv "PATH")))
  (setq exec-path (append (list dir) exec-path))))

;; 透明化
(set-frame-parameter (selected-frame) 'alpha '(85 85))

;; フォントの設定
(set-face-attribute 'default nil
                   :family "Ricty"
                   :height 160)
(set-fontset-font
 nil 'japanese-jisx0208
 (font-spec :family "Ricty"))


;; 言語を日本語にする
(set-language-environment 'Japanese)
;; 極力UTF-8とする
(prefer-coding-system 'utf-8)

;; C-hで１文字消去
(keyboard-translate ?\C-h ?\C-?)
(global-set-key "\C-h" nil)

;; 色を変更
(color-theme-initialize)
(color-theme-dark-laptop)

;; auto-complete
(require 'auto-complete-config)
(require 'auto-complete-clang)
(add-to-list 'ac-dictionary-directories ".emacs.d/ac-dict")
(ac-config-default)

;;補完キー指定
(ac-set-trigger-key "TAB")
;;ヘルプ画面が出るまでの時間（秒）
(setq ac-quick-help-delay 0.8)

(defun my-ac-cc-mode-setup ()
  ;; 読み込むプリコンパイル済みヘッダ
  ;; (setq ac-clang-prefix-header "~/.emacs.d/stdafx.pch")
  ;; 補完を自動で開始しない
  (setq ac-auto-start nil)
  (setq ac-clang-flags '("-w" "-ferror-limit" "1"))
  (setq ac-sources (append '(ac-source-clang
                             ac-source-yasnippet
                             ac-source-gtags)
                           ac-sources)))
(defun my-ac-config ()
  (global-set-key "\M-/" 'ac-start)
  ;; C-n/C-p で候補を選択
  (define-key ac-complete-mode-map "\C-n" 'ac-next)
  (define-key ac-complete-mode-map "\C-p" 'ac-previous)
  (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
  (add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)
  (add-hook 'ruby-mode-hook 'ac-css-mode-setup)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (global-auto-complete-mode t))

(my-ac-config)

;; Lua-mode
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;; markdown-mode
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; haskell-mode
(add-to-list 'load-path "~/.emacs.d/haskell-mode-2.8.0")
(require 'haskell-mode)
(require 'haskell-cabal)

(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.lhs$" . literate-haskell-mode))
(add-to-list 'auto-mode-alist '("\\.cabal\\'" . haskell-cabal-mode))

;; cmake-mode
(setq auto-mode-alist
      (append
       '(("CMakeLists\\.txt\\'" . cmake-mode))
       '(("\\.cmake\\'" . cmake-mode))
       auto-mode-alist))

;; Google C++ Style
(add-hook 'c-mode-common-hook 'google-set-c-style)

;; cpplint by flyckeck
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

(defun my-flycheck-c++-setup()
  (setq flycheck-clang-language-standard "c++11")
  (setq flycheck-clang-standard-library "libc++")
  (setq flycheck-gcc-language-standard "c++11")
  (setq flycheck-gcc-standard-library "libc++")
  (setq flycheck-clang-include-path
	(list (expand-file-name "~/Develop/socket.io-client-cpp/build/include")))
  (setq flycheck-clang-include-path
	(list (expand-file-name "~/Develop/emsdk/emscripten/tag-1.34.6/system/include/emscripten/emscripten.h")))
  (setq flycheck-clang-args
	(list "-Wno-unused-parameter"))
  )
(add-hook 'c++-mode-hook #'my-flycheck-c++-setup)

(eval-after-load 'flycheck
  '(progn
     (require 'flycheck-google-cpplint)
     (flycheck-add-next-checker 'c/c++-clang
                                '(warning . c/c++-googlelint))))

(custom-set-variables
 '(flycheck-c/c++-googlelint-executable "/usr/local/bin/cpplint.py")
 '(flycheck-googlelint-filter "-legal/copyright,-runtime/references,-build/c++11")
 '(flycheck-googlelint-linelength "100")
 )

(require 'flycheck-pos-tip)
(eval-after-load 'flycheck
  '(custom-set-variables
    '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

(provide 'init)
;;; init.el ends here