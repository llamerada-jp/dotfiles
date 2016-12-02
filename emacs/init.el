;; Load-pathの追加

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq load-path (cons "~/.cask" load-path))
(setq load-path (cons "~/.emacs.d/site-lisp" load-path))

(require 'cask)
(cask-initialize)

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

;; フォントの設定 for Linux
(if (eq system-type 'gnu/linux)
    (set-face-attribute 'default nil
			:family "Myrca"
			:height 100))

(if (eq system-type 'gnu/linux)
    (set-fontset-font
     nil 'japanese-jisx0208
     (font-spec :family "Myrca")))

;; フォントの設定 for macos
(if (eq system-type 'darwin)
    (set-face-attribute 'default nil
			:family "Ricty"
			:height 140))

(if (eq system-type 'darwin)
    (set-fontset-font
     nil 'japanese-jisx0208
     (font-spec :family "Ricty")))

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

;; fix problem with C++0x11 enum class
(defun inside-class-enum-p (pos)
  "Checks if POS is within the braces of a C++ \"enum class\"."
  (ignore-errors
    (save-excursion
      (goto-char pos)
      (up-list -1)
      (backward-sexp 1)
      (looking-back "enum[ \t]+class[ \t]+[^}]+"))))

(defun align-enum-class (langelem)
  (if (inside-class-enum-p (c-langelem-pos langelem))
      0
    (c-lineup-topmost-intro-cont langelem)))

(defun align-enum-class-closing-brace (langelem)
  (if (inside-class-enum-p (c-langelem-pos langelem))
      '-
    '+))

(defun fix-enum-class ()
  "Setup `c++-mode' to better handle \"class enum\"."
  (add-to-list 'c-offsets-alist '(topmost-intro-cont . align-enum-class))
  (add-to-list 'c-offsets-alist
               '(statement-cont . align-enum-class-closing-brace)))

(add-hook 'c++-mode-hook 'fix-enum-class)

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
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-c/c++-googlelint-executable "/usr/local/bin/cpplint.py")
 '(flycheck-disabled-checkers (quote (javascript-jshint javascript-jscs)))
 '(flycheck-display-errors-function (function flycheck-pos-tip-error-messages))
 '(flycheck-googlelint-filter "-legal/copyright,-runtime/references,-build/c++11")
 '(flycheck-googlelint-linelength "100")
 '(package-selected-packages
   (quote
    (yaml-mode web-mode scala-mode ruby-end ruby-block php-mode markdown-mode json-mode js2-mode haskell-mode google-c-style flycheck-pos-tip flycheck-google-cpplint color-theme cmake-mode auto-complete-clang))))

(require 'flycheck-pos-tip)
(eval-after-load 'flycheck
  '(custom-set-variables
    '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

;; eslint by flycheck
(eval-after-load 'flycheck
  '(custom-set-variables
    '(flycheck-disabled-checkers '(javascript-jshint javascript-jscs))
    ))
(setq js2-include-browser-externs nil)
(setq js2-mode-show-parse-errors nil)
(setq js2-mode-show-strict-warnings nil)
(setq js2-highlight-external-variables nil)
(setq js2-include-jslint-globals nil)
(add-hook 'web-mode-hook
          (lambda ()
            (flycheck-add-mode 'javascript-eslint 'web-mode)
            (flycheck-mode)))

;; php-mode
(require 'php-mode)
(add-hook 'php-mode-hook (lambda ()
			   (defun ywb-php-lineup-arglist-intro (langelem)
			     (save-excursion
			       (goto-char (cdr langelem))
			       (vector (+ (current-column) c-basic-offset))))
			   (defun ywb-php-lineup-arglist-close (langelem)
			     (save-excursion
			       (goto-char (cdr langelem))
			       (vector (current-column))))
			   (c-set-offset 'arglist-intro 'ywb-php-lineup-arglist-intro)
			   (c-set-offset 'arglist-close 'ywb-php-lineup-arglist-close)))

;; web-mode
(require 'web-mode)

;;; 適用する拡張子
(add-to-list 'auto-mode-alist '("\\.js$"        . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml$"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp$"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x$"   . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb$"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?$"     . web-mode))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq-default indent-tabs-mode nil)
  (setq web-mode-attr-indent-offset nil)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-html-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-style-padding 2)
  (setq web-mode-script-padding 2)
  (setq web-mode-block-padding 2)
  (setq web-mode-jsx-expression-padding 2)
  (setq tab-width 2)
  )
(add-hook 'web-mode-hook  'my-web-mode-hook)

;; jsx-mode
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-jsx-mode))
(flycheck-add-mode 'javascript-eslint 'js2-jsx-mode)
(add-hook 'js2-jsx-mode-hook 'flycheck-mode)
(autoload 'js2-mode "js2" nil t)
(add-hook 'js2-mode-hook
          '(lambda ()
             (setq js2-basic-offset 2
                   tab-width 2
                   indent-tabs-mode nil
                   js2-cleanup-whitespace nil)))

(provide 'init)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;;; init.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
