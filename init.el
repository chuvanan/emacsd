;; init.el --- AnChu's Emacs configuration

;;; Code:

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
;; keep the installed packages in .emacs.d
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)
;; update the package metadata if the local cache is missing
(unless package-archive-contents
  (package-refresh-contents))

(setq user-full-name "An Chu"
      user-mail-address "chuvanan.cva@gmail.com")

;; Always load newest byte code
(setq load-prefer-newer t)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; disable tool-bar mode
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; visual line
(setq line-move-visual t)

;; set fill column to 80 characters
(setq-default fill-column 80)
(setq-default default-tab-width 2)
(setq tab-always-indent 'complete)
;; (setq-default indent-tabs-mode nil)

;; require a newline at the end of files
(setq require-final-newline t)

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;; disable the annoying bell ring
(setq ring-bell-function 'ignore)

;; disable startup screen
(setq inhibit-startup-screen t)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; disable scroll-bar
(scroll-bar-mode -1)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; Newline at end of file
(setq require-final-newline t)

;; delete the selection with a keypress
(delete-selection-mode t)

;; truncate longline as default
(set-default 'truncate-lines t)

;; disable the current buffer mark when changing buffer
(transient-mark-mode 1)

;; set up coding system
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; speed up echo commands
(setq echo-keystrokes 0.3)

;; switch to other window
(global-set-key (kbd "M-o") 'other-window)

;; set delete indentation
(global-set-key (kbd "M-q") 'delete-indentation)

;; rebinding undo command
(global-set-key (kbd "C-z") 'undo)

;; highlight current line
(global-hl-line-mode +1)

;; use-package
(require 'use-package)
(setq use-package-verbose t)

(use-package imenu-anywhere
	:ensure t
	:bind (("C-." . imenu-anywhere)))

(use-package avy
  :ensure t
  :bind (("M-s" . avy-goto-word-or-subword-1)))

(use-package smex
  :ensure t
  :bind ("M-x" . smex))

(use-package which-key
  :ensure t
  :config
  (which-key-mode +1))

(use-package undo-tree
  :ensure t
  :config
  ;; autosave the undo-tree history
  (setq undo-tree-history-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq undo-tree-auto-save-history t))

(use-package recentf
  :config
  (setq recentf-max-saved-items 500
        recentf-max-menu-items 15
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never)
	(global-set-key "\C-x\ \C-r" 'recentf-open-files)
  (recentf-mode +1))

(use-package dired
	:config
	(require 'dired-x))

(use-package ido
  :ensure t
  :config
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-use-filename-at-point 'guess
        ido-max-prospects 10
        ido-default-file-method 'selected-window
        ido-auto-merge-work-directories-length -1)
  (ido-mode +1))

(use-package window-numbering
  :ensure t
  :config
  (window-numbering-mode 1))

(use-package easy-kill
  :defer t
  :init (global-set-key [remap kill-ring-save] 'easy-kill))

(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package smartparens
  :ensure t
  :config
  (smartparens-global-mode t))

(use-package company
  :ensure t
  :config
  (global-company-mode)
  (setq company-idle-delay .3)
  (setq company-echo-delay 0)
  (setq company-begin-commands '(self-insert-command)))

;; org-mode set up
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

;; expand region
(use-package expand-region
  :commands er/expand-region
  :bind ("C-=" . er/expand-region))

(use-package aggressive-indent
  :ensure t
  :config
	(add-hook 'ess-mode 'aggressive-indent-mode))

(require 'ess-site)
(require 'ess-rutils)
(autoload 'r-mode "ess-site" "(Autoload)" t)
;; dont ask for new data directory
(setq ess-ask-for-ess-directory nil)
;; don't print the evaluated commands
(setq ess-eval-visibly nil)

(setq load-path (append '("/home/anchu/.emacs.d/polymode/" "/home/anchu/.emacs.d/polymode/modes") load-path))
(require 'poly-R)
(require 'poly-markdown)

;; MARKDOWN
(add-to-list 'auto-mode-alist '("\\.md$" . poly-markdown-mode))

;; R related modes
(add-to-list 'auto-mode-alist '("\\.Snw$" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rnw$" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rmd$" . poly-markdown+r-mode))
(add-to-list 'auto-mode-alist '("\\.rapport$" . poly-rapport-mode))
(add-to-list 'auto-mode-alist '("\\.Rhtml$" . poly-html+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rbrew$" . poly-brew+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rcpp$" . poly-r+c++-mode))
(add-to-list 'auto-mode-alist '("\\.cppR$" . poly-c++r-mode))

(provide 'polymode-configuration)

;; chain operator
(defun then_R_operator ()
  "R - %>% operator or 'then' pipe operator"
  (interactive)
  (just-one-space 1)
  (insert "%>%")
  (reindent-then-newline-and-indent))
(define-key ess-mode-map (kbd "C-S-m") 'then_R_operator)
(define-key inferior-ess-mode-map (kbd "C-S-m") 'then_R_operator)

;; ess assignment operator
(setq ess-S-assign-key (kbd "M--"))
(ess-toggle-S-assign-key t) ; enable above key definition
;; leave my underscore key alone!
(ess-toggle-S-assign nil)
(ess-toggle-S-assign nil)
(ess-toggle-underscore nil)

;; active eldoc on ess-mode only
(setq ess-use-eldoc 'script-only)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(custom-safe-themes
	 (quote
		("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(default-input-method "vietnamese-telex")
 '(ess-R-font-lock-keywords
	 (quote
		((ess-R-fl-keyword:modifiers . t)
		 (ess-R-fl-keyword:fun-defs . t)
		 (ess-R-fl-keyword:keywords . t)
		 (ess-R-fl-keyword:assign-ops)
		 (ess-R-fl-keyword:constants . t)
		 (ess-fl-keyword:fun-calls)
		 (ess-fl-keyword:numbers)
		 (ess-fl-keyword:operators)
		 (ess-fl-keyword:delimiters)
		 (ess-fl-keyword:=)
		 (ess-R-fl-keyword:F&T)
		 (ess-R-fl-keyword:%op%))))
 '(inferior-R-font-lock-keywords
	 (quote
		((ess-S-fl-keyword:prompt . t)
		 (ess-R-fl-keyword:messages . t)
		 (ess-R-fl-keyword:modifiers . t)
		 (ess-R-fl-keyword:fun-defs . t)
		 (ess-R-fl-keyword:keywords . t)
		 (ess-R-fl-keyword:assign-ops)
		 (ess-R-fl-keyword:constants . t)
		 (ess-fl-keyword:matrix-labels)
		 (ess-fl-keyword:fun-calls)
		 (ess-fl-keyword:numbers)
		 (ess-fl-keyword:operators)
		 (ess-fl-keyword:delimiters)
		 (ess-fl-keyword:=)
		 (ess-R-fl-keyword:F&T))))
 '(package-selected-packages
	 (quote
		(helm-R helm which-key smex evil window-numbering company easy-kill use-package magit solarized-theme expand-region markdown-mode auto-complete smartparens org)))
 '(send-mail-function (quote mailclient-send-it))
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(tool-bar-mode nil)
 '(tooltip-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Ubuntu Mono" :foundry "unknown" :slant normal :weight normal :height 98 :width normal)))))
