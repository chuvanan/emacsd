;; init.el --- AnChu's Emacs configuration

;;; Code:

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

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

;; disable menu bar
(menu-bar-mode -1)

;; visual line
(setq line-move-visual t)
(setq scroll-margin 3)
;; (fringe-mode 0)
(fringe-mode '(8 . 0))

;; set fill column to 80 characters
(setq-default fill-column 80)
(setq-default default-tab-width 2)
(setq tab-always-indent 'complete)
(setq-default indent-tabs-mode nil)

;; require a newline at the end of files
(setq require-final-newline t)

;; insert new line if the point is at the end of the buffer
(setq next-line-add-newlines t)

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;; disable the annoying bell ring
(setq ring-bell-function 'ignore)

;; disable startup screen
(setq inhibit-startup-screen t)
(setq initial-major-mode 'text-mode)
(setq initial-scratch-message nil)

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
(setq echo-keystrokes 0.1)

;; switch to other window
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "M-%") 'query-replace-regexp)

;; set join line instead of fill paragraph
(global-set-key (kbd "M-q") 'delete-indentation)

;; rebinding undo command
(global-set-key (kbd "C-z") 'undo)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; csv mode
(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
(autoload 'csv-mode "csv-mode"
  "Major mode for editing comma-separated value files." t)

;; use-package
(require 'use-package)
(setq use-package-verbose t)

(use-package visible-mark
  :ensure t
  :config
  (global-visible-mark-mode 1))

(use-package smart-mode-line
  :ensure t
  :config (progn
						(setq sml/no-confirm-load-theme t)
						(sml/setup)))

(use-package imenu-anywhere
	:ensure t
	:bind (("C-." . imenu-anywhere)))

(use-package avy
  :ensure t
  :bind (("M-s M-s" . avy-goto-word-or-subword-1)))

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
  (setq undo-tree-history-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq undo-tree-auto-save-history t))

(use-package recentf
  :config
  (setq recentf-max-saved-items 500
        recentf-max-menu-items 15
        recentf-auto-cleanup 'never)
	(global-set-key "\C-x\ \C-r" 'recentf-open-files)
  (recentf-mode +1))

(use-package dired
	:config
  (setq dired-listing-switches "-alh")
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

;; markdown
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
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "03e3e79fb2b344e41a7df897818b7969ca51a15a67dc0c30ebbdeb9ea2cd4492" "0ae52e74c576120c6863403922ee00340a3bf3051615674c4b937f9c99b24535" "aed73c6d0afcf2232bb25ed2d872c7a1c4f1bda6759f84afc24de6a1aec93da8" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "f245c9f24b609b00441a6a336bcc556fe38a6b24bfc0ca4aedd4fe23d858ba31" "1dfd7a150e80fdb4563f594716d09d849f4c50bcea12825bd8d284c05a87a3e1" "9cb6358979981949d1ae9da907a5d38fb6cde1776e8956a1db150925f2dad6c1" "4d80487632a0a5a72737a7fc690f1f30266668211b17ba836602a8da890c2118" "15348febfa2266c4def59a08ef2846f6032c0797f001d7b9148f30ace0d08bcf" "3f5701c23d328be03536349b29cb24c5cfa79ea9ef9c46cf89668eda16b88a9c" "12b7ed9b0e990f6d41827c343467d2a6c464094cbcc6d0844df32837b50655f9" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
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
    (seoul256-theme icicles visible-mark company-jedi avy imenu-anywhere aggressive-indent zenburn-theme projectile powerline base16-theme tango-plus-theme greymatters-theme flatui-theme meaculpa-theme smart-mode-line csv-mode helm-R helm which-key smex evil window-numbering company easy-kill use-package magit solarized-theme expand-region markdown-mode auto-complete smartparens org)))
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


;; redefinde kill line and kill region
(defadvice kill-ring-save (before slick-copy activate compile) "When called
  interactively with no active region, copy a single line instead."
  (interactive (if mark-active (list (region-beginning) (region-end)) (message
  "Copied line") (list (line-beginning-position) (line-beginning-position
  2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
    (if mark-active (list (region-beginning) (region-end))
      (list (line-beginning-position)
        (line-beginning-position 2)))))

(defadvice ido-find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (and buffer-file-name
               (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun ess-rmarkdown ()
  "Compile R markdown (.Rmd). Should work for any output type."
  (interactive)
  ;; Check if attached R-session
  (condition-case nil
      (ess-get-process)
    (error
     (ess-switch-process)))
  (let* ((rmd-buf (current-buffer)))
    (save-excursion
      (let* ((sprocess (ess-get-process ess-current-process-name))
             (sbuffer (process-buffer sprocess))
             (buf-coding (symbol-name buffer-file-coding-system))
             (R-cmd
              (format "library(rmarkdown); rmarkdown::render(\"%s\")"
                      buffer-file-name)))
        (message "Running rmarkdown on %s" buffer-file-name)
        (ess-execute R-cmd 'buffer nil nil)
        (switch-to-buffer rmd-buf)
        (ess-show-buffer (buffer-name sbuffer) nil)))))

(define-key polymode-mode-map "\M-ns" 'ess-rmarkdown)

(defun ess-rshiny ()
  "Compile R markdown (.Rmd). Should work for any output type."
  (interactive)
  ;; Check if attached R-session
  (condition-case nil
      (ess-get-process)
    (error
     (ess-switch-process)))
  (let* ((rmd-buf (current-buffer)))
    (save-excursion
      (let* ((sprocess (ess-get-process ess-current-process-name))
             (sbuffer (process-buffer sprocess))
             (buf-coding (symbol-name buffer-file-coding-system))
             (R-cmd
              (format "library(rmarkdown); rmarkdown::run(\"%s\")"
                      buffer-file-name)))
        (message "Running shiny on %s" buffer-file-name)
        (ess-execute R-cmd 'buffer nil nil)
        (switch-to-buffer rmd-buf)
        (ess-show-buffer (buffer-name sbuffer) nil)))))

(define-key polymode-mode-map "\M-nr" 'ess-rshiny)

;; (defun push-mark-no-activate ()
;;   "Pushes `point' to `mark-ring' and does not activate the region
;;    Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
;;   (interactive)
;;   (push-mark (point) t nil)
;;   (message "Pushed mark to ring"))

(global-set-key (kbd "C-`") 'push-mark-no-activate)

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
  This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))
(global-set-key (kbd "C-j") 'jump-to-mark)
