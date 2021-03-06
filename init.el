;; init.el --- AnChu's Emacs configuration

;;; Code:

;; (require 'cl)
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
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(global-hl-line-mode +1)

;; visual line
(setq line-move-visual t)
(setq scroll-step 1
      scroll-margin 5
      hscroll-step 1
      hscroll-margin 3
      scroll-conservatively  10000)
;; (fringe-mode 0)
(fringe-mode '(4 . 0))
(put 'narrow-to-region 'disabled nil)

;; set fill column to 80 characters
(setq-default fill-column 80)
(setq-default default-tab-width 4)
(setq tab-always-indent 'complete)
(setq-default indent-tabs-mode nil)
(setq history-delete-duplicates t)

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
(setq initial-major-mode 'R-mode)
(setq initial-scratch-message nil)
(fset 'display-startup-echo-area-message #'ignore)
(setq use-dialog-box nil)

;; turn off bidirectional text
(setq-default bidi-paragraph-direction 'left-to-right)

;; mode line settings
(line-number-mode t)
(column-number-mode t)

;; mouse avoidance
(mouse-avoidance-mode 'banish)

;; fullscreen
(when (fboundp 'toggle-frame-maximized)
  (toggle-frame-maximized))

;; Indenting
(electric-indent-mode 1)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; delete the selection with a keypress
(delete-selection-mode t)

;; delete trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

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
(setq echo-keystrokes 0)
(setq focus-follows-mouse t)

;; switch to other window
;; (global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "<f5>") 'calendar)
(global-set-key (kbd "<f6>") 'calculator)
(global-set-key (kbd "M-]") 'forward-paragraph)
(global-set-key (kbd "M-[") 'backward-paragraph)
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; hippie expand is dabbrev expand on steroids
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

;; use hippie-expand instead of dabbrev
(global-set-key (kbd "M-/") #'hippie-expand)
(global-set-key (kbd "s-/") #'hippie-expand)

;; set join line instead of fill paragraph
(global-set-key (kbd "M-q") 'delete-indentation)

;; fill paragraph
(global-set-key (kbd "M-p") 'fill-paragraph)

;; rebinding undo command
(global-set-key (kbd "C-z") 'undo)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;; Rings and registers
(setq kill-ring-max 200                 ; More killed items
      kill-do-not-save-duplicates t     ; No duplicates in kill ring
      save-interprogram-paste-before-kill t)

;; use-package
(require 'use-package)
(setq use-package-verbose t)

;; go to last change
(use-package goto-last-change
  :ensure t
  :config
  (global-set-key (kbd "C-x C-\\") 'goto-last-change))

;; multiple cursors
(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C-c m") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C->") 'mc/mark-all-like-this))

;; paredit-everywhere
;; (use-package paredit-everywhere
;;   :ensure t
;;   :config
;;   (add-hook 'ess-mode-hook #'paredit-everywhere-mode))

;; yas-snipppet
(use-package yasnippet
  :ensure t
  :config
  (yas-reload-all)
  (add-hook 'ess-mode-hook #'yas-minor-mode)
  (add-hook 'markdown-mode-hook #'yas-minor-mode))

(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand)

;; Save point position in files
(use-package saveplace
  :init (save-place-mode 1))

;; Save minibuffer history
(use-package savehist
  :init (savehist-mode t)
  :config (setq savehist-save-minibuffer-history t
                savehist-autosave-interval 180))

;; add stripes to a buffer
;; (use-package stripe-buffer
;;   :ensure t
;;   :init (add-hook 'dired-mode-hook #'stripe-buffer-mode))

(use-package rainbow-delimiters
  :ensure t)

;; Show number of matches while searching
(use-package anzu
  :ensure t
  :config
  (global-anzu-mode t)
  (diminish 'anzu-mode)
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
  (global-set-key [remap query-replace] 'anzu-query-replace))

;; hungry delete
(use-package hungry-delete
  :ensure t
  :config
  (global-hungry-delete-mode))

;; silver searcher
(use-package ag
  :ensure t
  :config
  :bind (("<f3>" . ag-regexp)
         ("<f4>" . ag-dired-regexp)))

;; markdown mode
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "markdown"))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  ;; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)
  ;; don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

;; (use-package keyfreq
;;   :ensure t
;;   :config
;;   (keyfreq-mode 1)
;;   (keyfreq-autosave-mode 1))

;; (use-package imenu-anywhere
;; 	:ensure t
;; 	:bind (("C-." . helm-imenu-anywhere)))

(use-package avy
  :ensure t
  :bind (("M-s M-s" . avy-goto-word-or-subword-1)
         ("C-;" . avy-goto-word-or-subword-1)))

(use-package which-key
  :ensure t
  :config
  (which-key-mode +1))

;; (use-package undo-tree
;;   :ensure t
;;   :config
;;   (setq undo-tree-history-directory-alist
;;         `((".*" . ,temporary-file-directory)))
;;   (setq undo-tree-auto-save-history t))

(use-package recentf
  :config
  (setq recentf-max-saved-items 500
        recentf-max-menu-items 15
        recentf-auto-cleanup 'never)
  (recentf-mode +1))

(use-package dired
	:config
  (setq dired-listing-switches "-alh")
	(require 'dired-x))

(use-package window-numbering
  :ensure t
  :config
  (window-numbering-mode 1))

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :config
  (load-theme 'sanityinc-tomorrow-eighties t))

;; (use-package zenburn-theme
;;   :ensure t
;;   :config
;;   (load-theme 'zenburn t))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package smartparens
  :init
  (smartparens-global-mode t)
  :diminish smartparens-mode
  :ensure t
  :config
  (require 'smartparens-config)
  (define-key smartparens-mode-map (kbd "C-c s a") 'sp-beginning-of-sexp)
  (define-key smartparens-mode-map (kbd "C-c s e") 'sp-end-of-sexp)
  (define-key smartparens-mode-map (kbd "C-c s s") 'sp-forward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "C-c s r") 'sp-forward-barf-sexp)
  (define-key smartparens-mode-map (kbd "C-c s S") 'sp-backward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "C-c s R") 'sp-backward-barf-sexp)
  (define-key smartparens-mode-map (kbd "C-c s [") 'sp-select-previous-thing)
  (define-key smartparens-mode-map (kbd "C-c s ]") 'sp-select-next-thing)
  (define-key smartparens-mode-map (kbd "C-c s C-i") 'sp-splice-sexp)
  (define-key smartparens-mode-map (kbd "C-c s <delete>") 'sp-splice-sexp-killing-forward)
  (define-key smartparens-mode-map (kbd "C-c s <backspace>") 'sp-splice-sexp-killing-backward)
  (define-key smartparens-mode-map (kbd "C-c s C-<backspace>") 'sp-splice-sexp-killing-around))

(use-package company
  :ensure t
  :config
  (global-company-mode)
  (setq company-idle-delay 0)
  (setq company-echo-delay 0)
  (setq company-begin-commands '(self-insert-command)))

(require 'company-dabbrev)
(require 'company-dabbrev-code)
(setq company-dabbrev-code-everywhere t)
(setq company-dabbrev-code-ignore-case nil)
(setq company-dabbrev-ignore-case nil)
(add-to-list 'company-dabbrev-code-modes 'ess-mode)
(define-key company-active-map [tab] 'company-complete-common-or-cycle)
(define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)

(use-package expand-region
  :commands er/expand-region
  :bind ("C-=" . er/expand-region))

(use-package aggressive-indent
  :ensure t
  :config
  (add-hook 'ess-mode-hook #'aggressive-indent-mode)
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode))

;; turn off auto revert messages
(setq auto-revert-verbose nil)

;; auto refresh dired when file changes
(add-hook 'dired-mode-hook 'auto-revert-mode)

(require 'ess-site)
(require 'ess-rutils)
(setq ess-ask-for-ess-directory nil)
(setq ess-eval-visibly nil)
(setq ess-default-style 'RRR)

;; company backend
(setq-local company-backends
            (append '((company-dabbrev-code company-R-args company-R-objects))
                    company-backends))
(setq ess-use-company nil)

;; "Highlights delimiters such as parentheses, brackets or braces according to their depth."
(add-hook 'ess-mode-hook #'rainbow-delimiters-mode)

;; Fancy up the prompt (see also ~/.Rprofile)
(setq inferior-ess-primary-prompt "ℝ> ")
(setq inferior-S-prompt "[]a-zA-Z0-9.[]*\\(?:[>+.] \\)*ℝ+> ")

(setq load-path (append '("/home/anchu/.emacs.d/polymode/" "/home/anchu/.emacs.d/polymode/modes") load-path))
(use-package polymode
  :ensure t
  :config
  (require 'poly-R)
  (require 'poly-markdown)
  (add-to-list 'auto-mode-alist '("\\.md$" . poly-markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.Rmd$" . poly-markdown+r-mode))
  (add-to-list 'auto-mode-alist '("\\.Rcpp$" . poly-r+c++-mode))
  (add-to-list 'auto-mode-alist '("\\.cppR$" . poly-c++r-mode))
  )

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
(ess-disable-smart-S-assign nil)
;; active eldoc on ess-mode only
(setq ess-use-eldoc 'script-only)

;; ESS
(add-hook 'ess-mode-hook
          (lambda ()
            (ess-set-style 'RRR 'quiet)
            ;; Because
            ;;                                 DEF GNU BSD K&R C++
            ;; ess-indent-level                  2   2   8   5   4
            ;; ess-continued-statement-offset    2   2   8   5   4
            ;; ess-brace-offset                  0   0  -8  -5  -4
            ;; ess-arg-function-offset           2   4   0   0   0
            ;; ess-expression-offset             4   2   8   5   4
            ;; ess-else-offset                   0   0   0   0   0
            ;; ess-close-brace-offset            0   0   0   0   0
            (add-hook 'local-write-file-hooks
                      (lambda ()
                        (ess-nuke-trailing-whitespace)))))
;; (setq ess-nuke-trailing-whitespace-p 'ask)
;; or even
(setq ess-nuke-trailing-whitespace-p t)

(setq inferior-R-args "--no-restore-history --no-save")
(setq ess-offset-arguments 'prev-line)

(require 'electric-operator)
(add-hook 'ess-mode-hook #'electric-operator-mode)
(add-hook 'python-mode-hook #'electric-operator-mode)
(setq electric-operator-R-named-argument-style 'spaced)

;; custom := operator (data.table)
(electric-operator-add-rules-for-mode 'ess-mode
                                      (cons ":=" " := ")
                                      (cons "%" nil)
                                      (cons "%in%" " %in% ")
                                      (cons "%%" " %% ")
                                      (cons "!=" " != ")
                                      (cons "<=" " <= ")
                                      (cons ">=" " >= ")
                                      (cons ";" "; "))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(custom-safe-themes
   (quote
    ("b61c55259c639a54628f91452b060b99c550a1269eb947e372321b806b68f114" "fbcdb6b7890d0ec1708fa21ab08eb0cc16a8b7611bb6517b722eba3891dfc9dd" "1db337246ebc9c083be0d728f8d20913a0f46edc0a00277746ba411c149d7fe5" "5999e12c8070b9090a2a1bbcd02ec28906e150bb2cdce5ace4f965c76cf30476" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "f34b107e8c8443fe22f189816c134a2cc3b1452c8874d2a4b2e7bb5fe681a10b" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "e8e744a1b0726814ac3ab86ad5ccdf658b9ff1c5a63c4dc23841007874044d4a" "9b65cf71fd6b27a5362afeff062c6abd1c5d8a7c4d444c942f3da36bf0a151b1" "cdfb22711f64d0e665f40b2607879fcf2607764b2b70d672ddaa26d2da13049f" "232f715279fc131ed4facf6a517b84d23dca145fcc0e09c5e0f90eb534e1680f" "9956eace4d6a1df9bd8c5875406c3dab0b98dd385d3bc99a83aaf730526a6056" "c7f838704d7caa88bc337464867c22af0a502e32154558b0f6c9c3c6e8650122" "c3e6b52caa77cb09c049d3c973798bc64b5c43cc437d449eacf35b3e776bf85c" "5a0eee1070a4fc64268f008a4c7abfda32d912118e080e18c3c865ef864d1bea" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "03e3e79fb2b344e41a7df897818b7969ca51a15a67dc0c30ebbdeb9ea2cd4492" "0ae52e74c576120c6863403922ee00340a3bf3051615674c4b937f9c99b24535" "aed73c6d0afcf2232bb25ed2d872c7a1c4f1bda6759f84afc24de6a1aec93da8" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "f245c9f24b609b00441a6a336bcc556fe38a6b24bfc0ca4aedd4fe23d858ba31" "1dfd7a150e80fdb4563f594716d09d849f4c50bcea12825bd8d284c05a87a3e1" "9cb6358979981949d1ae9da907a5d38fb6cde1776e8956a1db150925f2dad6c1" "4d80487632a0a5a72737a7fc690f1f30266668211b17ba836602a8da890c2118" "15348febfa2266c4def59a08ef2846f6032c0797f001d7b9148f30ace0d08bcf" "3f5701c23d328be03536349b29cb24c5cfa79ea9ef9c46cf89668eda16b88a9c" "12b7ed9b0e990f6d41827c343467d2a6c464094cbcc6d0844df32837b50655f9" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(ess-R-font-lock-keywords
   (quote
    ((ess-R-fl-keyword:modifiers . t)
     (ess-R-fl-keyword:fun-defs . t)
     (ess-R-fl-keyword:keywords . t)
     (ess-R-fl-keyword:assign-ops)
     (ess-R-fl-keyword:constants . t)
     (ess-fl-keyword:fun-calls . t)
     (ess-fl-keyword:numbers . t)
     (ess-fl-keyword:operators)
     (ess-fl-keyword:delimiters)
     (ess-fl-keyword:=)
     (ess-R-fl-keyword:F&T)
     (ess-R-fl-keyword:%op%))))
 '(inferior-ess-r-font-lock-keywords
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
 '(org-agenda-files (quote ("/home/anchu/ownCloud/org-mode/rta-tasks.org")))
 '(package-selected-packages
   (quote
    (neotree swiper-helm dumb-jump paredit-everywhere electric-operator htmlize py-autopep8 gnuplot-mode elpy leuven yaml-mode sml-mode helm-swoop helm-ag helm-projectile color-theme-sanityinc-tomorrow flycheck goto-last-change polymode multiple-cursors stripe-buffer helm-descbinds ibuffer-vc ido-vertical-mode smart-mode-line-powerline smart-mode-line-powerline-theme rainbow-delimiters tldr anzu hungry-delete swiper r-autoyas beacon ag ido-ubiquitous ace-window keyfreq apropospriate-theme icicles visible-mark company-jedi avy imenu-anywhere aggressive-indent zenburn-theme projectile powerline meaculpa-theme smart-mode-line csv-mode helm-R helm which-key smex window-numbering company easy-kill use-package magit expand-region markdown-mode auto-complete smartparens org)))
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
 '(default ((t (:family "Ubuntu Mono" :foundry "unknown" :slant normal :weight normal :height 95 :width normal)))))

(setq show-paren-delay 0)

;; redefinde kill line and kill region
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive (if mark-active (list (region-beginning) (region-end))
                 (message "Copied line")
                 (list (line-beginning-position) (line-beginning-position 2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

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

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
  This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))
(global-set-key (kbd "C-j") 'jump-to-mark)

;; faster pop-to-mark command
(defun modi/multi-pop-to-mark (orig-fun &rest args)
  "Call ORIG-FUN until the cursor moves.
Try the repeated popping up to 10 times."
  (let ((p (point)))
    (dotimes (i 10)
      (when (= p (point))
        (apply orig-fun args)))))
(advice-add 'pop-to-mark-command :around
            #'modi/multi-pop-to-mark)
(setq set-mark-command-repeat-pop t)

(defun sk/smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.
Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.
If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))
  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'sk/smarter-move-beginning-of-line)

;; http://www.emacswiki.org/emacs/ZapToISearch
(defun isearch-exit-other-end (rbeg rend)
  "Exit isearch, but at the other end of the search string.
This is useful when followed by an immediate kill."
  (interactive "r")
  (isearch-exit)
  (goto-char isearch-other-end))

(define-key isearch-mode-map [(control return)] 'isearch-exit-other-end)

;; Search back/forth for the symbol at point
;; (defun isearch-yank-symbol ()
;;   "*Put symbol at current point into search string."
;;   (interactive)
;;   (let ((sym (symbol-at-point)))
;;     (if sym
;;         (progn
;;           (setq isearch-regexp t
;;                 isearch-string (concat "\\_<" (regexp-quote (symbol-name sym)) "\\_>")
;;                 isearch-message (mapconcat 'isearch-text-char-description isearch-string "")
;;                 isearch-yank-flag t))
;;       (ding)))
;;   (isearch-search-and-update))

;; (define-key isearch-mode-map "\C-\M-w" 'isearch-yank-symbol)


;;;; Tidy up the mode-line.  I don't need to see everything in there.
(require 'diminish)
(eval-after-load "aggressive-indent" '(diminish 'aggressive-indent-mode))
(eval-after-load "anzu"              '(diminish 'anzu-mode))
(eval-after-load "auto-complete"     '(diminish 'auto-complete-mode))
(eval-after-load "smartparens"       '(diminish 'smartparens-mode))
(eval-after-load "undo-tree"         '(diminish 'undo-tree-mode))
(eval-after-load "which-key"         '(diminish 'which-key-mode))
(eval-after-load "beacon"            '(diminish 'beacon-mode))
(eval-after-load "hungry-delete"     '(diminish 'hungry-delete-mode))
(eval-after-load "company"           '(diminish 'company-mode))
(eval-after-load "yasnippet"         '(diminish 'yasnippet-mode))
(eval-after-load "helm"         '(diminish 'helm-mode))

(require 'powerline)
(powerline-default-theme)
(setq powerline-arrow-shape 'arrow14)

;; exit ansi-term
(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)

;; Active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (ditaa . t)
   (dot . t)
   (latex . t)
   (lilypond . t)
   (python . t)
   (R . t)
   (ruby . t)
   (shell . t)
   (sql . t)
   (sqlite . t)
   )
 )

;; Evaluate Babel blocks without asking for confirmation
(setq org-confirm-babel-evaluate nil)
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
(add-hook 'org-mode-hook 'org-display-inline-images)
(setq org-html-validation-link nil)
(setq org-src-fontify-natively t)
(setq org-html-htmlize-output-type 'css)
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-export-with-timestamps nil)
(setq org-export-with-toc nil)

(setq org-latex-listings 'minted
      org-latex-packages-alist '(("" "minted"))
      org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(add-to-list 'org-structure-template-alist
             '("s" "#+NAME: ?\n#+BEGIN_SRC \n\n#+END_SRC"))

(defun anchu/set-cursor ()
  (cond
   (buffer-read-only
    (setq cursor-type 'box)
    (set-cursor-color "gold"))
   (t
    (setq cursor-type 'box)
    (set-cursor-color "gray")))
  ;; red cursor for overwrite mode
  (when overwrite-mode
    (set-cursor-color "red")))

(add-hook 'post-command-hook 'anchu/set-cursor)


;; kill as exit
(defadvice save-buffers-kill-emacs
    (around no-query-kill-emacs activate)
  "Prevent \"Active processes exist\" query on exit."
  (cl-flet ((process-list ())) ad-do-it))


(defun find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))
(global-set-key (kbd "C-c I") 'find-user-init-file)

;; (defun rename-file-and-buffer ()
;;   "Rename the current buffer and file it is visiting."
;;   (interactive)
;;   (let ((filename (buffer-file-name)))
;;     (if (not (and filename (file-exists-p filename)))
;;         (message "Buffer is not visiting a file!")
;;       (let ((new-name (read-file-name "New name: " filename)))
;;         (cond
;;          ((vc-backend filename) (vc-rename-file filename new-name))
;;          (t
;;           (rename-file filename new-name t)
;;           (set-visited-file-name new-name t t)))))))

;; (global-set-key (kbd "C-c r")  'rename-file-and-buffer)

;; remove vertical line between windows
(set-face-attribute 'vertical-border nil :foreground (face-attribute 'fringe :background))

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "->") t nil)))))

(defun do-yas-expand ()
  (let ((yas-maybe-expand 'return-nil))
    (yas-expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas-minor-mode) ;; xxx change this to point to right var
            (null (when (looking-at "\\_>") (do-yas-expand))))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))

(define-key prog-mode-map [tab] 'tab-indent-or-complete)
(define-key prog-mode-map (kbd "TAB") 'tab-indent-or-complete)

;; -----------------------------------------------------------------------------
;; helm - interactive completion
;; -----------------------------------------------------------------------------

(use-package helm-fuzzier
  :ensure t
  :disabled t
  :init
  (helm-fuzzier-mode))

(use-package helm
  :diminish helm-mode
  :init
  (progn
    (require 'helm-config)
    (setq helm-candidate-number-limit 100)
    ;; From https://gist.github.com/antifuchs/9238468
    (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
          helm-input-idle-delay 0.01  ; this actually updates things reeeelatively quickly.
          helm-yas-display-key-on-candidate t
          helm-quick-update t
          helm-M-x-requires-pattern nil
          helm-ff-skip-boring-files t
          helm-autoresize-mode 1
          helm-split-window-in-side-p t
          helm-move-to-line-cycle-in-source t
          helm-ff-search-library-in-sexp t
          helm-scroll-amount 8
          helm-ff-file-compressed-list t
          helm-ff-auto-update-initial-value t
          helm-apropos-fuzzy-match t
          helm-locate-fuzzy-match t
          helm-semantic-fuzzy-match t
          helm-imenu-fuzzy-match t
          helm-lisp-fuzzy-completion t
          helm-buffers-fuzzy-matching t
          helm-recentf-fuzzy-match t
          helm-M-x-fuzzy-match t)
    (helm-mode))
  :config
  (progn
    (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
    (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
    (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
    (when (executable-find "curl")
      (setq helm-net-prefer-curl t)))
  :bind (("C-c h" . helm-command-prefix) ; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
         ("C-x b" . helm-mini)
         ("C-h a" . helm-apropos)
         ("C-x C-b" . helm-buffers-list)
         ("C-x b" . helm-buffers-list)
         ("M-y" . helm-show-kill-ring)
         ("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-h i" . helm-info-emacs)
         ("C-x C-r" . helm-recentf)
         ("M-o" . helm-occur)
         ("C-c h x" . helm-register)
         ("C-c h g" . helm-google-suggest)
         ("C-c r b" . helm-filtered-bookmarks)
         ("C-x c SPC" . helm-all-mark-rings)))
(ido-mode -1) ;; Turn off ido mode in case I enabled it accidentally
(global-unset-key (kbd "C-x c"))

(use-package helm-ag
  :bind
  (("C-c g" . helm-ag)))

(use-package helm-swoop
  :init
  (progn
    (setq helm-swoop-split-direction 'split-window-vertically)
    (setq helm-swoop-speed-or-color nil)
    (setq helm-swoop-move-to-line-cycle t)
    (setq helm-swoop-split-with-multiple-windows t)
    (setq helm-multi-swoop-edit-save t)
    (setq helm-swoop-use-fuzzy-match t)
    (setq helm-swoop-use-line-number-face t))
  :bind
  (;; ("C-s" . helm-swoop)
   ("M-s s" . helm-swoop)
   ("C-c M-i" . helm-multi-swoop)
   ("C-x M-i" . helm-multi-swoop-all))
  :config
  (progn
    (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
    (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)))

(use-package helm-descbinds
  :defer t
  :bind (("C-h b" . helm-descbinds)
         ("C-h w" . helm-descbinds)))

(use-package helm-buffers
  :config
  (add-to-list 'helm-boring-buffer-regexp-list "^TAGS$")
  (add-to-list 'helm-boring-buffer-regexp-list "vc-diff")
  (add-to-list 'helm-boring-buffer-regexp-list "Message")
  (add-to-list 'helm-boring-buffer-regexp-list "ESS")
  (add-to-list 'helm-boring-buffer-regexp-list "log-edit-files")
  (add-to-list 'helm-boring-buffer-regexp-list "vc")
  (add-to-list 'helm-boring-buffer-regexp-list "Compile-Log"))

(helm-autoresize-mode 1)

;; -----------------------------------------------------------------------------

;; projectile mode
(use-package projectile
  :ensure t
  :config
  (projectile-mode +1))
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(setq projectile-switch-project-action 'helm-projectile)
(setq projectile-enable-caching t)

;; set up org-mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "CANCELED" "DONE")))
(setq org-agenda-files '("~/Dropbox/org-mode/"))
(setq org-return-follows-link t)
(setq org-startup-with-inline-images t)
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 6))))

(global-set-key (kbd "<f7>") 'ispell-word)

;; (require 'yaml-mode)
;; (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;; (setq sml-prog-name "/usr/bin/sml")
;; (add-to-list 'auto-mode-alist '("\\.\\(sml\\|sig\\)\\'" . sml-mode))
;; (defun my-sml-mode-hook () "Local defaults for SML mode"
;;        (setq sml-indent-level 2)        ; conserve on horizontal space
;;        (setq words-include-escape t)    ; \ loses word break status
;;        (setq indent-tabs-mode nil))     ; never ever indent with tabs
;; (add-hook 'sml-mode-hook 'my-sml-mode-hook)

(require 'gnuplot-mode)

;; (require 'adoc-mode)
;; (add-hook 'adoc-mode-hook (lambda() (buffer-face-mode t)))
;; (add-to-list 'auto-mode-alist (cons "\\.txt\\'" 'adoc-mode))

;; -----------------------------------------------------------------------------
;; for python

(package-initialize)
(elpy-enable)
(elpy-use-ipython)

;; enable autopep8 formatting on save
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

(setq python-shell-interpreter "/usr/bin/python3.5")
;; (setq python-shell-interpreter "/usr/local/bin/ipython3")
;; use IPython
;; (setq-default py-shell-name "ipython")
;; (setq-default py-which-bufname "IPython")
;; use the wx backend, for both mayavi and matplotlib
(setq py-python-command-args
      '("--gui=wx" "--pylab=wx" "-colors" "Linux"))
(setq py-force-py-shell-name-p t)

;; switch to the interpreter after executing code
(setq py-shell-switch-buffers-on-execute-p t)
(setq py-switch-buffers-on-execute-p t)

;; try to automagically figure out indentation
(setq py-smart-indentation t)


;; -----------------------------------------------------------------------------
;; C

(require 'cc-mode)
(require 'semantic)

(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)

(semantic-mode 1)

(setq company-backends (delete 'company-semantic company-backends))
(define-key c-mode-map  [(tab)] 'company-complete)
(define-key c++-mode-map  [(tab)] 'company-complete)


;; -----------------------------------------------------------------------------

(defun anchu/insert-minor-section ()
  "Insert minor section heading for a snippet of R codes."
  (interactive)
  (insert "## -----------------------------------------------------------------------------\n")
  (insert "## "))

(global-set-key (kbd "C-c C-a n") 'anchu/insert-minor-section)

(defun anchu/insert-r-code-chunk ()
  "Insert R Markdown code chunk."
  (interactive)
  (insert "```{r, include=FALSE}\n")
  (insert "\n")
  (save-excursion
    (insert "\n")
    (insert "\n")
    (insert "```\n")))

(global-set-key (kbd "C-c C-a c") 'anchu/insert-r-code-chunk)

(defun anchu/insert-major-section ()
  "Insert major section heading for a block of R codes."
  (interactive)
  (insert "## -----------------------------------------------------------------------------\n")
  (insert "## ")
  (save-excursion
    (insert "\n")
    (insert "## -----------------------------------------------------------------------------\n")))

(global-set-key (kbd "C-c C-a m") 'anchu/insert-major-section)

(defun anchu/insert-resource-header ()
  "Insert yaml-like header for R script resources."
  (interactive)
  (insert "## -----------------------------------------------------------------------------\n")
  (insert "## code: ")
  (save-excursion
    (insert "\n")
    (insert "## description: \n")
    (insert "## author: \n")
    (insert (concat "## date: " (current-time-string) "\n"))
    (insert "## -----------------------------------------------------------------------------\n")))

(global-set-key (kbd "C-c C-a r") 'anchu/insert-resource-header)

(defun anchu/insert-yalm-header ()
  "Insert Rmd header."
  (interactive)
  (insert "---\n")
  (insert "title: ")
  (save-excursion
    (newline)
    (insert "author: \n")
    (insert "date: \"`r format(Sys.time(), '%d-%m-%Y %H:%M:%S')`\"\n")
    (insert "runtime: shiny\n")
    (insert "output:\n")
    (indent-to-column 4)
    (insert "html_document:\n")
    (indent-to-column 8)
    (insert "theme: flatly\n")
    (insert "---")
    (newline)))

(global-set-key (kbd "C-c C-a y") 'anchu/insert-yalm-header)

(defun anchu/insert-named-comment (cmt)
  "Make comment header"
  (interactive "sEnter your comment: ")
  (let* ((user-cmt (concat "## " cmt " "))
         (len-user-cmt (length user-cmt))
         (len-hyphen (- 80 len-user-cmt)))
    (insert user-cmt (apply 'concat (make-list len-hyphen "-")))
    (newline)
    (newline)
    )
  )

(global-set-key (kbd "C-c C-a d") 'anchu/insert-named-comment)

(use-package dumb-jump
  :ensure t
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g b" . dumb-jump-back)
         ("M-g q" . dumb-jump-quick-look)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config (setq dumb-jump-selector 'helm))

(require 'neotree)
(setq neo-smart-open t)
(global-set-key [f9] 'neotree-toggle)

;; -----------------------------------------------------------------------------

(defun dired-back-to-top ()
  (interactive)
  (goto-char (point-min))
  (dired-next-line 4))

(define-key dired-mode-map
  (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)

(defun dired-jump-to-bottom ()
  (interactive)
  (goto-char (point-max))
  (dired-next-line -1))

(define-key dired-mode-map
  (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)

;; -----------------------------------------------------------------------------

;; Move more quickly
(global-set-key (kbd "C-S-n")
                (lambda ()
                  (interactive)
                  (ignore-errors (forward-line 5))))

(global-set-key (kbd "C-S-p")
                (lambda ()
                  (interactive)
                  (ignore-errors (forward-line -5))))

(global-set-key (kbd "C-S-f")
                (lambda ()
                  (interactive)
                  (ignore-errors (forward-char 5))))

(global-set-key (kbd "C-S-b")
                (lambda ()
                  (interactive)
                  (ignore-errors (backward-char 5))))

;; -----------------------------------------------------------------------------

(defun move-line-down ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))

(defun move-line-up ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines -1))
    (forward-line -1)
    (move-to-column col)))

(global-set-key (kbd "C-S-j") 'move-line-down)
(global-set-key (kbd "C-S-k") 'move-line-up)

(fset 'eval-code-chunk
      (lambda (&optional arg) "Keyboard macro."
        (interactive "p")
        (kmacro-exec-ring-item
         (quote ([18 96 96 96 13 14 67108896 19 96 96 96 13 16 3 3 134217838 134217742] 0 "%d")) arg)))

(global-set-key (kbd "<f8>") 'eval-code-chunk)
