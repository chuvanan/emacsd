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
(tool-bar-mode -1)
(global-hl-line-mode +1)

;; disable menu bar
(menu-bar-mode -1)

;; visual line
(setq line-move-visual t)
(setq scroll-margin 5)
(setq scroll-step            1
      scroll-conservatively  10000)
;; (fringe-mode 0)
(fringe-mode '(4 . 0))


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
(setq initial-major-mode 'org-mode)
(setq initial-scratch-message nil)
(fset 'display-startup-echo-area-message #'ignore)
;; (set-face-attribute 'vertical-border nil :foreground (face-attribute 'fringe :background))

;; turn off bidirectional text
(setq-default bidi-paragraph-direction 'left-to-right)

;; mode line settings
(line-number-mode t)
(column-number-mode t)

;; mouse avoidance
(mouse-avoidance-mode 'banish)

;; disable scroll-bar
(scroll-bar-mode -1)

;; fullscreen
(when (fboundp 'toggle-frame-maximized)
  (toggle-frame-maximized))

;; Indenting
(electric-indent-mode 1)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

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
(setq echo-keystrokes 0)

;; switch to other window
;; (global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "<f5>") 'calendar)
(global-set-key (kbd "<f6>") 'calculator)
(global-set-key (kbd "M-]") 'forward-paragraph)
(global-set-key (kbd "M-[") 'backward-paragraph)

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
      ;; Save the contents of the clipboard to kill ring before killing
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

;; yas-snipppet
(use-package yasnippet
  :ensure t
  :config
  (yas-reload-all)
  (add-hook 'ess-mode-hook #'yas-minor-mode)
  (add-hook 'markdown-mode-hook #'yas-minor-mode))

;; Save point position in files
(use-package saveplace
  :init (save-place-mode 1))

;; ibuffer
(use-package ibuffer
  :bind (([remap list-buffers] . ibuffer))
  ;; Show VC Status in ibuffer
  :config
  (setq ibuffer-formats
        '((mark modified read-only vc-status-mini " "
                (name 18 18 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                (vc-status 16 16 :left)
                " "
                filename-and-process)
          (mark modified read-only " "
                (name 18 18 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " " filename-and-process)
          (mark " " (name 16 -1) " " filename))))

(use-package ibuffer-vc
  :ensure t
  :defer t
  :init (add-hook 'ibuffer-hook
                  (lambda ()
                    (ibuffer-vc-set-filter-groups-by-vc-root)
                    (unless (eq ibuffer-sorting-mode 'alphabetic)
                      (ibuffer-do-sort-by-alphabetic)))))

;; Save minibuffer history
(use-package savehist
  :init (savehist-mode t)
  :config (setq savehist-save-minibuffer-history t
                savehist-autosave-interval 180))

;; add stripes to a buffer
(use-package stripe-buffer
  :ensure t
  :init (add-hook 'dired-mode-hook #'stripe-buffer-mode))

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

;; beacon
(use-package beacon
  :ensure t
  :config
  (beacon-mode 1)
  (setq beacon-push-mark 35)
  (setq beacon-color "#666600")
  (setq beacon-blink-duration 0.2)
  (setq beacon-size 30)
  (setq beacon-dont-blink-major-modes
        (append beacon-dont-blink-major-modes '(inferior-ess-mode))))

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

(use-package ido-ubiquitous
  :ensure t
  :config
  (ido-ubiquitous-mode +1))

(use-package keyfreq
  :ensure t
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

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

(use-package ido-vertical-mode
  :ensure t
  :config
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only))

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
	(add-hook 'ess-mode-hook #'aggressive-indent-mode))

(require 'ess-site)
(require 'ess-rutils)
;; (autoload 'r-mode "ess-site" "(Autoload)" t)
;; dont ask for new data directory
(setq ess-ask-for-ess-directory nil)
;; don't print the evaluated commands
(setq ess-eval-visibly nil)

;; style
(setq ess-default-style 'RStudio)

;; company backend
(setq-local company-backends
            (append '((company-dabbrev-code company-R-args company-R-objects))
                    company-backends))
(setq ess-use-company nil)

;; "Highlights delimiters such as parentheses, brackets or braces according to their depth."
(add-hook 'ess-mode-hook #'rainbow-delimiters-mode)

;; Fancy up the prompt (see also ~/.Rprofile)
;; http://www.wisdomandwonder.com/article/9687/i-wasted-time-with-a-custom-prompt-for-r-with-ess
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
    ("f34b107e8c8443fe22f189816c134a2cc3b1452c8874d2a4b2e7bb5fe681a10b" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "e8e744a1b0726814ac3ab86ad5ccdf658b9ff1c5a63c4dc23841007874044d4a" "9b65cf71fd6b27a5362afeff062c6abd1c5d8a7c4d444c942f3da36bf0a151b1" "cdfb22711f64d0e665f40b2607879fcf2607764b2b70d672ddaa26d2da13049f" "232f715279fc131ed4facf6a517b84d23dca145fcc0e09c5e0f90eb534e1680f" "9956eace4d6a1df9bd8c5875406c3dab0b98dd385d3bc99a83aaf730526a6056" "c7f838704d7caa88bc337464867c22af0a502e32154558b0f6c9c3c6e8650122" "c3e6b52caa77cb09c049d3c973798bc64b5c43cc437d449eacf35b3e776bf85c" "5a0eee1070a4fc64268f008a4c7abfda32d912118e080e18c3c865ef864d1bea" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "03e3e79fb2b344e41a7df897818b7969ca51a15a67dc0c30ebbdeb9ea2cd4492" "0ae52e74c576120c6863403922ee00340a3bf3051615674c4b937f9c99b24535" "aed73c6d0afcf2232bb25ed2d872c7a1c4f1bda6759f84afc24de6a1aec93da8" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "f245c9f24b609b00441a6a336bcc556fe38a6b24bfc0ca4aedd4fe23d858ba31" "1dfd7a150e80fdb4563f594716d09d849f4c50bcea12825bd8d284c05a87a3e1" "9cb6358979981949d1ae9da907a5d38fb6cde1776e8956a1db150925f2dad6c1" "4d80487632a0a5a72737a7fc690f1f30266668211b17ba836602a8da890c2118" "15348febfa2266c4def59a08ef2846f6032c0797f001d7b9148f30ace0d08bcf" "3f5701c23d328be03536349b29cb24c5cfa79ea9ef9c46cf89668eda16b88a9c" "12b7ed9b0e990f6d41827c343467d2a6c464094cbcc6d0844df32837b50655f9" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(default-input-method "vietnamese-telex")
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
    (flycheck goto-last-change polymode multiple-cursors stripe-buffer helm-descbinds ibuffer-vc ido-vertical-mode smart-mode-line-powerline smart-mode-line-powerline-theme rainbow-delimiters tldr anzu hungry-delete swiper r-autoyas beacon ag ido-ubiquitous ace-window evil-leader keyfreq apropospriate-theme seoul256-theme icicles visible-mark company-jedi avy imenu-anywhere aggressive-indent zenburn-theme projectile powerline base16-theme tango-plus-theme greymatters-theme flatui-theme meaculpa-theme smart-mode-line csv-mode helm-R helm which-key smex evil window-numbering company easy-kill use-package magit solarized-theme expand-region markdown-mode auto-complete smartparens org)))
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

(setq show-paren-delay 0)
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

;; http://www.emacswiki.org/emacs/ZapToISearch
(defun zap-to-isearch (rbeg rend)
  "Kill the region between the mark and the closest portion of
the isearch match string. The behaviour is meant to be analogous
to zap-to-char; let's call it zap-to-isearch. The deleted region
does not include the isearch word. This is meant to be bound only
in isearch mode.  The point of this function is that oftentimes
you want to delete some portion of text, one end of which happens
to be an active isearch word. The observation to make is that if
you use isearch a lot to move the cursor around (as you should,
it is much more efficient than using the arrows), it happens a
lot that you could just delete the active region between the mark
and the point, not include the isearch word."
  (interactive "r")
  (when (not mark-active)
    (error "Mark is not active"))
  (let* ((isearch-bounds (list isearch-other-end (point)))
         (ismin (apply 'min isearch-bounds))
         (ismax (apply 'max isearch-bounds))
         )
    (if (< (mark) ismin)
        (kill-region (mark) ismin)
      (if (> (mark) ismax)
          (kill-region ismax (mark))
        (error "Internal error in isearch kill function.")))
    (isearch-exit)
    ))

(define-key isearch-mode-map [(meta z)] 'zap-to-isearch)

;; Search back/forth for the symbol at point
;; See http://www.emacswiki.org/emacs/SearchAtPoint
(defun isearch-yank-symbol ()
  "*Put symbol at current point into search string."
  (interactive)
  (let ((sym (symbol-at-point)))
    (if sym
        (progn
          (setq isearch-regexp t
                isearch-string (concat "\\_<" (regexp-quote (symbol-name sym)) "\\_>")
                isearch-message (mapconcat 'isearch-text-char-description isearch-string "")
                isearch-yank-flag t))
      (ding)))
  (isearch-search-and-update))

(define-key isearch-mode-map "\C-\M-w" 'isearch-yank-symbol)


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
;; See http://orgmode.org/org.html#Languages
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

(defun anchu-set-cursor ()
  (set-cursor-color "gold") ;; set cursor color to gold
  (cond
   (buffer-read-only
    (setq cursor-type 'box))
   (t
    (setq cursor-type 'bar)))
  ;; red cursor for overwrite mode
  (when overwrite-mode
    (set-cursor-color "red")))

(add-hook 'post-command-hook 'anchu-set-cursor)


;; kill as exit
(require 'cl)
(defadvice save-buffers-kill-emacs
  (around no-query-kill-emacs activate)
  "Prevent \"Active processes exist\" query on exit."
  (flet ((process-list ())) ad-do-it))



(defun find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))

(global-set-key (kbd "C-c I") 'find-user-init-file)

(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(global-set-key (kbd "C-c r")  'rename-file-and-buffer)

(set-face-attribute 'vertical-border nil :foreground (face-attribute 'fringe :background))
