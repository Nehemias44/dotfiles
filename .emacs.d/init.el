;;; package --- Summary
;;; Commentary:
;;; Code:
;;; Basic settings
(setq default-frame-alist
      (append (list
	       '(font . "Sarasa Term J")
	       '(min-height . 1)
               '(height     . 45)
	       '(min-width  . 1)
               '(width      . 81)
	       '(left . 40)
               '(top . 50)
               '(vertical-scroll-bars . nil)
               '(internal-border-width . 30)
               '(left-fringe    . 1)
               '(right-fringe   . 1)
               '(tool-bar-lines . 0)
               '(menu-bar-lines . 0))))

(setq x-underline-at-descent-line t)

(setq window-divider-default-right-width 24)
(setq window-divider-default-places 'right-only)
(window-divider-mode 1)

(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-scratch-message nil
      initial-buffer-choice nil
      frame-title-format nil
      use-file-dialog nil
      use-dialog-box nil
      pop-up-windows nil
      indicate-empty-lines nil
      cursor-in-non-selected-windows nil
      initial-major-mode 'text-mode
      default-major-mode 'text-mode
      auto-fill-mode nil
      fill-column 80
      completion-styles '(basic substring)
      visible-bell              nil
      sentence-end-double-space nil
      make-backup-files         nil
      backup-directory-alist    '(("" . "~/.emacs.d/backup"))
      scroll-conservatively     1000)


(setq-default cursor-type 'bar
	      org-support-shift-select  t
	      c-basic-offset 8)

(fset 'yes-or-no-p 'y-or-n-p)
(electric-pair-mode +1)
(delete-selection-mode +1)

(require 'display-line-numbers)
(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(setq-default c-basic-offset 8)


;;; Packages settings
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
	"straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package doom-themes
  :straight t
  :config
  (load-theme 'doom-one-light t))

(straight-use-package
  '(nano-emacs :type git :host github :repo "rougier/nano-emacs"))

(require 'nano-theme-light)
(require 'nano-faces)
(nano-faces)

(set-face-attribute 'window-divider nil
                      :foreground (face-background 'nano-face-default))
(set-face-attribute 'window-divider-first-pixel nil
                    :foreground nano-color-background)
  ;;                  :foreground (face-background 'nano-face-subtle))
(set-face-attribute 'window-divider-last-pixel nil
                      :foreground nano-color-background)

(set-face-attribute 'mode-line nil
                      :height 0.1
		      :foreground (face-foreground 'nano-face-default)
                      :background (face-background 'nano-face-default)
                      ;; :underline  (if (display-graphic-p)
                      ;;                 (face-background 'nano-face-subtle)
                      ;;               t)
		      :underline t
                      :overline nil
                      :box nil)

(set-face-attribute 'mode-line-inactive nil
                    :height 0.1
		    :foreground (face-foreground 'nano-face-default)
                    :background (face-background 'nano-face-default)
                    ;; :underline (if (display-graphic-p)
                    ;;                (face-background 'nano-face-subtle)
                    ;;              t)
		    :underline t
                    :overline nil
                    :inherit nil
                    :box nil)

(set-face-attribute 'header-line nil
                    :weight 'light
                    ;; :foreground (face-foreground 'nano-face-default)
                    ;; :background (face-background 'nano-face-default)
		    :foreground (doom-color 'modeline-fg)
		    :background (doom-color 'modeline-bg)
                    :overline nil
                    :underline nil
                    :box nil
                    :box `(:line-width 1
                                       :color ,(face-background 'nano-face-default)
                                       :style nil)
                    :inherit nil)

(set-face-attribute 'internal-border nil
                    :background (face-background 'nano-face-default))

(set-face-foreground 'vertical-border nano-color-subtle)

(require 'nano-modeline)
(require 'nano-theme)

(use-package company
  :straight t
  :config
  (setq-default company-minimum-prefix-length 3
		company-idle-delay 0)
  (global-company-mode))

(use-package company-box
  :straight t
  :hook
  (company-mode . company-box-mode))

(use-package projectile
  :straight t
  :init
  (projectile-mode 1)
  :bind
  ("C-c p" . projectile-command-map))

(use-package which-key
  :straight t
  :config
  (which-key-mode t))

(use-package all-the-icons
  :straight t)

(use-package all-the-icons-dired
  :straight '(:type git :host github :repo "jtbm37/all-the-icons-dired")
  :hook
  (dired-mode . all-the-icons-dired-mode))

(use-package diredfl
  :straight t
  :hook
  (dired-mode . diredfl-mode))

(use-package dired-open
  :straight t
  :config
  (setq dired-open-extensions '(("gif" . "sxiv")
				("jpg" . "sxiv")
				("png" . "sxiv")
				("mkv" . "mpv")
				("mp4" . "mpv"))))

(use-package peep-dired
  :straight t)

(setq dired-listing-switches "-al --group-directories-first")

(use-package iedit
  :straight t)

(use-package lsp-mode
  :straight t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-eldoc-enable-hover nil)
  (setq lsp-ui-sideline-show-diagnostics nil)
  (setq lsp-rust-server 'rust-analyzer)
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (c-mode . lsp)
	 (c++-mode . lsp)
	 (rust-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package eglot
  :straight t
  :config
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
  :hook
  (c-mode-hook . eglot-straight)
  (c++-mode-hook . eglot-straight))

;; optionally
(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode)
;; if you are helm user
(use-package helm-lsp
  :straight t
  :commands helm-lsp-workspace-symbol)
;; optionally if you want to use debugger
;; (use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

(use-package yasnippet
  :straight t
  :config
  (use-package yasnippet-snippets
    :straight t)
  (setq yas-snippet-dirs
	       '("~/.emacs.d/snippets"
		 "~/.emacs.d/straight/repos/yasnippet-snippets/snippets"))
  (yas-global-mode 1))

(use-package rust-mode
  :straight t
  :hook
  (rust-mode . (lambda () (prettify-symbols-mode)))
  :config
  (define-key rust-mode-map (kbd "C-c C-c") 'rust-run))

(use-package flycheck
  :straight t
  :config
  (global-flycheck-mode))

(with-eval-after-load "ispell"
  (setenv "LANG" "es_ES.UTF-8")
  (defvar ispell-program-name "hunspell"))

(use-package org-bullets
  :straight t
  :hook
  (org-mode . (lambda () (org-bullets-mode 1))))

(use-package multiple-cursors
  :straight t
  :bind
  ("C->"            . mc/mark-next-like-this)
  ("C-<"            . mc/mark-previous-like-this)
  ("C-c C-<"        . mc/mark-all-like-this)
  ("C-S-<mouse-1>"  . mc/add-cursor-on-click))

(use-package tiny
  :straight t
  :bind
  ("C-:" . tiny-expand))

(use-package vterm
  :straight t
  :config
  (use-package vterm-toggle
    :straight t
    :config
    (setq vterm-toggle-fullscreen-p nil)
    (add-to-list 'display-buffer-alist
    '((lambda (bufname _) (equal bufname vterm-buffer-name))
       (display-buffer-reuse-window display-buffer-in-direction)
       (direction . bottom)
       (dedicated . t)
       (reusable-frames . visible)
       (window-height . 0.3)))
    :bind
    ("C-x C-t" . vterm-toggle)))

(use-package rainbow-mode
  :straight t
  :config
  (rainbow-mode t))

(use-package rainbow-delimiters
  :straight t
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package yaml-mode
  :straight t)
(require 'org)
;; (setq org-emphasis-alist
;;   '(("*" (bold :foreground "DarkOrchid")) ;; this make bold both italic and bold, but not color change
;;     ("/" (italic :foreground "grey30" )) ;; italic text, the text will be "dark salmon"
;;     ("_" underline :foreground "cyan") ;; underlined text, color is "cyan"
;;     ("=" (:background "snow1" :foreground "deep slate blue" )) ;; background of text is "snow1" and
;;     ("~" (:foreground "Green1" ))
;;     ("+" (:background (doom-color 'bg) :foreground "blue3"))
;;     ))

(setq org-startup-indented t)
(setq org-hide-emphasis-markers t)
(setq org-format-latex-options '(plist-put org-format-latex-options :scale 2.0))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-agenda-files '("~/org/Ungs.org"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(warning-suppress-log-types '((comp) (comp)))
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here
