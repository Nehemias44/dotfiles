;;; package --- Summary:
;;; Commentary:
;;; Code:
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

;; Using garbage magic hack.
(use-package gcmh
  :straight t
  :config
  (gcmh-mode 1))
;; Setting garbage collection threshold
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))
;; Silence compiler warnings as they can be pretty disruptive (setq comp-async-report-warnings-errors nil)

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

;; (use-package helm
;;   :straight t
;;   :bind
;;   ("M-x" . helm-M-x)
;;   ("C-x C-f" . helm-find-files)
;;   ("C-x b" . helm-buffers-list)
;;   :config
;;   (helm-mode 1))

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

(use-package latex-preview-pane
  :straight t
  :config
  (setq text-mode-hook '(lambda() (flyspell-mode t) ))
  (setq latex-preview-pane-use-frame t)
  :hook
  (LaTeX-mode . flyspell-mode)
  (LaTeX-mode . latex-preview-pane-mode))

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

(setq-default c-basic-offset 8)

(toggle-scroll-bar -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(electric-pair-mode +1)
(delete-selection-mode +1)

(setq-default cursor-type 'bar
	          frame-title-format '("Emacs %f")
		  org-support-shift-select  t)

(setq inhibit-startup-echo-area-message "tychoish"
      inhibit-startup-message   t
      ;initial-major-mode        'fundamental-mode
      initial-scratch-message   nil
      visible-bell              nil
      sentence-end-double-space nil
      make-backup-files         nil
      backup-directory-alist    '(("" . "~/.emacs.d/backup"))
      scroll-conservatively     10000)

(require 'cl)
(defun loadup-gen ()
  "Generate the lines to include in the lisp/loadup.el file
to place all of the libraries that are loaded by your .emacs
into the main dumped emacs"
  (interactive)
  (defun get-loads-from-*Messages* ()
    (save-excursion
      (let ((retval ()))
	(set-buffer "*Messages*")
	(beginning-of-buffer)
	(while (search-forward-regexp "^Loading " nil t)
	  (let ((start (point)))
	    (search-forward "...")
	    (backward-char 3)
	    (setq retval (cons (buffer-substring-no-properties start (point)) retval))))
	retval)))
  (map 'list
       (lambda (file) (princ (format "(load \"%s\")\n" file)))
       (get-loads-from-*Messages*)))
(loadup-gen)

(set-frame-font "Sarasa Term J-12")
(add-to-list 'default-frame-alist
	     '(font . "Sarasa Term J-12"))

(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(use-package hide-mode-line
  :straight t
  :hook
  (helm-mode      . hide-mode-line-mode)
  (which-key-mode . hide-mode-line-mode)
  (vterm-mode     . hide-mode-line-mode))

;(setq my-modeline-bg (doom-color 'modeline-bg))
;; (set-face-attribute 'header-line nil
;; 		    :font "Roboto Mono"
;; 		    :height 105)

(use-package yaml-mode
  :straight t)

(use-package svg-tag-mode
  :straight t)

(use-package org-fragtog
  :straight t
  :hook
  (org-mode . org-fragtog-mode))

;; (use-package mu4e
;;   :straight t)

;; (require 'mu4e)

;; ;; ---------------------------------------------
;; ;; General conf settings
;; ;; ---------------------------------------------

;; ;; Set keybind to enter mu4
;; (global-set-key (kbd "C-x t") 'mu4e)

;; (setq mu4e-user-mail-address-list '("leandro.nehemias@gmail.com"
;; 					;"mu4e.example@outlook.it"
;; 				    ))

;; ;; viewing options
;; (setq mu4e-view-show-addresses t)
;; ;; Do not leave message open after it has been sent
;; (setq message-kill-buffer-on-exit t)
;; ;; Don't ask for a 'context' upon opening mu4e
;; (setq mu4e-context-policy 'pick-first)
;; ;; Don't ask to quit
;; (setq mu4e-confirm-quit nil)

;; (setq mu4e-maildir-shortcuts
;;       '(("/GmailAccount/INBOX" . ?g)
;; 	;("/OutlookAccount/INBOX" . ?o)
;; 	))

;; ;; attachments go here
;; (setq mu4e-attachment-dir  "~/Descargas/MailAttachments")

;; ;; modify behavior when putting something in the trash (T flag) so as
;; ;; to make it sync to the remote server. This code deals with the bug
;; ;; that, whenever a message is marked with the trash label T,
;; ;; offlineimap wont sync it back to the gmail servers.
;; ;;
;; ;; NOTE: Taken from
;; ;; http://cachestocaches.com/2017/3/complete-guide-email-emacs-using-mu-and-/
;; (defun remove-nth-element (nth list)
;;   (if (zerop nth) (cdr list)
;;     (let ((last (nthcdr (1- nth) list)))
;;       (setcdr last (cddr last))
;;       list)))
;; (setq mu4e-marks (remove-nth-element 5 mu4e-marks))
;; (add-to-list 'mu4e-marks
;; 	     '(trash
;; 	       :char ("d" . "▼")
;; 	       :prompt "dtrash"
;; 	       :dyn-target (lambda (target msg) (mu4e-get-trash-folder msg))
;; 	       :action (lambda (docid msg target)
;; 			 (mu4e~proc-move docid
;; 					 (mu4e~mark-check-target target) "-N"))))


;; ;; ---------------------------------------------
;; ;; Contexts conf settings
;; ;; ---------------------------------------------
;; (setq mu4e-contexts
;;       `(
;; 	,(make-mu4e-context
;; 	  :name "Gmail Account"
;; 	  :match-func (lambda (msg)
;; 			(when msg
;; 			  (mu4e-message-contact-field-matches
;; 			   msg '(:from :to :cc :bcc) "leandro.nehemias@gmail.com")))

;; 	  :vars '(
;; 		  (mu4e-trash-folder . "/GmailAccount/[Gmail].Papelera")
;; 		  (mu4e-refile-folder . "/GmailAccount/[Gmail].Archive")
;; 		  (mu4e-drafts-folder . "/GmailAccount/[Gmail].Borradores")
;; 		  (mu4e-sent-folder . "/GmailAccount/[Gmail].Enviados")
;; 		  (user-mail-address  . "leandro.nehemias@gmail.com")
;; 		  (user-full-name . "Leandro Camperoz")
;; 		  (smtpmail-smtp-user . "leandro.nehemias")
;; 		  (smtpmail-local-domain . "gmail.com")
;; 		  (smtpmail-default-smtp-server . "smtp.gmail.com")
;; 		  (smtpmail-smtp-server . "smtp.gmail.com")
;; 		  (smtpmail-smtp-service . 587)
;; 		  ))

;; 	;; ,(make-mu4e-context
;; 	;;   :name "Outlook Account"
;; 	;;   :match-func (lambda (msg) (when msg
;; 	;; 			      (string-prefix-p "/OutlookAccount" (mu4e-message-field msg :maildir))))
;; 	;;   :vars '(
;; 	;; 	  (mu4e-trash-folder . "/OutlookAccount/Junk")
;; 	;; 	  (mu4e-refile-folder . "/OutlookAccount/Archivio")
;; 	;; 	  (mu4e-drafts-folder . "/OutlookAccount/Drafts")
;; 	;; 	  (mu4e-sent-folder . "/OutlookAccount/Sent")
;; 	;; 	  (user-mail-address . "mu4e.example@outlook.it")
;; 	;; 	  (smtpmail-smtp-user . "mu4e.example")
;; 	;; 	  (smtpmail-local-domain . "outlook.it")
;; 	;; 	  (smtpmail-default-smtp-server . "outlook.it")
;; 	;; 	  (smtpmail-smtp-server . "smtp.outlook.it")
;; 	;; 	  (smtpmail-smtp-service . 587)
;; 	;; 	  ))
;; 	))

;; ;; Set how email is to be sent
;; (setq send-mail-function (quote smtpmail-send-it))

(add-to-list 'load-path "~/.emacs.d/elisp")

(use-package doom-themes
  :straight t
  :config
  (setq doom-themes-enable-italic t
	doom-themes-enable-bold t)
  (load-theme 'doom-one-light t))

(use-package ts
  :straight t)

(require 'nano-faces)
(require 'nano-modeline)
(nano-faces)

(require 'org)
(setq org-emphasis-alist
  '(("*" (bold :foreground "DarkOrchid")) ;; this make bold both italic and bold, but not color change
    ("/" (italic :foreground "grey30" )) ;; italic text, the text will be "dark salmon"
    ("_" underline :foreground "cyan") ;; underlined text, color is "cyan"
    ("=" (:background (doom-color 'bg) :foreground "red" )) ;; background of text is "snow1" and
    ("~" (:foreground "green4" ))
    ("+" (:background (doom-color 'bg) :foreground "blue3"))
    ))

(setq org-startup-indented t)
(setq org-hide-emphasis-markers t)
(setq org-format-latex-options '(plist-put org-format-latex-options :scale 2.0))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-agenda-files '("~/org/Ungs.org"))
(require 'org-beauty)

(setq-default mode-line-format "")

(set-face-attribute 'font-lock-keyword-face nil
		    :slant 'italic)
(set-face-attribute 'font-lock-comment-face nil
		    :slant 'italic
		    :weight 'bold)
(set-face-attribute 'font-lock-type-face nil
		    :slant 'italic)

(set-face-attribute 'header-line nil
		    :underline nil
		    :overline nil)

(set-face-attribute 'mode-line nil
		    :background (doom-color 'bg)
		    :foreground (doom-color 'base3)
		    :underline t
		    :overline nil)

(set-face-attribute 'mode-line-inactive nil
		    :background (doom-color 'bg)
		    :foreground (doom-color 'modeline-bg)
		    :underline t
		    :overline nil)

(require 'frame)
(setq-default window-divider-default-right-width 18
	      window-divider-default-bottom-width 1
	      window-divider-default-places 'right-only)
(window-divider-mode t)
;; Make sure new frames use window-divider
(add-hook 'before-make-frame-hook 'window-divider-mode)

(set-face-attribute 'window-divider nil
		    :foreground (doom-color 'bg))
(set-face-attribute 'window-divider-first-pixel nil
		    :foreground (doom-color 'bg))
(set-face-attribute 'window-divider-last-pixel nil
		    :foreground (doom-color 'bg))
(set-face-background 'vertical-border (doom-color 'bg))

(provide 'init)
;;; init.el ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values '((eval ((require 'org-beauty) (org-beauty)))))
 '(warning-suppress-log-types '((comp) (comp)))
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
