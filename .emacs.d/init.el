(setq default-frame-alist
      (append (list
	       '(font . "Iosevka Nerd Font-13")
	       '(min-height . 1)
               '(height     . 42)
	       '(min-width  . 1)
               '(width      . 86)
	       '(left . 40)
               '(top . 50)
               '(vertical-scroll-bars . nil)
               '(internal-border-width . 30)
               '(left-fringe    . 1)
               '(right-fringe   . 1)
               '(tool-bar-lines . 0)
               '(menu-bar-lines . 0))))

(setq x-underline-at-descent-line t)

(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      inhibit-x-resources t
      initial-scratch-message nil
      initial-buffer-choice nil
      help-window-select t
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

(setq-default org-support-shift-select  t
	      c-basic-offset 8)

(fset 'yes-or-no-p 'y-or-n-p)
(electric-pair-mode +1)
(delete-selection-mode +1)

(setq byte-compile-warnings '(cl-functions))

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

(setq-default cursor-type '(bar . 2))
(set-cursor-color "#a626a4")

(use-package doom-themes
  :straight t
  :config
   (setq doom-themes-enable-bold t    ;if nil, bold is universally disabled
         doom-themes-enable-italic t) ;if nil, italics is universally disabled
   (load-theme 'doom-gruvbox-light t))

(straight-use-package
  '(nano-emacs :type git :host github :repo "rougier/nano-emacs"))

(require 'nano-theme-light)
(require 'nano-theme)

(setq nano-color-foreground (doom-color 'fg))
(setq nano-color-background (doom-color 'bg))
(setq nano-color-highlight  (doom-color 'base3))
(setq nano-color-critical   (doom-color 'red))
(setq nano-color-salient    (doom-color 'magenta))
(setq nano-color-strong     (doom-color 'base8))
(setq nano-color-popout     (doom-color 'green))
(setq nano-color-subtle     (doom-color 'base1))
(setq nano-color-faded      (doom-color 'base4))

(require 'nano-faces)
(require 'nano-modeline)
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
		      :foreground nano-color-highlight
                      :background (face-background 'nano-face-default)
                      ;; :underline  (if (display-graphic-p)
                      ;;                 (face-background 'nano-face-subtle)
                      ;;               t)
		      :underline t
                      :overline nil
                      :box nil)

(set-face-attribute 'mode-line-inactive nil
                    :height 0.1
		    :foreground nano-color-highlight
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
                    :foreground (face-foreground 'nano-face-default)
                    :background (face-background 'nano-face-default)
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

;; (set-face-attribute 'nano-face-header-default nil
;; 		    :background (doom-color 'bg-alt))
;; (set-face-attribute 'nano-face-header-strong nil
;; 		    :background (doom-color 'bg-alt))

(set-face-attribute 'font-lock-keyword-face nil
		    :weight 'bold
		    :slant 'italic)
(set-face-attribute 'font-lock-comment-face nil
		    :slant 'italic
		    :weight 'bold)
(set-face-attribute 'font-lock-type-face nil
		    :slant 'italic)
(set-face-attribute 'line-number nil
		    :slant 'italic)


(use-package all-the-icons
  :straight t)

(use-package projectile
  :straight t)

(use-package dashboard
  :straight t
  :config
  (setq dashboard-banner-logo-title "Welcome to Emacs Dashboard")
  (setq dashboard-startup-banner "~/.emacs.d/emacs-e.svg")
  (setq dashboard-center-content nil)
  (setq dashboard-show-shortcuts t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-items '((recents  . 3)
                          (bookmarks . 5)
                          (projects . 3)
                          (agenda . 5)))
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  (dashboard-setup-startup-hook))

(use-package all-the-icons-dired
  :straight '(:type git :host github :repo "jtbm37/all-the-icons-dired")
  :hook (dired-mode . all-the-icons-dired-mode))

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

;; Utilities
(use-package multiple-cursors
  :straight t
  :bind
  ("C->"            . mc/mark-next-like-this)
  ("C-<"            . mc/mark-previous-like-this)
  ("C-c C-<"        . mc/mark-all-like-this)
  ("C-S-<mouse-1>"  . mc/add-cursor-on-click))

(use-package ibuffer
  :defer 0.2
  :bind ("C-x C-b" . ibuffer))

;; Autocomplete
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

(use-package yasnippet
  :straight t
  :config
  (use-package yasnippet-snippets
    :straight t)
  (setq yas-snippet-dirs
	       '("~/.emacs.d/snippets"
		 "~/.emacs.d/straight/repos/yasnippet-snippets/snippets"))
  (yas-global-mode 1))

;; Style
(use-package rainbow-mode
  :straight t
  :config
  (rainbow-mode t))

(use-package rainbow-delimiters
  :straight t
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package rust-mode
  :straight t
  :hook
  (rust-mode . (lambda () (setq indent-tabs-mode nil)))
  (rust-mode . (lambda () (prettify-symbols-mode)))
  :config
  (setq rust-format-on-save t)
  (define-key rust-mode-map (kbd "C-c C-c") 'rust-run)
  (define-key rust-mode-map (kbd "C-c C-t") 'rust-test)
  (define-key rust-mode-map (kbd "C-c C-o") 'rust-compile)
  (define-key rust-mode-map (kbd "C-c C-r") 'rust-run-clippy))

;; Emacs like vscode
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

(use-package dap-mode
  :straight t)

(use-package lsp-java
  :straight t
  :hook
  (java-mode . lsp))

(use-package dap-java)

(use-package flycheck
  :straight t
  :config
  (global-flycheck-mode))

;; Corrector ortografico
(setq ispell-program-name "aspell")
(setq ispell-local-dictionary "espanol")
(setq ispell-local-dictionary-alist
      '(("es_ES" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8)))

(use-package flyspell
  :defer 1
  :delight
  :custom
  (flyspell-abbrev-p t)
  (flyspell-issue-message-flag nil)
  (flyspell-issue-welcome-flag nil)
  (flyspell-mode 1))


(use-package flyspell-correct-ivy
  :after flyspell
  :bind (:map flyspell-mode-map
	    ("C-;" . flyspell-correct-word-generic))
  :custom (flyspell-correct-interface 'flyspell-correct-ivy))

(use-package ispell
  :custom
  (ispell-silently-savep t))

;; Activar Flyspell en modo texto y modos relacionados
(add-hook 'text-mode-hook
	'(lambda () (flyspell-mode 1)))


;; Como habitualmente necesito cambiar entre el diccionario English y Spanish, uso esta función:
(defun fd-switch-dictionary()
  (interactive)
  (let* ((dic ispell-current-dictionary)
       (change (if (string= dic "espanol") "english" "espanol")))
    (ispell-change-dictionary change)
    (message "Dicionario cambiado desde %s a %s" dic change)
    ))

;; optionally
(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode)

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

;; Latex
(use-package tex
  :straight auctex
  :hook (LaTeX-mode . reftex-mode)
  :custom
  (TeX-PDF-mode t)
  (TeX-auto-save t)
  (TeX-byte-compile t)
  (TeX-clean-confirm nil)
  (TeX-master 'dwim)
  (TeX-parse-self t)
  (TeX-source-correlate-mode t)
  (TeX-view-program-selection '((output-pdf "Evince")
				(output-html "xdg-open"))))

(use-package bibtex
  :straight t
  :after auctex
  :hook (bibtex-mode . my/bibtex-fill-column)
  :preface
  (defun my/bibtex-fill-column ()
    "Ensures that each entry does not exceed 120 characters."
    (setq fill-column 120)))

(use-package company-auctex
  :after (auctex company)
  :config (company-auctex-init))

(use-package company-math :after (auctex company))
(setq-default TeX-engine 'xetex)


;; Org config
;(setq org-startup-indented t)
(add-hook 'org-mode-hook 'org-indent-mode)
(with-eval-after-load 'org
  (add-hook 'org-mode-hook #'visual-line-mode))
(setq org-startup-with-inline-images t)
(setq org-image-actual-width nil)
(setq org-hide-emphasis-markers t)
(setq org-format-latex-options '(plist-put org-format-latex-options :scale 2.0))

;; Para mejorar los bullets y que sean más bonitos 
(font-lock-add-keywords 'org-mode
			'(("^ +\\([-*]\\) "
			   (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))


(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (setq col (current-column))
  (beginning-of-line) (setq start (point))
  (end-of-line) (forward-char) (setq end (point))
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (insert line-text)
    ;; restore point to original column in moved line
    (forward-line -1)
    (forward-char col)))

(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))

(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)

;; Adaptar el texto a la ventana
(add-hook 'org-mode-hook 'visual-line-mode)

;; Autocompletado en org-mode
(use-package org-ac
  :straight t
  :config
  (org-ac/config-default))

(setq calendar-date-style 'iso)
(setq calendar-week-start-day 1)
(setq calendar-day-header-array   ["Do" "Lu" "Ma" "Mi" "Ju" "Vi" "Sá"])
(setq calendar-day-name-array     ["domingo" "lunes" "martes" "miércoles" "jueves" "viernes" "sábado"])
(setq calendar-month-abbrev-array ["Ene" "Feb" "Mar" "Abr" "May" "Jun" "Jul" "Ago" "Sep" "Oct" "Nov" "Dic"])
(setq calendar-month-name-array   ["enero" "febrero" "marzo" "abril" "mayo" "junio" "julio" "agosto" "septiembre" "octubre" "noviembre" "diciembre"])

(setq calendar-holidays '((holiday-fixed 1 1   "Año Nuevo")
			  (holiday-fixed 1 6   "Reyes Magos")
			  (holiday-fixed 2 4   "Dia del Veterano")
			  (holiday-fixed 4 18  "Jueves Santo")
			  (holiday-fixed 4 19  "Viernes Santo")
			  (holiday-fixed 5 1   "Dia del Trabajador")
			  (holyday-fixed 5 25  "Revolucíon de Mayo")
			  (holyday-fixed 6 20  "Dia de la Bandera")
			  (holiday-fixed 12 08 "Inmaculada Concepcion")
			  (holiday-fixed 12 25 "Navidad")
			  ))

(use-package calfw
  :straight t
  :bind ([f8] . my/mi-calendario)
  :custom
  (cfw:org-overwrite-default-keybinding t)) ;; atajos de teclado de la agenda org-mode
;(setq cfw:display-calendar-holidays nil) ;; esconder fiestas calendario emacs

(use-package calfw-org
  :straight t)

(defun my/mi-calendario ()
  "calendarios a mostrar en calfw-org cuando pulso f8"
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:org-create-source)
    )))

(setq org-todo-keywords
      '((sequence "PORHACER(p!)"
		  "ENPROCESO(e!)"		  
		  "BLOQUEADO(b!)"
		  "|" "HECHO(h!)" "CANCELADO(c!)" "ARCHIVAR(a!)")))

(setq org-todo-keyword-faces
      '(("PORHACER"  . (:foreground "#50a14f" :weight bold :slant italic))
	("ENPROCESO" . "#a626a4")
	("BLOQUEADO" . "#e45649")
	("HECHO"     . "#9ca0a4")
	("CANCELADO" , "#9ca0a4")	
	("ARCHIVAR"  . "#986801")))

;; Mejorando aún más el aspecto de los bullets gracias al paquete org-bullets
(use-package org-bullets
  :straight t
  :config
  (add-hook 'org-mode-hook (org-bullets-mode 1)))

(setq org-log-done 'time)
(setq org-log-into-drawer "LOGBOOK")
;; Alinea etiquetas
(setq org-tags-column 80)

(setq org-archive-location "~/org/archivo/%s_archivo.org::datetree/")

(setq org-agenda-files '("~/org/agenda.org"
			 "~/org/notas.org"
			 "~/org/blog.org"
			 "~/org/habitos.org"
			 "~/org/trabajo.org"))

(add-to-list 'org-modules 'org-habit)
(require 'org-habit)      

(use-package org-agenda
  :straight nil
  :after org
  :bind ("C-c a" . org-agenda)
  :custom
  ;(org-agenda-dim-blocked-tasks t)
  (org-agenda-include-diary nil)
  ;(org-agenda-inhibit-startup t)
  (org-agenda-show-log t)
  ;(org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)
  (org-agenda-span 7)
  (org-agenda-start-on-weekday 1)
  (org-deadline-warning-days 7)
  (org-agenda-sticky nil)
  (org-agenda-window-setup 'current-window) ; Sobrescribe la ventana actual con la agenda
  (org-agenda-tags-column -100)
  ;(org-agenda-time-grid '((daily today require-timed)))
  (org-agenda-use-tag-inheritance t)
  ;(org-enforce-todo-dependencies t)
  (org-habit-show-habits-only-for-today nil)
  ;(org-track-ordered-property-with-tag t)
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(warning-suppress-log-types '((holidays) (holidays)))
 '(warning-suppress-types '((holidays))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
