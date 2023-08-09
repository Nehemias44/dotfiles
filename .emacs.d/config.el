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

(setq-default c-basic-offset 8)

(fset 'yes-or-no-p 'y-or-n-p)
(electric-pair-mode +1)
(delete-selection-mode +1)

(setq byte-compile-warnings '(cl-functions))
;; add to $DOOMDIR/init.el
(defvar native-comp-deferred-compilation-deny-list nil)

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

(add-to-list 'load-path "~/.emacs.d/lisp")

(setq default-frame-alist
      (append (list
	       '(font . "Iosevka Nerd Font Light-13")
	       '(min-height . 1)
	       '(height     . 44)
	       '(min-width  . 1)
	       '(width      . 70)
	       ;;	       '(left . 40)
	       ;;               '(top . 50)
	       '(vertical-scroll-bars . nil)
	       '(internal-border-width . 30)
	       '(left-fringe    . 10)
	       '(right-fringe   . 10)
	       '(tool-bar-lines . 0)
	       '(menu-bar-lines . 0)
	       '(line-spacing   . 0.1))))

(setq x-underline-at-descent-line t)

(defun my/frame-recenter (&optional frame)
  "Center FRAME on the screen.
FRAME can be a frame name, a terminal name, or a frame.
If FRAME is omitted or nil, use currently selected frame."
  (interactive)
  (unless (eq 'maximised (frame-parameter nil 'fullscreen))
    (let* ((frame (or (and (boundp 'frame)
			   frame)
		      (selected-frame)))
	   (frame-w (frame-pixel-width frame))
	   (frame-h (frame-pixel-height frame))
	   ;; frame-monitor-workarea returns (x y width height) for the monitor
	   (monitor-w (nth 2 (frame-monitor-workarea frame)))
	   (monitor-h (nth 3 (frame-monitor-workarea frame)))
	   (center (list (/ (- monitor-w frame-w) 2)
			 (/ (- monitor-h frame-h) 2))))
      (apply 'set-frame-position (flatten-list (list frame center))))))

(add-hook 'after-init-hook #'my/frame-recenter)
(add-hook 'after-make-frame-functions #'my/frame-recenter)

(straight-use-package
  '(nano-emacs :type git :host github :repo "rougier/nano-emacs"))

(require 'nano-theme)
(require 'nano-theme-light)

(setq nano-color-background "#faf4ed")
(setq nano-color-foreground "#575279")
(setq nano-color-highlight  "#f2e9de")
(setq nano-color-critical   "#b4637a")
(setq nano-color-salient    "#907aa9")
(setq nano-color-strong     "#6e6a86")
(setq nano-color-popout     "#56949f")
(setq nano-color-subtle     "#f2e9de")
(setq nano-color-faded      "#6e6a86")

(require 'nano-faces)
(require 'nano-modeline)
(nano-faces)

(set-face-attribute 'window-divider nil
                      :foreground (face-background 'nano-face-default))
(set-face-attribute 'window-divider-first-pixel nil
                    :foreground nano-color-background
                    :foreground (face-background 'nano-face-subtle))

(set-face-attribute 'window-divider-last-pixel nil
                      :foreground nano-color-background)

(set-face-attribute 'mode-line nil
                      :height 0.1
                      :foreground nano-color-subtle
                      :background (face-background 'nano-face-default)
                      ;; :underline  (if (display-graphic-p)
                      ;;                 (face-background 'nano-face-subtle)
                      ;;               t)
                      :underline t
                      :overline nil
                      :box nil)

(set-face-attribute 'mode-line-inactive nil
                    :height 0.1
                    :foreground nano-color-subtle
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

(set-face-attribute 'nano-face-header-default nil
                    :foreground nano-color-foreground
                    :background nano-color-subtle)

(set-face-attribute 'nano-face-header-strong nil
                    :foreground nano-color-foreground
                    :background nano-color-subtle)

(set-face-attribute 'nano-face-default nil
                    :foreground nano-color-foreground
                    :background nano-color-subtle)

(set-face-attribute 'default nil
                    :background nano-color-background
                    :foreground nano-color-foreground)

(set-face-attribute 'line-number nil
                    :background nano-color-background
                    :foreground "#9893a5"
                    :slant 'italic
                    :weight 'semi-light)

(set-face-attribute 'line-number-current-line nil
                    :background nano-color-highlight
                    :foreground "#575279"
                    :weight 'bold)

(set-face-attribute 'font-lock-keyword-face nil
                    :foreground "#286983"
                    :slant 'italic
                    :weight 'bold)

(set-face-attribute 'font-lock-comment-face nil
                    :foreground "#9893a5"
                    :slant 'italic
                    :weight 'semi-bold)

(set-face-attribute 'font-lock-variable-name-face nil
                    :foreground "#d7827e"
                    :slant 'italic)

(set-face-attribute 'font-lock-constant-face nil
                    :foreground "#286983")

(set-face-attribute 'font-lock-type-face nil
                    :foreground "#56949f"
                    :slant 'italic)

(set-face-attribute 'font-lock-string-face nil
                    :foreground "#ea9d34")

(set-face-attribute 'font-lock-function-name-face nil
                    :foreground "#b4637a"
                    :slant 'italic)

(set-face-attribute 'fringe nil
                    :background nano-color-background)

(use-package rainbow-mode
  :straight t
  :hook
  (prog-mode . rainbow-mode))

(use-package rainbow-delimiters
  :straight t
  :config
  (rainbow-delimiters-mode +1)
  (set-face-attribute 'rainbow-delimiters-depth-1-face 'nil :weight 'bold)
  (set-face-attribute 'rainbow-delimiters-depth-2-face 'nil :weight 'bold)
  (set-face-attribute 'rainbow-delimiters-depth-3-face 'nil :weight 'bold)
  (set-face-attribute 'rainbow-delimiters-depth-4-face 'nil :weight 'bold)
  (set-face-attribute 'rainbow-delimiters-depth-5-face 'nil :weight 'bold)
  (set-face-attribute 'rainbow-delimiters-depth-6-face 'nil :weight 'bold)
  (set-face-attribute 'rainbow-delimiters-depth-7-face 'nil :weight 'bold)
  (set-face-attribute 'rainbow-delimiters-depth-8-face 'nil :weight 'bold)
  (set-face-attribute 'rainbow-delimiters-depth-9-face 'nil :weight 'bold)
  )

(setq-default cursor-type '(bar . 2))
(set-cursor-color "#b4637a")
(blink-cursor-mode 1)

(hl-line-mode +1)
(set-face-attribute 'hl-line nil
		    :background nano-color-highlight)

(require 'display-line-numbers)
(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(use-package all-the-icons
  :straight t)

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

(use-package which-key
  :straight t
  :config
  (which-key-mode +1))

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

(use-package iedit
  :straight t
  :bind
  ("C-:" . iedit-mode))

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

(use-package projectile
  :straight t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(defun my-projectile-run-project (&optional prompt)
  (interactive "P")
  (let ((compilation-read-command
         (or (not (projectile-run-command (projectile-compilation-dir)))
             prompt)))
    (projectile-run-project prompt)))

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

(use-package java-snippets
  :straight t)

(use-package flycheck
  :straight t
  :config
  (global-flycheck-mode))

(use-package eglot
  :straight t
  :config
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
  :hook
  (c-mode-hook . eglot-straight)
  (c++-mode-hook . eglot-straight))

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

(use-package yaml-mode
  :straight t)

(use-package lua-mode
  :straight t)

(use-package lsp-jedi
  :straight t)

(use-package lsp-java
  :straight t)

(use-package dap-mode
  :straight t)

(use-package dap-java)

(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode)

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
         (python-mode . lsp)
         (lua-mode . lsp)
         (java-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(setenv "DICTIONARY" "es_ES")
(setq ispell-program-name "hunspell")
(setq ispell-hunspell-dict-paths '(("es_ES" "/usr/share/hunspell/es_ES.aff")
                                   ("en_US" "/usr/share/hunspell/en_US.aff")))
(setq ispell-local-dictionary "es_ES")
(setq ispell-local-dictionary-alist
      '(("es_ES" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8)))

(use-package flyspell
  :straight t
  :custom
  (flyspell-abbrev-p t)
  (flyspell-issue-message-flag nil)
  (flyspell-issue-welcome-flag nil)
  (flyspell-mode 1))

(use-package ispell
  :custom
  (ispell-silently-savep t))

;; Activar Flyspell en modo texto y modos relacionados
(add-hook 'text-mode-hook
	'(lambda () (flyspell-mode 1)))

(defun fd-switch-dictionary()
  (interactive)
  (let* ((dic ispell-current-dictionary)
         (change (if (string= dic "espanol") "english" "espanol")))
    (ispell-change-dictionary change)
    (message "Dicionario cambiado desde %s a %s" dic change)
    ))

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

(add-hook 'org-mode-hook 'org-indent-mode)
(with-eval-after-load 'org
  (add-hook 'org-mode-hook #'visual-line-mode))
(setq org-startup-with-inline-images t)
(setq org-image-actual-width nil)
(setq org-hide-emphasis-markers t)
(setq org-format-latex-options '(plist-put org-format-latex-options :scale 2.0))
(setq-default org-support-shift-select t)

(use-package org-bullets
  :straight t
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

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
  "Calendarios a mostrar en calfw-org cuando pulso f8."
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
        ("CANCELADO" , "#383a42")	
        ("ARCHIVAR"  . "#a0bcf8")))

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
  (org-habit-show-habits-only-for-today t)
                                        ;(org-track-ordered-property-with-tag t)
  )

(use-package dashboard
  :straight t
  :config
  (setq dashboard-banner-logo-title "Welcome to Emacs Dashboard")
  (setq dashboard-startup-banner "~/.emacs.d/emacs-e.png")
  (setq dashboard-center-content nil)
  (setq dashboard-show-shortcuts t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-items '((recents  . 3)
                          (bookmarks . 3)
                          (projects . 3)
                          (agenda . 3)))
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  (dashboard-setup-startup-hook))

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

(use-package simple-httpd
    :straight t)
  (use-package js2-mode
    :straight t)
  (use-package skewer-mode
    :straight t)
(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)
