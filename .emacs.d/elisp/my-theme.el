(defgroup my/emacs nil
  "My EMACS."
  :group 'convenience)

(defgroup my/emacs-theme nil
  "My EMACS Theme"
  :group 'my/emacs)

(defgroup my/theme-light nil
  "My light color palette"
  :group 'my/emacs-theme)

(defgroup my/theme-dark nil
  "My dark color palette"
  :group 'my/emacs-theme)

(defgroup my/theme-moon nil
  "My dark color palette alternative"
  :group 'my/emacs-theme)

(defcustom my/theme-light-base "#faf4ed"
  "Primary background
   General background, e.g. windows, tabs
   Extended panels, e.g. sidebars"
  :type 'color :group 'my/theme-light)

(defcustom my/theme-light-surface "#fffaf3"
  "Secondary background
   Accessory panels, e.g. popups, floats, editor terminals
   Inputs, e.g. text search, checkboxes"
  :type 'color :group 'my/theme-light)

(defcustom my/theme-light-overlay "#f2e9e1"
  "Tertiary background
   Active backgrounds, e.g. tabs, list items
   High contrast inputs, e.g. text search, checkboxes
   Hover selections
   Terminal black"
  :type 'color :group 'my/theme-light)

(defcustom my/theme-light-muted "#9893a5"
  "Ignored content, e.g. ignored Git files.
   Terminal bright black
   Comments"
  :type 'color :group 'my/theme-light)

(defcustom my/theme-light-subtle "#797593"
  "Inactive foregrounds, e.g. tabs, list items
   Operators, Punctuation"
  :type 'color :group 'my/theme-light)

(defcustom my/theme-light-text "#575279"
  "Active foregrounds, e.g. tabs, list items
   Cursor foreground paired with highlight high background
   Selection foreground paired with highlight med background
   Terminal white, bright white
   Variables"
  :type 'color :group 'my/theme-light)

(defcustom my/theme-light-love "#b4637a"
  "Diagnostic errors
   Deleted Git files
   Terminal red, bright red
   Builtins"
  :type 'color :group 'my/theme-light)

(defcustom my/theme-light-gold "#ea9d34"
  "Diagnostic warnings
   Terminal yellow, bright yellow
   Strings"
  :type 'color :group 'my/theme-light)

(defcustom my/theme-light-rose "#d7827e"
  "Matching search background paired with base foreground
   Modified Git files
   Terminal cyan, bright cyan
   Booleans, Functions"
  :type 'color :group 'my/theme-light)

(defcustom my/theme-light-pine "#286983"
  "Renamed Git files
   Terminal green, bright green
   Conditionals, Keywords"
  :type 'color :group 'my/theme-light)

(defcustom my/theme-light-foam "#56949f"
  "Diagnostic information
   Git additions
   Terminal blue, bright blue
   Keys, Tags, Types"
  :type 'color :group 'my/theme-light)

(defcustom my/theme-light-iris "#907aa9"
  "Diagnostic hints
   Inline links
   Merged and staged Git modifications
   Terminal magenta, bright magenta
   Methods, Parameters"
  :type 'color :group 'my/theme-light)

(defcustom my/theme-light-highlight-low "#f4ede8"
  "Cursorline background"
  :type 'color :group 'my/theme-light)

(defcustom my/theme-light-highlight-med "#dfdad9"
  "Selection background paired with text foreground"
  :type 'color :group 'my/theme-light)

(defcustom my/theme-light-highlight-high "#cecacd"
  "Borders / visual dividers
   Cursor background paired with text foreground"
  :type 'color :group 'my/theme-light)

(defcustom my/theme-dark-base "#191724"
  "Primary background
   General background, e.g. windows, tabs
   Extended panels, e.g. sidebars"
  :type 'color :group 'my/theme-dark)

(defcustom my/theme-dark-surface "#1f1d2e"
  "Secondary background
   Accessory panels, e.g. popups, floats, editor terminals
   Inputs, e.g. text search, checkboxes"
  :type 'color :group 'my/theme-dark)

(defcustom my/theme-dark-overlay "#26233a"
  "Tertiary background
   Active backgrounds, e.g. tabs, list items
   High contrast inputs, e.g. text search, checkboxes
   Hover selections
   Terminal black"
  :type 'color :group 'my/theme-dark)

(defcustom my/theme-dark-muted "#6e6a86"
  "Ignored content, e.g. ignored Git files.
   Terminal bright black
   Comments"
  :type 'color :group 'my/theme-dark)

(defcustom my/theme-dark-subtle "#908caa"
  "Inactive foregrounds, e.g. tabs, list items
   Operators, Punctuation"
  :type 'color :group 'my/theme-dark)

(defcustom my/theme-dark-text "#e0def4"
  "Active foregrounds, e.g. tabs, list items
   Cursor foreground paired with highlight high background
   Selection foreground paired with highlight med background
   Terminal white, bright white
   Variables"
  :type 'color :group 'my/theme-dark)

(defcustom my/theme-dark-love "#eb6f92"
  "Diagnostic errors
   Deleted Git files
   Terminal red, bright red
   Builtins"
  :type 'color :group 'my/theme-dark)

(defcustom my/theme-dark-gold "#f6c177"
  "Diagnostic warnings
   Terminal yellow, bright yellow
   Strings"
  :type 'color :group 'my/theme-dark)

(defcustom my/theme-dark-rose "#ebbcba"
  "Matching search background paired with base foreground
   Modified Git files
   Terminal cyan, bright cyan
   Booleans, Functions"
  :type 'color :group 'my/theme-dark)

(defcustom my/theme-dark-pine "#31748f"
  "Renamed Git files
   Terminal green, bright green
   Conditionals, Keywords"
  :type 'color :group 'my/theme-dark)

(defcustom my/theme-dark-foam "#9ccfd8"
  "Diagnostic information
   Git additions
   Terminal blue, bright blue
   Keys, Tags, Types"
  :type 'color :group 'my/theme-dark)

(defcustom my/theme-dark-iris "#c4a7e7"
  "Diagnostic hints
   Inline links
   Merged and staged Git modifications
   Terminal magenta, bright magenta
   Methods, Parameters"
  :type 'color :group 'my/theme-dark)

(defcustom my/theme-dark-highlight-low "#21202e"
  "Cursorline background"
  :type 'color :group 'my/theme-dark)

(defcustom my/theme-dark-highlight-med "#403d52"
  "Selection background paired with text foreground"
  :type 'color :group 'my/theme-dark)

(defcustom my/theme-dark-highlight-high "#524f67"
  "Borders / visual dividers
   Cursor background paired with text foreground"
  :type 'color :group 'my/theme-dark)

(defcustom my/theme-moon-base "#232136"
  "Primary background
   General background, e.g. windows, tabs
   Extended panels, e.g. sidebars"
  :type 'color :group 'my/theme-moon)

(defcustom my/theme-moon-surface "#2a273f"
  "Secondary background
   Accessory panels, e.g. popups, floats, editor terminals
   Inputs, e.g. text search, checkboxes"
  :type 'color :group 'my/theme-moon)

(defcustom my/theme-moon-overlay "#393552"
  "Tertiary background
   Active backgrounds, e.g. tabs, list items
   High contrast inputs, e.g. text search, checkboxes
   Hover selections
   Terminal black"
  :type 'color :group 'my/theme-moon)

(defcustom my/theme-moon-muted "#6e6a86"
  "Ignored content, e.g. ignored Git files.
   Terminal bright black
   Comments"
  :type 'color :group 'my/theme-moon)

(defcustom my/theme-moon-subtle "#908caa"
  "Inactive foregrounds, e.g. tabs, list items
   Operators, Punctuation"
  :type 'color :group 'my/theme-moon)

(defcustom my/theme-moon-text "#e0def4"
  "Active foregrounds, e.g. tabs, list items
   Cursor foreground paired with highlight high background
   Selection foreground paired with highlight med background
   Terminal white, bright white
   Variables"
  :type 'color :group 'my/theme-moon)

(defcustom my/theme-moon-love "#eb6f92"
  "Diagnostic errors
   Deleted Git files
   Terminal red, bright red
   Builtins"
  :type 'color :group 'my/theme-moon)

(defcustom my/theme-moon-gold "#f6c177"
  "Diagnostic warnings
   Terminal yellow, bright yellow
   Strings"
  :type 'color :group 'my/theme-moon)

(defcustom my/theme-moon-rose "#ea9a97"
  "Matching search background paired with base foreground
   Modified Git files
   Terminal cyan, bright cyan
   Booleans, Functions"
  :type 'color :group 'my/theme-moon)

(defcustom my/theme-moon-pine "#3e8fb0"
  "Renamed Git files
   Terminal green, bright green
   Conditionals, Keywords"
  :type 'color :group 'my/theme-moon)

(defcustom my/theme-moon-foam "#9ccfd8"
  "Diagnostic information
   Git additions
   Terminal blue, bright blue
   Keys, Tags, Types"
  :type 'color :group 'my/theme-moon)

(defcustom my/theme-moon-iris "#c4a7e7"
  "Diagnostic hints
   Inline links
   Merged and staged Git modifications
   Terminal magenta, bright magenta
   Methods, Parameters"
  :type 'color :group 'my/theme-moon)

(defcustom my/theme-moon-highlight-low "#2a283e"
  "Cursorline background"
  :type 'color :group 'my/theme-moon)

(defcustom my/theme-moon-highlight-med "#44415a"
  "Selection background paired with text foreground"
  :type 'color :group 'my/theme-moon)

(defcustom my/theme-moon-highlight-high "#56526e"
  "Borders / visual dividers
   Cursor background paired with text foreground"
  :type 'color :group 'my/theme-moon)

(defun colir-join (r g b)
  "Build a color from R G B.
   Inverse of `color-values'."
  (interactive)
  (format "#%02x%02x%02x"
          (ash r -8)
          (ash g -8)
          (ash b -8)))

(defun colir-blend (c1 c2 &optional alpha)
  "Blend the two colors C1 and C2 with ALPHA.
   C1 and C2 are in the format of `color-values'.
   ALPHA is a number between 0.0 and 1.0 which corresponds to the
   influence of C1 on the result."
  (interactive)
  (setq alpha (or alpha 0.5))
  (apply #'colir-join
         (cl-mapcar
          (lambda (x y)
            (round (+ (* x alpha) (* y (- 1 alpha)))))
          c1 c2)))

(defun my/blend-hex-colors (c1 c2 &optional alpha)
  "Blend two color with hex code."
  (interactive)
  (setq alpha (or alpha 0.5))
  (colir-blend
   (color-values c1)
   (color-values c2) alpha))

(defun my/theme-mode (variant)
  "Apply VARIANT color scheme"
  (interactive)
  (let* ((light         '((background light)))
	 (dark          '((background dark)))
	 (moon          '((background moon)))
	 (base           (cond ((eq variant 'light) my/theme-light-base)
			       ((eq variant 'dark)  my/theme-dark-base)
			       ((eq variant 'moon)  my/theme-moon-base)
			       (t                   my/theme-light-base)))
	 (surface        (cond ((eq variant 'light) my/theme-light-surface)
			       ((eq variant 'dark)  my/theme-dark-surface)
			       ((eq variant 'moon)  my/theme-moon-surface)
			       (t                   my/theme-light-surface)))
	 (overlay        (cond ((eq variant 'light) my/theme-light-overlay)
			       ((eq variant 'dark)  my/theme-dark-overlay)
			       ((eq variant 'moon)  my/theme-moon-overlay)
			       (t                   my/theme-light-overlay)))
	 (muted          (cond ((eq variant 'light) my/theme-light-muted)
			       ((eq variant 'dark)  my/theme-dark-muted)
			       ((eq variant 'moon)  my/theme-moon-muted)
			       (t                   my/theme-light-muted)))
	 (subtle         (cond ((eq variant 'light) my/theme-light-subtle)
			       ((eq variant 'dark)  my/theme-dark-subtle)
			       ((eq variant 'moon)  my/theme-moon-subtle)
			       (t                   my/theme-light-subtle)))
	 (text           (cond ((eq variant 'light) my/theme-light-text)
			       ((eq variant 'dark)  my/theme-dark-text)
			       ((eq variant 'moon)  my/theme-moon-text)
			       (t                   my/theme-light-text)))
	 (love           (cond ((eq variant 'light) my/theme-light-love)
			       ((eq variant 'dark)  my/theme-dark-love)
			       ((eq variant 'moon)  my/theme-moon-love)
			       (t                   my/theme-light-love)))
	 (gold           (cond ((eq variant 'light) my/theme-light-gold)
			       ((eq variant 'dark)  my/theme-dark-gold)
			       ((eq variant 'moon)  my/theme-moon-gold)
			       (t                   my/theme-light-gold)))
	 (rose           (cond ((eq variant 'light) my/theme-light-rose)
			       ((eq variant 'dark)  my/theme-dark-rose)
			       ((eq variant 'moon)  my/theme-moon-rose)
			       (t                   my/theme-light-rose)))
	 
	 (pine           (cond ((eq variant 'light) my/theme-light-pine)
			       ((eq variant 'dark)  my/theme-dark-pine)
			       ((eq variant 'moon)  my/theme-moon-pine)
			       (t                   my/theme-light-pine)))
	 
	 (foam           (cond ((eq variant 'light) my/theme-light-foam)
			       ((eq variant 'dark)  my/theme-dark-foam)
			       ((eq variant 'moon)  my/theme-moon-foam)
			       (t                   my/theme-light-foam)))
	 (iris           (cond ((eq variant 'light) my/theme-light-iris)
			       ((eq variant 'dark)  my/theme-dark-iris)
			       ((eq variant 'moon)  my/theme-moon-iris)
			       (t                   my/theme-light-iris)))
	 (highlightLow   (cond ((eq variant 'light) my/theme-light-highlight-low)
			       ((eq variant 'dark)  my/theme-dark-highlight-low)
			       ((eq variant 'moon)  my/theme-moon-highlight-low)
			       (t                   my/theme-light-highlight-low)))
	 (highlightMed   (cond ((eq variant 'light) my/theme-light-highlight-med)
			       ((eq variant 'dark)  my/theme-dark-highlight-med)
			       ((eq variant 'moon)  my/theme-moon-highlight-med)
			       (t                   my/theme-light-highlight-med)))
	 (highlightHigh  (cond ((eq variant 'light) my/theme-light-highlight-high)
			       ((eq variant 'dark)  my/theme-dark-highlight-high)
			       ((eq variant 'moon)  my/theme-moon-highlight-high)
			       (t                   my/theme-light-highlight-high)))
	 (header-line-bg (cond ((eq variant 'light) my/theme-light-overlay)
			       ((eq variant 'dark)  (my/blend-hex-colors base overlay))
			       ((eq variant 'moon)  (my/blend-hex-colors base overlay))
			       (t                   my/theme-light-overlay))))
    
    (custom-theme-set-faces
     'user
     `(default                          ((t (:foreground ,text :background ,base))))   
     `(hl-line                          ((t (:background ,highlightLow))))
     `(fringe                           ((t (:background ,base))))
     `(highlight                        ((t (:background ,highlightLow))))

     `(minibuffer-prompt                ((t (:foreground ,foam))))
     `(link                             ((t (:foreground ,foam :underline t))))
     `(link-visited                     ((t (:foreground ,iris :underline t))))

     `(company-echo                     ((t (:background ,overlay))))
     `(company-echo-common              ((t (:background ,surface))))

     `(company-box-annotation           ((t (:foreground ,text :background ,overlay))))
     `(company-box-background           ((t (:background ,overlay))))
     `(company-box-selection            ((t (:background ,highlightMed))))
     `(company-preview                  ((t (:background ,overlay))))
     `(company-preview-common           ((t (:background ,overlay))))

     `(company-scrollbar-fg             ((t (:background ,muted))))
     `(company-scrollbar-bg             ((t (:background ,surface))))

     `(company-box-scrollbar            ((t (:foreground ,muted :background ,surface))))
     `(company-tooltip                  ((t (:background ,overlay))))
     `(company-tooltip-common           ((t (:background ,overlay))))
     `(company-tooltip-search           ((t (:background ,overlay))))

     `(region                           ((t (:background ,highlightMed))))
     `(show-paren-match                 ((t (:background ,highlightMed))))
     `(show-paren-match-expression      ((t (:background ,highlightMed))))
     `(show-paren-mismatch              ((t (:background ,highlightMed))))
     
     ;; Line numbers
     `(line-number                      ((t (:foreground ,muted :background ,base :weight light :slant italic))))
     `(line-number-current-line         ((t (:foreground ,iris :background ,highlightLow :weight bold :slant italic))))          

     ;; Windows dividers
     `(window-divider                   ((t (:foreground ,base :background ,base))))
     `(window-divider-first-pixel       ((t (:foreground ,base :background ,base))))
     `(window-divider-last-pixel        ((t (:foreground ,base :background ,base))))

     ;; Mode line
     `(mode-line                        ((t (:foreground ,text :background ,overlay))))
     `(mode-line-active                 ((t (:foreground ,text :background ,overlay))))
     `(mode-line-inactive               ((t (:foreground ,text :background ,overlay))))     

     ;; Header line
     `(header-line                      ((t (:background ,base :box (:line-width 3 :color ,base)))))
     `(header-line-active               ((t (:background ,base :box (:line-width 3 :color ,base)))))
     `(header-line-inactive             ((t (:background ,base :box (:line-width 3 :color ,base)))))     

     ;; Custom Header line
     `(header-line-default    ((t (:background ,header-line-bg :foreground ,text :box (:line-width 3 :color ,header-line-bg)))))
     `(header-line-status-RO  ((t (:inherit nil :box (:line-width 2 :color ,foam) :weight regular :slant normal :background ,foam :foreground ,overlay))))
     `(header-line-status-RW  ((t (:inherit nil :box (:line-width 2 :color ,text) :weight regular :slant normal :background ,text :foreground ,overlay))))
     `(header-line-status-**  ((t (:inherit nil :box (:line-width 2 :color ,love) :weight regular :slant normal :background ,love :foreground ,overlay))))
    
     ;; Font locks
     `(font-lock-warning-face           ((t (:foreground ,love   :weight regular))))
     `(font-lock-function-name-face     ((t (:foreground ,rose   :weight regular   :slant italic))))
     `(font-lock-function-call-face     ((t (:foreground ,rose   :weight semilight :slant normal))))
     `(font-lock-variable-name-face     ((t (:foreground ,text   :weight regular   :slant normal))))
     `(font-lock-variable-use-face      ((t (:foreground ,text   :weight semilight :slant normal))))
     `(font-lock-keyword-face           ((t (:foreground ,pine   :weight regular   :slant italic))))
     `(font-lock-comment-face           ((t (:foreground ,muted  :weight light     :slant italic))))
     `(font-lock-comment-delimiter-face ((t (:foreground ,muted  :weight regular   :slant italic))))
     `(font-lock-type-face              ((t (:foreground ,foam   :weight regular   :slant italic))))
     `(font-lock-constant-face          ((t (:foreground ,foam   :weight light     :slant italic))))
     `(font-lock-builtin-face           ((t (:foreground ,love   :weight regular   :slant normal))))
     `(font-lock-preprocessor-face      ((t (:foreground ,love   :weight light     :slant normal))))
     `(font-lock-string-face            ((t (:foreground ,gold   :weight light     :slant normal))))
     `(font-lock-doc-face               ((t (:foreground ,gold   :weight light     :slant normal))))
     `(font-lock-doc-markup-face        ((t (:foreground ,foam   :weight light     :slant normal))))
     `(font-lock-negation-char-face     ((t (:foreground ,love   :weight regular   :slant italic))))
     `(font-lock-scape-face             ((t (:foreground ,love   :weight light     :slant normal))))
     `(font-lock-number-face            ((t (:foreground ,rose   :weight light     :slant normal))))
     `(font-lock-operator-face          ((t (:foreground ,subtle   :weight regular  :slant normal))))
     `(font-lock-punctuation-face       ((t (:foreground ,subtle   :weight regular    :slant normal))))
     `(font-lock-delimiter-face         ((t (:foreground ,rose   :weight regular   :slant normal))))
     `(font-lock-property-name-face     ((t (:foreground ,foam   :weight light     :slant normal))))
     `(font-lock-property-use-face      ((t (:foreground ,foam   :weight light     :slant italic))))
     `(font-lock-misc-punctuation-face  ((t (:foreground ,iris   :weight regular))))
     
     ;; VTerm mode
     `(vterm-color-black          ((t (:foreground ,muted))))
     `(vterm-color-red            ((t (:foreground ,love))))
     `(vterm-color-green          ((t (:foreground ,pine))))
     `(vterm-color-yellow         ((t (:foreground ,gold))))
     `(vterm-color-blue           ((t (:foreground ,foam))))
     `(vterm-color-magenta        ((t (:foreground ,iris))))
     `(vterm-color-cyan           ((t (:foreground ,rose))))
     `(vterm-color-white          ((t (:foreground ,text))))
     `(vterm-color-bright-black   ((t (:foreground ,muted))))
     `(vterm-color-bright-red     ((t (:foreground ,love))))
     `(vterm-color-bright-green   ((t (:foreground ,pine))))
     `(vterm-color-bright-yellow  ((t (:foreground ,gold))))
     `(vterm-color-bright-blue    ((t (:foreground ,foam))))
     `(vterm-color-bright-magenta ((t (:foreground ,iris))))
     `(vterm-color-bright-cyan    ((t (:foreground ,rose))))
     `(vterm-color-bright-white   ((t (:foreground ,text))))

     ;; Rainbow delimiters mode
     `(rainbow-delimiters-depth-1-face  ((t (:foreground ,pine :weight semibold))))
     `(rainbow-delimiters-depth-2-face  ((t (:foreground ,love :weight semibold))))
     `(rainbow-delimiters-depth-3-face  ((t (:foreground ,iris :weight semibold))))
     `(rainbow-delimiters-depth-4-face  ((t (:foreground ,gold :weight semibold))))
     `(rainbow-delimiters-depth-5-face  ((t (:foreground ,foam :weight semibold))))
     `(rainbow-delimiters-depth-6-face  ((t (:foreground ,pine :weight semibold))))
     `(rainbow-delimiters-depth-7-face  ((t (:foreground ,love :weight semibold))))
     `(rainbow-delimiters-depth-8-face  ((t (:foreground ,iris :weight semibold))))
     `(rainbow-delimiters-depth-9-face  ((t (:foreground ,foam :weight semibold))))
     `(rainbow-delimiters-unmatched-face  ((t (:foreground ,muted :weight semibold))))
     `(rainbow-delimiters-mismatched-face ((t (:foreground ,love  :weight semibold))))

     ;; Org Mode Faces
     `(variable-pitch ((t (:family "Roboto" :weight light :height 140))))
     `(fixed-pitch    ((t (:family "Roboto Mono" :weight light :height 140))))

     `(org-level-8 ((t (:family "Roboto Mono" :weight normal :foreground ,love))))
     `(org-level-7 ((t (:family "Roboto Mono" :weight normal :foreground ,text))))
     `(org-level-6 ((t (:family "Roboto Mono" :weight normal :foreground ,rose))))
     `(org-level-5 ((t (:family "Roboto Mono" :weight normal :foreground ,gold))))
     `(org-level-4 ((t (:family "Roboto Mono" :weight normal :foreground ,foam))))
     `(org-level-3 ((t (:family "Roboto Mono" :weight normal :foreground ,pine))))
     `(org-level-2 ((t (:family "Roboto Mono" :weight normal :foreground ,love))))
     `(org-level-1 ((t (:family "Roboto Mono" :weight normal :foreground ,text))))
     
     `(org-document-title        ((t (:weight bold :height 1.2 :underline nil))))
     `(org-block                 ((t (:inherit fixed-pitch :background ,highlightLow))))
     `(org-code                  ((t (:inherit (shadow fixed-pitch)))))
     `(org-document-info         ((t (:inherit variable-pitch :foreground ,text ))))
     `(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
     `(org-indent                ((t (:inherit (org-hide fixed-pitch)))))
     `(org-link                  ((t (:foreground ,iris :underline t))))
     `(org-meta-line             ((t (:inherit (font-lock-comment-face fixed-pitch)))))
     `(org-property-value        ((t (:inherit fixed-pitch))) t)
     `(org-special-keyword       ((t (:inherit (font-lock-comment-face fixed-pitch)))))
     `(org-table                 ((t (:inherit fixed-pitch :foreground ,subtle)))) 
     `(org-verbatim              ((t (:inherit (shadow fixed-pitch)))))
     `(org-drawer                ((t (:inherit fixed-pitch :foreground ,muted))))
     `(org-hide                  ((t (:inherit fixed-pitch :foreground ,base))))
     `(org-tag                   ((t (:foreground ,muted :weight regular ))))
     `(org-todo                  ((t (:foreground ,love :weight bold))))
     `(org-done                  ((t (:foreground ,muted :weight bold))))
     `(org-date                  ((t (:foreground ,muted :weight light :underline nil))))

     `(org-agenda-calendar-daterange ((t (:foreground ,text :weight regular))))
     `(org-agenda-calendar-event     ((t (:foreground ,text :weight regular))))
     `(org-agenda-calendar-sexp      ((t (:foreground ,text :weight regular))))
     `(org-agenda-clocking           ((t (:foreground ,text :weight regular))))
     `(org-agenda-column-dateline    ((t (:foreground ,text :weight regular))))
     `(org-agenda-current-time       ((t (:foreground ,text :weight regular))))
     `(org-agenda-date               ((t (:foreground ,foam :weight light :slant normal)))) 
     `(org-agenda-date-today         ((t (:foreground ,pine :weight bold :slant italic))))
     `(org-agenda-date-weekend       ((t (:foreground ,pine :weight bold :slant italic))))
     `(org-agenda-date-weekend-today ((t (:foreground ,text :weight regular))))
     `(org-agenda-diary              ((t (:foreground ,text :weight regular))))
     `(org-agenda-dimmed-todo-face   ((t (:foreground ,text :weight regular))))
     `(org-agenda-done               ((t (:foreground ,muted :weight light))))
     `(org-agenda-filter-category    ((t (:foreground ,text :weight regular))))
     `(org-agenda-filter-effort      ((t (:foreground ,text :weight regular))))
     `(org-agenda-filter-regexp      ((t (:foreground ,text :weight regular))))
     `(org-agenda-filter-tags        ((t (:foreground ,text :weight regular))))
     `(org-agenda-restriction-lock   ((t (:foreground ,text :weight regular))))
     `(org-agenda-structure          ((t (:foreground ,text :weight bold :slant italic)))) 
     `(org-agenda-structure-filter   ((t (:foreground ,love :weight light :slant italic))))
     `(org-agenda-structure-secondar ((t (:foreground ,text :weight regular))))
     `(org-scheduled                 ((t (:foreground ,text :weight light))))
     `(org-scheduled-previously      ((t (:foreground ,iris :weight light))))
     `(org-scheduled-today           ((t (:foreground ,text :weight light))))
     `(org-imminent-deadline         ((t (:foreground ,love :weight light))))
     `(org-upcoming-deadline         ((t (:foreground ,love :weight light))))
     `(org-upcoming-distant-deadline ((t (:foreground ,rose :weight light))))
     `(org-time-grid                 ((t (:foreground ,gold :weight light))))
     `(org-priority                  ((t (:foreground ,pine :weight regular)))))))

(defun my/set-theme-interactively (variant)
  "Set color scheme interactively"
  (interactive
   (list
    (completing-read "Choos one variant of color scheme: "
		     '("light" "dark" "moon"))))
  (my/theme-mode (cond ((string= variant "light") 'light)
		       ((string= variant "dark")  'dark)
		       ((string= variant "moon")  'moon)
		       (t                         'light))))

(defun my/dired-mode-theme ()
  "Todo write documentation"
  (interactive)
  (face-remap-add-relative 'default :background my/theme-light-overlay))

(global-set-key (kbd "M-s t") #'my/set-theme-interactively)

