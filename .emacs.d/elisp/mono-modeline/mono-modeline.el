;;; File name: mono-modeline.el
(defgroup mono-modeline nil
  "My own header-line-mode"
  :group 'convenience)

(defface header-line-status-RO
  '((t (:inherit nil
	:background "#4F4F4f"
	:foreground "#FFFFFF"
	:weight regular
	:slant normal)))
  "Face for status file Read only"
  :group 'mono-modeline)

(defface header-line-status-RW
  '((t (:inherit nil	 
	:background "#323232"
	:foreground "#FFFFFF"
	:weight regular
	:slant normal)))
  "Face for status file read and write"
  :group 'mono-modeline)

(defface header-line-status-**
  '((t (:inherit nil	 
	:background "#6D6D6D"
	:foreground "#FFFFFF"
	:weight regular
	:slant normal)))
  "Face for status file modified"
  :group 'mono-modeline)

(defface header-line-default
  '((t (:inherit nil
	:background "#DDDDDD"
	:foreground "#323232"
	:weight light
	:slant italic)))
  "Face for default headerline"
  :group 'mono-modeline)

(defun mono-vc-branch ()
  "Get version control branch."
  (if vc-mode
      (let ((backend (vc-backend buffer-file-name)))
       	(concat "#"
		(substring-no-properties
		 vc-mode
       		 (+ (if (eq backend 'Hg) 2 3) 2)))) nil))


(defun mono-mode-line-fill (reserve)
  "Return empty space using FACE and leaving
     RESERVE space on the right."
  (when (and window-system (eq 'right (get-scroll-bar-mode)))
    (setq reserve (- reserve 3)))
  (propertize " "
  	      'face `(:inherit header-line-default)
  	      'display
  	      `((space
		 :align-to (- (+ right right-fringe right-margin)
			      ,reserve)))))

(defun mono-thin-mode-line ()
  "Set modeline like a underline"
  (interactive)
  (setq mode-line-format (list ""))
  (setq-default mode-line-format (list ""))
  (dolist (face '(mode-line mode-line-active mode-line-inactive))
    (if (display-graphic-p)
	(progn
	  (set-face-attribute face nil
			      :box nil
			      :underline nil
			      :overline nil
			      :height 0.1
			      :background (face-background 'mode-line)
			      :foreground (face-background 'mode-line)))
      (set-face-attribute face nil
			  :box nil
			  :height 0.1
			  :background (face-background 'default)
			  :foreground (face-background 'mode-line)
			  :overline t
			  :underline nil))))

(defun mono-nil-mode-line ()
  "Disable modeline mode"
  (interactive)
  (dolist (face '(mode-line mode-line-active mode-line-inactive))
    (face-remap-add-relative face
			     :box nil
			     :underline nil
			     :overline nil
			     :height 0.1
			     :background (face-background 'default)
			     :foreground (face-background 'default))))

(defun mono-modeline-mode ()
  "My own header-line-mode."
  (interactive)
  (setq header-line-format
  	'(
	  (:eval (cond
  		  (buffer-read-only
		   (propertize " RO " 'face 'header-line-status-RO))
  		  ((buffer-modified-p)
		   (propertize " ** " 'face 'header-line-status-**))
  		  (t (propertize " RW " 'face 'header-line-status-RW))))
  	  (:propertize
	   (:eval (concat " " (buffer-name))) face header-line-default)
  	  (:propertize
  	   (:eval (concat " (" mode-name
  			  (if (mono-vc-branch)
			      (concat " " (mono-vc-branch)) "") ")"))
  	   face header-line-default)
  	  (:eval (mono-mode-line-fill
		  (+ (length (format-mode-line "%l:%c")) 1)))
  	  (:propertize (:eval (concat (format-mode-line "%l:%c") " "))
  		       face header-line-default))))

(provide 'mono-modeline)
;;; mono-modeline.el ends here
