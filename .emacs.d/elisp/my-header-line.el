(defgroup my/header-line nil
  "My mode-line-mode"
  :group 'convenience)

(defface header-line-status-RO
  '((t (:inherit nil
	:box (:line-width 3 :color "green")
	:background "green"
	:foreground "black"
	:weight regular
	:slant normal)))
  "Face for status file Read only"
  :group 'my/header-line)

(defface header-line-status-RW
  '((t (:inherit nil
	:box (:line-width 3 :color "grey")
	:background "grey"
	:foreground "black"
	:weight regular
	:slant normal)))
  "Face for status file read and write"
  :group 'my/header-line)

(defface header-line-status-**
  '((t (:inherit nil
	:box (:line-width 3 :color "red")
	:background "red"
	:foreground "black"
	:weight regular
	:slant normal)))
  "Face for status file modified"
  :group 'my/header-line)

(defface header-line-default
  '((t (:inherit nil
	:box (:line-width 3 :color "black")
	:background "black"
	:foreground "white"
	:weight light
	:slant italic)))
  "Face for default headerline"
  :group 'my/header-line)

(defun my/vc-branch ()
  "Get version control branch."
  (if vc-mode
      (let ((backend (vc-backend buffer-file-name)))
       	(concat "#"
		(substring-no-properties
		 vc-mode
       		 (+ (if (eq backend 'Hg) 2 3) 2)))) nil))


(defun my/mode-line-fill (reserve)
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

(defun my/thin-mode-line ()
  "Set modeline like a underline"
  (interactive)
  (dolist (face '(mode-line mode-line-active mode-line-inactive))
    (face-remap-add-relative face
			     :box nil
			     :underline nil
			     :overline nil
			     :height 0.1
			     :background (face-background 'mode-line)
			     :foreground (face-background 'mode-line))))

(defun my/nil-mode-line ()
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

(defun my/header-line-mode ()
  "My own header-line-mode."
  (setq header-line-format
  	'((:eval (cond
  		  (buffer-read-only
		   (propertize " RO " 'face 'header-line-status-RO))
  		  ((buffer-modified-p)
		   (propertize " ** " 'face 'header-line-status-**))
  		  (t (propertize " RW " 'face 'header-line-status-RW))))
  	  (:propertize
	   (:eval (concat " " (buffer-name))) face header-line-default)
  	  (:propertize
  	   (:eval (concat " (" mode-name
  			  (if (my/vc-branch)
			      (concat " " (my/vc-branch)) "") ")"))
  	   face header-line-default)
  	  (:eval (my/mode-line-fill
		  (+ (length (format-mode-line "%l:%c")) 1)))
  	  (:propertize (:eval (concat (format-mode-line "%l:%c") " "))
  		       face header-line-default)))
  (my/thin-mode-line))
