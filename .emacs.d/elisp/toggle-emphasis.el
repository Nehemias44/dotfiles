
(defun my-toggle-emphasis ()
  "Toggle emphasis in buffer"
  (interactive)
  (if (eq org-hide-emphasis-markers t)
      (progn
        (setq org-hide-emphasis-markers nil)
        (font-lock-fontify-buffer)
        (message "Emphasis toggled OFF"))
    (progn
      (setq org-hide-emphasis-markers t)
      (font-lock-fontify-buffer)
      (message "Emphasis toggled ON"))))

(map! :leader :desc "Emphasis" :m "t e" #'ig/toggle-emphasis)
