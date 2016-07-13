(use-package bind-key
  :ensure t)

(defun dired-timesort (filename &optional wildcards)
  (let ((dired-listing-switches "-lhat"))
    (dired filename wildcards)))

(defmacro quick-find (key file &optional path find-args)
  `(bind-key
    ,key
    (cond
     ((stringp ,find-args)
      '(lambda (&optional arg)
         (interactive)
         (find-dired (expand-file-name ,file ,path) ,find-args)))
     ((and
       ;; (not (tramp-tramp-file-p (expand-file-name ,file ,path)))
       (or (file-directory-p (expand-file-name ,file ,path))
           (not (file-exists-p (expand-file-name ,file ,path)))))
      '(lambda (&optional arg)
         (interactive)
         (dired-timesort (expand-file-name ,file ,path))))
     (t
      '(lambda (&optional arg)
         (interactive)
         (find-file (expand-file-name ,file ,path)))))))

;;; Files
(quick-find "C-h C-`"     "~/")
(quick-find "C-h C-t"     "/tmp/")
;; (quick-find "C-h C-e"     "/sudo::/etc/")
(quick-find "C-h C-x C-o" "~/org")
(quick-find "C-h C-o"     "~/Documents")
(quick-find "C-h C-d"     "~/Downloads")
(quick-find "C-h C-i"     user-init-file)
(quick-find "C-h C-x C-u" custom-file)
(quick-find "C-h C-x C-k" user-keys-file)
(quick-find "C-h C-x C-c" "Cask" user-emacs-directory)
(quick-find "C-h C-x C-e" (format ".cask/%s/elpa/" emacs-version) user-emacs-directory)
(quick-find "C-h C-x e"   "emacs" "~/workspace")
(quick-find "C-h C-x C-q" "quick-find.el" user-emacs-directory)
(quick-find "C-h C-x C-w" "~/workspace")
(quick-find "C-h C-x p"   "~/Pictures")
(quick-find "C-h C-x C-s" "~/.ssh/config")
(quick-find "C-h C-x C-b" "~/.bash_profile")
;; (quick-find "C-h C-x C-b" (crux-find-shell-init-file))
(quick-find "C-h C-x C-s" "~/.ssh/config")
;; (quick-find "C-h C-x C-h" "/sudo::/etc/hosts")
(quick-find "C-h C-x s" "settings.el" user-emacs-directory)
