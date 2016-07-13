;;; lunaryorn-files.el --- File utilities            -*- lexical-binding: t; -*-

;; Copyright (c) 2012-2015 Sebastian Wiesner <swiesner@lunaryorn.com>

;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>
;; URL: https://gihub.com/lunaryorn/.emacs.d

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Additional utilities for file handling, see:
;; http://emacsredux.com/blog/2013/05/04/rename-file-and-buffer/
;; http://emacsredux.com/blog/2013/04/03/delete-file-and-buffer/
;; http://emacsredux.com/blog/2013/03/27/copy-filename-to-the-clipboard/
;; http://emacsredux.com/blog/2013/04/05/recently-visited-files/
;; https://github.com/bbatsov/prelude/blob/master/core/prelude-core.el

;;; Code:

(require 'package)
(require 'lisp-mnt)
(require 'find-func)
(require 'subr-x)

;; We only access these variables if the corresponding library is loaded
(defvar recentf-list)
(defvar projectile-require-project-root)

;; Assert the byte compiler that dired functions are defined, because we never
;; call them for non-dired buffers, so we can be sure that dired is always
;; loaded first.
(declare-function dired-get-marked-files "dired")
(declare-function dired-current-directory "dired")


;;; Utilities
(defun lunaryorn-current-file ()
  "Gets the \"file\" of the current buffer.

The file is the buffer's file name, or the `default-directory' in
`dired-mode'."
  (if (derived-mode-p 'dired-mode)
      default-directory
    (buffer-file-name)))


;;; Working with file names
;;;###autoload
(defun lunaryorn-copy-filename-as-kill (&optional arg)
  "Copy the name of the currently visited file to kill ring.

With a zero prefix arg, copy the absolute file name.  With
\\[universal-argument], copy the file name relative to the
current Projectile project, or to the current buffer's
`default-directory', if the file is not part of any project.
Otherwise copy the non-directory part only."
  (interactive "P")
  (if-let ((file-name (lunaryorn-current-file))
           (name-to-copy
            (cond
             ((zerop (prefix-numeric-value arg)) file-name)
             ((consp arg)
              (let* ((projectile-require-project-root nil)
                     (directory (and (fboundp 'projectile-project-root)
                                     (projectile-project-root))))
                (file-relative-name file-name directory)))
             (t (file-name-nondirectory file-name)))))
      (progn
        (kill-new name-to-copy)
        (message "%s" name-to-copy))
    (user-error "This buffer is not visiting a file")))


;;; Working with the current file
;;;###autoload
(defun lunaryorn-rename-file-and-buffer ()
  "Rename the current file and buffer."
  (interactive)
  (let* ((filename (buffer-file-name))
         (old-name (if filename
                       (file-name-nondirectory filename)
                     (buffer-name)))
         (new-name (read-file-name "New name: " nil nil nil old-name)))
    (cond
     ((not (and filename (file-exists-p filename))) (rename-buffer new-name))
     ((vc-backend filename) (vc-rename-file filename new-name))
     (t
      (rename-file filename new-name 'force-overwrite)
      (set-visited-file-name new-name 'no-query 'along-with-file)))))

;;;###autoload
(defun lunaryorn-delete-file-and-buffer ()
  "Delete the current file and kill the buffer."
  (interactive)
  (let ((filename (buffer-file-name)))
    (cond
     ((not filename) (kill-buffer))
     ((vc-backend filename) (vc-delete-file filename))
     (t
      (delete-file filename)
      (kill-buffer)))))

;;;###autoload
(defun lunaryorn-launch-dwim ()
  "Open the current file externally."
  (interactive)
  (if (derived-mode-p 'dired-mode)
      (let ((marked-files (dired-get-marked-files)))
        (if marked-files
            (launch-files marked-files 'confirm)
          (launch-directory (dired-current-directory))))
    (if (buffer-file-name)
        (launch-file (buffer-file-name))
      (user-error "The current buffer is not visiting a file"))))

(defun lunaryorn-open-in-intellij ()
  "Open the current file in IntelliJ IDEA."
  (interactive)
  (let ((idea (executable-find "idea")))
    (unless idea
      (user-error "IntelliJ launcher does not exist.
Create with Tools -> Create Command-line launcher in IntelliJ"))
    (unless (= 0 (call-process idea nil nil nil
                               "--line" (number-to-string (line-number-at-pos))
                               (expand-file-name (buffer-file-name))))
      (error "IntelliJ failed"))))


;;; Init file and packages
;;;###autoload
(defun lunaryorn-find-user-init-file-other-window ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))

;;;###autoload
(defun lunaryorn-recompile-packages ()
  "Recompile all packages."
  (interactive)
  (byte-recompile-directory package-user-dir nil 'force))

(defun lunaryorn-all-init-files (&optional with-packages)
  "Return a list of all Emacs Lisp files in my configuration.

If WITH-PACKAGES is given and non-nil include 3rd party
packages."
  (append (list user-init-file)
          (directory-files-recursively (locate-user-emacs-file "lisp/")
                                       (rx ".el" eos))
          (if with-packages
              (directory-files-recursively package-user-dir
                                           (rx ".el" eos))
            nil)))

;;;###autoload
(defun lunaryorn-count-config-lines (&optional with-packages)
  "Show a buffer with LoC statistics for my Emacs config.

If WITH-PACKAGES is given and non-nil include 3rd party packages
into the count."
  (interactive "P")
  (let ((cloc (executable-find "cloc")))
    (unless cloc
      (user-error "Please install cloc"))
    (with-current-buffer (get-buffer-create " *LoC Emacs configuration*")
      (text-mode)
      (read-only-mode)
      (view-mode)
      (let ((inhibit-read-only t)
            (files (lunaryorn-all-init-files with-packages)))
        (erase-buffer)
        (goto-char (point-min))
        (apply #'call-process cloc nil t t "--quiet" files))
      (pop-to-buffer (current-buffer)))))


;;; URLs and browsing
(defun lunaryorn-browse-feature-url (feature)
  "Browse the URL of the given FEATURE.

Interactively, use the symbol at point, or prompt, if there is
none."
  (interactive
   (let ((symbol (or (symbol-at-point)
                     (completing-read "Feature: " features nil
                                      'require-match))))
     (list symbol)))
  (let* ((library (if (symbolp feature) (symbol-name feature) feature))
         (library-file (find-library-name library)))
    (when library-file
      (with-temp-buffer
        (insert-file-contents library-file)
        (let ((url (lm-header "URL")))
          (if url
              (browse-url url)
            (user-error "Library %s has no URL header" library)))))))

(provide 'lunaryorn-files)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; lunaryorn-files.el ends here
