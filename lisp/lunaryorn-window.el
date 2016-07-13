;;; lunaryorn-window.el --- Utilities for windows   -*- lexical-binding: t; -*-

;; Copyright (c) 2015-2016 Sebastian Wiesner <swiesner@lunaryorn.com>

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

;; Provide additional functions to manage windows

;;; Code:

(defun lunaryorn-find-side-windows (&optional side)
  "Get all side window if any.

If SIDE is non-nil only get windows on that side."
  (let (windows)
    (walk-window-tree
     (lambda (window)
       (let ((window-side (window-parameter window 'window-side)))
         (when (and window-side (or (not side) (eq window-side side)))
           (push window windows)))))
    windows))

;;;###autoload
(defun lunaryorn-quit-all-side-windows ()
  "Quit all side windows of the current frame."
  (interactive)
  (dolist (window (lunaryorn-find-side-windows))
    (when (window-live-p window)
      (quit-window nil window)
      ;; When the window is still live, delete it
      (when (window-live-p window)
        (delete-window window)))))

;; Taken graciously from Spacemacs
;;;###autoload
(defun lunaryorn-switch-to-minibuffer-window ()
  "Switch to current minibuffer window (if active)."
  (interactive)
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))

;; From http://dfan.org/blog/2009/02/19/emacs-dedicated-windows/
;;;###autoload
(defun lunaryorn-toggle-current-window-dedication ()
  "Toggle dedication state of a window."
  (interactive)
  (let* ((window    (selected-window))
         (dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not dedicated))
    (message "Window %sdedicated to %s"
             (if dedicated "no longer " "")
             (buffer-name))))

(provide 'lunaryorn-window)

;;; lunaryorn-window.el ends here
