;;; lunaryorn-elisp.el --- Utilities for Emacs Lisp  -*- lexical-binding: t; -*-
;; Copyright (C) 2014-2015  Sebastian Wiesner <swiesner@lunaryorn.com>

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

;; Utilities for Lisp.

;;; Code:

(require 'rx)

(defun lunaryorn-elisp-find-cask-file (other-window)
    "Find the Cask file for this buffer.

When OTHER-WINDOW is non-nil, find the Cask file in another
window."
    (interactive "P")
    (unless (buffer-file-name)
      (user-error "The buffer has no file"))
    (let ((directory (locate-dominating-file (buffer-file-name) "Cask")))
      (unless directory
        (user-error "No Cask file found for this file"))
      (funcall (if other-window #'find-file-other-window #'find-file)
               (expand-file-name "Cask" directory))))

(defun lunaryorn-elisp-current-feature ()
  "Return the feature provided by the current buffer."
  (save-excursion
    (goto-char (point-min))
    (when (search-forward-regexp (rx line-start "(provide '"))
      (symbol-name (symbol-at-point)))))

(defconst lunaryorn-use-package-imenu-expression
  `("Use Package" ,(rx "(use-package" (optional "-with-elapsed-timer")
                       symbol-end (1+ (syntax whitespace)) symbol-start
                       (group-n 1 (1+ (or (syntax word) (syntax symbol))))
                       symbol-end) 1)
  "IMenu expression for `use-package' declarations.")

(defun lunaryorn-add-use-package-to-imenu ()
  "Add `use-package' declarations to `imenu'."
  (add-to-list 'imenu-generic-expression
               lunaryorn-use-package-imenu-expression))

(provide 'lunaryorn-elisp)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; lunaryorn-elisp.el ends here
