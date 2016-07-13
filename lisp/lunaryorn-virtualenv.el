;;; lunaryorn-virtualenv.el --- Personal virtualenv tools  -*- lexical-binding: t; -*-

;; Copyright (c) 2015 Sebastian Wiesner <swiesner@lunaryorn.com>
;;
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

;; Automatically set the virtualenv root from workon-home if a virtualenv with
;; the same name as the current project exists.

;;; Code:

(require 'projectile)
(require 'python)

(defvar lunaryorn-virtualenv-workon-home (or (getenv "WORKON_HOME")
                                             (expand-file-name "~/.virtualenvs"))
  "The $WORKON_HOME path.")

(defun lunaryorn-virtualenv-init-from-workon-home ()
  "Set the current virtualenv for this buffer."
  (let* ((name (projectile-project-name))
         (venv-dir (expand-file-name name lunaryorn-virtualenv-workon-home)))
    (when (file-directory-p venv-dir)
      (setq-local python-shell-virtualenv-root venv-dir))))

(provide 'lunaryorn-virtualenv)
;;; lunaryorn-virtualenv.el ends here
