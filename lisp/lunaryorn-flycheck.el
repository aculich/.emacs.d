;;; lunaryorn-flycheck.el --- Additional utilities for Flycheck  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2016  Sebastian Wiesner

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

;; Helpers for Flycheck

;;; Code:

(require 'flycheck)
(require 'dash)

(defun lunaryorn-flycheck-find-config-file-in-sbt-project (file &optional _checker)
  "Find a config FILE in sbt project/ directories."
  (-when-let* ((file-name (buffer-file-name))
               (root-dir (locate-dominating-file file-name "build.sbt"))
               (project-dir (expand-file-name "project/" root-dir))
               (config-file (expand-file-name file project-dir)))
    (when (file-exists-p config-file)
      config-file)))

(defun lunaryorn-flycheck-set-load-path-for-user-configuration ()
  "Set Flycheck load path for files in user configuration."
  (when (and (buffer-file-name)
             (flycheck-in-user-emacs-directory-p (buffer-file-name)))
    (setq-local flycheck-emacs-lisp-load-path
                (cons (locate-user-emacs-file "lisp/")
                      flycheck-emacs-lisp-load-path))))

(defun lunaryorn-discard-undesired-html-tidy-error (err)
  "Discard ERR if it is undesired.

Tidy is very verbose, so we prevent Flycheck from highlighting
most errors from HTML Tidy."
  ;; A non-nil result means to inhibit further processing (i.e. highlighting)
  ;; of the error
  (and (eq (flycheck-error-checker err) 'html-tidy)
       ;; Only allow warnings about missing tags, or unexpected end tags being
       ;; discarded
       (not (string-match-p (rx (or "missing" "discarding"))
                            (flycheck-error-message err)))))

(defun lunaryorn-use-js-executables-from-node-modules ()
  "Set executables of JS checkers from local node modules."
  (-when-let* ((file-name (buffer-file-name))
               (root (locate-dominating-file file-name "node_modules"))
               (module-directory (expand-file-name "node_modules" root)))
    (pcase-dolist (`(,checker . ,module) '((javascript-jshint . "jshint")
                                           (javascript-eslint . "eslint")
                                           (javascript-jscs   . "jscs")))
      (let ((package-directory (expand-file-name module module-directory))
            (executable-var (flycheck-checker-executable-variable checker)))
        (when (file-directory-p package-directory)
          (set (make-local-variable executable-var)
               (expand-file-name (concat "bin/" module ".js")
                                 package-directory)))))))

(provide 'lunaryorn-flycheck)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; lunaryorn-flycheck.el ends here
