;;; lunaryorn-markdown.el --- Additional tools for Markdown  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2016  Sebastian Wiesner <swiesner@lunaryorn.com>

;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>
;; URL: https://gihub.com/lunaryorn/.emacs.d
;; Keywords: convenience, abbrev

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

;; Markdown tools.

;;; Code:

(require 'skeleton)
(require 'rx)
(require 'subr-x)

(define-skeleton lunaryorn-markdown-post-header
  "Insert a header for blog posts."
  (read-from-minibuffer "Title: ")
  "---\n"
  "title: " str "\n"
  "---\n\n"
  -)

(defun lunaryorn-markdown-publish-jekyll-draft ()
  "Publish a Jekyll draft in this buffer as a post."
  (interactive)
  (let ((current-dir (directory-file-name (file-name-directory (buffer-file-name))))
        (source-dir (locate-dominating-file (buffer-file-name) "_config.yml")))
    (unless source-dir
      (user-error "Failed to find _config.yml.  This file does not seem to be part of a Jekyll site"))
    (unless (string= (file-name-base current-dir) "_drafts")
      (user-error "This buffer is not a draft"))
    (let* ((post-dir (expand-file-name "_posts/" source-dir))
           (new-name (concat (format-time-string "%F-")
                             (file-name-nondirectory (buffer-file-name))))
           (target (expand-file-name new-name post-dir)))
      (rename-file (buffer-file-name) target)
      (set-visited-file-name target 'no-query 'along-with-file)
      (message "Moved to %s" target))))

(provide 'lunaryorn-markdown)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; lunaryorn-markdown.el ends here
