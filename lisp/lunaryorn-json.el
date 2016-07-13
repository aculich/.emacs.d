;;; lunaryorn-json.el --- Additional tools for JSON -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2015  Sebastian Wiesner <swiesner@lunaryorn.com>

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

;; JSON tools.

;;; Code:

(require 'skeleton)

;;;###autoload
(define-skeleton lunaryorn-json-chef-role
  "Insert a Chef role skeleton."
  nil
  "{\n"
  > "\"json_class\": \"Chef::Role\",\n"
  > "\"name\": \"" (if (buffer-file-name)
                       (file-name-base (buffer-file-name))
                     (buffer-name))
  "\",\n"
  > "\"run_list\": [\n"
  > - "\n"
  "]" > "\n"
  "}" > "\n")

(provide 'lunaryorn-json)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; lunaryorn-json.el ends here
