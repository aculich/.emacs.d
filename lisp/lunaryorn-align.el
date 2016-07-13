;;; lunaryorn-align.el --- Personal align enhancements  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Sebastian Wiesner
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors

;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>

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

;; Personal extensions to `align', graciously taken from Spacemacs.

;;; Code:

;;;###autoload
(defun lunaryorn-align-repeat (start end regexp &optional justify-right after)
  "Repeat alignment with respect to the given regular expression.
If JUSTIFY-RIGHT is non nil justify to the right instead of the
left. If AFTER is non-nil, add whitespace to the left instead of
the right."
  (interactive "r\nsAlign regexp: ")
  (let ((complete-regexp (if after
                             (concat regexp "\\([ \t]*\\)")
                           (concat "\\([ \t]*\\)" regexp)))
        (group (if justify-right -1 1)))
    (align-regexp start end complete-regexp group 1 t)))

;; Modified answer from
;; http://emacs.stackexchange.com/questions/47/align-vertical-columns-of-numbers-on-the-decimal-point
(defun lunaryorn-align-repeat-decimal (start end)
  "Align a table of numbers on decimal points and dollar signs (both optional)."
  (interactive "r")
  (require 'align)
  (align-region start end nil
                '((nil (regexp . "\\([\t ]*\\)\\$?\\([\t ]+[0-9]+\\)\\.?")
                       (repeat . t)
                       (group 1 2)
                       (spacing 1 1)
                       (justify nil t)))
                nil))

(defmacro lunaryorn-create-align-repeat-x (name regexp &optional justify-right default-after)
  (let ((new-func (intern (concat "lunaryorn-align-repeat-" name))))
    `(defun ,new-func (start end switch)
       (interactive "r\nP")
       (let ((after (not (eq (if switch t nil) (if ,default-after t nil)))))
         (lunaryorn-align-repeat start end ,regexp ,justify-right after)))))

(lunaryorn-create-align-repeat-x "comma" "," nil t)
(lunaryorn-create-align-repeat-x "semicolon" ";" nil t)
(lunaryorn-create-align-repeat-x "colon" ":" nil t)
(lunaryorn-create-align-repeat-x "equal" "=")
(lunaryorn-create-align-repeat-x "math-oper" "[+\\-*/]")
(lunaryorn-create-align-repeat-x "ampersand" "&")
(lunaryorn-create-align-repeat-x "bar" "|")
(lunaryorn-create-align-repeat-x "left-paren" "(")
(lunaryorn-create-align-repeat-x "right-paren" ")" t)

(provide 'lunaryorn-align)
;;; lunaryorn-align.el ends here
