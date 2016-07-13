;; Global keybindings

(defun switch-to-buffer-force (&optional args)
  (interactive)
  (switch-to-buffer args))

(defun kill-buffer-force (&optional args)
  (interactive)
  (kill-buffer))

;;; Cursor Movement
(bind-key "C-x C-x"           'exchange-point-and-mark)
(bind-key "A-SPC"             'pop-to-mark-command)
(bind-key " "                 'pop-to-mark-command)
(bind-key "A-C-SPC"           'pop-to-mark-command)
(bind-key "C-x C-."           'pop-global-mark)
(bind-key "M-SPC"             'pop-global-mark)
(bind-key "C-c C-n"           'next-error)
(bind-key "C-c C-p"           'previous-error)
(bind-key "C-n"               'next-line)
(bind-key "C-p"               'previous-line)
(bind-key "A-j"               'forward-paragraph)
(bind-key "A-k"               'backward-paragraph)
(bind-key "A-h"               'mark-paragraph)
(bind-key "C-c C-n"           'forward-page)
(bind-key "C-c C-p"           'backward-page)
(bind-key "M-a"               'backward-up-list)

;;; Selection
(bind-key "C-x C-l"           'select-current-line)

;;; Search
(bind-key "C-A-g"             'custom-grep-find)
(bind-key "A-f"               'find-name-dired)
(bind-key "C-h C-w"           'find-function-on-key)
(bind-key "C-h C-f"           'find-function-at-point)
(bind-key "C-r"               'isearch-backward-regexp)
(bind-key "C-s"               'isearch-forward-regexp)
(bind-key "C-M-r"             'isearch-backward)
(bind-key "C-M-s"             'isearch-forward)

(bind-key "<M-up>"            'search-word-backward)
(bind-key "<M-down>"          'search-word-forward)
(bind-key "M-p"               'search-word-backward)
(bind-key "M-n"               'search-word-forward)
(bind-key "A-M-f"             'find-dired)
(bind-key "A-l"               'locate)

;;; Replace
(bind-key "C-t"               'replace-regexp)
(bind-key "C-M-%"             'query-replace)
(bind-key "M-%"               'query-replace-regexp)

;;; Text Modification
(bind-key "C-x M-$"           'ispell-buffer)
(bind-key "M-Q"               'unfill-paragraph)
(bind-key "M-;"               'comment-or-uncomment-current-line-or-region)
(bind-key "C-;"               'comment-or-uncomment-current-line-or-region)
(bind-key "C-o"               'open-line)
(bind-key "M-|"               'align-regexp)
(bind-key "RET"               'newline-and-indent)
(bind-key "A-d"               'duplicate-current-line-or-region)
;; (bind-key "M-c"               'duplicate-current-line-or-region)
;; (bind-key "¢"                 'duplicate-current-line-or-region)
(bind-key "A-c"               'capitalize-word)
(bind-key "A-u"               'upcase-word)
(bind-key "A-C-<backspace>"     'delete-trailing-whitespace)

;;; Killing
(bind-key "C-M-S-k"           'backward-kill-sexp)
(bind-key "C-M-S-w"           'backward-kill-sexp)
(bind-key "C-q"               'kill-region)
(bind-key "C-w"               'backward-kill-word)
(bind-key "C-S-l"             'append-next-kill)
(bind-key "C-S-k"             'kill-whole-line)
(bind-key "A-C-d A-C-m A-C-l" 'delete-matching-lines)
(bind-key "A-C-d A-C-n A-C-l" 'delete-non-matching-lines)
(bind-key "C-M-<backspace>"   'kill-back-to-indentation)

;;; Buffers
(bind-key "C-="               'ediff-buffers)
(bind-key "C-M-S-l"           'switch-to-buffer-force)
(bind-key "C-x C-b"           'switch-to-buffer)
;;;;; TODO: force git commit if killed buffer has uncommited changes
(bind-key "C-x C-k"           'kill-buffer-force)
(bind-key "C-z"               'ido-switch-buffer)
(bind-key "A-b"               'ido-switch-buffer)
(bind-key "C-A-b"             'switch-to-buffer-force)
(bind-key "C-A-l"             'switch-to-buffer-force)
(bind-key "A-l"               'switch-to-buffer-force)
(bind-key "A-J"               'next-buffer)
(bind-key "A-K"               'previous-buffer)
(bind-key "A-s"               'save-buffer)

;;; Windows
(bind-key "C-^"               'enlarge-window)
(bind-key "C-x C--"           'split-window-vertically)
(bind-key "C-x C-\\"          'split-window-horizontally)
(bind-key "C-x l"             'balance-windows-area)
(bind-key "C-{"               'shrink-window-horizontally)
(bind-key "C-}"               'enlarge-window-horizontally)

(bind-key "C-A-0"             'delete-window-or-frame)
(bind-key "C-A-0"             'delete-window)
(bind-key "C-A-1"             'delete-other-windows)
(bind-key "C-A-2"             'split-window-vertically)
(bind-key "C-A-3"             'split-window-horizontally)
(bind-key "C-A-4"             'dired-jump)
(bind-key "C-A-5"             'delete-window-make-new-frame)
(bind-key "C-A-y"             'kill-whole-line-force)
(bind-key "C-A-9"             'kill-buffer-and-window)
(bind-key "C-A--"             'bury-buffer)
(bind-key "C-x 9"             'kill-buffer-and-window)
(bind-key "C-x C-9"           'kill-buffer-and-window)

(bind-key "C-'"               'winner-undo-redo)
(bind-key "C-c C-;"           'winner-undo-redo)
(bind-key "C-S-<iso-lefttab>" 'other-window-previous)
(bind-key "C-S-<tab>"         'other-window-previous)
(bind-key "C-<tab>"           'other-window)
(bind-key "C-x C-o"           'other-window)
(bind-key "C-x o"             'other-window)
(bind-key "C-x C-o"           'other-window)
(bind-key "C-x C-d"           'dired-other-window)

;;; Frames
(bind-key "<C-menu>"          'toggle-menu-bar-mode-from-frame)
(bind-key "C-x C-;"           'delete-frame)

;;; Fonts
(bind-key "A-+"               'text-scale-increase)
(bind-key "A-_"               'text-scale-decrease)
(bind-key "A-="               'text-scale-increase)
(bind-key "A--"               'text-scale-decrease)
(bind-key "A-C-="             'text-scale-set)
(bind-key "A-C-+"             'text-scale-adjust)

;;; Customization
(bind-key "C-h C-a"           'customize-apropos-all)
(bind-key "C-h C-a"           'customize-apropos)
(bind-key "C-h C-c"           'customize-apropos)
(bind-key "C-h C-r"           'customize-apropos)
(bind-key "C-h g"             'customize-group)
(bind-key "C-h C-v"           'customize-variable)

;;; Packages
(unbind-key "C-h d")
(bind-key "C-h d i"           'list-packages)
(bind-key "C-h d l"           'list-packages)

;;; Help Documentation
(bind-key "C-h A-v"           'apropos-value)
(bind-key "C-x v C-h"         'describe-prefix-bindings)
(bind-key "A-a"               'manual-entry)
(bind-key "A-m"               'manual-entry)

;;; Shell
;(bind-key "A-e"               'shell)
(bind-key "A-;"               'async-shell-command)
(bind-key "M-!"               'async-shell-command)
(bind-key "C-M-!"             'shell-command)

;;; Misc
(bind-key "C-c C-\\"          'toggle-input-method)
(bind-key "C-x C-z"           'toggle-truncate-lines)
(bind-key "C-x C-4"           'set-selective-display)
(bind-key "C-h C-n"           'linum-mode)
(bind-key "A-q"               'quoted-insert)
(bind-key "C-c C-c"           'compile)
(bind-key "C-h o"             'list-processes)

;;; Mac OS X

(bind-key "C-<f4>"             'other-frame)
(bind-key "A-`"                'other-frame)
(bind-key "A-h"                'ns-do-hide-emacs)

(provide 'global-keys)
