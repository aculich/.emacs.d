;;; extras.el --- small extra functions -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'pp)

;; System

(defun numcores ()
  "Return the number of logical processors on this system."
  (or
   ;; Linux
   (when (file-exists-p "/proc/cpuinfo")
     (with-temp-buffer
       (insert-file-contents "/proc/cpuinfo")
       (how-many "^processor[[:space:]]+:")))
   ;; Windows
   (let ((number-of-processors (getenv "NUMBER_OF_PROCESSORS")))
     (when number-of-processors
       (string-to-number number-of-processors)))
   ;; BSD+OSX
   (with-temp-buffer
     (ignore-errors
       (when (zerop (call-process "sysctl" nil t nil "-n" "hw.ncpu"))
         (string-to-number (buffer-string)))))
   ;; Default
   1))

;; Move line functions
(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (let* ((column (current-column))
         (start (progn (beginning-of-line) (point)))
         (end (progn (end-of-line) (forward-char) (point)))
         (line-text (delete-and-extract-region start end)))
    (forward-line n)
    (insert line-text)
    (forward-line -1)
    (forward-char column)))

(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))

(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)

;; Takes a multi-line paragraph and makes it into a single line of text
(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

;; Fibonacci

(defun fib (n)
  "Fibanacci sequence, slow implementation."
  (if (<= n 2) 1
    (+ (fib (- n 2)) (fib (- n 1)))))

;; Window width

(defun set-window-width (n)
  "Set the selected window's width."
  (adjust-window-trailing-edge (selected-window) (- n (window-width)) t))

(defun set-80-columns ()
  "Set the selected window to 80 columns. If given a prefix
argument, set so that number of columns instead."
  (interactive)
  (set-window-width (or current-prefix-arg 80)))

(global-set-key (kbd "C-x ~") #'set-80-columns)

;; ID: 6a3f3d99-f0da-329a-c01c-bb6b868f3239
(defmacro measure-time (&rest body)
  "Measure and return the running time of the code block."
  (declare (indent defun))
  (let ((start (make-symbol "start")))
    `(let ((,start (float-time)))
       ,@body
       (- (float-time) ,start))))

(defun insert-random (n)
  "Insert a random number between 0 and the prefix argument."
  (interactive "P")
  (insert (number-to-string (random n))))
(global-set-key (kbd "C-c r") 'insert-random)

(defun eval-and-replace (value)
  "Evaluate the sexp at point and replace it with its value."
  (interactive (list (eval-last-sexp nil)))
  (kill-sexp -1)
  (insert (format "%S" value)))

(defun launch (command)
  "Launch an application from Emacs, with its own output
buffer. This is like asynch-shell-command but allows for any
number of processes at a time, rather than just one. If given a
prefix argument, the process's buffer is displayed."
  (interactive (list (read-shell-command (concat default-directory "$ "))))
  (let* ((name (car (split-string-and-unquote command)))
         (buffer (generate-new-buffer (concat "*" name "*"))))
    (set-process-sentinel (start-process-shell-command name buffer command)
                          'launch-sentinel)
    (if (eq (car current-prefix-arg) 4)
        (display-buffer buffer))))

(defun launch-sentinel (proc event)
  "Reports on changes in `launch'ed applications."
  (message (format "%s: %s" proc event)))

(global-set-key (kbd "s-x") 'launch)

;; Dictionary lookup

(autoload 'ispell-get-word "ispell")

(defun lookup-word (word)
  (interactive (list (save-excursion (car (ispell-get-word nil)))))
  (browse-url (format "http://en.wiktionary.org/wiki/%s" word)))

(global-set-key (kbd "M-#") 'lookup-word)

;; Region fashing (from Slime)

(defun flash-region (start end &optional timeout)
  "Temporarily highlight region from START to END."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face 'secondary-selection)
    (run-with-timer (or timeout 0.2) nil 'delete-overlay overlay)))

;; File input

(defun slurp (file)
  "Return FILE contents as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

;; Quick switch to scratch buffers

(defmacro scratch-key (key buffer-name mode)
  `(global-set-key ,key (lambda ()
                          (interactive)
                          (switch-to-buffer ,buffer-name)
                          (unless (eq major-mode ',mode)
                            (,mode)))))

(declare-function js2-mode nil)
(declare-function clojure-mode nil)
;; (scratch-key (kbd "C-c s") "*scratch*"    emacs-lisp-mode)
;; (scratch-key (kbd "C-c j") "*javascript*" js2-mode)
;; (scratch-key (kbd "C-c x") "*css*"        css-mode)
;; (scratch-key (kbd "C-c h") "*html*"       html-mode)

;; ID: 72dc0a9e-c41c-31f8-c8f5-d9db8482de1e
(defun find-all-files (dir)
  "Open all files and sub-directories below the given directory."
  (interactive "DBase directory: ")
  (let* ((list (directory-files dir t "^[^.]"))
         (files (cl-remove-if 'file-directory-p list))
         (dirs (cl-remove-if-not 'file-directory-p list)))
    (dolist (file files)
      (find-file-noselect file))
    (dolist (dir dirs)
      (find-file-noselect dir)
      (find-all-files dir))))

;; Dedicated windows
(defun toggle-current-window-dedication ()
  (interactive)
  (let* ((window (selected-window))
         (dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not dedicated))
    (message "Window %sdedicated to %s"
             (if dedicated "no longer " "")
             (buffer-name))))

(global-set-key (kbd "<pause>") #'toggle-current-window-dedication)

(defun eval-buffer* (&optional buffer)
  "Like `eval-buffer', but obey `lexical-binding'. It does
everything the original function does, except for modifying
`load-history'."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (goto-char (point-min))
      (cl-loop while (< (point) (point-max))
               for sexp = (condition-case _
                              (read (current-buffer))
                            (end-of-file nil))
               do (eval sexp lexical-binding)))
    (message "%S loaded" (current-buffer))))

(defun what-face (pos)
  "Show the name of face under point."
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun eshell-as (name)
  "Start or find an eshell buffer named NAME and pop to it."
  (interactive (list (buffer-name)))
  (let* ((buffer-name (concat "*eshell " name "*"))
         (buffer (or (get-buffer buffer-name)
                     (save-window-excursion (eshell t)))))
    (pop-to-buffer buffer)
    (setf (buffer-name) buffer-name)
    buffer))

;;; Process menu killing

(define-key process-menu-mode-map (kbd "k") #'process-menu-kill)

(defun process-menu-kill ()
  "Kill selected process in the process menu buffer."
  (interactive)
  (let ((process (get-text-property (point) 'tabulated-list-id)))
    (when (processp process) (kill-process process))
    (run-at-time 0.1 nil (lambda ()
                           (let ((n (line-number-at-pos)))
                             (revert-buffer)
                             (forward-line (1- n)))))))

;; pp

(defun pp-macroexpand-all-last-sexp (arg)
  "Run `macroexpand-all' on sexp before point.
With argument, pretty-print output into current buffer.
Ignores leading comment characters."
  (interactive "P")
  (if arg
      (insert (pp-to-string (eval (pp-last-sexp))))
    (pp-display-expression (macroexpand-all (pp-last-sexp))
                           "*Pp Macroexpand Output*")))

;; Help mode assistance

(defun push-first-button ()
  "Find and push the first button in this buffer, intended for `help-mode'."
  (interactive)
  (cl-block :find-button
    (goto-char (point-min))
    (while (< (point) (point-max))
      (if (get-text-property (point) 'button)
          (cl-return-from :find-button (push-button))
        (forward-char)))))

;; Window dimmer

(defun my-make-dimmer (x)
  (lambda ()
    (interactive)
    (let ((alpha (or (frame-parameter (selected-frame) 'alpha) 100)))
      (setf (frame-parameter (selected-frame) 'alpha)
            (min 100 (max 0 (+ alpha x)))))))
(global-set-key (kbd "M-<next>") (my-make-dimmer -5))
(global-set-key (kbd "M-<prior>") (my-make-dimmer 5))

;; Buffers

(defun list-all-buffers (&optional files-only)
  "Display a list of names of existing buffers.
The list is displayed in a buffer named `*Buffer List*'.
Non-null optional arg FILES-ONLY means mention only file buffers."
  (interactive "P")
  (display-buffer (list-buffers-noselect files-only (buffer-list))))

(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.
With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; Tabs

(defun toggle-tab-width ()
  (interactive)
  (let* ((loop [8 4 2])
         (match (or (cl-position tab-width loop) -1)))
    (setf tab-width (aref loop (mod (1+ match) (length loop))))))

(global-set-key (kbd "C-h t") #'toggle-tab-width)

;; Cygwin compatibility

(defun cygwin-init ()
  (let ((cygwin-root "c:/cygwin64"))
    (unless (file-directory-p cygwin-root)
      (error "Can't find cygwin"))
    (setenv "PATH" (concat cygwin-root "/bin" ";" (getenv "PATH")))
    (push (concat cygwin-root "/bin") exec-path)
    (setf shell-file-name "bash.exe")
    ;; Translate paths for Cygwin Git
    (defadvice magit-expand-git-file-name
        (before magit-expand-git-file-name-cygwin activate)
      (save-match-data
        (when (string-match "^/cygdrive/\\([a-z]\\)/\\(.*\\)" filename)
          (let ((drive (match-string 1 filename))
                (path (match-string 2 filename)))
            (setf filename (concat drive ":/" path))))))))

;; Civ 5 game search

(defun filthyrobot-playlist (n)
  (interactive "nGame number: ")
  (browse-url
   (format "https://www.youtube.com/results?search_query=filthyrobot+%%22game+%d%%22&filters=playlist" n)))

(provide 'extras)

;;; extras.el ends here
