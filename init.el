;;; init.el --- Emacs configuration of Aaron Culich -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2016 Aaron Culich <aculich@gmail.com>
;; Copyright (c) 2012-2016 Sebastian Wiesner <swiesner@lunaryorn.com>
;; Copyright (c) 2016 WellonsChristopher  <wellons@nullprogram.com>
;;
;; Author: Aaron Culich <aculich@gmail.com>
;; URL: https://gihub.com/aculich/.emacs.d
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
;; USA.

;;; Commentary:

;; Emacs configuration of Aaron Culich
;;
;; Based heavily on (e.g. forked directly from) Sebastian Wiesner's awesome
;; Emacs setup: https://github.com/lunaryorn/.emacs.d
;;
;; With many awesome additions taken from Christopher Wellons's configs:
;; https://github.com/skeeto/.emacs.d

;;; Code:

;;; Debugging
(setq message-log-max 10000)


;;;; Paths

(prefer-coding-system 'utf-8-unix)
(set-language-environment "UTF-8")
(setq iso-transl-char-map nil)          ; http://emacs.stackexchange.com/a/17524/2138

(defvar user-setup-directory          (expand-file-name "setup"          user-emacs-directory))
(defvar user-setup-builtins-directory (expand-file-name "setup/builtins" user-emacs-directory))
(defvar local-dev-package-directory   (expand-file-name "packages"       user-emacs-directory))
(defvar user-data-directory           (expand-file-name ""               user-emacs-directory))
(defvar user-cache-directory          (expand-file-name ".cache"         user-emacs-directory))
(defvar user-bin-directory            (expand-file-name "bin"            "~"))
(setq custom-file                     (expand-file-name "settings.el"    user-emacs-directory))
(make-directory user-cache-directory t)


;;; Package management

;; Please don't load outdated byte code
(setq load-prefer-newer t)

(require 'cask "~/.cask/cask.el")
(ignore-errors (cask-initialize))

;; (require 'package)
;; (setq package-enable-at-startup nil)
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; (package-initialize)

;; ;; Bootstrap `use-package'
;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))


;;; Requires

(eval-when-compile
  (require 'use-package)
  (setq use-package-verbose t))

(use-package benchmark-init
  :ensure t)

(require 'diminish)                ;; if you use :diminish
(require 'bind-key)

(require 'subr-x)
(require 'time-date)


;;; Initialization
(when (version< emacs-version "25")
  (warn "This configuration needs Emacs trunk, but this is %s!" emacs-version))

;; And disable the site default settings
(setq inhibit-default-init t)

(defun lunaryorn-snapshot-version-p (version)
  "Whether VERSION is an Emacs snapshot version."
  (pcase-let ((`(,_ ,_ ,build) (version-to-list version)))
    ;; Snapshots with build numbers > 90 are pretests which come from proper
    ;; release tarballs and don't need to be rebuild weekly
    (and (>= build 50) (<= build 90))))

(when (lunaryorn-snapshot-version-p emacs-version)
  ;; When on a snapshot version, warn if the build is older than a week to
  ;; ensure that we stay up to date.
  (run-with-idle-timer
   2 nil
   (lambda ()
     (let ((time-since-build (time-subtract (current-time) emacs-build-time)))
       (when (> (time-to-number-of-days time-since-build) 7)
         (lwarn 'emacs :warning "Your Emacs build is more than a week old!"))))))


;;; Environment fixup
(use-package exec-path-from-shell
  :ensure t
  :if (and (eq system-type 'darwin) (display-graphic-p))
  :config
  (progn
    (when (string-match-p "/zsh$" (getenv "SHELL"))
      ;; Use a non-interactive login shell.  A login shell, because my
      ;; environment variables are mostly set in `.zprofile'.
      (setq exec-path-from-shell-arguments '("-l")))

    ;; Import additional environment variables beyond just $PATH
    (dolist (var '("PYTHONPATH"         ; Python modules
                   "INFOPATH"           ; Info directories
                   "JAVA_OPTS"          ; Options for java processes
                   "SBT_OPTS"           ; Options for SBT
                   "RUST_SRC_PATH"      ; Rust sources, for racer
                   "CARGO_HOME"         ; Cargo home, for racer
                   "EMAIL"              ; My personal email
                   ))
      (add-to-list 'exec-path-from-shell-variables var))

    ;; Initialize Emacs' environment from the shell
    (exec-path-from-shell-initialize)

    ;; Tell Emacs about my email address
    ;; FIXME: bug triggered in tramp-loaddefs.el:543 if user-mail-address is nil
    ;;                         /Applications/Emacs.app/Contents/Resources/lisp/net/
    ;; report upstream in two places:
    ;;     https://github.com/lunaryorn/.emacs.d/
    ;;     upstream emacs source: tramp-loaddefs.el:543
    (setq user-mail-address (or (getenv "EMAIL") "unconfigured@example.com"))

    ;; Re-initialize the `Info-directory-list' from $INFOPATH.  Since package.el
    ;; already initializes info, we need to explicitly add the $INFOPATH
    ;; directories to `Info-directory-list'.  We reverse the list of info paths
    ;; to prepend them in proper order subsequently
    (with-eval-after-load 'info
      (dolist (dir (nreverse (parse-colon-path (getenv "INFOPATH"))))
        (when dir
          (add-to-list 'Info-directory-list dir))))))


;;; Customization
(defconst lunaryorn-custom-file (locate-user-emacs-file "custom.el")
  "File used to store settings from Customization UI.")

(use-package cus-edit
  :defer t
  :config
  (setq custom-file lunaryorn-custom-file
        custom-buffer-done-kill nil            ; Kill when existing
        custom-buffer-verbose-help nil         ; Remove redundant help text
        ;; Show me the real variable name
        custom-unlispify-tag-names nil
        custom-unlispify-menu-entries nil)
  :init (load lunaryorn-custom-file 'no-error 'no-message))


;;; OS X support
(use-package ns-win                     ; OS X window support
  :defer t
  :if (eq system-type 'darwin)
  :config
  (setq ns-pop-up-frames nil            ; Don't pop up new frames from the
                                        ; workspace
        mac-option-modifier 'meta       ; Option is simply the natural Meta
        mac-command-modifier 'meta      ; But command is a lot easier to hit
        mac-right-command-modifier 'left
        mac-right-option-modifier 'none ; Keep right option for accented input
        ;; Just in case we ever need these keys
        mac-function-modifier 'hyper))

(use-package lunaryorn-osx              ; Personal OS X tools
  :if (eq system-type 'darwin)
  :load-path "lisp/"
  :defer t)

(use-package osx-trash                  ; Trash support for OS X
  :if (eq system-type 'darwin)
  :ensure t
  :init (osx-trash-setup))


;;; Fonts

;; We use these fonts:
;;
;; - Monoid (http://larsenwork.com/monoid/) as default
;; - XITS Math (https://github.com/khaledhosny/xits-math) as fallback for math
;;
;; Source Code Pro (https://github.com/adobe-fonts/source-code-pro) is a good
;; monospace font, too.  An alternative is Consolas.  Another great monospace
;; font is and Pragmata Pro (http://www.fsd.it/fonts/pragmatapro.htm,
;; proprietary, around 200$).
;;
;; Currently this setup only works for OS X, as we rely on Apple's Emoji and
;; Symbol fonts.
;;
;; TODO:  Find Emoji and symbol fonts for Linux and Windows

(set-face-attribute 'default nil
                    :family "Source Code Pro" :height 110)
(set-face-attribute 'variable-pitch nil
                    :family "Fira Sans" :height 120 :weight 'regular)

;; Font setup
(defun lunaryorn-configure-fonts (frame)
  "Set up fonts for FRAME.

Set the default font, and configure various overrides for
symbols, emojis, greek letters, as well as fall backs for."
  ;; Additional fonts for special characters and fallbacks
  ;; Test range: 🐷 ❤ ⊄ ∫ 𝛼 α 🜚 Ⓚ
  (dolist (script '(symbol mathematical))
    (set-fontset-font t script (font-spec :family "XITS Math")
                      frame 'prepend))

  ;; Define a font set stack for symbols, greek and math characters
  (dolist (script '(symbol greek mathematical))
    (set-fontset-font t script (font-spec :family "Arial Unicode MS")
                      frame 'prepend)
    (set-fontset-font t script (font-spec :family "Menlo")
                      frame 'prepend)
    (set-fontset-font t script (font-spec :family "DejaVu Sans Mono")
                      frame 'prepend))

  (when (eq system-type 'darwin)
    ;; Colored Emoji on OS X, prefer over everything else!
    (set-fontset-font t nil (font-spec :family "Apple Color Emoji")
                      frame 'prepend))

  ;; Fallbacks for math and generic symbols
  (set-fontset-font t nil (font-spec :family "Apple Symbols")
                    frame 'append))

(when-let (frame (selected-frame))
  (lunaryorn-configure-fonts frame))
(add-hook 'after-make-frame-functions #'lunaryorn-configure-fonts)

(use-package face-remap                 ; Face remapping
  :bind (("C-c w z" . text-scale-adjust)))


;;; User interface

;; Get rid of tool bar, menu bar and scroll bars.  On OS X we preserve the menu
;; bar, since the top menu bar is always visible anyway, and we'd just empty it
;; which is rather pointless.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (and (not (eq system-type 'darwin)) (fboundp 'menu-bar-mode))
  (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; No blinking and beeping, no startup screen, no scratch message and short
;; Yes/No questions.
(blink-cursor-mode -1)
(setq ring-bell-function #'ignore
      inhibit-startup-screen t
      initial-scratch-message "Hello there!\n")
(fset 'yes-or-no-p #'y-or-n-p)
;; Opt out from the startup message in the echo area by simply disabling this
;; ridiculously bizarre thing entirely.
(fset 'display-startup-echo-area-message #'ignore)

;; Insert my logo into scratch <3
(add-hook 'after-init-hook
          (lambda ()
            (with-current-buffer "*scratch*"
              (goto-char (point-min))
              (insert-image (create-image (locate-user-emacs-file "logo.png"))
                            "logo")
              (insert "\n"))))

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :init
  (progn
    (load-theme 'sanityinc-tomorrow-night :no-confirm)
    (setf frame-background-mode 'dark)
    (global-hl-line-mode 1)
    (custom-set-faces
     '(cursor               ((t :background "#eebb28")))
     '(diff-added           ((t :foreground "green" :underline nil)))
     '(diff-removed         ((t :foreground "red" :underline nil)))
     '(highlight            ((t :background "black" :underline nil)))
     '(magit-item-highlight ((t :background "black")))
     '(hl-line              ((t :background "gray10"))))))

(use-package solarized                  ; My colour theme
  :disabled t
  :ensure solarized-theme
  :config
  ;; Disable variable pitch fonts in Solarized theme
  (setq solarized-use-variable-pitch nil
        ;; Prefer italics over bold
        solarized-use-less-bold t
        solarized-use-more-italic t
        solarized-distinct-doc-face t ; Emphasize docstrings
        ;; I find different font sizes irritating.
        solarized-height-minus-1 1.0
        solarized-height-plus-1 1.0
        solarized-height-plus-2 1.0
        solarized-height-plus-3 1.0
        solarized-height-plus-4 1.0)

  (load-theme 'solarized-dark 'no-confirm))

(bind-key "C-c t v" #'variable-pitch-mode)

(use-package page-break-lines           ; Turn page breaks into lines
  :ensure t
  :init (global-page-break-lines-mode)
  :diminish page-break-lines-mode)

(use-package beacon                     ; Highlight cursor position in buffer
  :ensure t
  :init (beacon-mode 1)
  :diminish beacon-mode)

(use-package stripe-buffer              ; Add stripes to a buffer
  :disabled t
  :ensure t
  :init (add-hook 'dired-mode-hook #'stripe-buffer-mode))


;;; Keys and key bindings

;; Our key bindings are solely in the user space C-c <letter>.  The list of
;; which-key prefixes documents the meaning of specific key prefixes, such as
;; C-c f for file commands.  C-c m is special in that it always holds commands
;; that are only for the current major mode.  The mode-specific which-key
;; prefixes document the individual bindings for major modes under C-c m.
;;
;; We may also bind F5 to F9.  Since we can't document these in code, the
;; following list shows their abstract meanings:
;;
;; * F5: Compile
;; * F6: n/a
;; * F7: n/a
;; * F8: n/a
;; * F9: n/a
;;
;; All of these keys have default global bindings, but major and minor modes may
;; override them if there's a better command for the specific purpose available.
;;
;; Note that the notation for the function keys is <f5>, i.e. the lowercase name
;; surrounded by angle brackets.

(use-package which-key                  ; Show help popups for prefix keys
  :ensure t
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.4
        which-key-sort-order 'which-key-prefix-then-key-order
        ;; Let's go unicode :)
        which-key-key-replacement-alist
        '(("<\\([[:alnum:]-]+\\)>" . "\\1")
          ("up"                    . "↑")
          ("right"                 . "→")
          ("down"                  . "↓")
          ("left"                  . "←")
          ("DEL"                   . "⌫")
          ("deletechar"            . "⌦")
          ("RET"                   . "⏎"))
        which-key-description-replacement-alist
        '(("Prefix Command" . "prefix")
          ;; Lambdas
          ("\\`\\?\\?\\'"   . "λ")
          ;; Prettify hydra entry points
          ("/body\\'"       . "|=")
          ;; Drop/shorten package prefixes
          ("\\`lunaryorn-"  . "")
          ("projectile-"    . "proj-")
          ("helm-"          . "h-")
          ("magit-"         . "ma-")))

  (which-key-declare-prefixes
    ;; Prefixes for global prefixes and minor modes
    "C-c @" "outline"
    "C-c !" "flycheck"
    "C-c 8" "typo"
    "C-c 8 -" "typo/dashes"
    "C-c 8 <" "typo/left-brackets"
    "C-c 8 >" "typo/right-brackets"
    ;; Prefixes for my personal bindings
    "C-c a" "applications"
    "C-c b" "buffers"
    "C-c c" "compile-and-comments"
    "C-c e" "errors"
    "C-c f" "files"
    "C-c f v" "variables"
    "C-c g" "git"
    "C-c g g" "github/gist"
    "C-c h" "helm/help"
    "C-c i" "insert"
    "C-c i l" "licenses"
    "C-c j" "jump"
    "C-c l" "language/spelling"
    "C-c m" "major mode"
    "C-c o" "cursors"
    "C-c p" "projects"
    "C-c p s" "projects/search"
    "C-c p x" "projects/execute"
    "C-c p 4" "projects/other-window"
    "C-c s" "search"
    "C-c t" "toggle"
    "C-c w" "windows/frames"
    "C-c x" "text")

  ;; Prefixes for major modes
  (which-key-declare-prefixes-for-mode 'markdown-mode
    "C-c TAB" "markdown/images"
    "C-c C-a" "markdown/links"
    "C-c C-c" "markdown/process"
    "C-c C-s" "markdown/style"
    "C-c C-t" "markdown/header"
    "C-c C-x" "markdown/structure"
    "C-c m" "markdown/personal")

  (which-key-declare-prefixes-for-mode 'emacs-lisp-mode
    "C-c m" "elisp/personal"
    "C-c m e" "eval")

  (which-key-declare-prefixes-for-mode 'js2-mode
    "C-c m" "js/personal"
    "C-c m r" "refactor")

  (which-key-declare-prefixes-for-mode 'scala-mode
    "C-c C-b" "ensime/build"
    "C-c C-d" "ensime/debug"
    "C-c C-r" "ensime/refactor"
    "C-c C-v" "ensime/misc"
    "C-c m" "scala/personal"
    "C-c m b" "scala/build")

  (which-key-declare-prefixes-for-mode 'haskell-mode
    "C-c m" "haskell/personal"
    "C-c m i" "haskell/imports")

  (which-key-declare-prefixes-for-mode 'rust-mode
    "C-c C-c" "rust/cargo")

  (which-key-declare-prefixes-for-mode 'web-mode
    "C-c C-a" "web/attributes"
    "C-c C-b" "web/blocks"
    "C-c C-d" "web/dom"
    "C-c C-e" "web/element"
    "C-c C-t" "web/tags")

  :diminish which-key-mode)

(use-package hydra                      ; Bindings that stick
  :ensure t)

(use-package helm-descbinds             ; Describe key bindings with Helm
  :ensure t
  :bind ("C-h C-b" . describe-bindings)
  :init (helm-descbinds-mode))

(use-package help
  :config
  (defun describe-key-copy-as-kill (&optional key insert untranslated)
    (kill-new (format "%s" (key-binding key))))
  (advice-add 'describe-key-briefly :after #'describe-key-copy-as-kill))


;; Package manager and init file
(use-package paradox                    ; Better package menu
  :ensure t
  :bind (("C-c a p" . paradox-list-packages)
         ("C-c a P" . package-list-packages-no-fetch))
  :config
  (setq paradox-execute-asynchronously nil ; No async update, please
        paradox-spinner-type 'moon      ; Fancy spinner
        ;; Show all possible counts
        paradox-display-download-count t
        paradox-display-star-count t
        ;; Don't star automatically
        paradox-automatically-star nil
        ;; Hide download button, and wiki packages
        paradox-use-homepage-buttons nil ; Can type v instead
        paradox-hide-wiki-packages t))

(use-package bug-hunter                 ; Search init file for bugs
  :ensure t)


;;; The mode line
(line-number-mode)
(column-number-mode)

(use-package fancy-battery              ; Fancy battery info for mode line
  :ensure t
  :defer t
  :init (fancy-battery-mode))

(use-package anzu                       ; Position/matches count for isearch
  :ensure t
  :bind
  (([remap query-replace] . anzu-query-replace)
   ([remap query-replace-regexp] . anzu-query-replace-regexp)
   :map isearch-mode-map
   ([remap isearch-query-replace] . anzu-isearch-query-replace)
   ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :init (global-anzu-mode)
  :config (setq anzu-cons-mode-line-p nil)
  :diminish anzu-mode)

(use-package which-func                 ; Current function name
  :init (which-function-mode)
  :config
  (setq which-func-unknown "⊥" ; The default is really boring…
        which-func-format
        `((:propertize (" ➤ " which-func-current)
                       local-map ,which-func-keymap
                       face which-func
                       mouse-face mode-line-highlight
                       help-echo "mouse-1: go to beginning\n\
mouse-2: toggle rest visibility\n\
mouse-3: go to end"))))

(use-package spaceline-config           ; A beautiful mode line
  :ensure spaceline
  :config
  (spaceline-helm-mode)                 ; Enable a special Helm mode line

  (spaceline-compile
   'lunaryorn
   ;; Left side of the mode line (all the important stuff)
   '(((buffer-modified buffer-size input-method) :face highlight-face)
     anzu
     '(buffer-id remote-host buffer-encoding-abbrev)
     ((point-position line-column buffer-position selection-info)
      :separator " | ")
     major-mode
     process
     (flycheck-error flycheck-warning flycheck-info)
     (python-pyvenv :fallback python-pyenv)
     ((which-function projectile-root) :separator " @ ")
     ((minor-modes :separator spaceline-minor-modes-separator) :when active)
     nyan-cat)
   ;; Right segment (the unimportant stuff)
   '((version-control :when active)
     battery))

  ;; FIXME: create a new issue at https://github.com/lunaryorn/.emacs.d/
  ;; (setq-default mode-line-format '("%e" (:eval (spaceline-ml-lunaryorn))))
  )

;; Because you know you need it: http://youtu.be/QH2-TGUlwu4
;; Because you know you can't get enough: http://youtu.be/wZZ7oFKsKzY
(use-package nyan-mode                  ; NYAN CATS!!!
  :ensure t
  :init (nyan-mode))

(use-package powerline                  ; The work-horse of Spaceline
  :ensure t
  :after spaceline-config
  :config (setq powerline-height (truncate (* 1.0 (frame-char-height)))
                powerline-default-separator 'utf-8))


;;; Minibuffer and Helm
(setq history-length 1000               ; Store more history
      use-dialog-box nil                ; Never use dialogs for minibuffer input
      )

(use-package savehist                   ; Save minibuffer history
  :init (savehist-mode t)
  :config (setq savehist-save-minibuffer-history t
                savehist-autosave-interval 180))

(use-package helm                       ; Powerful minibuffer input framework
  :ensure t
  :bind (("A-C-o"   . helm-resume)
         ("C-x C-f" . helm-find-files)
         ("A-f"     . helm-find)
         ("C-h SPC" . helm-all-mark-rings))
  :init
  ;; FIXME: these are experimental convenience keybindings used by aculich that
  ;; may not be more generally useful nor wanted. Revisit these in the future.
  (bind-keys
   :map helm-map
   ("C-w" . backward-kill-word)
   ("M-w" . helm-yank-text-at-point)
   ("C-z" . helm-select-action)
   ("C-i" . helm-execute-persistent-action)
   ("C-j" . helm-maybe-exit-minibuffer)
   ("C-<return>" . helm-execute-persistent-action))

  (bind-keys
     :prefix-map help-helm-map
     :prefix "C-c h"
     ("a" . helm-apropos)
     ("b" . helm-buffers-list)
     ("c" . helm-colors)
     ("e" . helm-register)
     ("f" . helm-find-files)
     ("g" . helm-git-grep)
     ("i" . helm-semantic-or-imenu)
     ("k" . helm-man-woman)
     ("m" . helm-all-mark-rings)
     ("o" . helm-occur)
     ("O" . helm-multi-occur)
     ("p" . helm-list-emacs-process)
     ("r" . helm-regexp)
     ("l" . helm-resume)
     ("R" . helm-resume)
     ("t" . helm-top)
     ("y" . helm-show-kill-ring)
     ("/" . helm-find))
  (helm-mode 1)
  (with-eval-after-load 'helm-config
    (warn "`helm-config' loaded! Get rid of it ASAP!"))
  :config
  ;; Split inside selected window with Helm
  (setq helm-split-window-in-side-p t)

  ;; FIXME: still experimenting with the right combination of settings
  ;; here. Suggestions welcome.
  (setq
   helm-always-two-windows nil

   helm-truncate-lines t
   helm-full-frame nil
   helm-split-window-default-side 'left
   helm-reuse-last-window-split-state t
   helm-split-window-in-side-p nil
   helm-ff-file-name-history-use-recentf t
   helm-ff-search-library-in-sexp t
   ;; helm-buffers-fuzzy-matching t
   ;; helm-man-or-woman-function 'woman
   ;; helm-quick-update t

)

  :diminish helm-mode)

(use-package helm-command               ; Command execution with Helm
  :ensure helm
  :defer t
  :bind (([remap execute-extended-command] . helm-M-x)))


;;; Buffer, Windows and Frames
(setq frame-resize-pixelwise t          ; Resize by pixels
      frame-title-format
      '(:eval (if (buffer-file-name)
                  (abbreviate-file-name (buffer-file-name)) "%b"))
      ;; Size new windows proportionally wrt other windows
      window-combination-resize t)

(setq-default line-spacing 0.2)         ; A bit more spacing between lines

(defun lunaryorn-display-buffer-fullframe (buffer alist)
  "Display BUFFER in fullscreen.

ALIST is a `display-buffer' ALIST.

Return the new window for BUFFER."
  (let ((window (display-buffer-pop-up-window buffer alist)))
    (when window
      (delete-other-windows window))
    window))

;; Configure `display-buffer' behaviour for some special buffers.
(setq display-buffer-alist
      `(
        ;; Magit status window in fullscreen
        (,(rx "*magit: ")
         (lunaryorn-display-buffer-fullframe)
         (reusable-frames . nil))
        ;; Give Helm Help a non-side window because Helm as very peculiar ideas
        ;; about how to display its help
        (,(rx bos "*Helm Help" (* nonl) "*" eos)
         (display-buffer-use-some-window
          display-buffer-pop-up-window))
        ;; Nail Helm to the side window
        (,(rx bos "*" (* nonl) "helm" (* nonl) "*" eos)
         (display-buffer-in-side-window)
         (side . bottom)
         (window-height . 0.4)
         (window-width . 0.6))
        ;; Put REPLs and error lists into the bottom side window
        (,(rx bos
              (or "*Help"                 ; Help buffers
                  "*Warnings*"            ; Emacs warnings
                  "*Compile-Log*"         ; Emacs byte compiler log
                  "*compilation"          ; Compilation buffers
                  "*Flycheck errors*"     ; Flycheck error list
                  "*shell"                ; Shell window
                  "*sbt"                  ; SBT REPL and compilation buffer
                  "*ensime-update*"       ; Server update from Ensime
                  "*SQL"                  ; SQL REPL
                  "*Cargo"                ; Cargo process buffers
                  (and (1+ nonl) " output*") ; AUCTeX command output
                  ))
         (display-buffer-reuse-window
          display-buffer-in-side-window)
         (side            . bottom)
         (reusable-frames . visible)
         (window-height   . 0.33))
        ;; Let `display-buffer' reuse visible frames for all buffers.  This must
        ;; be the last entry in `display-buffer-alist', because it overrides any
        ;; later entry with more specific actions.
        ("." nil (reusable-frames . visible))))

(use-package frame                      ; Frames
  :bind (("C-c w F" . toggle-frame-fullscreen))
  :init (progn
          ;; Kill `suspend-frame'
          (global-set-key (kbd "C-z") nil)
          (global-set-key (kbd "C-x C-z") nil))
  ;; :config (add-to-list 'initial-frame-alist '(fullscreen . fullboth))
  )

(use-package focus-autosave-mode        ; Save buffers when focus is lost
  :ensure t
  :init (focus-autosave-mode)
  :diminish focus-autosave-mode)

(use-package helm-buffers               ; Manage buffers with Helm
  :ensure helm
  :defer t
  :bind (([remap switch-to-buffer] . helm-mini))
  :config (setq helm-buffers-fuzzy-matching t))

(use-package lunaryorn-buffers          ; Personal buffer tools
  :load-path "lisp/"
  :demand t                          ; Prevent a mysterious recursive load error
  :bind (("C-c b k" . lunaryorn-kill-this-buffer))
  :config
  (add-hook 'kill-buffer-query-functions
                  'lunaryorn-do-not-kill-important-buffers))

(use-package uniquify                   ; Make buffer names unique
  :config (setq uniquify-buffer-name-style
                ;; 'forward
                'post-forward-angle-brackets
                ))

(use-package ibuffer                    ; Better buffer list
  :bind (([remap list-buffers] . ibuffer)
         ("C-x b" . ibuffer))
  ;; Show VC Status in ibuffer
  :config
  (setq ibuffer-expert t)
  (setq ibuffer-formats
        '((mark modified read-only vc-status-mini " "
                (name 18 18 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                (vc-status 16 16 :left)
                " "
                filename-and-process)
          (mark modified read-only " "
                (name 18 18 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " " filename-and-process)
          (mark " " (name 16 -1) " " filename))))

(use-package ibuffer-vc                 ; Group buffers by VC project and status
  :ensure t
  :defer t
  :init (add-hook 'ibuffer-hook
                  (lambda ()
                    (ibuffer-vc-set-filter-groups-by-vc-root)
                    (unless (eq ibuffer-sorting-mode 'alphabetic)
                      (ibuffer-do-sort-by-alphabetic)))))

(use-package ibuffer-projectile         ; Group buffers by Projectile project
  :ensure t
  :disabled t
  :defer t
  ;; :commands (ibuffer-projectile-filter)
  ;; :init
  ;; (progn
  ;;   (defun ibuffer-projectile-filter (&optional arg)
  ;;     (ibuffer-projectile-set-filter-groups)
  ;;     (unless (eq ibuffer-sorting-mode 'alphabetic)
  ;;       (ibuffer-do-sort-by-alphabetic)))

  ;;   (add-hook 'ibuffer-hook 'ibuffer-projectile-filter))
  :init (add-hook 'ibuffer-hook #'ibuffer-projectile-set-filter-groups))

;; Standard window commands
(bind-key "C-c w =" #'balance-windows)
(bind-key "C-c w k" #'delete-window)
(bind-key "C-c w /" #'split-window-right)
(bind-key "C-c w -" #'split-window-below)
(bind-key "C-c w m" #'delete-other-windows)

(use-package lunaryorn-window           ; Personal window utilities
  :load-path "lisp/"
  :defer t
  :bind (("C-c w q" . lunaryorn-quit-all-side-windows)
         ("C-c w d" . lunaryorn-toggle-current-window-dedication)
         ("C-c w b" . lunaryorn-switch-to-minibuffer-window)))

(use-package windmove                   ; Move between windows with Shift+Arrow
  :bind (("C-c w <left>"  . windmove-left)
         ("C-c w <right>" . windmove-right)
         ("C-c w <up>"    . windmove-up)
         ("C-c w <down>"  . windmove-down))
  :config (windmove-default-keybindings 'shift))

(use-package winner                     ; Undo and redo window configurations
  :commands (winner-undo-redo)
  :bind (("M-N"     . winner-redo)
         ("C-'"     . winner-undo)
         ("M-P"     . winner-undo)
         ("C-c C-;" . winner-undo))
  :init
  (progn
    (winner-mode 1)
    (windmove-default-keybindings))
  :config
  (progn
    (defun winner-undo-redo (&optional arg)
      (interactive "P")
      (if arg (winner-redo)
        (winner-undo)))))

(use-package ace-window                 ; Fast window switching
  :ensure t
  :bind (("C-x o" . ace-window)
         ("C-c w w" . ace-window)))

(use-package golden-ratio               ; Automatically resize windows
  :ensure t
  :init
  (defun lunaryorn-toggle-golden-ratio ()
    (interactive)
    (if (bound-and-true-p golden-ratio-mode)
        (progn
          (golden-ratio-mode -1)
          (balance-windows))
      (golden-ratio-mode)
      (golden-ratio)))
  :bind (("C-c t g" . lunaryorn-toggle-golden-ratio))
  :config
  (setq golden-ratio-extra-commands '(windmove-up
                                      windmove-down
                                      windmove-left
                                      windmove-right
                                      ace-window
                                      ace-delete-window
                                      ace-select-window
                                      ace-swap-window
                                      ace-maximize-window)
        ;; Exclude a couple of special modes from golden ratio, namely
        ;; Flycheck's error list, calc
        golden-ratio-exclude-modes '(flycheck-error-list-mode
                                     calc-mode
                                     dired-mode
                                     ediff-mode
                                     )
        ;; Exclude a couple of special buffers from golden ratio, namely Helm,
        ;; WhichKey, NeoTree, etc.
        golden-ratio-exclude-buffer-regexp
        `(,(rx bos "*" (any "h" "H") "elm*" eos)
          ,(rx bos "*which-key*" eos)
          ,(rx bos "*NeoTree*" eos)))
  :diminish (golden-ratio-mode . "ⓖ"))

(use-package ediff-wind                 ; Ediff window management
  :defer t
  :config
  ;; Prevent Ediff from spamming the frame
  (setq ediff-window-setup-function #'ediff-setup-windows-plain
        ediff-split-window-function #'split-window-horizontally))

(use-package desktop                    ; Save buffers, windows and frames
  :disabled t
  :init (desktop-save-mode)
  :config
  ;; Save desktops a minute after Emacs was idle.
  (setq desktop-auto-save-timeout 60)

  ;; Don't save Magit and Git related buffers
  (dolist (mode '(magit-mode magit-log-mode))
    (add-to-list 'desktop-modes-not-to-save mode))
  (add-to-list 'desktop-files-not-to-save (rx bos "COMMIT_EDITMSG")))

(use-package writeroom-mode             ; Distraction-free editing
  :ensure t
  :bind (("C-c t r" . writeroom-mode)))

(use-package popup                      ; Popup menus
  ;; We don't ensure this package, because we definitely don't want to have this
  ;; mess, but unfortunately it's a dependency of Ensime :(
  :ensure nil
  :defer t
  :config
  ;; Bring Popup bindings in line with Company bindings, by getting rid of C-n/p
  ;; for navigation and introducing M-n/p
  (define-key popup-menu-keymap "\C-n" nil)
  (define-key popup-menu-keymap [down] nil)
  (define-key popup-menu-keymap "\C-p" nil)
  (define-key popup-menu-keymap [up] nil)
  (define-key popup-menu-keymap (kbd "M-n") #'popup-next)
  (define-key popup-menu-keymap (kbd "M-p") #'popup-previous))


;;; File handling

;; Keep backup and auto save files out of the way
(setq backup-directory-alist `((".*" . ,(locate-user-emacs-file ".backup")))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Delete files to trash
(setq delete-by-moving-to-trash t)

(use-package files                      ; Core commands for files
  :bind (("C-c f z" . revert-buffer)
         ("C-c f /" . revert-buffer))
  :config
  ;; Use GNU ls for Emacs
  (when-let (gnu-ls (and (eq system-type 'darwin) (executable-find "gls")))
    (setq insert-directory-program gnu-ls)))

(use-package helm-files                 ; Manage files with Helm
  :ensure helm
  :defer t
  :bind (([remap find-file] . helm-find-files)
         ("C-c f f" . helm-for-files)
         ("C-c f r" . helm-recentf))
  :config
  (setq helm-recentf-fuzzy-match t
        ;; Use recentf to manage file name history
        helm-ff-file-name-history-use-recentf t
        ;; Find libraries from `require', etc.
        helm-ff-search-library-in-sexp t)

  (when (eq system-type 'darwin)
    ;; Replace locate with spotlight for `helm-for-files'
    (setq helm-for-files-preferred-list
          (append (delq 'helm-source-locate
                        helm-for-files-preferred-list)
                  '(helm-source-mac-spotlight)))))

(use-package ffap                       ; Find files at point
  ;; Please stop pinging random hosts!  See
  ;; https://github.com/technomancy/emacs-starter-kit/issues/39
  :config (setq ffap-machine-p-known 'reject))

(use-package server                     ; The server of `emacsclient'
  :init (server-mode)
  :diminish (server-buffer-clients . " ⓒ"))

(use-package dired                      ; Edit directories
  :defer t
  :config
  (setq dired-auto-revert-buffer t    ; Revert on re-visiting
        ;; Better dired flags: `-l' is mandatory, `-a' shows all files, `-h'
        ;; uses human-readable sizes, and `-F' appends file-type classifiers
        ;; to file names (for better highlighting)
        dired-listing-switches "-alhF"
        dired-ls-F-marks-symlinks t   ; -F marks links with @
        ;; Inhibit prompts for simple recursive operations
        dired-recursive-copies 'always
        ;; Auto-copy to other Dired split window
        dired-dwim-target t)

  (when (or (memq system-type '(gnu gnu/linux))
            (string= (file-name-nondirectory insert-directory-program) "gls"))
    ;; If we are on a GNU system or have GNU ls, add some more `ls' switches:
    ;; `--group-directories-first' lists directories before files, and `-v'
    ;; sorts numbers in file names naturally, i.e. "image1" goes before
    ;; "image02"
    (setq dired-listing-switches
          (concat dired-listing-switches " --group-directories-first -v"))))

(use-package dired-x                    ; Additional tools for Dired
  :defer nil
  :commands dired-kill-buffer-jump
  :bind (("C-c f j" . dired-jump)
         ("C-x C-j" . dired-jump)
         ("C-x C-j" . dired-jump)
         ("C-A-j"       . dired-jump)
         ("C-A-x C-A-k" . dired-jump-kill-buffer)
         ("C-A-k"       . dired-jump-kill-buffer)
         ("C-x C-j"     . dired-jump))
  :init
  (add-hook 'dired-mode-hook #'dired-omit-mode)
  :after dired
  :config
  (setq dired-omit-verbose nil)        ; Shut up, dired

  (defun dired-jump-kill-buffer (&rest)
    (interactive)
    (let ((buf (current-buffer)))
      (dired-jump)
      (kill-buffer buf)))

  (when (eq system-type 'darwin)
    ;; OS X bsdtar is mostly compatible with GNU Tar
    (setq dired-guess-shell-gnutar "tar"))

  ;; Diminish dired-omit-mode. We need this hack, because Dired Omit Mode has
  ;; a very peculiar way of registering its lighter explicitly in
  ;; `dired-omit-startup'.  We can't just use `:diminish' because the lighter
  ;; isn't there yet after dired-omit-mode is loaded.
  (add-function :after (symbol-function 'dired-omit-startup)
                (lambda () (diminish 'dired-omit-mode " ⓞ"))
                '((name . dired-omit-mode-diminish))))

(use-package neotree
  :ensure t
  :bind (("C-c f t" . neotree-toggle))
  :config (setq neo-window-width 32
                neo-create-file-auto-open t
                neo-banner-message nil
                neo-show-updir-line nil
                neo-mode-line-type 'neotree
                neo-smart-open t
                neo-dont-be-alone t
                neo-persist-show nil
                neo-show-hidden-files t
                neo-auto-indent-point t))

(use-package ignoramus                  ; Ignore uninteresting files everywhere
  :ensure t
  :config
  ;; Ignore some additional directories and file extensions
  (dolist (name '(".cask"
                  ".vagrant"
                  ".ensime_cache" ".ensime"
                  ".stack-work"))
    ;; Ignore some additional directories
    (add-to-list 'ignoramus-file-basename-exact-names name))

  (dolist (ext '(".fls" ".out" ; LaTeX
                 ))
    (add-to-list 'ignoramus-file-endings ext))

  (ignoramus-setup))

(use-package hardhat                    ; Protect user-writable files
  :ensure t
  :init (global-hardhat-mode)
  :config (setq hardhat-mode-lighter " Ⓗ"))

(use-package bookmark                   ; Bookmarks for Emacs buffers
  :bind (("C-c f b" . list-bookmarks))
  ;; Save bookmarks immediately after a bookmark was added
  :config (setq bookmark-save-flag 1
                bookmark-default-file (expand-file-name "bookmarks" user-cache-directory)))

(use-package recentf                    ; Save recently visited files
  :init (recentf-mode)
  :config
  (setq recentf-max-saved-items 200
        recentf-max-menu-items 15
        ;; Cleanup recent files only when Emacs is idle, but not when the mode
        ;; is enabled, because that unnecessarily slows down Emacs. My Emacs
        ;; idles often enough to have the recent files list clean up regularly
        recentf-auto-cleanup 'never ;; disable before we start recentf! If using Tramp a lot.
        recentf-exclude (list "/\\.git/.*\\'" ; Git contents
                              "/elpa/.*\\'" ; Package files
                              "/itsalltext/" ; It's all text temp files
                              ;; And all other kinds of boring files
                              #'ignoramus-boring-p)))

(use-package saveplace                  ; Save point position in files
  :init (save-place-mode 1)
  :config
  (progn
    (setq-default save-place t)
    (setq-default save-place-file (expand-file-name "places" user-cache-directory))))

;;;; disable because of weird interaction with other modes
;; (setq view-read-only t)                 ; View read-only files

(use-package autorevert                 ; Auto-revert buffers of changed files
  :init (global-auto-revert-mode)
  :config
  (setq auto-revert-verbose nil         ; Shut up, please!
        ;; Revert Dired buffers, too
        global-auto-revert-non-file-buffers t)

  (when (eq system-type 'darwin)
    ;; File notifications aren't supported on OS X
    (setq auto-revert-use-notify nil))
  :diminish (auto-revert-mode . " Ⓐ"))

(use-package image-file                 ; Visit images as images
  :init (auto-image-file-mode))

(use-package launch                     ; Open files in external programs
  :ensure t
  :defer t)

(use-package sudo-edit                  ; Edit files as root, through Tramp
  :ensure t
  :defer t
  :bind (("C-c f s" . sudo-edit)
         ("C-c f S" . sudo-edit-current-file)))

(use-package reveal-in-osx-finder       ; Reveal current buffer in finder
  :ensure t
  ;; Bind analogous to `dired-jump' at C-c f j
  :bind (("C-c f J" . reveal-in-osx-finder)))

(use-package lunaryorn-files            ; Personal file tools
  :load-path "lisp/"
  :commands (lunaryorn-recompile-packages)
  :bind (("C-c f D" . lunaryorn-delete-file-and-buffer)
         ("C-c f i" . lunaryorn-open-in-intellij)
         ("C-c f o" . lunaryorn-launch-dwim)
         ("C-c f R" . lunaryorn-rename-file-and-buffer)
         ("C-c f w" . lunaryorn-copy-filename-as-kill)
         ("C-c f u" . lunaryorn-find-user-init-file-other-window)
         ("C-c f ." . lunaryorn-browse-feature-url)))

;; Additional bindings for built-ins
(bind-key "C-c f v d" #'add-dir-local-variable)
(bind-key "C-c f v l" #'add-file-local-variable)
(bind-key "C-c f v p" #'add-file-local-variable-prop-line)


;;; Navigation and scrolling
(setq scroll-conservatively 1000        ; Never recenter the screen while scrolling
      scroll-error-top-bottom t         ; Move to beg/end of buffer before
                                        ; signalling an error
      ;; These settings make trackpad scrolling on OS X much more predictable
      ;; and smooth
      mouse-wheel-progressive-speed nil
      mouse-wheel-scroll-amount '(1))

(use-package page                       ; Page navigation
  :bind (("C-x ]" . lunaryorn-pages/forward-page)
         ("C-x [" . lunaryorn-pages/backward-page))
  :init
  (defhydra lunaryorn-pages ()
    "Pages"
    ("[" backward-page "backward")
    ("]" forward-page "forward")))

(use-package avy-jump                   ; Jump to characters in buffers
  :ensure avy
  :bind (("C-c j w" . avy-goto-word-1)
         ("C-c j l" . avy-goto-line)
         ("C-c j b" . avy-pop-mark)
         ("C-c j j" . avy-goto-char-2)))

(use-package helm-imenu                 ; Jump to tags with Helm
  :ensure helm
  :defer t
  :bind (("C-c j t" . helm-imenu)
         ("A-i" . helm-imenu))
  :config
  (setq helm-imenu-fuzzy-match t
        ;; Don't automatically jump to imenu candidate if only one match,
        ;; because it makes the behaviour of this command unpredictable, and
        ;; prevents me from getting an overview over the buffer if point is on a
        ;; matching symbol.
        helm-imenu-execute-action-at-once-if-one nil))

(use-package helm-semantic
  :ensure helm
  :defer t
  :bind (("A-i" . helm-semantic-or-imenu)))

(use-package ace-link                   ; Fast link jumping
  :ensure t
  :defer t
  :bind (:map Info-mode-map ("C-c m l" . ace-link-info)
         :map help-mode-map ("C-c m l" . ace-link-help)))

(use-package outline                    ; Navigate outlines in buffers
  :defer t
  :init (dolist (hook '(text-mode-hook prog-mode-hook))
          (add-hook hook #'outline-minor-mode))
  :diminish outline-minor-mode)

(use-package nlinum                     ; Line numbers in display margin
  :ensure t
  :bind (("C-c t l" . nlinum-mode)))


;;; Search
(use-package "isearch"                  ; Search buffers
  ;; Defer because `isearch' is not a feature and we don't want to `require' it
  :defer t
  :init
  ;; `:diminish' doesn't work for isearch, because it uses eval-after-load on
  ;; the feature name, but isearch.el does not provide any feature.  For the
  ;; same reason we have to use `:init', but isearch is always loaded anyways.
  (diminish 'isearch-mode)

  ;; Please, isearch, let me scroll during search
  (setq isearch-allow-scroll t))

(use-package visual-regexp              ; Regexp replace with in-buffer display
  :ensure t
  :bind (("C-c s r" . vr/query-replace)
         ("C-c s R" . vr/replace)))

(use-package helm-regexp                ; Regexp search with Helm
  :ensure helm
  :defer t
  :bind (([remap occur] . helm-occur)))


(use-package swoop
  :bind ("A-o" . swoop))

(use-package helm-swoop                 ; Powerful buffer search for Emacs
  :ensure t
  :bind  (("C-c s s"   . helm-swoop)
          ("C-c s S"   . helm-multi-swoop)
          ("C-c s C-s" . helm-multi-swoop-all)
          ("A-O"       . helm-swoop-back-to-last-point)
          ([remap swoop] . helm-swoop))
  :init
  (bind-keys
   :map isearch-mode-map
   ("<tab>" . helm-swoop-from-isearch)
   ("C-i"   . helm-swoop-from-isearch)
   ("A-o"   . helm-swoop-from-isearch))

  (use-package help-mode
      :defer t
      :config
      (bind-keys
       :map help-mode-map
       ("s" . isearch-forward)
       ("s" . helm-swoop-from-isearch)
       ("o" . helm-swoop-from-isearch)))
  :config
  (bind-keys
   :map helm-swoop-map
   ("C-w" . helm-yank-text-at-point)
   ("C-r" . helm-previous-line)
   ("A-r" . helm-previous-line)
   ("C-s" . helm-next-line)
   ("A-s" . helm-next-line)
   ("A-g" . helm-confirm-and-exit-minibuffer)
   ("C-e" . helm-maybe-exit-minibuffer)
   ("C-a" . helm-maybe-exit-minibuffer)
   ("A-j" . helm-maybe-exit-minibuffer)
   ("C-;" . helm-toggle-resplit-window)
   ("A-;" . helm-maybe-exit-minibuffer)
   ("A-o" . previous-history-element)
   ("A-p" . previous-history-element)
   ("A-n" . next-history-element))
  (setq helm-swoop-speed-or-color t     ; Colour over speed 8)
        ;; Split window like Helm does
        helm-swoop-split-window-function #'helm-default-display-buffer))

(use-package grep                       ; Control grep from Emacs
  :defer t
  :config
  (when-let (gnu-find (and (eq system-type 'darwin)
                           (executable-find "gfind")))
    (setq find-program gnu-find))

  (when-let (gnu-xargs (and (eq system-type 'darwin)
                            (executable-find "gxargs")))
    (setq xargs-program gnu-xargs)))

(use-package locate                     ; Search files on the system
  :defer t
  :config
  ;; Use mdfind as locate substitute on OS X, to utilize the Spotlight database
  (when-let (mdfind (and (eq system-type 'darwin) (executable-find "mdfind")))
    (setq locate-command mdfind)))

(use-package helm-ag                    ; Helm frontend for Ag
  :ensure t
  :bind (("C-c s a" . helm-ag)
         ("C-c s A" . helm-do-ag))
  :config
  (setq helm-ag-fuzzy-match t                   ; Fuzzy matching
        helm-ag-insert-at-point 'symbol         ; Default to symbol at point
        helm-ag-edit-save t                     ; save buffers after editing
        ))


;;; Rings and registers
(setq kill-ring-max 200                 ; More killed items
      kill-do-not-save-duplicates t     ; No duplicates in kill ring
      ;; Save the contents of the clipboard to kill ring before killing
      save-interprogram-paste-before-kill t)

(use-package helm-ring                  ; Browse rings and registers with Helm
  :ensure helm
  :defer t
  :bind (([remap yank-pop]        . helm-show-kill-ring)
         ([remap insert-register] . helm-register)))

(use-package easy-kill                  ; Easy killing and marking on C-w
  :ensure t
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp]      . easy-mark))
  :config
  (defun easy-kill-digit-argument-x (&optional x)
    (interactive "p")
    (easy-kill-digit-argument 1))

  (defun easy-kill-set-next-key (&rest args)
    (bind-keys
     :map easy-kill-base-map
     ("C-M-SPC" . easy-kill-digit-argument-x)))

  (advice-add #'easy-mark :before #'easy-kill-set-next-key)
  (advice-add #'easy-mark-sexp :before #'easy-kill-set-next-key)

  (defun easy-kill-expand-on-scan-error (orig-fun &rest args)
    (condition-case nil
        (apply orig-fun args)
      (scan-error
       (bind-keys
        :map easy-kill-base-map
        ("C-M-SPC" . easy-kill-expand))
       (easy-kill-expand))))
  (advice-add #'easy-kill-thing :around #'easy-kill-expand-on-scan-error)
  (advice-add #'easy-kill-thing-forward-1 :around #'easy-kill-expand-on-scan-error))


;;; Basic editing
;; Disable tabs, but given them proper width
(setq-default indent-tabs-mode nil
              tab-width 8)
;; Make Tab complete if the line is indented
(setq tab-always-indent 'complete)

;; Indicate empty lines at the end of a buffer in the fringe, but require a
;; final new line
(setq indicate-empty-lines t
      require-final-newline t)

;; Configure a reasonable fill column, indicate it in the buffer and enable
;; automatic filling
(setq-default fill-column 80)
(add-hook 'text-mode-hook #'auto-fill-mode)
(diminish 'auto-fill-function " Ⓕ")

(bind-key "C-c x i" #'indent-region)

(use-package simple
  :defer t
  :bind (("M-g n" . lunaryorn-errors/next-error)
         ("M-g p" . lunaryorn-errors/previous-error))
  :init
  (defhydra lunaryorn-errors ()
    "Errors."
    ("n" next-error "next")
    ("p" previous-error "previous")
    ("f" first-error "first")))

(use-package lunaryorn-simple           ; Personal editing helpers
  :load-path "lisp/"
  :bind (([remap kill-whole-line]        . lunaryorn-smart-kill-whole-line)
         ([remap move-beginning-of-line] . lunaryorn-back-to-indentation-or-beginning-of-line)
         ("C-<backspace>"                . lunaryorn-smart-backward-kill-line)
         ("C-S-j"                        . lunaryorn-smart-open-line)
         ("C-<return>"                   . lunaryorn-smart-open-line)
         ;; Additional utilities
         ("C-c i d"                      . lunaryorn-insert-current-date)
         ("C-c i l a"                    . lunaryorn-insert-apache2)
         ("C-c i l m"                    . lunaryorn-insert-mit/x11))
  :commands (lunaryorn-auto-fill-comments-mode)
  ;; Auto-fill comments in programming modes
  :init (add-hook 'prog-mode-hook #'lunaryorn-auto-fill-comments-mode))

(use-package delsel                     ; Delete the selection instead of insert
  :defer t
  :init (delete-selection-mode))

(use-package newcomment                 ; Built-in comment features
  :bind (("C-c c d" . comment-dwim)
         ("C-c c l" . comment-line)
         ("C-c c r" . comment-region)))

(use-package whitespace-cleanup-mode    ; Cleanup whitespace in buffers
  :ensure t
  :bind (("C-c t c" . whitespace-cleanup-mode)
         ("C-c x w" . whitespace-cleanup))
  :init (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
          (add-hook hook #'whitespace-cleanup-mode))
  :diminish (whitespace-cleanup-mode . " Ⓦ"))

(use-package subword                    ; Subword/superword editing
  :defer t
  :diminish subword-mode)

(use-package adaptive-wrap              ; Choose wrap prefix automatically
  :ensure t
  :defer t
  :init (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode))

(use-package visual-fill-column         ; Fill column wrapping for Visual Line Mode
  :ensure t
  :defer t
  :init (add-hook 'visual-line-mode-hook #'visual-fill-column-mode))

(use-package zop-to-char                ; Better zapping
  :ensure t
  :bind (("M-z" . zop-to-char)
         ("M-Z" . zop-up-to-char)))

(use-package align                      ; Align text in buffers
  :bind (("C-c x a a" . align)
         ("C-c x a c" . align-current)))

(use-package lunaryorn-align
  :load-path "lisp/"
  :bind (("C-c x a r" . lunaryorn-align-repeat)
         ("C-c x a m" . lunaryorn-align-repeat-math-oper)
         ("C-c x a ." . lunaryorn-align-repeat-decimal)
         ("C-c x a ," . lunaryorn-align-repeat-comma)
         ("C-c x a ;" . lunaryorn-align-repeat-semicolon)
         ("C-c x a :" . lunaryorn-align-repeat-colon)
         ("C-c x a =" . lunaryorn-align-repeat-equal)
         ("C-c x a &" . lunaryorn-align-repeat-ampersand)
         ("C-c x a |" . lunaryorn-align-repeat-bar)
         ("C-c x a (" . lunaryorn-align-repeat-left-paren)
         ("C-c x a )" . lunaryorn-align-repeat-right-paren)))

(use-package multiple-cursors           ; Edit text with multiple cursors
  :ensure t
  :bind (("C-c o <SPC>" . mc/vertical-align-with-space)
         ("C-c o a"     . mc/vertical-align)
         ("C-c o e"     . mc/mark-more-like-this-extended)
         ("C-c o h"     . mc/mark-all-like-this-dwim)
         ("C-c o l"     . mc/edit-lines)
         ("C-c o n"     . mc/mark-next-like-this)
         ("C-c o p"     . mc/mark-previous-like-this)
         ("C-c o r"     . vr/mc-mark)
         ("C-c o C-a"   . mc/edit-beginnings-of-lines)
         ("C-c o C-e"   . mc/edit-ends-of-lines)
         ("C-c o C-s"   . mc/mark-all-in-region)
         ("C-c e"       . mc/edit-lines)
         ("C-<"         . mc/mark-previous-like-this)
         ("C->"         . mc/mark-next-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-c C-," . mc/mark-all-like-this))
  :config
  (setq mc/mode-line
        ;; Simplify the MC mode line indicator
        '(:propertize (:eval (concat " " (number-to-string (mc/num-cursors))))
                      face font-lock-warning-face)))

(use-package expand-region              ; Expand region by semantic units
  :disabled t                           ; prefer easy-kill instead
  :ensure t
  :bind (("C-c v"           . er/expand-region)
         ("C-="             . er/expand-region)
         ([remap mark-sexp] . er/expand-region)
         ;; ("M-C-j"           . er/expand-region)
         ("M-C-;"           . er/expand-region)
         ("A-C-;"           . er/expand-region)
         ("A-C-h"           . er/expand-region))
  :init
  (defun expand-region-on-scan-error (orig-fun &rest args)
    "On error of mark-sexp, switch to er/expand-region."
    (condition-case err
        (apply orig-fun args)
      (scan-error
       (apply 'er/expand-region (list (or (car args) 1))))))
  (advice-add #'mark-sexp :around #'expand-region-on-scan-error))

(use-package undo-tree                  ; Branching undo
  :disabled t
  :ensure t
  :init (global-undo-tree-mode)
  :diminish undo-tree-mode)

;; Give us narrowing back!
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; Same for region casing
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Additional keybindings
(bind-key [remap just-one-space] #'cycle-spacing)


;;; Internationalisation
(prefer-coding-system 'utf-8)

(use-package mule-cmds                  ; Input methods
  :defer t
  :bind (("C-c t i" . toggle-input-method))
  :config
  (setq default-input-method "german-postfix"))

(use-package helm-unicode               ; Unicode input with Helm
  :ensure t
  :bind ("C-c i 8" . helm-unicode))


;;; Paired delimiters
;; (use-package smartparens
;;   :diminish smartparens-mode
;;   :init
;;   (progn
;;     (setq sp-base-key-bindings 'smartparens)
;;     (setq sp-autoskip-closing-pair 'always)
;;     (setq sp-hybrid-kill-entire-symbol t)
;;     (sp-use-paredit-bindings)
;;     (use-package smartparens-config)
;;     (use-package smartparens-ruby)
;;     (use-package smartparens-html)
;;     (smartparens-global-mode 1)
;;     (show-smartparens-global-mode 1))
;;   :config
;;   (progn
;;     (setq smartparens-strict-mode t)
;;     (setq sp-autoescape-string-quote nil)
;;     (setq sp-autoinsert-if-followed-by-word t)
;;     (sp-local-pair 'emacs-lisp-mode "`" nil :when '(sp-in-string-p)))
;;   :bind
;;   (("C-M-k" . sp-kill-sexp-with-a-twist-of-lime)
;;    ("M-s" . sp-splice-sexp)
;;    ("M-r" . sp-splice-sexp-killing-around)
;;    ("C-)" . sp-forward-slurp-sexp)
;;    ("C-}" . sp-forward-barf-sexp)
;;    ("C-(" . sp-backward-slurp-sexp)
;;    ("C-{" . sp-backward-barf-sexp)
;;    ("M-S" . sp-split-sexp)
;;    ("M-J" . sp-join-sexp)
;;    ("C-M-t" . sp-transpose-sexp)))

(use-package smartparens                ; Parenthesis editing and balancing
  :ensure t
  :bind (("C-c k" . lunaryorn-smartparens/body)
         :map smartparens-strict-mode-map
         ;; A fill paragraph in strict mode
         ("M-q" . sp-indent-defun))
  :init
  ;; Hydra for Smartparens
  (defhydra lunaryorn-smartparens (:hint nil)
    "
Sexps (quit with _q_)

^Nav^            ^Barf/Slurp^                 ^Depth^
^---^------------^----------^-----------------^-----^-----------------
_f_: forward     _→_:          slurp forward   _R_: splice
_b_: backward    _←_:          barf forward    _r_: raise
_u_: backward ↑  _C-<right>_:  slurp backward  _↑_: raise backward
_d_: forward ↓   _C-<left>_:   barf backward   _↓_: raise forward
_p_: backward ↓
_n_: forward ↑

^Kill^           ^Misc^                       ^Wrap^
^----^-----------^----^-----------------------^----^------------------
_w_: copy        _j_: join                    _(_: wrap with ( )
_k_: kill        _s_: split                   _{_: wrap with { }
^^               _t_: transpose               _'_: wrap with ' '
^^               _c_: convolute               _\"_: wrap with \" \"
^^               _i_: indent defun"
    ("q" nil)
    ;; Wrapping
    ("(" (lambda (_) (interactive "P") (sp-wrap-with-pair "(")))
    ("{" (lambda (_) (interactive "P") (sp-wrap-with-pair "{")))
    ("'" (lambda (_) (interactive "P") (sp-wrap-with-pair "'")))
    ("\"" (lambda (_) (interactive "P") (sp-wrap-with-pair "\"")))
    ;; Navigation
    ("f" sp-forward-sexp )
    ("b" sp-backward-sexp)
    ("u" sp-backward-up-sexp)
    ("d" sp-down-sexp)
    ("p" sp-backward-down-sexp)
    ("n" sp-up-sexp)
    ;; Kill/copy
    ("w" sp-copy-sexp)
    ("k" sp-kill-sexp)
    ;; Misc
    ("t" sp-transpose-sexp)
    ("j" sp-join-sexp)
    ("s" sp-split-sexp)
    ("c" sp-convolute-sexp)
    ("i" sp-indent-defun)
    ;; Depth changing
    ("R" sp-splice-sexp)
    ("r" sp-splice-sexp-killing-around)
    ("<up>" sp-splice-sexp-killing-backward)
    ("<down>" sp-splice-sexp-killing-forward)
    ;; Barfing/slurping
    ("<right>" sp-forward-slurp-sexp)
    ("<left>" sp-forward-barf-sexp)
    ("C-<left>" sp-backward-barf-sexp)
    ("C-<right>" sp-backward-slurp-sexp))

  ;; TODO: Define hydra for smartparens!
  (smartparens-global-mode)
  (show-smartparens-global-mode)

  (dolist (hook '(inferior-emacs-lisp-mode-hook
                  emacs-lisp-mode-hook))
    (add-hook hook #'smartparens-strict-mode))
  :config
  (require 'smartparens-config)

  (setq sp-autoskip-closing-pair 'always
        ;; Don't kill entire symbol on C-k
        sp-hybrid-kill-entire-symbol nil)
  :diminish (smartparens-mode . " ⓟ"))

(use-package lunaryorn-smartparens      ; Personal Smartparens extensions
  :disabled t
  :load-path "lisp/"
  :after smartparens
  :config (lunaryorn-smartparens-bind-keys))

(use-package embrace                    ; Wrap semantic units with pairs
  :ensure t
  :bind (("C-c y" . lunaryorn-embrace/body)
         ("C-c x e" . lunaryorn-embrace/body))
  :init
  (defhydra lunaryorn-embrace (:hint nil)
    "
Add (_a_), change (_c_) or delete (_d_) a pair.  Quit with _q_.
"
    ("a" embrace-add)
    ("c" embrace-change)
    ("d" embrace-delete)
    ("q" nil)))


;;; Highlights and fontification
(defun lunaryorn-whitespace-style-no-long-lines ()
  "Configure `whitespace-mode' for Org.

Disable the highlighting of overlong lines."
  (setq-local whitespace-style (-difference whitespace-style
                                            '(lines lines-tail))))

(defun lunaryorn-whitespace-mode-local ()
  "Enable `whitespace-mode' after local variables where set up."
  (add-hook 'hack-local-variables-hook #'whitespace-mode nil 'local))

(use-package whitespace                 ; Highlight bad whitespace
  :bind (("C-c t w" . whitespace-mode))
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
    (add-hook hook #'lunaryorn-whitespace-mode-local))
  :config
  ;; Highlight tabs, empty lines at beg/end, trailing whitespaces and overlong
  ;; portions of lines via faces.  Also indicate tabs via characters
  (setq whitespace-style '(face indentation space-after-tab space-before-tab
                                tab-mark empty trailing lines-tail)
        whitespace-line-column nil)     ; Use `fill-column' for overlong lines
  :diminish (whitespace-mode . " ⓦ"))

(use-package hl-line                    ; Highlight the current line
  :init (global-hl-line-mode 1))

(use-package rainbow-delimiters         ; Highlight delimiters by depth
  :ensure t
  :defer t
  :init
  (dolist (hook '(text-mode-hook prog-mode-hook))
    (add-hook hook #'rainbow-delimiters-mode)))

(use-package hi-lock                    ; Custom regexp highlights
  :diminish hi-lock-mode
  :init
  (setq hi-lock-file-patterns-policy t)
  (global-hi-lock-mode 1)
  :config
  (bind-keys
   :map hi-lock-map
   ("C-z C-h" . highlight-lines-matching-regexp)
   ("C-z i"   . hi-lock-find-patterns)
   ("C-z h"   . highlight-regexp)
   ("C-z p"   . highlight-phrase)
   ("C-z r"   . unhighlight-regexp)
   ("C-z b"   . hi-lock-write-interactive-patterns)))


;; (use-package idle-highlight-mode
;;   :diminish idle-highlight-mode)

(use-package highlight-numbers          ; Fontify number literals
  :ensure t
  :defer t
  :init (add-hook 'prog-mode-hook #'highlight-numbers-mode))

(use-package rainbow-mode               ; Fontify color values in code
  :ensure t
  :bind (("C-c t r" . rainbow-mode))
  :config (add-hook 'css-mode-hook #'rainbow-mode))

(use-package highlight-symbol           ; Highlighting and commands for symbols
  :ensure t
  :defer t
  :bind
  (("C-c s %" . highlight-symbol-query-replace)
   ("C-c s n" . highlight-symbol-next-in-defun)
   ("C-c s p" . highlight-symbol-prev-in-defun))
  ;; Navigate occurrences of the symbol under point with M-n and M-p, and
  ;; highlight symbol occurrences
  :init
  (dolist (fn '(highlight-symbol-nav-mode highlight-symbol-mode))
    (add-hook 'prog-mode-hook fn))
  :config
  (setq highlight-symbol-idle-delay 0.4     ; Highlight almost immediately
        highlight-symbol-on-navigation-p t) ; Highlight immediately after
                                        ; navigation
  :diminish highlight-symbol-mode)

(use-package hl-todo                    ; Highlight TODOs in buffers
  :ensure t
  :defer t
  :init (global-hl-todo-mode))


;;; Skeletons, completion and expansion

;; In `completion-at-point', do not pop up silly completion buffers for less
;; than five candidates.  Cycle instead.
(setq completion-cycle-threshold 5)

(use-package hippie-exp                 ; Powerful expansion and completion
  :bind (([remap dabbrev-expand] . hippie-expand))
  :config
  (progn
    (setq hippie-expand-try-functions-list
          '(try-expand-dabbrev
            try-expand-dabbrev-all-buffers
            try-expand-dabbrev-from-kill
            try-complete-file-name-partially
            try-complete-file-name
            try-expand-all-abbrevs
            try-expand-list
            try-complete-lisp-symbol-partially
            try-complete-lisp-symbol
            lunaryorn-try-complete-lisp-symbol-without-namespace))))

(use-package lunaryorn-hippie-exp       ; Custom expansion functions
  :load-path "lisp/"
  :after hippie-exp
  :commands (lunaryorn-try-complete-lisp-symbol-without-namespace))

(use-package yasnippet                  ; Snippets
  :ensure t
  :defer t
  :diminish (yas-minor-mode . " Ⓨ"))

;; (use-package yasnippet
;;   :diminish yas-minor-mode
;;   :init
;;   (progn
;;     (setq user-snippets-directory (expand-file-name "snippets" user-emacs-directory))
;;     (make-directory user-snippets-directory t)
;;     ;; (use-package yasnippets)
;;     (yas-global-mode 1)
;;     (setq-default yas/prompt-functions '(yas/ido-prompt))))

(use-package company                    ; Graphical (auto-)completion
  :ensure t
  :diminish company-mode
  :init (global-company-mode)
  :config
  (setq company-tooltip-align-annotations t
        company-tooltip-flip-when-above t
        ;; Easy navigation to candidates with M-<n>
        company-show-numbers t)
  :diminish company-mode)

(use-package company-quickhelp          ; Show help in tooltip
  :disabled t                           ; M-h clashes with mark-paragraph
  :ensure t
  :after company
  :config (company-quickhelp-mode))

(use-package company-statistics         ; Sort company candidates by statistics
  :ensure t
  :after company
  :config (company-statistics-mode))

(use-package company-math               ; Completion for Math symbols
  :ensure t
  :after company
  :config
  ;; Add backends for math characters
  (add-to-list 'company-backends 'company-math-symbols-unicode)
  (add-to-list 'company-backends 'company-math-symbols-latex))

(use-package company-emoji              ; Emojis completion like Github/Slack
  :ensure t
  :after company
  :config (add-to-list 'company-backends 'company-emoji))

(use-package helm-company               ; Helm frontend for company
  :ensure t
  :defer t
  :bind (:map company-mode-map
         ([remap complete-symbol] . helm-company)
         ([remap completion-at-point] . helm-company)
         :map company-active-map
         ("C-:" . helm-company)))

(use-package auto-insert                ; Automatic insertion into new files
  :defer t
  :bind (("C-c i a" . auto-insert)))

(use-package copyright                  ; Deal with copyright notices
  :defer t
  :bind (("C-c i c" . copyright-update))
  :init
  ;; Update copyright when visiting files
  (defun lunaryorn-copyright-update ()
    (interactive)
    (unless buffer-read-only
      (copyright-update nil 'interactive)
      (unless copyright-update
        ;; Fix years when the copyright information was updated
        (copyright-fix-years))))
  (add-hook 'find-file-hook #'lunaryorn-copyright-update)
  :config
  ;; Use ranges to denote consecutive years
  (setq copyright-year-ranges t
        ;; Limit copyright changes to my own copyright
        copyright-names-regexp (regexp-quote user-full-name)))


;;; Spelling and syntax checking
(use-package ispell                     ; Spell checking
  :defer t
  :config
  (defun ispell-init-process-shutup (orig-fun &rest args)
    (let ((inhibit-message t))
      (apply orig-fun args)))
  (advice-add #'ispell-init-process :around #'ispell-init-process-shutup)
  (setq ispell-program-name (if (eq system-type 'darwin)
                                (executable-find "aspell")
                              (executable-find "hunspell"))
        ispell-dictionary "en"     ; Default dictionnary
        ispell-silently-savep t       ; Don't ask when saving the private dict
        ;; Increase the height of the choices window to take our header line
        ;; into account.
        ispell-choices-win-default-height 5)

  (unless ispell-program-name
    (warn "No spell checker available.  Install Hunspell or ASpell for OS X.")))

(use-package flyspell                   ; On-the-fly spell checking
  :bind (("C-c t s" . flyspell-mode)
         ("C-c l b" . flyspell-buffer))
  :init (progn (dolist (hook '(text-mode-hook message-mode-hook))
                 (add-hook hook 'turn-on-flyspell))
               (add-hook 'prog-mode-hook 'flyspell-prog-mode))
  :config
  (progn
    (setq flyspell-use-meta-tab nil
          ;; Make Flyspell less chatty
          flyspell-issue-welcome-flag nil
          flyspell-issue-message-flag nil)

    ;; Free C-M-i for completion
    (define-key flyspell-mode-map "\M-\t" nil)
    ;; Undefine mouse buttons which get in the way
    (define-key flyspell-mouse-map [down-mouse-2] nil)
    (define-key flyspell-mouse-map [mouse-2] nil))
  :diminish (flyspell-mode . " ⓢ"))

(use-package helm-flyspell              ; Helm interface to Flyspell
  :ensure t
  :bind
  (:map flyspell-mode-map
        ([remap flyspell-auto-correct-previous-word] . helm-flyspell-correct)
        ("C-c l c" . helm-flyspell-correct)))

(use-package auto-dictionary            ; Automatically infer dictionary
  :ensure t
  ;; Always change dictionary through adict, because it triggers hooks that let
  ;; us automatically update the "language" for other modes (e.g. Typo Mode) as
  ;; well
  :bind (("C-c l l" . adict-change-dictionary)
         ("C-c l g" . adict-guess-dictionary))
  :init
  (add-hook 'flyspell-mode-hook #'auto-dictionary-mode))

(use-package flycheck                   ; On-the-fly syntax checking
  :ensure t
  :bind (("C-c e" . lunaryorn-flycheck-errors/body)
         ("C-c t f" . flycheck-mode))
  :init
  (defhydra lunaryorn-flycheck-errors ()
    "Flycheck errors."
    ("n" flycheck-next-error "next")
    ("p" flycheck-previous-error "previous")
    ("f" flycheck-first-error "first")
    ("l" flycheck-list-errors "list")
    ("w" flycheck-copy-errors-as-kill "copy message")
    ;; See `helm-flycheck' package below
    ("h" helm-flycheck "list with helm"))

  (global-flycheck-mode)
  :config
  (setq flycheck-standard-error-navigation nil
        flycheck-display-errors-function
        #'flycheck-display-error-messages-unless-error-list
        flycheck-scalastylerc "scalastyle_config.xml")
  :diminish (flycheck-mode . " Ⓢ"))

(use-package lunaryorn-flycheck         ; Personal Flycheck extensions
  :load-path "lisp/"
  :commands (lunaryorn-flycheck-find-config-file-in-sbt-project
             lunaryorn-discard-undesired-html-tidy-error
             lunaryorn-use-js-executables-from-node-modules
             lunaryorn-flycheck-set-load-path-for-user-configuration)
  :init
  ;; Don't highlight undesired errors from html tidy
  (add-hook 'flycheck-process-error-functions
            #'lunaryorn-discard-undesired-html-tidy-error)
  (add-hook 'flycheck-locate-config-file-functions
            #'lunaryorn-flycheck-find-config-file-in-sbt-project)
  (dolist (hook-fn '(lunaryorn-use-js-executables-from-node-modules
                     lunaryorn-flycheck-set-load-path-for-user-configuration))
    (add-hook 'flycheck-mode-hook hook-fn)))

(use-package helm-flycheck              ; Helm frontend for Flycheck errors
  :ensure t
  :after flycheck)


;;; Text editing
(use-package table                      ; Edit tables in text files
  :defer t
  :init
  (add-hook 'text-mode-hook #'table-recognize))

(use-package tildify                    ; Insert non-breaking spaces on the fly
  :defer t
  :bind (("C-c x t" . tildify-region)
         ("C-c t ~" . tildify-mode))
  :config
  ;; Use the right space for LaTeX
  (add-hook 'latex-mode-hook
            (lambda () (setq-local tildify-space-string "~"))))

(use-package typo                       ; Automatically use typographic quotes
  :ensure t
  :bind (("C-c t t" . typo-mode)
         ("C-c l L" . typo-change-language))
  :init
  (typo-global-mode)
  (add-hook 'text-mode-hook #'typo-mode)
  :config
  ;; TODO: Automatically set from ispell dictionary in
  ;; `adict-change-dictionary-hook', to update the typo language whenever the
  ;; spelling language changed
  (setq-default typo-language "English")
  :diminish (typo-mode . " Ⓣ"))


;;; LaTeX with AUCTeX
(use-package tex-site                   ; AUCTeX initialization
  :ensure auctex)

(use-package tex                        ; TeX editing/processing
  :ensure auctex
  :defer t
  :config
  (setq TeX-parse-self t                ; Parse documents to provide completion
                                        ; for packages, etc.
        TeX-auto-save t                 ; Automatically save style information
        TeX-electric-sub-and-superscript t ; Automatically insert braces after
                                        ; sub- and superscripts in math mode
        TeX-electric-math '("\\(" "\\)")
        ;; Don't insert magic quotes right away.
        TeX-quote-after-quote t
        ;; Don't ask for confirmation when cleaning
        TeX-clean-confirm nil
        ;; Provide forward and inverse search with SyncTeX
        TeX-source-correlate-mode t
        TeX-source-correlate-method 'synctex)
  (setq-default TeX-master nil          ; Ask for the master file
                TeX-engine 'luatex      ; Use a modern engine
                ;; Redundant in 11.88, but keep for older AUCTeX
                TeX-PDF-mode t)

  ;; Move to chktex
  (setcar (cdr (assoc "Check" TeX-command-list)) "chktex -v6 %s"))

(use-package tex-buf                    ; TeX buffer management
  :ensure auctex
  :defer t
  ;; Don't ask for confirmation when saving before processing
  :config (setq TeX-save-query nil))

(use-package tex-style                  ; TeX style
  :ensure auctex
  :defer t
  :config
  ;; Enable support for csquotes
  (setq LaTeX-csquotes-close-quote "}"
        LaTeX-csquotes-open-quote "\\enquote{"))

(use-package tex-fold                   ; TeX folding
  :ensure auctex
  :defer t
  :init (add-hook 'TeX-mode-hook #'TeX-fold-mode))

(use-package tex-mode                   ; TeX mode
  :ensure auctex
  :defer t
  :config
  (font-lock-add-keywords 'latex-mode
                          `((,(rx "\\"
                                  symbol-start
                                  "fx" (1+ (or (syntax word) (syntax symbol)))
                                  symbol-end)
                             . font-lock-warning-face))))

(use-package latex                      ; LaTeX editing
  :ensure auctex
  :defer t
  :config
  ;; Teach TeX folding about KOMA script sections
  (setq TeX-outline-extra `((,(rx (0+ space) "\\section*{") 2)
                            (,(rx (0+ space) "\\subsection*{") 3)
                            (,(rx (0+ space) "\\subsubsection*{") 4)
                            (,(rx (0+ space) "\\minisec{") 5))
        ;; No language-specific hyphens please
        LaTeX-babel-hyphen nil)

  (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode))    ; Easy math input

(use-package auctex-latexmk             ; latexmk command for AUCTeX
  :ensure t
  :defer t
  :after latex
  :config (auctex-latexmk-setup))

(use-package auctex-skim                ; Skim as viewer for AUCTeX
  :load-path "lisp/"
  :commands (auctex-skim-select)
  :after tex
  :config (auctex-skim-select))

(use-package bibtex                     ; BibTeX editing
  :defer t
  :config
  ;; Run prog mode hooks for bibtex
  (add-hook 'bibtex-mode-hook (lambda () (run-hooks 'prog-mode-hook)))

  ;; Use a modern BibTeX dialect
  (bibtex-set-dialect 'biblatex))

(defun lunaryorn-reftex-find-ams-environment-caption (environment)
  "Find the caption of an AMS ENVIRONMENT."
  (let ((re (rx-to-string `(and "\\begin{" ,environment "}"))))
    ;; Go to the beginning of the label first
    (re-search-backward re)
    (goto-char (match-end 0)))
  (if (not (looking-at (rx (zero-or-more space) "[")))
      (error "Environment %s has no title" environment)
    (let ((beg (match-end 0)))
      ;; Move point onto the title start bracket and move over to the end,
      ;; skipping any other brackets in between, and eventually extract the text
      ;; between the brackets
      (goto-char (1- beg))
      (forward-list)
      (buffer-substring-no-properties beg (1- (point))))))

(use-package reftex                     ; TeX/BibTeX cross-reference management
  :defer t
  :init (add-hook 'LaTeX-mode-hook #'reftex-mode)
  :config
  ;; Plug into AUCTeX
  (setq reftex-plug-into-AUCTeX t
        ;; Automatically derive labels, and prompt for confirmation
        reftex-insert-label-flags '(t t)
        reftex-label-alist
        '(
          ;; Additional label definitions for RefTeX.
          ("definition" ?d "def:" "~\\ref{%s}"
           lunaryorn-reftex-find-ams-environment-caption
           ("definition" "def.") -3)
          ("theorem" ?h "thm:" "~\\ref{%s}"
           lunaryorn-reftex-find-ams-environment-caption
           ("theorem" "th.") -3)
          ("example" ?x "ex:" "~\\ref{%s}"
           lunaryorn-reftex-find-ams-environment-caption
           ("example" "ex") -3)
          ;; Algorithms package
          ("algorithm" ?a "alg:" "~\\ref{%s}"
           "\\\\caption[[{]" ("algorithm" "alg") -3)))

  ;; Provide basic RefTeX support for biblatex
  (unless (assq 'biblatex reftex-cite-format-builtin)
    (add-to-list 'reftex-cite-format-builtin
                 '(biblatex "The biblatex package"
                            ((?\C-m . "\\cite[]{%l}")
                             (?t . "\\textcite{%l}")
                             (?a . "\\autocite[]{%l}")
                             (?p . "\\parencite{%l}")
                             (?f . "\\footcite[][]{%l}")
                             (?F . "\\fullcite[]{%l}")
                             (?x . "[]{%l}")
                             (?X . "{%l}"))))
    (setq reftex-cite-format 'biblatex))
  :diminish reftex-mode)


;;; Markup languages
(use-package rst                        ; ReStructuredText
  :defer t
  :config
  ;; Indent with 3 spaces after all kinds of literal blocks
  (setq rst-indent-literal-minimized 3
        rst-indent-literal-normal 3)

  (bind-key "C-=" nil rst-mode-map)
  ;; For similarity with AUCTeX
  (bind-key "C-c C-j" #'rst-insert-list rst-mode-map)
  ;; …and with Markdown Mode
  (bind-key "M-RET" #'rst-insert-list rst-mode-map))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :config
  (progn
    (bind-key "M-n" 'open-line-below markdown-mode-map)
    (bind-key "M-p" 'open-line-above markdown-mode-map)))

(use-package markdown-mode+
  :ensure t
  :defer t)

(use-package markdown-mode              ; Markdown
  :ensure t
  ;; Just no, dear Markdown Mode.  Don't force that bastard Github dialect upon
  ;; me!
  :mode ("\\.md\\'" . markdown-mode)
  :config
  ;; Process Markdown with Pandoc, using a custom stylesheet for nice output
  (let ((stylesheet (expand-file-name
                     (locate-user-emacs-file "etc/pandoc.css"))))

    ;; (add-hook 'markdown-mode-hook        ; https://github.com/skeeto/.emacs.d
    ;;           (lambda ()
    ;;             (remove-hook 'fill-nobreak-predicate
    ;;                          'markdown-inside-link-text-p t)))

    ;; (setf sentence-end-double-space nil  ; https://github.com/skeeto/.emacs.d
    ;;       markdown-command
    ;;       "pandoc -f markdown -t html5 -s --self-contained --smart")

    (setq markdown-command
          (mapconcat #'shell-quote-argument
                     `("pandoc" "--toc" "--section-divs"
                       "--css" ,(concat "file://" stylesheet)
                       "--standalone" "-f" "markdown" "-t" "html5")
                     " ")))

  ;; No filling in GFM, because line breaks are significant.
  (add-hook 'gfm-mode-hook #'turn-off-auto-fill)
  ;; Use visual lines instead
  (add-hook 'gfm-mode-hook #'visual-line-mode)
  (add-hook 'gfm-mode-hook #'lunaryorn-whitespace-style-no-long-lines)

  (bind-key "C-c C-s C" #'markdown-insert-gfm-code-block markdown-mode-map)
  (bind-key "C-c C-s P" #'markdown-insert-gfm-code-block markdown-mode-map)

  ;; Fight my habit of constantly pressing M-q.  We should not fill in GFM
  ;; Mode.
  (bind-key "M-q" #'ignore gfm-mode-map))

(use-package lunaryorn-markdown         ; Personal Markdown extensions
  :load-path "lisp/"
  :bind (:map markdown-mode-map
              ("C-c m h" . lunaryorn-markdown-post-header)
              ("C-c m p" . lunaryorn-markdown-publish-jekyll-draft)))

(use-package yaml-mode                  ; YAML
  :ensure t
  :defer t
  :mode ("\\.yml$" . yaml-mode)
  :config
  (add-hook 'yaml-mode-hook
            (lambda ()
              (setq-local paragraph-separate ".*>-$\\|[   ]*$")
              (setq-local paragraph-start paragraph-separate)
              (run-hooks 'prog-mode-hook))))

(use-package json-mode                  ; JSON files
  :ensure t
  :defer t
  :config
  (progn                                ; https://github.com/skeeto/.emacs.d
    (setf json-reformat:pretty-string? t
          json-reformat:indent-width 2)
    (define-key json-mode-map (kbd "M-q")
      (lambda ()
        (interactive)
        (if (region-active-p)
            (call-interactively #'json-reformat-region)
          (json-reformat-region (point-min) (point-max)))))

    (add-hook 'json-mode-hook
              ;; Fix JSON mode indentation
              (lambda () (setq-local js-indent-level 4)))))

(use-package lunaryorn-json             ; Personal JSON tools
  :load-path "lisp/"
  :bind (:map json-mode-map
              ("C-c m r" . lunaryorn-json-chef-role)))

(use-package json-reformat              ; Reformat JSON
  :ensure t
  :defer t
  :bind (("C-c x j" . json-reformat-region)))

(use-package graphviz-dot-mode          ; Graphviz
  :ensure t
  :defer t
  :config
  (setq graphviz-dot-indent-width 4))


;;; Programming utilities
(use-package prog-mode                  ; Prog Mode
  :bind (("C-c t p" . prettify-symbols-mode)))

(use-package compile                    ; Compile from Emacs
  :bind (("C-c c C" . recompile))
  :config
  (setq compilation-ask-about-save nil
        ;; Kill old compilation processes before starting new ones,
        compilation-always-kill t
        ;; Automatically scroll
        compilation-scroll-output 'first-error
        ;; Skip over warnings and info messages in compilation
        compilation-skip-threshold 2
        ;; Don't freeze when process reads from stdin
        compilation-disable-input t
        ;; Show three lines of context around the current message
        compilation-context-lines 3)

  (require 'ansi-color)

  (defun lunaryorn-colorize-compilation-buffer ()
    "Colorize a compilation mode buffer.

Taken from http://stackoverflow.com/a/3072831/355252."
    (interactive)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point-max))))

  (add-hook 'compilation-filter-hook #'lunaryorn-colorize-compilation-buffer))

(use-package helm-make                  ; Run makefile targets through Helm
  :ensure t
  :bind (("C-c c c" . helm-make-projectile)
         ;; FIXME: Write a more sophisticated command that checks whether a
         ;; Makefile exists and falls back to an alternative if not.
         ("<f5>" . helm-make-projectile)))

(use-package elide-head                 ; Elide lengthy GPL headers
  :bind (("C-c t e" . elide-head))
  :init (add-hook 'prog-mode-hook #'elide-head))

(use-package eldoc                      ; Documentation in minibuffer
  :defer t
  ;; Enable Eldoc for `eval-expression', too
  :init
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
  (add-hook 'ielm-mode-hook 'eldoc-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook 'eldoc-mode)
  :config
  (setq-default eldoc-documentation-function #'describe-char-eldoc)
  :diminish (eldoc-mode . " ⓓ"))

(use-package emacs-lisp-mode
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook (lambda() (setq mode-name "el")))
    (bind-key "C-<return>" 'eval-region-or-last-sexp emacs-lisp-mode-map)
    (use-package macrostep
      :bind (("C-c e" . macrostep-expand)
             ("C-A-e" . macrostep-expand))
      :config
      (progn
        (bind-keys
         :map macrostep-keymap
         ("j" . macrostep-next-macro)
         ("k" . macrostep-prev-macro)
         ("C-A-n" . macrostep-next-macro)
         ("C-A-p" . macrostep-prev-macro)
         )))
    (use-package ert
      :config (add-to-list 'emacs-lisp-mode-hook 'ert--activate-font-lock-keywords)))
  :bind (("M-." . find-function-at-point))
  :interpreter (("emacs" . emacs-lisp-mode))
  :mode ("Cask" . emacs-lisp-mode))

(use-package etags                      ; Tag navigation
  :defer t
  :config
  ;; Do not query before reverting TAGS tables
  (setq tags-revert-without-query t))

(use-package restclient                 ; ReST REPL for Emacs
  :ensure t
  :defer t)

(use-package company-restclient         ; Company support for restclient
  :ensure t
  :after company
  :config (add-to-list 'company-backends 'company-restclient))


;;; Emacs Lisp
(bind-key "C-c t d" #'toggle-debug-on-error)

(use-package elisp-slime-nav            ; Jump to definition of symbol at point
  :ensure t
  :init (add-hook 'emacs-lisp-mode-hook #'elisp-slime-nav-mode)
  :bind (:map elisp-slime-nav-mode-map
              ("C-c h ." . elisp-slive-nav-describe-elisp-thing-at-point))
  :config
  (dolist (key '("C-c C-d d" "C-c C-d C-d"))
    (define-key elisp-slime-nav-mode-map (kbd key) nil))
  :diminish elisp-slime-nav-mode)

(use-package flycheck-cask              ; Setup Flycheck by Cask projects
  :ensure t
  :defer t
  :init (add-hook 'flycheck-mode-hook #'flycheck-cask-setup))

(use-package flycheck-package           ; Check package conventions with Flycheck
  :ensure t
  :defer t
  :after flycheck
  :config (flycheck-package-setup))

(use-package pcre2el                    ; Convert regexps to RX and back
  :disabled t
  :ensure t
  :init (rxt-global-mode))

(use-package ielm                       ; Emacs Lisp REPL
  :bind (("C-c a '" . ielm)
         ("C-c :"   . ielm))
  :config                               ; https://github.com/skeeto/.emacs.d
  (progn
    (define-key ielm-map (kbd "C-c C-z") #'quit-window)
    (defadvice ielm-eval-input (after ielm-paredit activate)
      "Begin each ielm prompt with a paredit pair."
      (paredit-open-round))
    (defun my-ielm-return ()
      (interactive)
      (let ((end-of-sexp (save-excursion
                           (goto-char (point-max))
                           (skip-chars-backward " \t\n\r")
                           (point))))
        (if (>= (point) end-of-sexp)
            (progn
              (goto-char (point-max))
              (skip-chars-backward " \t\n\r")
              (delete-region (point) (point-max))
              (call-interactively #'ielm-return))
          (call-interactively #'paredit-newline))))

    (defun my-ielm-return-again ()
      (interactive)
      (let ((p (point))
            (end-of-sexp (save-excursion
                           (goto-char (point-max))
                           (skip-chars-backward " \t\n\r")
                           (point))))
        (goto-char (point-max))
        (skip-chars-backward " \t\n\r")
        (delete-region (point) (point-max))
        (call-interactively #'ielm-return)
        (comint-previous-input 1)
        (backward-char (- end-of-sexp p))
        (comint-kill-region (point-min) (+ end-of-sexp 1))))

    (add-hook 'ielm-mode-hook
              (function
               (lambda ()
                 (bind-key "C-<return>" 'my-ielm-return ielm-map)))
              t)
    (add-hook 'ielm-mode-hook
              (function
               (lambda ()
                 (bind-key "<return>" 'my-ielm-return-again ielm-map)))
              t)))

(use-package elisp-mode                 ; Emacs Lisp editing
  :defer t
  :interpreter ("emacs" . emacs-lisp-mode)
  :bind (:map emacs-lisp-mode-map
              ("C-c m e r" . eval-region)
              ("C-c m e b" . eval-buffer)
              ("C-c m e e" . eval-last-sexp)
              ("C-c m e f" . eval-defun)))

(use-package lunaryorn-elisp            ; Personal tools for Emacs Lisp
  :load-path "lisp/"
  :commands (lunaryorn-add-use-package-to-imenu)
  :bind (:map emacs-lisp-mode-map ("C-c m f" . lunaryorn-elisp-find-cask-file))
  :init
  (add-hook 'emacs-lisp-mode-hook #'lunaryorn-add-use-package-to-imenu))

(use-package helm-elisp                 ; Helm commands for elisp
  :ensure helm
  :defer t
  :bind (([remap apropos-command] . helm-apropos)
         ("C-c f l" . helm-locate-library)))

(use-package el-search                  ; pcase-based search for elisp
  :ensure t
  :bind (:map emacs-lisp-mode-map
              ("C-c m s" . el-search-pattern)
              ("C-c m r" . el-search-query-replace)))

(use-package cask-mode                  ; A major mode for Cask files
  :ensure t
  :defer t)

(use-package macrostep                  ; Interactively expand macros in code
  :ensure t
  :bind (:map emacs-lisp-mode-map ("C-c m x" . macrostep-expand)
         :map lisp-interaction-mode-map ("C-c m x" . macrostep-expand)))

(use-package ert                        ; Unit test framework
  ;; Load after Emacs Lisp Mode to support writing ERT tests
  :after elisp-mode)

(use-package buttercup                  ; BDD test framework for Emacs
  :ensure t
  :after elisp-mode)


;;; Scala

(use-package scala-mode                 ; Scala editing
  :ensure t
  :defer t
  :config
  (setq scala-indent:default-run-on-strategy
        scala-indent:operator-strategy)

  (defun lunaryorn-newline-and-indent-with-asterisk ()
    (interactive)
    (newline-and-indent)
    (scala-indent:insert-asterisk-on-multiline-comment))

  (define-key scala-mode-map (kbd "RET")
    #'lunaryorn-newline-and-indent-with-asterisk))

(use-package sbt-mode                   ; Scala build tool
  :ensure t
  :defer t
  :bind (:map scala-mode-map
              ("C-c m b c" . sbt-command)
              ("C-c m b r" . sbt-run-previous-command))
  :config
  ;; Do not pop up SBT buffers automatically
  (setq sbt:display-command-buffer nil)

  (defun lunaryorn-scala-pop-to-sbt (new-frame)
    "Open SBT REPL for this project, optionally in a NEW-FRAME.

Select the SBT REPL for the current project in a new window.  If
the REPL is not yet running, start it.  With prefix arg, select
the REPL in a new frame instead."
    (interactive "P")
    ;; Start SBT when no running, taken from `sbt:command'
    (when (not (comint-check-proc (sbt:buffer-name)))
      (sbt:run-sbt))

    (let ((display-buffer-overriding-action
           (if new-frame '(display-buffer-pop-up-frame) nil)))
      (pop-to-buffer (sbt:buffer-name))))

  (with-eval-after-load 'scala-mode
    (bind-key "C-c m s" #'lunaryorn-scala-pop-to-sbt scala-mode-map))

  ;; Disable Smartparens Mode in SBT buffers, because it frequently
  ;; hangs while trying to find matching delimiters
  (add-hook 'sbt-mode-hook
            (lambda ()
              (when (fboundp 'smartparens-mode)
                (smartparens-mode -1)))))

(use-package ensime                     ; Scala interaction mode
  :ensure t
  :after scala-mode
  :bind (:map ensime-mode-map
              ("C-c m E" . ensime-reload)
              ;; Free M-n and M-p again
              ("M-n" . nil)
              ("M-p" . nil)
              ("<f5>" . ensime-sbt-do-compile)
         :map scala-mode-map ("C-c m e" . ensime))
  :config
  ;; ;; Enable Ensime for all Scala buffers.
  (add-hook 'scala-mode-hook #'ensime-mode)

  ;; Compile on save.  My projects are small enough :)
  (setq ensime-sbt-perform-on-save "test:compile"))

(use-package ensime-expand-region       ; Integrate Ensime into expand-region
  :ensure ensime
  :after ensime)

(use-package play-routes-mode           ; Mode for Play 2 routes files
  :ensure t
  :defer t)

(use-package flycheck-ensime            ; Ensime-based checker for Flycheck
  :disabled t
  :load-path "lisp/"
  :defer t)


;;; Haskell
(use-package haskell-mode               ; Haskell major mode
  :ensure t
  :defer t
  :bind (:map haskell-mode-map
              ("M-." . haskell-mode-jump-to-def-or-tag)
              ("C-c m i j" . haskell-navigate-imports)
              ("C-c m i s" . haskell-sort-imports)
              ("C-c m i a" . haskell-align-imports)
              ;; Recommended Haskell Mode bindings, see
              ;; http://haskell.github.io/haskell-mode/manual/latest/Interactive-Haskell.html
            )
  :config
  (setq haskell-tags-on-save t          ; Regenerate TAGS on save
        haskell-process-log t           ; Show log for GHCI process
        ;; Remove unused imports and auto-import modules
        haskell-process-suggest-remove-import-lines t
        haskell-process-auto-import-loaded-modules t)

  (add-hook 'haskell-mode-hook #'haskell-decl-scan-mode) ; IMenu support
  (add-hook 'haskell-mode-hook #'interactive-haskell-mode))

(use-package haskell                    ; Interactive Haskell
  :ensure haskell-mode
  :defer t
  :bind (:map haskell-mode-map
         ("C-c C-l" . haskell-process-load-file)
         ("C-`" . haskell-interactive-bring)
         ("C-c C-t" . haskell-process-do-type)
         ("C-c C-i" . haskell-process-do-info)
         ("C-c C-c" . haskell-process-cabal-build)
         ("C-c C-k" . haskell-interactive-mode-clear)
         ("C-c c" . haskell-process-cabal)
         :map interactive-haskell-mode-map
         ("C-c m t" . haskell-mode-show-type-at))
  :init (add-hook 'haskell-mode-hook 'interactive-haskell-mode))

(use-package haskell-compile            ; Haskell compilation
  :ensure haskell-mode
  :defer t
  :bind (:map haskell-mode-map
              ("C-c m c" . haskell-compile)
              ("<f5>" . haskell-compile))
  :config
  ;; Build with Stack
  (setq haskell-compile-cabal-build-command "stack build"))

(use-package cabal-mode                 ; Cabal files
  :ensure haskell-mode
  :defer t
  :bind (:map haskell-cabal-mode-map
              ("C-`" . haskell-interactive-bring)
              ("C-c C-k" . haskell-interactive-mode-clear)
              ("C-c C-c" . haskell-process-cabal-build)
              ("C-c c" . haskell-process-cabal)))

(use-package hindent                    ; Haskell indentation
  :ensure t
  :defer t
  :init
  (add-hook 'haskell-mode-hook #'hindent-mode)
  :config
  (setq hindent-style "gibiansky"))


;;; Python
(use-package python                     ; Python editing
  :defer t
  :config
  ;; PEP 8 compliant filling rules, 79 chars maximum
  (add-hook 'python-mode-hook (lambda () (setq fill-column 79)))
  (add-hook 'python-mode-hook #'subword-mode)

  (let ((ipython (executable-find "ipython3")))
    (if ipython
        (setq python-shell-interpreter ipython)
      (warn "IPython is missing, falling back to default python")))

  (bind-key "C-<return>" 'python-shell-send-region python-mode-map)
  (bind-key "M-p" 'backward-paragraph python-mode-map)
  (bind-key "M-n" 'forward-paragraph python-mode-map)

  (setq python-shell-interpreter-args ""
        python-shell-interpreter-interactive-arg "")
  (use-package eval-in-repl-python
    :init
    (progn
      (defun eir-eval-in-python ()
        "eval-in-repl for Python."
        (interactive)
        ;; Define local variables
        (let* ((script-window (selected-window)))
          ;;
          (eir-repl-start "*Python*" #'run-python)

          ;; Check if selection is present
          (if (and transient-mark-mode mark-active)
              ;; If selected, send region
              (let* ((body (buffer-substring-no-properties (point) (mark)))
                     (paste (concat "%cpaste -q \n" body "\n--")))
                (eir-send-to-python paste))

            ;; If not selected, do all the following
            ;; Move to the beginning of line
            (beginning-of-line)
            ;; Set mark at current position
            (set-mark (point))
            ;; Go to the end of statement
            (python-nav-end-of-statement)
            ;; Go to the end of block
            (python-nav-end-of-block)
            ;; Send region if not empty
            (if (not (equal (point) (mark)))
                ;; Add one more character for newline unless at EOF
                ;; This does not work if the statement asks for an input.
                (let* ((body (buffer-substring-no-properties
                              (min (+ 1 (point)) (point-max))
                              (mark)))
                       (paste (concat "%cpaste -q \n" body "\n--")))
                  (eir-send-to-python paste))
              ;; If empty, deselect region
              (setq mark-active nil))
            ;; Move to the next statement
            (python-nav-forward-statement)

            ;; Switch to the shell
            (python-shell-switch-to-shell)
            ;; Switch back to the script window
            (select-window script-window))))

      (bind-key "C-<return>" 'eir-eval-in-python python-mode-map))))

(use-package lunaryorn-virtualenv       ; Personal virtualenv tools
  :load-path "lisp/"
  :commands (lunaryorn-virtualenv-init-from-workon-home)
  :init (add-hook 'python-mode-hook #'lunaryorn-virtualenv-init-from-workon-home))

(use-package flycheck-virtualenv        ; Setup Flycheck by virtualenv
  :load-path "lisp/"
  :after python
  :commands (flycheck-virtualenv-setup)
  :config (add-hook 'flycheck-mode-hook #'flycheck-virtualenv-setup))

(use-package anaconda-mode              ; Powerful Python backend for Emacs
  :ensure t
  :defer t
  :init (add-hook 'python-mode-hook #'anaconda-mode))

(use-package company-anaconda           ; Python backend for Company
  :defer t
  :ensure t
  :after company
  :config (add-to-list 'company-backends 'company-anaconda))

(use-package pip-requirements           ; requirements.txt files
  :ensure t
  :defer t)


;;; Rust
(use-package rust-mode                  ; Rust major mode
  :ensure t
  :defer t)

(use-package flycheck-rust              ; Flycheck setup for Rust
  :ensure t
  :defer t
  :after rust-mode
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package racer                      ; Completion and navigation for Rust
  :ensure t
  :defer t
  :init (add-hook 'rust-mode-hook #'racer-mode)
  :config
  (setq racer-rust-src-path (getenv "RUST_SRC_PATH"))
  :diminish (racer-mode . "ⓡ"))

(use-package cargo                      ; Control Cargo
  :ensure t
  :bind (:map rust-mode-map ("<f5>" . cargo-process-build))
  :init (add-hook 'rust-mode-hook #'cargo-minor-mode)
  :diminish cargo-minor-mode)

(use-package toml-mode                  ; Toml for Cargo files
  :ensure t
  :defer t)


;;; HTML & Javascript
(use-package web-mode
  :ensure t
  :defer t
  :mode (("\\.html\\'" . web-mode)))

(use-package css-mode                   ; CSS
  :defer t
  :config (setq css-indent-offset 2))

(use-package js2-mode                   ; Powerful Javascript mode
  :ensure t
  :defer t
  :interpreter ("node"   . js2-mode)
  :mode (("\\.js\\'"     . js2-mode)
         ("\\.jsx\\'"    . js2-jsx-mode)
         ("\\.json$"     . js-mode)
         ("\\.template$" . json-mode)
         ("Jakefile$"    . js2-mode))
  ;; :config
  ;; ;; Disable parser errors and strict warnings.  We have Flycheck 8)
  ;; (setq js2-mode-show-parse-errors nil
  ;;       js2-mode-show-strict-warnings nil
  ;;       js2-highlight-level 3           ; Try to highlight most ECMA built-ins
  ;;       )
  :config
  (progn
    (add-hook 'js2-mode-hook (lambda () (setq mode-name "js2")))
    (add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 2)))
    (add-hook 'js2-mode-hook (lambda ()
                               (bind-key "M-j" 'join-line-or-lines-in-region js2-mode-map)))
    (setq js2-skip-preprocessor-directives t
          js2-mode-show-parse-errors nil
          js2-mode-show-strict-warnings nil
          js2-highlight-level 3           ; Try to highlight most ECMA built-ins
          )
    (setq-default js2-additional-externs
                  '("$" "unsafeWindow" "localStorage" "jQuery"
                    "setTimeout" "setInterval" "location" "skewer"
                    "console" "phantom"))))

(use-package js2-refactor               ; Refactor Javascript
  :ensure t
  :defer t
  :init
  (add-hook 'js2-mode-hook 'js2-refactor-mode)
  :config
  (js2r-add-keybindings-with-prefix "C-c m r"))

(use-package coffee-mode
  :defer t
  :config
  (progn
    (add-hook 'coffee-mode-hook
              (lambda ()
                (bind-key "C-j" 'coffee-newline-and-indent coffee-mode-map)
                (bind-key "C-M-h" 'backward-kill-word coffee-mode-map)
                (setq coffee-tab-width 2)))))

(use-package web-mode
  :defer t
  :init (progn
          (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))
  :config (progn
            (add-hook 'web-mode-hook
                      (lambda ()
                        (setq web-mode-style-padding 2)
                        (setq web-mode-script-padding 2)))))

(use-package nvm
  :defer t)
(use-package html-script-src
  :defer t)
(use-package haml-mode
  :defer t)
(use-package sass-mode
  :defer t)

(use-package tern                       ; Javascript IDE backend
  :ensure t
  :defer t
  :init (add-hook 'js2-mode-hook #'tern-mode)
  :config
  ;; Don't generate port files
  (add-to-list 'tern-command "--no-port-file" 'append))

(use-package company-tern               ; Auto-completion for javascript
  :ensure t
  :defer t
  :after company
  :config (add-to-list 'company-backends 'company-tern))


;;; Misc programming languages
(use-package sh-script                  ; Shell scripts
  :defer t
  :mode ("\\.zsh\\'" . sh-mode)
  :config
  ;; Use two spaces in shell scripts.
  (setq sh-indentation 2                ; The basic indentation
        sh-basic-offset 2               ; The offset for nested indentation
        ))

(use-package nxml-mode                  ; XML editing
  :defer t
  ;; Complete closing tags, and insert XML declarations into empty files
  :config (setq nxml-slash-auto-complete-flag t
                nxml-auto-insert-xml-declaration-flag t))

(use-package html5-schema               ; HTML5 schemata for NXML
  :ensure t
  :defer t)

(use-package thrift                     ; Thrift interface files
  :ensure t
  :defer t
  :init (put 'thrift-indent-level 'safe-local-variable #'integerp)
  :config (add-hook 'thrift-mode-hook (lambda () (run-hooks 'prog-mode-hook))))

(use-package homebrew-mode              ; Homebrew Formulae
  :ensure t
  :defer t)


;;; Databases
(use-package sql                        ; SQL editing and REPL
  :defer t
  :bind (("C-c a s" . sql-connect)
         :map sql-mode-map
         ("C-c m p" . sql-set-product)))

(use-package sqlup-mode                 ; Upcase SQL keywords
  :defer t
  :ensure t
  :bind (:map sql-mode-map
              ("C-c m u" . sqlup-capitalize-keywords-in-region))
  :config (add-hook 'sql-mode-hook #'sqlup-mode))


;;; Version control
(use-package vc-hooks                   ; Simple version control
  :defer t
  :config
  ;; Always follow symlinks to files in VCS repos
  (setq vc-follow-symlinks t))

(use-package what-the-commit            ; Insert random commit messages
  :ensure t
  :defer t
  :bind (("C-c i w" . what-the-commit-insert)
         ("C-c g w" . what-the-commit)))

(use-package diff-hl                    ; Highlight hunks in fringe
  :ensure t
  :defer t
  :init
  ;; Highlight changes to the current file in the fringe
  (global-diff-hl-mode)
  ;; Highlight changed files in the fringe of Dired
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)

  ;; Fall back to the display margin, if the fringe is unavailable
  (unless (display-graphic-p)
    (diff-hl-margin-mode))

  ;; Refresh diff-hl after Magit operations
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
;; :config (add-hook 'vc-checkin-hook 'diff-hl-update)
  )

(use-package magit                      ; The one and only Git frontend
  :ensure t
  :bind (("C-c g c" . magit-clone)
         ("C-c g s" . magit-status)
         ("C-c g b" . magit-blame)
         ("C-c g l" . magit-log-buffer-file)
         ("C-c g p" . magit-pull)
         ("C-c C-v" . magit-status)
         ("A-v"     . magit-status)
         ("A-w"     . magit-status)
         ("s-w"     . magit-status)
         ("C-x g"   . magit-status)
         ("C-x G"   . magit-status-with-prefix)
         ("C-A-g"   . custom-grep-find)
         ("A-r"     . magit-reflog))
  :init
  (progn
    (use-package dired
      :bind (:map dired-mode-map
                  ("c" . magit-clone)))
    (setq magit-auto-revert-mode t)
    (autoload 'magit-toplevel "magit")
    (defun custom-grep-find (&optional arg)
      (interactive "P")
      (if (and (not (equal arg '(4)))
               (magit-toplevel "."))
          (call-interactively 'helm-git-grep-from-here)
        ;; also consider igrep-find
        (call-interactively 'grep-find))))
  :config
  ;; Shut up, Magit
  (setq magit-save-repository-buffers 'dontask
        magit-refs-show-commit-count 'all
        ;; Use separate buffers for one-file logs so that we don't need to reset
        ;; the filter everytime for full log view
        magit-log-buffer-file-locked t
        ;; This is creepy, Magit
        magit-revision-show-gravatars nil)

  ;; Set Magit's repo dirs for `magit-status' from Projectile's known
  ;; projects.  Initialize the `magit-repository-directories'
  ;; immediately after Projectile was loaded, and update it every time
  ;; we switched projects, because the new project might have been
  ;; unknown before
  (defun lunaryorn-magit-set-repo-dirs-from-projectile ()
    "Set `magit-repo-dirs' from known Projectile projects."
    (let ((project-dirs (bound-and-true-p projectile-known-projects)))
      ;; Remove trailing slashes from project directories, because
      ;; Magit adds trailing slashes again, which breaks the
      ;; presentation in the Magit prompt.
      (setq magit-repository-directories
            (mapcar #'directory-file-name project-dirs))))

  (with-eval-after-load 'projectile
    (lunaryorn-magit-set-repo-dirs-from-projectile))

  (add-hook 'projectile-switch-project-hook
            #'lunaryorn-magit-set-repo-dirs-from-projectile)

  (bind-keys
   :map magit-status-mode-map
   ("C-<tab>" . other-window)
   ("`" . magit-section-cycle)
   (";" . magit-worktree-status))

  (use-package fullframe
    :init
    (progn
      (fullframe magit-status magit-mode-quit-window)))
)

(use-package git-commit                 ; Git commit message mode
  :ensure t
  :defer t
  :config
  ;; Oh, really?  Come on… I know what I'm doing…
  (remove-hook 'git-commit-finish-query-functions
               #'git-commit-check-style-conventions))

(use-package gitconfig-mode             ; Git configuration mode
  :ensure t
  :defer t
  :config (add-hook 'gitconfig-mode-hook
                    (lambda ()
                      (setf indent-tabs-mode nil
                            tab-width 4))))

(use-package gitignore-mode             ; .gitignore mode
  :ensure t
  :defer t)

(use-package gitattributes-mode         ; Git attributes mode
  :ensure t
  :defer t)

(use-package git-timemachine            ; Go back in Git time
  :ensure t
  :defer t
  :bind (("C-c g t" . git-timemachine)))

(use-package helm-gitignore             ; Generate gitignore files
  :ensure t
  :defer t
  :bind ("C-c g I" . helm-gitignore))

(use-package git-gutter
  :diminish git-gutter-mode
  :commands (stage-or-commit)
  :bind (("C-x C-g" . git-gutter:toggle)
         ("C-x v =" . git-gutter:popup-hunk)
         ;; ("C-x p"   . git-gutter:previous-hunk)
         ;; ("C-x n"   . git-gutter:next-hunk)
         ("A-p"     . git-gutter:previous-hunk)
         ("A-n"     . git-gutter:next-hunk)
         ("C-A-p"   . git-gutter:previous-hunk)
         ("C-A-n"   . git-gutter:next-hunk-diff)
         ("C-A-c"   . stage-or-commit)
         ("C-x v s" . git-gutter:stage-hunk)
         ("C-x v r" . git-gutter:revert-hunk)
         ("C-A-r"   . git-gutter:revert-hunk))
  :init (global-git-gutter-mode +1)
  :config
  (progn
    (defun git-gutter:diff-hunk ()
      "Popup diff of current hunk."
      (interactive)
      (git-gutter:awhen (git-gutter:search-here-diffinfo git-gutter:diffinfos)
        (git-gutter:popup-hunk it)
        (git-gutter:popup-buffer-window)))


    (defun git-gutter:next-hunk-diff (&optional arg)
      (interactive "p")
      (git-gutter:next-hunk arg)
      (recenter nil)
      (git-gutter:diff-hunk))


    (defun stage-or-commit (&optional arg)
      (interactive "p")
      (if (ignore-errors (git-gutter:search-here-diffinfo git-gutter:diffinfos))
          (git-gutter:stage-hunk)
        (progn
          (save-excursion
            (magit-diff-staged)
            (magit-commit))))
      (when (functionp 'magit-update-status-on-save)
        (magit-update-status-on-save)))

    (require 'noflet)
    ;; override y/n question-asking
    (defadvice git-gutter:stage-hunk (around quick-stage activate)
      (noflet ((yes-or-no-p (&rest args) t))
        ad-do-it))
    ))


;;; Github integration
(use-package gh                         ; Github API library
  :defer t
  ;; Change the default profile.  The profile itself is set up via customize,
  ;; and includes auth data, to prevent it from storing tokens in Git config
  :config (setq gh-profile-default-profile "aculich"))

(use-package magit-gh-pulls             ; Show Github PRs in Magit
  :ensure t
  :defer t
  :init (add-hook 'magit-mode-hook #'turn-on-magit-gh-pulls))

(use-package github-clone               ; Clone and fork from Github
  :ensure t
  :defer t
  :bind ("C-c g g c" . github-clone))

(use-package helm-open-github ; Open Github pages for current repo
  ;; FIXME: Triggers a password prompt during load?!
  :disabled t
  :ensure t
  :bind (("C-c g g i" . helm-open-github-from-issues)
         ("C-c g g p" . helm-open-github-from-pull-requests)))

(use-package gist                       ; Create and list Gists
  :ensure t
  :bind (("C-c g g l" . gist-list)
         ("C-c g g b" . gist-region-or-buffer)))

;;; Project management with Projectile
;; http://writequit.org/org/settings.html
;; http://joelmccracken.github.io/entries/project-local-variables-in-projectile-with-dirlocals/
(use-package projectile                 ; Project management for Emacs
  :ensure t
  :bind (([remap compile] . projectile-compile-project))
  :init (projectile-global-mode)
  :config
  ;; Remove dead projects when Emacs is idle
  (run-with-idle-timer 10 nil #'projectile-cleanup-known-projects)

  (setq projectile-completion-system 'helm
        projectile-find-dir-includes-top-level t)

  (defun lunaryorn-neotree-project-root (&optional directory)
    "Open a NeoTree browser for a project DIRECTORY."
    (interactive)
    (let ((default-directory (or directory default-directory)))
      (if (and (fboundp 'neo-global--window-exists-p)
               (neo-global--window-exists-p))
          (neotree-hide)
        (neotree-find (projectile-project-root)))))

  (progn
    (bind-keys
     :map projectile-mode-map
     ("C-x C-f" . helm-find-files)
     ("C-x C-p" . projectile-find-file)
     ("C-c p d" . projectile-dired)
     ("C-c p D" . projectile-find-dir))
    (setq projectile-known-projects-file (expand-file-name  "projectile-bookmarks.eld" user-cache-directory)
          projectile-cache-file (expand-file-name  "projectile.cache" user-cache-directory))
    (setq projectile-enable-caching t)
    (setq projectile-require-project-root nil)
    (setq projectile-completion-system 'ido)
    (add-to-list 'projectile-globally-ignored-files ".DS_Store")

    (use-package cc-mode
      :defer t
      :config (bind-key "C-c C-c" 'projectile-compile-project java-mode-map))
    (use-package make-mode
      :defer t
      :config (bind-key "C-c C-c" 'projectile-compile-project makefile-mode-map)))

  :diminish projectile-mode)

(use-package helm-projectile            ; Helm frontend for Projectile
  :defer t
  :ensure t
  :after projectile
  :bind (([remap projectile-find-file] . helm-projectile-find-file)
         ("C-c s p" . helm-projectile-ag)
         :map helm-projectile-projects-map
              ("C-t" . lunaryorn-neotree-project-root))
  :config
  (helm-projectile-on)

  (setq projectile-switch-project-action #'helm-projectile)

  (helm-add-action-to-source "Open NeoTree `C-t'"
                             #'lunaryorn-neotree-project-root
                             helm-source-projectile-projects 1))


;;; Processes and commands
(use-package proced                     ; Edit system processes
  :defer t
  ;; Proced isn't available on OS X
  :if (not (eq system-type 'darwin))
  :bind ("C-x p" . proced))


;;; Date and time
(use-package calendar                   ; Built-in calendar
  :defer t
  :bind ("C-c a c" . calendar)
  :config
  ;; In Europe we start on Monday
  (setq calendar-week-start-day 1))

(use-package time                       ; Show current time
  :bind (("C-c a c" . display-time-world))
  :config
    ;; (display-time-mode t)
  (setq display-time-world-time-format "%H:%M %Z, %d. %b"
        display-time-24hr-format t
        display-time-world-list '(("Europe/Berlin"    "Berlin")
                                  ("Europe/London"    "London")
                                  ("Europe/Istanbul"  "Istanbul")
                                  ("America/Winnipeg" "Winnipeg (CA)")
                                  ("America/New_York" "New York (USA)")
                                  ("Asia/Tokyo"       "Tokyo (JP)"))))


;;; Terminal emulation and shells
(use-package shell                      ; Dump shell in Emacs
  :defer t
  :bind ("C-c a t" . shell))

(use-package term                       ; Terminal emulator in Emacs
  :defer t
  :bind ("C-c a T" . ansi-term))


;;; Documents
(use-package doc-view
  :defer t
  :config
  ;; Render PDFs at 300dpi
  (setq doc-view-resolution 300)

  (defconst lunaryorn-doc-view-mutool-program "mutool")

  (defun lunaryorn-doc-view-pdf->png-converter-mutool (pdf png page callback)
    "Convert a PDF file to PNG at PAGE.

After conversion invoke CALLBACK.  See `doc-view-start-process'
for more information about CALLBACK."
    (doc-view-start-process
     "pdf->png" lunaryorn-doc-view-mutool-program
     `("draw"
       ,(concat "-o" png)
       ,(format "-r%d" (round doc-view-resolution))
       ,pdf
       ,@(if page `(,(format "%d" page))))
     callback))

  ;; If `mutool' exists use our own converter function to call "mutool draw".
  ;; Otherwise check whether docview found mudraw and warn if it didn't
  (if (executable-find lunaryorn-doc-view-mutool-program)
      (setq doc-view-pdf->png-converter-function
            #'lunaryorn-doc-view-pdf->png-converter-mutool)
    ;; Warn if Doc View falls back to Ghostscript for rendering
    (unless (eq doc-view-pdf->png-converter-function
                'doc-view-pdf->png-converter-mupdf)
      (warn "Doc View is not using mupdf!"))))


;;; Net & Web
(use-package browse-url                 ; Browse URLs
  :bind (("C-c a u" . browse-url))
  :defer t                              ; https://github.com/skeeto/.emacs.d
  :init (setf url-cache-directory (locate-user-emacs-file "local/url"))
  :config
  (when (executable-find "firefox")
    (setf browse-url-browser-function #'browse-url-firefox
          browse-url-generic-program "xombrero"
          browse-url-generic-args '("-n"))))

(use-package bug-reference              ; Turn bug refs into browsable buttons
  :defer t
  :init
  (add-hook 'prog-mode-hook #'bug-reference-prog-mode)
  (add-hook 'text-mode-hook #'bug-reference-mode))

(use-package goto-addr                  ; Make links clickable
  :defer t
  :bind (("C-c t a" . goto-address-mode)
         ("C-c t A" . goto-address-prog-mode))
  :init
  (add-hook 'prog-mode-hook #'goto-address-prog-mode)
  (add-hook 'text-mode-hook #'goto-address-mode))

(use-package eww                        ; Emacs' built-in web browser
  :bind (("C-c a w b" . eww-list-bookmarks)
         ("C-c a w w" . eww)
         ("C-c a w u" . eww-browse-url)))

(use-package sx                         ; StackExchange client for Emacs
  :ensure t
  :bind (("C-c a S a" . sx-ask)
         ("C-c a S s" . sx-tab-all-questions)
         ("C-c a S q" . sx-tab-all-questions)
         ("C-c a S f" . sx-tab-all-questions)
         ("C-c a S n" . sx-tab-newest)))

(use-package sx-compose                 ; Write questions/answers for Stack Exchange
  :ensure sx
  :defer t
  :config
  ;; Don't fill in SX questions/answers, and use visual lines instead.  Plays
  ;; more nicely with the website.
  (add-hook 'sx-compose-mode-hook #'turn-off-auto-fill)
  (add-hook 'sx-compose-mode-hook #'visual-line-mode)
  (add-hook 'sx-compose-mode-hook
            #'lunaryorn-whitespace-style-no-long-lines)

  ;; Clean up whitespace before sending questions
  (add-hook 'sx-compose-before-send-hook
            (lambda () (whitespace-cleanup) t))

  (bind-key "M-q" #'ignore sx-compose-mode-map))

(use-package sx-question-mode           ; Show Stack
  :ensure sx
  :defer t
  ;; Display questions in the same window
  :config (setq sx-question-mode-display-buffer-function #'switch-to-buffer))


;;; Fun
(use-package zone                       ; Emacs screen saver
  :ensure shut-up
  :commands (zone-when-idle)
  :bind ("C-c z" . zone)
  :init
  (setq zone-timeout 30)
  (defun zone-when-idle-and-focused ()
    "Use with `focus-in-hook' to only zone when our focus is back in Emacs."
    (zone-when-idle (or zone-timeout 30)))
  (defun zone-nodoze ()
    "Use with `focus-out-hook' so we don't zone when our focus is elsewhere."
    (let ((inhibit-message t)) (zone-leave-me-alone)))
  (add-hook 'focus-in-hook #'zone-when-idle-and-focused)
  (add-hook 'focus-out-hook #'zone-nodoze)
  (zone-when-idle zone-timeout))

(use-package zone-nyan                  ; Not exactly useful but <3
  :defer t
  :ensure t
  :after zone
  :config (setq zone-programs (vconcat [zone-nyan] zone-programs)))


;;; Online Help
(use-package find-func                  ; Find function/variable definitions
  :bind (("C-c h F"   . find-function)
         ("C-c h 4 F" . find-function-other-window)
         ("C-c h K"   . find-function-on-key)
         ("C-c h V"   . find-variable)
         ("C-c h 4 V" . find-variable-other-window)))

(use-package info                       ; Info manual viewer
  :defer t
  :config
  ;; Fix the stupid `Info-quoted' face.  Courier is an abysmal face, so go back
  ;; to the default face.
  (set-face-attribute 'Info-quoted nil :family 'unspecified
                      :inherit font-lock-type-face))

(use-package niceify-info               ; Prettify Info rendering
  :disabled t
  :ensure t
  ;; Adds emphasis to text between * and _, tries to fontify Emacs Lisp code,
  ;; tries to cross-reference symbol names in backticks, tries to fontify
  ;; headers, etc.q
  :init (add-hook 'Info-selection-hook #'niceify-info))

(use-package helm-man                   ; Man pages with Helm
  :ensure helm
  :defer t
  :bind (("C-c h m" . helm-man-woman)))

(use-package helm-info                  ; Info pages with Helm
  :ensure helm
  :bind (([remap info] . helm-info-at-point)
         ("C-c h e"    . helm-info-emacs)))

(use-package ansible-doc                ; Documentation lookup for Ansible
  :ensure t
  :defer t
  :init (add-hook 'yaml-mode-hook #'ansible-doc-mode)
  :diminish (ansible-doc-mode . " Ⓓ"))

(use-package dash
  :config (dash-enable-font-lock))

(use-package dash-at-point              ; Jump to Dash docset at point
  :ensure t
  :defer t
  :bind (("C-c h d" . dash-at-point)
         ("C-c h D" . dash-at-point-with-docset)))

(bind-key "C-c h b" #'describe-personal-keybindings)



;;;; https://github.com/skeeto/.emacs.d

(make-directory (locate-user-emacs-file "local") :no-error)
(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/etc")


;; Load local "packages"
(require 'unannoy)
(require 'imgur)
(require 'extras)
(require 'utility)

;; Some global keybindings
(global-set-key (kbd "C-j") #'join-line)
(global-set-key (kbd "M-g") #'goto-line)
(global-set-key (kbd "C-x C-k") #'compile)
(global-set-key (kbd "<f5>") (expose #'revert-buffer nil t))
(global-set-key (kbd "C-=") #'calc)

;;; auto-mode-alist entries
(add-to-list 'auto-mode-alist '("\\.mom$" . nroff-mode))
(add-to-list 'auto-mode-alist '("[._]bash.*" . shell-script-mode))
(add-to-list 'auto-mode-alist '("Cask" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("[Mm]akefile" . makefile-gmake-mode))
(add-to-list 'auto-mode-alist '("\\.mak$" . makefile-gmake-mode))
(add-to-list 'auto-mode-alist '("\\.make$" . makefile-gmake-mode))

;;; Individual package configurations

(use-package apt-sources-mode
  :defer t
  :mode "sources.list$")

(use-package asm-mode
  :config
  (add-hook 'asm-mode-hook (lambda () (setf indent-tabs-mode t
                                            tab-always-indent t))))

(use-package batch-mode
  :defer t)

(use-package calc
  :defer t
  :config (setf calc-display-trail nil))

(use-package make-mode
  :defer t
  :config
  (progn
    (bind-key "C-c C-c" 'compile makefile-mode-map)))

(use-package cc-mode
  :defer t
  :init
  (defun skeeto/c-hook ()
    (setf c-basic-offset 4)
    (c-set-offset 'case-label '+)
    (c-set-offset 'access-label '/)
    (c-set-offset 'label '/))
  :config
  (progn
    (define-key java-mode-map (kbd "C-x I") 'add-java-import)
    (add-hook 'c-mode-hook #'skeeto/c-hook)
    (add-hook 'c++-mode-hook #'skeeto/c-hook)
    (add-to-list 'c-default-style '(c-mode . "k&r"))
    (add-to-list 'c-default-style '(c++-mode . "k&r"))
    (add-hook 'java-mode-hook
              (lambda ()
                (setq c-basic-offset 2
                      tab-width 2
                      indent-tabs-mode nil)))

    (defun java-build (&optional arg)
      (interactive)
      (save-buffer)
      (let ((compilation-read-command nil))
        (projectile-compile-project nil)))
    (bind-key "C-<return>" 'java-build java-mode-map)
    (bind-key "C-c C-c" 'compile java-mode-map)))

;; (use-package cc-mode
;;   :config
;;   (progn
;;     (add-hook 'c-mode-hook (lambda () (c-set-style "bsd")))
;;     (add-hook 'java-mode-hook (lambda () (c-set-style "bsd") (setq tab-width 2) (setq c-basic-offset 2) (setq indent-tabs-mode nil)))
;;     (setq tab-width 2)
;;     (setq c-basic-offset 2)))

(use-package clojure-mode
  :defer t
  :ensure t
  :mode "\\.cljs$")

(use-package comint
  :defer t
  :config
  (progn
    (define-key comint-mode-map (kbd "<down>") #'comint-next-input)
    (define-key comint-mode-map (kbd "<up>") #'comint-previous-input)
    (define-key comint-mode-map (kbd "C-n") #'comint-next-input)
    (define-key comint-mode-map (kbd "C-p") #'comint-previous-input)
    (define-key comint-mode-map (kbd "C-r") #'comint-history-isearch-backward)
    (setf comint-prompt-read-only t
          comint-history-isearch t)))

(use-package compile-bind
  :demand t
  :bind (("C-h g" . compile-bind-set-command)
         ("C-h G" . compile-bind-set-root-file))
  :config
  (progn
    (setf compilation-always-kill t
          compilation-scroll-output 'first-error
          compile-bind-command (format "make -kj%d" (numcores)))
    (compile-bind* (current-global-map)
                   ("C-x c" ""
                    "C-x r" 'run
                    "C-x t" 'test
                    "C-x C" 'clean))))

(use-package counsel
  :ensure t)

(use-package dabbrev
  :defer t
  :init (setf abbrev-file-name (locate-user-emacs-file "local/abbrev_defs"))
  :config (setf dabbrev-case-fold-search nil))

(use-package diff-mode
  :defer t
  :config (add-hook 'diff-mode-hook #'read-only-mode))

(use-package elfeed
  :ensure t
  :bind ("C-x w" . elfeed)
  :init (setf url-queue-timeout 30)
  :config
  (require 'feed-setup)
  (setf bookmark-default-file (locate-user-emacs-file "local/bookmarks")))

(use-package erc
  :defer t
  :config
  (when (eq 0 (string-match "wello" (user-login-name)))
    (setf erc-nick "skeeto")))

(use-package eshell
  :bind (([f1] . eshell-as)
         ("A-e" . eshell))
  :init
  (setf eshell-directory-name (locate-user-emacs-file "local/eshell"))
  (add-hook 'eshell-first-time-mode-hook
            (lambda ()
              (add-to-list 'eshell-visual-commands "htop")))
  :config
  (add-hook 'eshell-mode-hook ; Bad, eshell, bad!
            (lambda ()
              (define-key eshell-mode-map (kbd "<f1>") #'quit-window)))
  (setq eshell-history-size 5000)
  (setq eshell-save-history-on-exit t)

  (use-package nyan-prompt
    :init (add-hook 'eshell-load-hook 'nyan-prompt-enable)))

(use-package ido
  :init
  (progn
    (setq ido-enable-prefix nil
          ido-enable-flex-matching t
          ido-create-new-buffer 'always
          ido-use-filename-at-point 'guess
          ido-max-prospects 10
          ido-save-directory-list-file (expand-file-name "ido.last" user-cache-directory)
          ido-default-file-method 'selected-window
          ido-auto-merge-work-directories-length -1
          ido-everywhere t)
    (ido-mode +1)))

;; (use-package ido-vertical-mode
;;   :init (ido-vertical-mode 1))

(use-package flx
  :ensure t)

(use-package flx-ido
  :after (ido flx)
  :init
  (progn
    (use-package ido
      :config
      (progn
        ;; disable ido faces to see flx highlights
        (setq ido-use-faces nil)
        (flx-ido-mode +1)))))

(use-package gamegrid
  :defer t
  :init
  (setf gamegrid-user-score-file-directory (locate-user-emacs-file "games")))

(use-package ggtags
  :ensure t
  :defer t
  :init
  (progn
    (add-hook 'c-mode-common-hook
              (lambda ()
                (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                  (ggtags-mode 1))))))

(use-package glsl-mode
  :ensure t
  :mode ("\\.fs$" "\\.vs$"))

(use-package gnuplot-mode
  :ensure t
  :defer t)

(use-package help-mode
  :defer t
  :config
  (define-key help-mode-map (kbd "f") #'push-first-button))

(use-package impatient-mode
  :defer t
  :ensure t
  :config
  (defun imp-markdown-filter (in)
    (let ((out (current-buffer)))
      (with-current-buffer in
        (markdown out))))
  (push (cons 'markdown-mode #'imp-markdown-filter)
        imp-default-user-filters))

(use-package javadoc-lookup
  :ensure t
  :defer t
  :bind ("C-h j" . javadoc-lookup)
  :config
  (ignore-errors
    (setf javadoc-lookup-cache-dir (locate-user-emacs-file "local/javadoc"))))

(use-package jekyll
  :demand t
  :functions httpd-send-header
  :config
  (progn
    (setf jekyll-home "~/src/skeeto.github.com/")
    (when (file-exists-p jekyll-home)
      (require 'simple-httpd)
      (setf httpd-root (concat jekyll-home "_site"))
      (ignore-errors
        (httpd-start)
        (jekyll/start))
      (defservlet robots.txt text/plain ()
        (insert "User-agent: *\nDisallow: /\n")))))

(use-package lisp-mode
  :defer t
  :config
  (progn
    (defun ert-all ()
      (interactive)
      (ert t))
    (defun ielm-repl ()
      (interactive)
      (pop-to-buffer (get-buffer-create "*ielm*"))
      (ielm))
    ;; (define-key emacs-lisp-mode-map (kbd "C-x r")   #'ert-all)
    (define-key emacs-lisp-mode-map (kbd "C-c C-z") #'ielm-repl)
    (define-key emacs-lisp-mode-map (kbd "C-c C-k") #'eval-buffer*)
    (defalias 'lisp-interaction-mode 'emacs-lisp-mode)
    (font-lock-add-keywords
     'emacs-lisp-mode
     `((,(concat "(\\(\\(?:\\(?:\\sw\\|\\s_\\)+-\\)?"
                 "def\\(?:\\sw\\|\\s_\\)*\\)\\_>"
                 "\\s-*'?" "\\(\\(?:\\sw\\|\\s_\\)+\\)?")
        (1 'font-lock-keyword-face)
        (2 'font-lock-function-name-face nil t)))
     :low-priority)))

(use-package lua-mode
  :defer t
  :ensure t
  :config
  (require 'lua-extras)
  (setf lua-default-application "luajit"
        lua-always-show nil)
  (define-key lua-mode-map (kbd "C-x C-e") #'lua-send-current-line)
  (define-key lua-mode-map (kbd "C-M-x")   #'lua-send-defun)
  (define-key lua-mode-map (kbd "C-c C-k") #'skeeto/lua-send-buffer)
  (define-key lua-mode-map (kbd "C-c C-z") #'skeeto/lua-toggle-process-buffer)
  (add-function :after (symbol-function 'lua-start-process)
                #'skeeto/lua-add-filter))

(use-package memoize
  :defer t
  :ensure t)

(use-package message
  :defer t
  :config (define-key message-mode-map (kbd "C-c C-s") nil)) ; super annoying

(use-package nasm-mode
  :ensure t
  :defer t
  :mode ("\\.nasm$" "\\.asm$" "\\.s$")
  :config
  (add-hook 'nasm-mode-hook (lambda () (setf indent-tabs-mode t))))

(use-package notmuch
  :ensure t
  :bind ("C-x m" . notmuch)
  :config
  (progn
    (require 'email-setup)
    (require 'notmuch-address)
    (define-key notmuch-common-keymap "q" (expose #'kill-buffer))
    (setf notmuch-command "notmuch-remote"
          message-send-mail-function 'smtpmail-send-it
          message-kill-buffer-on-exit t
          smtpmail-smtp-server "localhost"
          smtpmail-smtp-service 2525
          notmuch-address-command "addrlookup-remote"
          notmuch-fcc-dirs nil
          notmuch-search-oldest-first nil
          notmuch-archive-tags '("-inbox" "-unread" "+archive")
          hashcash-path (executable-find "hashcash"))
    (custom-set-faces
     '(notmuch-search-subject ((t :foreground "#afa")))
     '(notmuch-search-date    ((t :foreground "#aaf")))
     '(notmuch-search-count   ((t :foreground "#777"))))
    (setq notmuch-hello-sections
          '(notmuch-hello-insert-header
            notmuch-hello-insert-saved-searches
            notmuch-hello-insert-search))))

(use-package octave
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))
  (setf octave-block-offset 4))

(use-package paredit
  :ensure t
  :defer t
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
    (add-hook 'lisp-mode-hook #'paredit-mode)
    (add-hook 'scheme-mode-hook #'paredit-mode)
    (add-hook 'ielm-mode-hook #'paredit-mode)
    (add-hook 'clojure-mode-hook #'paredit-mode))
  :config (define-key paredit-mode-map (kbd "C-j") #'join-line))

(use-package paren
  :config (show-paren-mode))

(use-package pov-mode
  :defer t
  :ensure t)

(use-package ps-print
  :defer t
  :config (setf ps-print-header nil))

(use-package simple-httpd
  :ensure t
  :defer t
  :functions httpd-send-header
  :config
  (progn
    (defservlet uptime "text/plain" ()
      (princ (emacs-uptime)))
    (defun httpd-here ()
      (interactive)
      (setf httpd-root default-directory))
    (defadvice httpd-start (after httpd-query-on-exit-flag activate)
      (let ((httpd-process (get-process "httpd")))
        (when httpd-process
          (set-process-query-on-exit-flag httpd-process nil))))))

(use-package skewer-mode
  :ensure t
  :defer t
  :init (skewer-setup)
  :config
  (progn
    (setf skewer-bower-cache-dir (locate-user-emacs-file "local/skewer"))
    (define-key skewer-mode-map (kbd "C-c $")
      (expose #'skewer-bower-load "jquery" "1.9.1"))))

(use-package skewer-repl
  :defer t
  :config (define-key skewer-repl-mode-map (kbd "C-c C-z") #'quit-window))

(use-package swiper
  :ensure t
  :defer nil
  :init (ivy-mode 1)
  :config
  (setf ivy-wrap t
        ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
  (define-key ivy-minibuffer-map (kbd "C-s") #'ivy-next-line)
  (define-key ivy-minibuffer-map (kbd "C-r") #'ivy-previous-line)
  (define-key ivy-minibuffer-map (kbd "C-l")
    (lambda ()
      "Be like like Helm."
      (interactive)
      (unless (eql (char-before) ?/)
        (ivy-backward-kill-word))
      (ivy-backward-delete-char))))

(use-package tramp
  :defer t
  :config
  (setf tramp-persistency-file-name
        (concat temporary-file-directory "tramp-" (user-login-name))))

(use-package uuid-simple
  :demand t
  :bind ("C-x !" . uuid-insert)
  :config (random (make-uuid)))

(use-package vimrc-mode
  :ensure t
  :defer t)

(use-package websocket
  :ensure t
  :defer t)

(use-package x86-lookup
  :ensure t
  :defer t
  :bind ("C-h x" . x86-lookup)
  :functions x86-lookup-browse-pdf-evince
  :config
  (let ((pdf-regexp "^64-ia-32-.*-instruction-set-.*\\.pdf$")
        (pdf-dir "~/doc/"))
    (setf x86-lookup-browse-pdf-function #'x86-lookup-browse-pdf-evince
          x86-lookup-pdf (ignore-errors
                           (car (directory-files pdf-dir t pdf-regexp))))))



;;;; aculich-specific

(use-package color-moccur)

(use-package direx
  :defer t
  :bind (("A-C-j" . dired-jump))
  :config
  (progn

    (defun direx:jump ()
      (interactive)
      (let* ((item (direx:item-at-point!))
             (file (direx:item-tree item))
             (filename (direx:file-full-name file))
             (default-directory (file-name-directory filename))
             (name (file-name-nondirectory filename)))
        (dired-jump nil filename)))

    (defun direx-project:jump-to-project-root-or-dired ()
      (interactive)
      (condition-case err
          (direx-project:jump-to-project-root)
        (error
         (dired (file-name-directory
                 (or buffer-file-name
                     default-directory))))))

    (bind-keys
     :map dired-mode-map
     ("A-C-j" . direx-project:jump-to-project-root-or-dired))

    (bind-keys
     :map direx:direx-mode-map
     ("A-C-j" . direx:jump)
     ("s" . isearch-forward)
     ("r" . isearch-backward))))

(use-package direx-grep
  :defer t)

(use-package dirtree
  :defer t)

(use-package dirtree-prosjekt
  :defer t)

(use-package eldoc-eval
;;  :diminish eldoc-minor-mode
;;  :diminish eldoc-mode
  :bind (("M-:" . eldoc-eval-expression))
  :config
  (progn
    (eldoc-in-minibuffer-mode 1)))

(use-package eldoc-extension
  :defer t)

(use-package flx-isearch)

(use-package helm-git-grep
  :defer t
  :config
  (progn
    (setq helm-git-grep-candidate-number-limit 1000)))

(use-package hideshow
  :init
  (progn
    (add-hook 'prog-mode-hook 'hs-minor-mode))
  :config
  (progn
    (bind-keys :map hs-minor-mode-map
               ("C-c C-h C-b"    . hs-hide-block   )
               ("C-c C-h C-s"    . hs-show-block   )
               ("C-c C-h C-h"    . hs-hide-all     )
               ("C-c C-h C-a"    . hs-show-all     )
               ("C-c C-h C-l"    . hs-hide-level   )
               ("C-c C-h C-c"    . hs-toggle-hiding))))

(use-package ido-at-point)

(use-package ido-load-library)

(use-package ido-select-window)

(use-package ido-sort-mtime)

(use-package ido-ubiquitous
  :init
  (progn
    (use-package ido
      :init
      (progn
        (ido-ubiquitous-mode +1)))))

(use-package ido-vertical-mode)

(use-package idomenu
  :bind ("M-i" . idomenu))

(use-package loccur)

(use-package narrowed-page-navigation
  :defer t
  :bind (([remap forward-page] . narrowed-page-navigation-next)
         ([remap backward-page] . narrowed-page-navigation-previous)))

(use-package noccur)

(use-package occur-context-resize)

(use-package osx-lib)

(use-package paredit-everywhere)

(use-package paredit-menu)

(use-package paren-face)

(use-package popwin
  :commands popwin-mode
  :demand t
  :init (popwin-mode 1)
  :config
  (progn
    (defvar popwin:special-display-config-backup popwin:special-display-config)
    (setq display-buffer-function 'popwin:display-buffer)

    ;; basic
    (push '("*Help*" :stick t :noselect t) popwin:special-display-config)
    (push '("*helm world time*" :stick t :noselect t) popwin:special-display-config)
    (push '("*Pp Eval Output*" :stick t) popwin:special-display-config)
    ;;      ("*Pp Macroexpand Output*" :noselect t)
    ;;      ("*Miniedit Help*" :noselect t)
    ;;      (help-mode)

    ;; quickrun
    (push '("*quickrun*" :stick t) popwin:special-display-config)

    ;; dictionaly
    (push '("*dict*" :stick t) popwin:special-display-config)
    (push '("*sdic*" :stick t) popwin:special-display-config)

    ;; popwin for slime
    (push '(slime-repl-mode :stick t) popwin:special-display-config)
    ;;      (sldb-mode :stick t)
    ;;      ("*slime-apropos*")
    ;;      ("*slime-macroexpansion*")
    ;;      ("*slime-description*")
    ;;      ("*slime-compilation*" :noselect t)
    ;;      ("*slime-xref*")
    ;;      (slime-repl-mode)
    ;;      (slime-connection-list-mode)

    ;; man
    (push '(Man-mode :stick t :height 20) popwin:special-display-config)

    ;; Elisp
    (push '("*ielm*" :stick t) popwin:special-display-config)
    (push '("*eshell pop*" :stick t) popwin:special-display-config)

    ;; pry
    (push '(inf-ruby-mode :stick t :height 20) popwin:special-display-config)

    ;; python
    (push '("*Python*"   :stick t) popwin:special-display-config)
    (push '("*Python Help*" :stick t :height 20) popwin:special-display-config)
    (push '("*jedi:doc*" :stick t :noselect t) popwin:special-display-config)

    ;; Haskell
    (push '("*haskell*" :stick t) popwin:special-display-config)
    (push '("*GHC Info*") popwin:special-display-config)

    ;; sgit
    (push '("*sgit*" :position right :width 0.5 :stick t)
          popwin:special-display-config)

    ;; magit
    (push '("*magit-process*" :stick t) popwin:special-display-config)

    ;; git-gutter
    (push '("*git-gutter:diff*" :position right :width 0.5 :stick t :noselect t) popwin:special-display-config)
    ;;      ("*vc-diff*")
    ;;      ("*vc-change-log*")

    ;; direx
    (push '(direx:direx-mode :position left :width 40 :dedicated t)
          popwin:special-display-config)

    (push '("*Occur*" :stick t) popwin:special-display-config)
    ;;      (grep-mode :noselect t)
    ;;      (occur-mode :noselect t)

    ;; prodigy
    (push '("*prodigy*" :stick t) popwin:special-display-config)

    ;; malabar-mode
    (push '("*Malabar Compilation*" :stick t :height 30)
          popwin:special-display-config)

    ;; org-mode
    (push '("*Org tags*" :stick t :height 30)
          popwin:special-display-config)

    ;; Completions
    (push '("*Completions*" :stick t :noselect t) popwin:special-display-config)
    ;;      (completion-list-mode :noselect t)

    ;; ggtags
    (push '("*ggtags-global*" :stick t :noselect t :height 30) popwin:special-display-config)

    ;; async shell commands
    (push '("*Async Shell Command*" :regexp nil :position right :width 80 :noselect t :dedicated t :stick t) popwin:special-display-config)
    (push '("*Shell Command Output*" :regexp nil :position right :width 80 :noselect t :dedicated t :stick t) popwin:special-display-config)
    (push '("*compilation*" :regexp nil :position right :width 80 :noselect t :dedicated t :stick t) popwin:special-display-config)
    ;;      (compilation-mode :noselect t)
    ;;      (" *undo-tree*" :width 60 :position right)

    (defun my/popup-downloads ()
      "Pop up the downloads buffer (3rd eshell buffer for me"
      (interactive)
      (popwin:popup-buffer "*eshell*<3>"))

    ;; eshell 3 is always my "download stuff" buffer
    (global-set-key (kbd "C-x M-d") #'my/popup-downloads)))

(use-package powershell
  :defer t)

;; use recompile-on-save instead of ascmd auto-shell-command
(use-package recompile-on-save
  :disabled t
  :init
  (progn
    (recompile-on-save-mode 1)
    (recompile-on-save-advice compile)
    ))

(use-package smex
  :disabled t
  :bind (("M-x"     . smex)
         ("M-X"     . smex-major-mode-commands)
         ("C-c M-x" . execute-extended-command))
  :init
  (progn
    (setq smex-save-file (expand-file-name "smex-items" user-cache-directory))
    (smex-initialize)

    ;; (defalias 'describe-bindings 'helm-descbinds)
    (defun smex-prepare-ido-bindings ()
      (bind-keys
       :map ido-completion-map
       ("TAB"     . minibuffer-complete)
       ("C-h f"   . smex-describe-function)
       ("C-h w"   . smex-where-is)
       ("C-h C-h" . describe-bindings)
       ("C-h C-f" . smex-find-function)
       ("M-."     . smex-find-function)
       ("C-a"     . move-beginning-of-line))
      )
    ))

(use-package ssh
  :defer t)

(use-package vagrant
  :defer t
  :config
  (progn
    (use-package vagrant-tramp)))

(use-package wand
  :bind (("C-<return>" . wand:execute))
  :config
  (progn
    (setq wand:*rules*
          (list (wand:create-rule :match "\\$ "
                                  :capture :after
                                  :action async-shell-command)
                (wand:create-rule :match "https?://"
                                  :capture :whole
                                  :action browse-url-at-point)
                (wand:create-rule :match "file:"
                                  :capture :after
                                  :action toolbox:open-file)
                (wand:create-rule :match "#> "
                                  :capture :after
                                  :action ~add-bracket-and-eval)))

    ;; (wand:add-rule-by-pattern :match "\\$ "
    ;;                           :capture :after
    ;;                           :action popup-shell-command)
    ))




(use-package org
  :mode ("\\.org$" . org-mode)
  :bind (("C-c l" . org-store-link)
         ("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-<tab>" . other-window)
         ("C-S-<tab>" . other-window-previous)
         ;; ("C-'" . org-cycle-agenda-files)
         ("C-c b" . org-iswitchb))
  :config (progn (load "dot-org")

                 ;;(setq org-default-notes-file (concat org-directory "/notes.org"))
                 (setq org-tags-column -90)
                 (setq org-capture-bookmark t)
                 (setq org-refile-use-outline-path 'file)
                 (setq org-startup-folded 'showeverything)
                 (setq org-log-done 'note)
                 ;; When clocking in, just use the time from the last clocked out
                 ;; item.
                 (setq org-clock-continuously t)

                 ;; When creating or completing a TODO, record the timestamps.
                 (setq org-log-done 'time)

                 ;; Syntax highlight org code snippets.
                 (setq org-src-fontify-natively t)

                 (define-key org-mode-map (kbd "C-M-\\") 'org-indent-region)
                 (add-hook 'org-mode-hook
                           '(lambda ()
                              (setq mode-name " ꙮ ")))
                 (add-hook 'org-after-todo-state-change-hook
                           (lambda ()
                             (when (string= org-state "TODO")
                               (save-excursion
                                 (org-back-to-heading)
                                 (org-expiry-insert-created)))))
                 ;; Show drawers, e.g. :PROPERTIES:, when we expand a heading.
                 ;; See http://emacs.stackexchange.com/a/22540/304
                 (remove-hook 'org-cycle-hook #'org-cycle-hide-drawers)

                 ;; Don't underline dates, it's distracting.
                 (custom-set-faces
                  '(org-date ((((class color)) (:underline nil))) t))
                 ))

(use-package org-bullets
  :after org
  :commands org-bullets-mode
  :init (add-hook 'org-mode (lambda () (org-bullets-mode 1))))

(use-package org-cliplink
  :after org
  :bind ("C-M-y" . org-cliplink))

(use-package org-trello
  :after org
  :commands org-trello-mode
  :init (add-hook 'org-mode (lambda () (org-trello-mode 1))))

(use-package org-dashboard
  :after org
  :commands org-dashboard-display)

(use-package org-expiry
  :after org
  :defer t)

(use-package org-autolist
  :after org
  :commands org-autolist-mode)



(use-package custom
  :config (setq custom-unlispify-tag-names nil))


(use-package drag-stuff
  :diminish drag-stuff-mode
  :init (drag-stuff-global-mode 1)
  :bind (("M-N" . drag-stuff-down)
         ("M-P" . drag-stuff-up)))

(use-package misc
  :bind ("M-z" . zap-up-to-char))



(use-package git-messenger
  :defer t
  :bind (("C-x v p" . git-messenger:popup-message))
  :config (bind-keys :map git-messenger-map
                     ("m" . magit-blame-mode)))

(use-package ace-jump-mode
  :bind ("C-c SPC" . ace-jump-mode))

(use-package ace-isearch
  :disabled t
  :init
  (progn
    (setq ace-isearch-submode 'ace-jump-word-mode)
    ;; (setq ace-isearch-submode 'ace-jump-line-mode)
    (global-ace-isearch-mode +1)))


(use-package aggressive-indent
  :disabled t
  :init
  (progn
    (global-aggressive-indent-mode 1)
    (add-to-list 'aggressive-indent-excluded-modes 'html-mode)))

(use-package ess
  :defer t
  :init
  (progn
    (add-to-list 'safe-local-variable-values '(outline-minor-mode))
    (add-to-list 'safe-local-variable-values '(whitespace-style face tabs spaces trailing lines space-before-tab::space newline indentation::space empty space-after-tab::space space-mark tab-mark newline-mark))
    ))

(use-package ruby-mode
  :defer t
  :init
  (progn
    (use-package rvm
      :defer t
      :init (rvm-use-default)
      :config (setq rvm-verbose nil))
    (use-package ruby-tools
      :defer t)
    (use-package rhtml-mode
      :defer t
      :mode (("\\.rhtml$" . rhtml-mode)
             ("\\.html\\.erb$" . rhtml-mode)))
    (use-package rinari
      :defer t
      :init (global-rinari-mode 1)
      :config
      (progn
        (setq ruby-insert-encoding-magic-comment nil)
        (add-hook 'magit-mode-hook 'rinari-launch)))
    (use-package rspec-mode
      :defer t
      :config
      (progn
        (setq rspec-use-rvm t)
        (setq rspec-use-rake-when-possible nil)
        (defadvice rspec-compile (around rspec-compile-around activate)
          "Use BASH shell for running the specs because of ZSH issues."
          (let ((shell-file-name "/bin/bash"))
            ad-do-it)))))
  :config
  (progn
    (add-hook 'ruby-mode-hook 'rvm-activate-corresponding-ruby)
    (setq ruby-deep-indent-paren nil))
  :bind (("C-M-h" . backward-kill-word)
         ("C-M-n" . scroll-up-five)
         ("C-M-p" . scroll-down-five))
  :mode (("\\.rake$" . ruby-mode)
         ("\\.gemspec$" . ruby-mode)
         ("\\.ru$" . ruby-mode)
         ("Rakefile$" . ruby-mode)
         ("Gemfile$" . ruby-mode)
         ("Capfile$" . ruby-mode)
         ("Guardfile$" . ruby-mode)))

(use-package feature-mode
  :defer t
  :mode ("\\.feature$" . feature-mode)
  :config
  (add-hook 'feature-mode-hook
            (lambda ()
              (electric-indent-mode -1))))

(use-package editorconfig
  :init
  (progn
    (remove-hook 'find-file-hook 'edconf-find-file-hook)
    ))

(use-package prodigy
  :init (progn
          (add-hook 'prodigy-mode-hook
                    (lambda ()
                      (setq-local show-trailing-whitespace nil))))
  :demand t
  :bind ("C-x p" . prodigy))

(use-package discover
  :init (global-discover-mode 1))

(use-package ert-async
  :defer t
  :config (progn
            (remove-hook 'emacs-lisp-mode-hook 'ert--activate-font-lock-keywords)
            (add-hook 'emacs-lisp-mode-hook 'ert-async-activate-font-lock-keywords)))

(use-package cl-lib-highlight
  :init (cl-lib-highlight-initialize))

(use-package httprepl
  :defer t)

;; (use-package ack-and-a-half)

(use-package ag
  :defer t)

(use-package path-headerline-mode
  :init
  (progn
    (path-headerline-mode +1)))

(use-package epa-file
    :defer t)

;;; http://metasandwich.com/2013/01/19/emacs-config-youre-doing-it-wrong/
;;; http://www.fieggen.com/shoelace/ianknot.htm
;; (defun imenu-elisp-sections ()
;;   (setq imenu-prev-index-position-function nil)
;;   (add-to-list 'imenu-generic-expression '("Sections" "^;;;; \\(.+\\)$" 1) t))
;; (add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)




(use-package google-this
  :commands (google-this-mode-submap)
  :bind (("C-x g" . google-this-mode-submap)))

(use-package dired-toggle-sudo
  :defer t
  :bind (("C-x s" . dired-toggle-sudo)))

(use-package save-visited-files
  :disabled t
  :init
  (progn
    (setq-default save-visited-files-location (expand-file-name "emacs-visited-files" user-cache-directory))
    (turn-on-save-visited-files-mode)))

(use-package pos-tip)

(use-package register-channel
  :defer t
  :init
  (progn
    (register-channel-mode 1)))

(use-package url
  :defer t
  :init
  (progn
    (setq-default url-configuration-directory (expand-file-name "url" user-cache-directory))
    (setq-default url-cache-directory (expand-file-name "cache" url-configuration-directory))))



(use-package crux
  :ensure t)

(when (eq system-type 'darwin)
  (load (expand-file-name "osx.el" user-emacs-directory)))

(defvar user-keys-file (expand-file-name "global-keys.el" user-emacs-directory))
(load user-keys-file)
(defvar user-quick-find-file (expand-file-name "quick-find.el" user-emacs-directory))
(load user-quick-find-file)
(setenv "EMACS" "/Applications/Emacs.app/Contents/MacOS/Emacs")



(benchmark-init/show-durations-tree)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here
