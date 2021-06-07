;; Allow for using 'y' or 'n' instead of having to type 'yes' and 'no' when
;; asked by Emacs
(defalias 'yes-or-no-p 'y-or-n-p)

;; Put backup files in tmp and let the OS deal with it. Use version control.
(setq make-backup-files nil)
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Always require a file to end in a newline. Makes git happier.
(setq require-final-newline t)

;; Display line numbers
(global-display-line-numbers-mode)

;; Allow hash to be entered
(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))

;; Setup MELPA and Maramalade
(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))

;; Refresh package list
(when (not package-archive-contents)
  (package-refresh-contents))

;; Disable menus
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-scratch-message nil)

;; Load theme
(load-theme 'tango-dark t)

;; Highlight extra whitespace and long lines
(require 'whitespace)
(unless (member 'whitespace-mode prog-mode-hook)
  (add-hook 'prod-mode-hook 'whitespace-mode))
(global-set-key (kbd "C-c w") 'whitespace-cleanup)
(set-default 'indicate-empty-lines t)
(set-default 'indent-tabs-mode nil)
(setq whitespace-style
      '(face tabs newline tab-mark newlink-mark trailing
             lines-tail)
      whitespace-line-column 100)

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))

(global-set-key (kbd "C-c n") 'cleanup-buffer)

;; Add line and column numbers to status bar
(line-number-mode 1)
(column-number-mode 1)

;; Configure Magit
(require 'magit)
(global-set-key (kbd "C-c g") 'magit-status)
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defadvice magit-mode-quit-window (after magit-restore-screen activate)
  "Restores the previous window configuration and kills the magit buffer"
  (jump-to-register :magit-fullscreen))

(define-key magit-status-mode-map (kbd "q") 'magit-mode-quit-window)

;; Configure Dash
(require 'dash-at-point)
(global-set-key (kbd "C-c d") 'dash-at-point)

(require 'kotlin-mode-lexer)
(setq kotlin-tab-width 4)

;; Configure orgmode
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-startup-indented t)

;; Configure Helm
(require 'helm)
(helm-mode 1)
(require 'helm-ls-git)
(global-set-key (kbd "C-x C-d") 'helm-browse-project)
(global-set-key (kbd "C-x C-l") 'helm-ls-git-ls)
