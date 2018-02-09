;; No variable for $HOME. Define here.
(defconst user-home-directory
  (getenv "HOME")
  "The user's home directory.")

;; Allow for using 'y' or 'n' instead of having to type 'yes' and 'no' when
;; asked by Emacs
(defalias 'yes-or-no-p 'y-or-n-p)

;; Put backup files in tmp and let the OS deal with it. Use version control.
(setq make-backup-files nil)
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Always require a file to end in a newline. Makes git happier.
(setq require-final-newline t)

;; Setup MELPA and Maramalade
(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

;; Add default packages
(setq package-archive-enable-alist
      '(("melpa" magit)))

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

;; Configure Flymake
(defun flymake-keys ()
  "Add keys for navigation between errors found by Flymake."
  (local-set-key (kbd "C-c C-n") 'flymake-goto-next-error)
  (local-set-key (kbd "C-c C-p") 'flymake-goto-prev-error))
(add-hook 'flymake-mode-hook 'flymake-keys)

;; Configure Erlang executables
(defconst my/erlang/root
  "~/.kerl/"
  "The root directory where various versions of Erlang are installed. We currently use erln8 to manage and install Erlang versions.")

(defvar my/erlang/latest
  "20.0.4"
  "The latest version of Erlang installed.")

(defvar my/erlang/latest/tools-version
  "2.10.1"
  "The version of the 'tools' application in the latest Erlang.")

(defun my/erlang/latest/root ()
  "Computes the root directory of the latest Erlang installed."
  (expand-file-name my/erlang/latest my/erlang/root))

(defun my/erlang/latest/bin ()
  "Computes the latest Erlang's executable directory."
  (expand-file-name "bin" (my/erlang/latest/root)))

(defun my/erlang/latest/lib ()
  "Computes the latest Erlang's library directory."
  (expand-file-name "lib" (my/erlang/latest/root)))

(defun my/erlang/latest/man ()
  "Computes the latest Erlang's library directory."
  (expand-file-name "man" (my/erlang/latest/root)))

(defun my/erlang/latest/emacs ()
  "Computes the location of the OTP emacs mode."
  (expand-file-name "emacs" (expand-file-name (format "tools-%s" my/erlang/latest/tools-version) (my/erlang/latest/lib))))

;; Configure and start erlang-mode
(add-to-list 'load-path (my/erlang/latest/emacs))
(add-to-list 'exec-path (my/erlang/latest/bin))
(setq erlang-root-dir (my/erlang/latest/lib))

(require 'erlang-start)
(add-hook 'erlang-mode-hook 'whitespace-mode)
(setq erlang-font-lock-level 0)

;; Enable erlang-mode when editing rebar.conf
(add-to-list 'auto-mode-alist '("rebar.conf" . erlang-mode))

;; Configure Flymake for Erlang mode
;; (require 'erlang-flymake)
(setq erlang-flymake-command (expand-file-name "erlc" (my/erlang/latest/bin)))

(defun rebar3/erlang-flymake-get-include-dirs ()
  (append
   (erlang-flymake-get-include-dirs)
   (file-expand-wildcards (concat (erlang-flymake-get-app-dir) "_build/*/lib"))))

;;(setq erlang-flymake-get-include-dirs-function 'rebar3/erlang-flymake-get-include-dirs)

(defun rebar3/erlang-flymake-get-code-path-dirs ()
  (append
   (erlang-flymake-get-code-path-dirs)
   (file-expand-wildcards (concat (erlang-flymake-get-app-dir) "_build/*/lib/*/ebin"))))

;;(setq erlang-flymake-get-code-path-dirs-function 'rebar3/erlang-flymake-get-code-path-dirs)

(defun ebm-find-rebar-top-recr (dirname)
  (let* ((project-dir (locate-dominating-file dirname "rebar.config")))
    (if project-dir
        (let* ((parent-dir (file-name-directory (directory-file-name project-dir)))
               (top-project-dir (if (and parent-dir (not (string= parent-dir "/")))
                                    (ebm-find-rebar-top-recr parent-dir)
                                  nil)))
          (if top-project-dir
              top-project-dir
            project-dir))
      project-dir)))

(defun ebm-find-rebar-top ()
  (interactive)
  (let* ((dirname (file-name-directory (buffer-file-name)))
         (project-dir (ebm-find-rebar-top-recr dirname)))
    (if project-dir
        project-dir
      (erlang-flymake-get-app-dir))))

(defun ebm-directory-dirs (dir name)
  "Find all directories in DIR."
  (unless (file-directory-p dir)
    (error "Not a directory `%s'" dir))
  (let ((dir (directory-file-name dir))
        (dirs '())
        (files (directory-files dir nil nil t)))
    (dolist (file files)
      (unless (member file '("." ".."))
        (let ((absolute-path (expand-file-name (concat dir "/" file))))
          (when (file-directory-p absolute-path)
            (if (string= file name)
                (setq dirs (append (cons absolute-path
                                         (ebm-directory-dirs absolute-path name))
                                   dirs))
              (setq dirs (append
                          (ebm-directory-dirs absolute-path name)
                          dirs)))))))
    dirs))

(defun ebm-get-deps-code-path-dirs ()
  (ebm-directory-dirs (ebm-find-rebar-top) "ebin"))

(defun ebm-get-deps-include-dirs ()
  (ebm-directory-dirs (ebm-find-rebar-top) "include"))

(fset 'erlang-flymake-get-code-path-dirs 'ebm-get-deps-code-path-dirs)
(fset 'erlang-flymake-get-include-dirs-function 'ebm-get-deps-include-dirs)

;; Configure flymake-rust
(require 'flymake-rust)
(add-hook 'rust-mode-hook 'flymake-rust-load)
(add-hook 'rust-mode-hook 'whitespace-mode)

;; Configuration Markdown Mode
(add-to-list 'auto-mode-alist '(".md$" . gfm-mode))

;; Configure Ruby Mode
(add-hook 'ruby-mode-hook 'whitespace-mode)

;; Add and configuration YAML Mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-hook 'yaml-mode-hook
          '(lambda ()
             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;; Add java mode configurations
(require 'gradle-mode)
(add-hook 'java-mode-hook (lambda ()
                            (setq c-basic-offset 2
                                  tab-width 2
                                  indent-tabs-mode nil)))
(add-hook 'java-mode-hook 'whitespace-mode)
(add-hook 'java-mode-hook 'gradle-mode)

;; Add jsx major mode
(add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))

;; Edit JSON files in Javascript Mode
(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))

;; Edit .gradle files in Groovy Mode
(add-to-list 'auto-mode-alist '("\\.gradle$" . groovy-mode))
(add-to-list 'auto-mode-alist '("Jenkinsfile" . groovy-mode))

;; Configure Magit
(require 'magit)
(global-set-key (kbd "C-c g") 'magit-status)
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))
(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer."
  (interactive)
  (when (get-register :magit-fullscreen)
    (jump-to-register :magit-fullscreen)))

(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

;; Configure Dash
(require 'dash-at-point)
(global-set-key (kbd "C-c d") 'dash-at-point)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(haskell-mode-hook
   (quote
    (turn-on-haskell-indent turn-on-haskell-indentation turn-on-haskell-doc-mode turn-on-haskell-unicode-input-method)))
 '(package-selected-packages
   (quote
    (terraform-mode rjsx-mode gradle-mode elixir-mode flymake-rust rust-mode elm-mode scala-mode haskell-mode yaml-mode solarized-theme robe rbenv protobuf-mode markdown-mode magit groovy-mode go-mode flymake-ruby flymake-elixir expand-region dash-at-point))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
