;; Set Erlang root directory
(if (not (boundp 'erlang-root-dir))
    (setq erlang-root-dir "~/.erln8.d/otps/19.0.5/dist/"))

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory)))

;; Setup MELPA
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa-stable" . "http://stable.melpa.org/packages/")
   t)
  (package-initialize))

;; Remove backup files that are older than 1 week
(message "Deleting old backup files...")
(let ((week (* 60 60 24 7))
      (current (float-time (current-time))))
  (dolist (file (directory-files temporary-file-directory t))
    (when (and (backup-file-name-p file)
	       (> (- current (float-time (nth 5 (file-attributes file))))
		  week))
      (message "%s" file)
      (delete-file file))))

;; Load Erlang-Mode configuration
(add-to-list 'load-path "~/.emacs.d/erlang-rig/")
(require 'erlang-config)

;; Disable menu bar
(menu-bar-mode -1)

;; Load Enhanced Ruby Mode
;;(add-to-list 'load-path "~/.emacs.d/enhanced-ruby-mode")
;;(autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for Ruby files" t)

;;(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
;;(add-to-list 'auto-mode-alist '("Rakefile$" . enh-ruby-mode))
;;(add-to-list 'auto-mode-alist '("Gemfile$" . enh-ruby-mode))
;;(add-to-list 'auto-mode-alist '("Capfile$" . enh-ruby-mode))
;;(add-to-list 'auto-mode-alist '("\\.gemspec$" . enh-ruby-mode))

;;(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))

;; Load Robe
;(add-hook 'ruby-mode-hook 'robe-mode)
;;(add-hook 'enh-ruby-mode-hook 'robe-mode)

;; Load RbEnv
;;(setq rbenv-installation-dir "/usr/local/opt/rbenv")

;; Load color theme
(add-to-list 'custom-theme-load-path "~/emacs.d/themes/")
(load-theme 'tango t)

;; Highlight extra whitespace and long lines
(require 'whitespace)
(setq whitespace-style '(face empty lines-tail trailing))
(global-whitespace-mode t)

;; Configure auto-insert-mode
(auto-insert-mode)
(setq auto-insert-query nil)

;; Automatically add Apache 2.0 license to Erlang files
(define-auto-insert "\.[e|h]rl" "apache-license.erl")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (tango-dark))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
