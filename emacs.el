;; Set Erlang root directory
(if (not (boundp 'erlang-root-dir))
    (setq erlang-root-dir "~/.erln8.d/otps/18.0.1/dist/"))

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory)))

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

;; Load color theme
(load-theme 'tango-dark t)

;; Highlight extra whitespace and long lines
(require 'whitespace)
(setq whitespace-style '(face empty lines-tail trailing))
(global-whitespace-mode t)

;; Configure auto-insert-mode
(auto-insert-mode)
(setq auto-insert-directory "~/.emacs.d/templates/")
(setq auto-insert-query nil)

;; Automatically add Apache 2.0 license to Erlang files
(define-auto-insert "\.[e|h]rl" "apache-license.erl")

;; Enable git-status mode
(add-to-list 'load-path "/usr/local/share/git-core/contrib/emacs/")
(require 'git)
(require 'git-blame)
