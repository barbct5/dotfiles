;; Set Erlang root directory
(if (not (boundp 'erlang-root-dir))
    (setq erlang-root-dir "/opt/erlang/17.5"))

;; Load Erlang-Mode configuration
(add-to-list 'load-path "~/.emacs.d/erlang-rig/")
(require 'erlang-config)

;; Load color theme
(load-theme 'tango-dark t)

;; Highlight extra whitespace and long lines
(require 'whitespace)
(setq whitespace-style '(face empty lines-tail trailing))
(global-whitespace-mode t)

;; Add apache license to the beginning of new .erl and .hrl files
(auto-insert-mode)
(setq auto-insert-directory "~/.emacs.d/templates/")
(setq auto-insert-query nil)
(define-auto-insert "\.[e|h]rl" "apache-license.erl")

(add-to-list 'load-path "/usr/local/Cellar/git/2.4.3/share/git-core/contrib/emacs")
(require 'git)
(require 'git-blame)
