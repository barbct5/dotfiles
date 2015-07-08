(if
    (not (boundp 'erlang-root-dir))
    (message "Skipping erlang-mode: erlang-root-dir not defined. To hook up erlang mode, set erlang-root-dir in your .emacs file before the call to 'require my-config'.")
  (progn
    (set 'erlang-bin (concat erlang-root-dir "bin/"))
    (set 'erlang-lib (concat erlang-root-dir "lib/erlang/lib/"))
    (if
	(not (boundp 'erlang-mode-path))
	(set 'erlang-mode-path
	     (concat
	      erlang-lib
	      (file-name-completion "tools-" erlang-lib)
	      "emacs/erlang.el")))
    (if
	(and
	 (file-readable-p erlang-mode-path)
	 (file-readable-p erlang-bin))
	(progn
	  (message "Setting up erlang-mode")
	  (set 'exec-path (cons erlang-bin exec-path))
	  (set 'load-path (cons
			   (concat
			    erlang-lib
			    (file-name-completion "tools-" erlang-lib)
			    "emacs")
			   load-path))
	  (set 'load-path (cons (file-name-directory erlang-mode-path) load-path))
	  (require 'erlang-start))
      (message "Skipping erlang-mode: %s and/or %s not readable" erlang-bin erlang-mode-path))))

(provide 'erlang-config)
