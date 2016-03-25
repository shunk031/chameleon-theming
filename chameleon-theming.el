;; chameleon-theming

(defgroup chameleon-theming nil
  "Change the emacs theme in the keybindings"
  :prefix "chameleon-"
  )

(defcustom chameleon-load-overwrite-theme-derectory
  (expand-file-name (concat (if (boundp 'use-emacs-directory)
				(file-name-as-directory use-emacs-directory)
			      "~/.emacs.d")
			    "overwrite-themes"))
  "Default directory of overwrite theme files"
  :type 'directory
  :group 'chameleon-theming
  )

