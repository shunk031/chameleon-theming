;;; chameleon-theming.el --- Change the emacs themes in the keybindings

;;; Code:

(defgroup chameleon-theming nil
  "Change the emacs themes in the keybindings."
  :prefix "chameleon-"
  )

(defcustom chameleon-load-overwrite-themes-directory
  (cocat
   (file-name-directory (find-library-name "chameleon-theming"))
   "overwrite-themes")
  "Default directory of overwrite theme files."
  :type 'directory
  :group 'chameleon-theming)

(defcustom chameleon-overwrite-regexp "\\overwrite-"
  "Regular expression of overwrite theme file names."
  :type 'regexp
  :group 'chameleon-theming)

(defun chameleon-powerline-reset ()
  "Call 'powerline-reset' function when the package exists."
  (if (find-lisp-object-file-name 'powerline-reset 'defun)
      (powerline-reset)))

(defun chameleon-load-theme (theme)
  "Set 'theme' as the current theme."
  (interactive
   (list
    (intern (completing-read "Load theme: " chameleon-gui-themes nil t))))
  (when (chameleon--theme-set-p)
    (disable-theme chameleon-current-theme))
  (setq chameleon-current-theme theme)
  (load-theme theme t)

  ;; load overwrite theme
  (chameleon-load-overwrite-theme theme)

  ;; Reload and reset powerline theme
  (chameleon-powerline-reset)
  (message "Loaded theme %s" theme)
  )

(defun chameleon-load-overwrite-theme (overwrite-theme)
  "Load overwrite theme when the overwrite theme file exists."
  )






(defun chameleon--theme-set-p ()
  (boundp 'chameleon-current-theme))
