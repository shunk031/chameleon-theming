;;; chameleon-theming.el --- Change the emacs themes in the keybindings

;; Author: Shunsuke KITADA <septemtrio.ager@gmail.com>
;; URL: https://github.com/shunk031/chameleon-theming
;; Keywords: color theme keybindings overwrite
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Place chameleon-theming.el somewhere in your `load-path'. Then, add the
;; following lines to ~/.emacs or ~/.emacs.d/init.el:
;;
;;     (setq chameleon-gui-themes '(hoge-theme fuga-theme))
;; 
;;     (global-set-key (kbd "C-c t n") 'chameleon-load-next-theme)
;;     (global-set-key (kbd "C-c t p") 'chameleon-load-prev-theme)
;;
;; Then, you can easily switch between your multiple emacs themes.
;;

;;; Code:

(defgroup chameleon-theming nil
  "Change the emacs themes in the keybindings."
  :prefix "chameleon-")

(defcustom chameleon-overwrite-themes-directory
  (concat
   (file-name-directory (find-library-name "chameleon-theming"))
   "overwrite-themes/")
  "Default directory of overwrite theme files."
  :type 'directory
  :group 'chameleon-theming)

(defcustom chameleon-overwrite-regexp "overwrite-"
  "Regular expression of overwrite theme file names."
  :type 'regexp
  :group 'chameleon-theming)

(defcustom chameleon-initial-alpha-value 100
  "Initial value of alpha parameter."
  :type 'number
  :group 'chameleon-theming)

(defun chameleon-set-initial-alpha-value ()
  "Set initial value of alpha parameter for the current frame"
  (interactive)
  (if (equal (frame-parameter nil 'alpha) nil)
      (set-frame-parameter nil 'alpha chameleon-initial-alpha-value)))

(defun chameleon-powerline-reset ()
  "Call 'powerline-reset' function when the package exists."
  (interactive)
  (if (find-lisp-object-file-name 'powerline-reset 'defun)
      (powerline-reset)))

(defun chameleon-reset-frame-alpha ()
  "Reset flame alpha value."
  (interactive)
  (chameleon-set-initial-alpha-value)
  (if (not (eq (frame-parameter nil 'alpha) chameleon-initial-alpha-value))
      (set-frame-parameter nil 'alpha chameleon-initial-alpha-value)))

(defun* chameleon-load-theme (theme &optional (ow-dir chameleon-overwrite-themes-directory))
  "Set 'theme' as the current theme."
  (interactive
   (list
    (intern (completing-read "Load theme: " chameleon-gui-themes nil t))))
  (when (chameleon--theme-set-p)
    (disable-theme chameleon-current-theme))
  (setq chameleon-current-theme theme)
  (load-theme theme t)
  ;; load overwrite theme
  (chameleon-load-overwrite-theme theme ow-dir)
  ;; Reload and reset powerline theme
  (chameleon-powerline-reset)
  (message "Loaded theme %s" theme))

(defun chameleon-load-overwrite-theme (overwrite-theme overwrite-theme-dir)
  "Load overwrite theme when the overwrite theme file exists."
  (add-to-list 'load-path overwrite-theme-dir)
  (let ((filename
	 (concat chameleon-overwrite-regexp
		 (symbol-name overwrite-theme))))
    (chameleon-reset-frame-alpha)
    (load filename "missing ok")))

(defun chameleon-load-next-theme ()
  "Load the next theme in the `chameleon-gui-themes' list of themes."
  (interactive)
  (let* ((current-idx (if (chameleon--theme-set-p)
			  (cl-position chameleon-current-theme chameleon-gui-themes)
			-1))
	 (theme (chameleon--next-element current-idx chameleon-gui-themes)))
    (chameleon-load-theme theme)))

(defun chameleon-load-prev-theme ()
  (interactive)
  "Load the previous theme in the `chameleon-gui-themes' list of themes."
  (let* ((current-idx (if (chameleon--theme-set-p)
			  (cl-position chameleon-current-theme chameleon-gui-themes)
			1))
	 (theme (chameleon--prev-element current-idx chameleon-gui-themes)))
    (chameleon-load-theme theme)))

(defun chameleon--theme-set-p ()
  "Tells whether there's a currently set theme."
  (boundp 'chameleon-current-theme))

(defun chameleon--next-element (current-idx list)
  "Returns the element after `current-idx' in `list' (wrapping around the list)."
  (let ((next-idx (% (+ 1 current-idx) (length list))))
    (nth next-idx list)))

;; Returns the element before `current-idx' in `list' (wrapping around the list).
(defun chameleon--prev-element (current-idx list)
  "Returns the element before `current-idx' in `list' (wrapping around the list)."
  (let ((next-idx (% (- (+ current-idx (length list)) 1) (length list))))
    (nth next-idx list)))

(provide 'chameleon-theming)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; chameleon-theming.el ends here
