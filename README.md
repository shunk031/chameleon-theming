# Chameleon Theming

## What is it?

`chameleon-theming` can easily switch between your multiple emacs themes. In addition to the switching of the theme, you can change [tabbar](https://github.com/dholm/tabbar) and [powerline](https://github.com/milkypostman/powerline) themes at the same time.

## Sample Code

You have to specify your emacs theme and bind some keys to some of those commands in a usual manner, for example:

```lisp
(require 'chameleon-theming)

;; Lists the themes you want to switch
(setq chameleon-gui-themes '(solarized-dark tango-dark monokai))

;; bind some keys
(global-set-key (kbd "C-c t n") 'chameleon-load-next-theme)
(global-set-key (kbd "C-c t p") 'chameleon-load-prev-theme)
```

## Features

### Change the theme to easily

By setting the keybindings and themes, you can easily switch between the themes.

### Overwrite themes

You can overwrite themes by loading the `overwrite-` prefix files. 

1. Set `chameleon-overwrite-themes-directory` with a directory where your overwrite theme files are located.

   ```lisp
   ;; Set the default overwrite themes directory
   ;; If you did not do this setting, then default overwrite-themes directory is used.
   (setq chameleon-overwrite-themes-directory "/path/to/overwrite-themes/")
   ```
   
2. Prepare `overwrite-(theme name).el` files that describes the setting of overwrite theme.
3. Put the created file to `chameleon-overwrite-themes-directory`.
   
### Set default of transparency

You can set default frame transparency. The default value is 100.

```lisp
;; Set the default transparency to 80 
(setq chameleon-initial-alpha-value 80)
```

## Others

If you are a `use-package` user, please try to set as follows:

```lisp
(use-package chameleon-themning
 :bind (("C-c t n" . chameleon-load-next-theme)
        ("C-c t p" . chameleon-load-prev-theme))
 :init (setq chameleon-gui-themes
        '(solarized-dark tango-dark monokai)))
```
