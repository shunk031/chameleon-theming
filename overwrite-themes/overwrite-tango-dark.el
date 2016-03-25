;; overwrite-tango-dark

;; ウィンドウの透過度を変更する

(if window-system
    (progn
      (set-background-color "Black")
      (set-foreground-color "LightGray")
      (set-frame-parameter nil 'alpha 80)
      ))

;; powerline



;; tabbar
