(straight-use-package 'smart-mode-line)
(straight-use-package 'twilight-bright-theme)

;; Hide UI Clutter
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Set Font
(set-frame-font "Iosevka SS15 14")

;; Mode Line
(setq sml/no-confirm-load-theme t)
(setq sml/theme 'light)
(sml/setup)

;; Theming
(load-theme 'twilight-bright t)
(set-face-attribute 'fringe nil
		    :inherit 'default
		    :background nil)

(set-face-attribute 'font-lock-doc-face nil
		    :extend t)

(set-face-attribute 'font-lock-comment-face nil
		    :extend t)

;; Whitespace highlighting
(require 'whitespace)
(setq whitespace-line-column 80
      whitespace-style '(face lines-tail))
(set-face-attribute 'whitespace-line nil
		    :inherit 'error
		    :foreground nil
		    :background nil)

(add-hook 'prog-mode-hook 'whitespace-mode)

(provide 'setup-ui)
