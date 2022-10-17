;;; Package Management ===================================================== ;;;

;; Use Straight for package management to keep all configuration
;; contained within init.el.  This needs to go first because all other
;; packages are pulled in with straight.

(setq straight-use-package-by-default t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
			 user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Pull in `use-package' to allow grouping of configuration by
;; package and make autoloading based on keybinds and hooks easy.

(straight-use-package 'use-package)

;;; Windowing ============================================================== ;;;

(use-package ace-window
  :ensure t
  :after posframe
  :bind
  ("M-o" . 'ace-window)

  :custom
  (aw-dispatch-always t)
  (aw-dispatch-when-more-than 1)
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

  :custom-face
  (aw-leading-char-face ((t (:inherit default :foreground "red" :height 600))))

  :config
  ;; NB. horz/vert here identifies the axis that is being split, which
  ;; is the opposite to how vim's split/vsplit works.
  (defun amnn/aw-split-other-window-vert (window)
    "Split WINDOW horizontally and fill the original WINDOW."
    (select-window window)
    (split-window-vertically)
    (other-window 1)
    window)

  (defun amnn/aw-split-other-window-horz (window)
    "Split WINDOW vertically and fill the original WINDOW."
    (select-window window)
    (split-window-horizontally)
    (other-window 1)
    window)

  (setq aw-dispatch-alist
   '((?x aw-delete-window                "Delete Window")
     (?m aw-swap-window                  "Swap Windows")
     (?M aw-move-window                  "Move Window")
     (?c aw-copy-window                  "Copy Window")
     (?n aw-flip-window                  "Previous Window")
     (?F aw-split-window-fair            "Split Window Fair")
     (?v aw-split-window-horz            "Split Window Right")
     (?V amnn/aw-split-other-window-horz "Split Window Left")
     (?b aw-split-window-vert            "Split Window Below")
     (?B amnn/aw-split-other-window-vert "Split Window Above")
     (?o delete-other-windows            "Delete Other Windows")
     (?T aw-transpose-frame              "Transpose Frame")
     (?? aw-show-dispatch-help)))

  (ace-window-posframe-mode))

(use-package posframe :ensure t)

;;; Minibuffer and Completion Frameworks =================================== ;;;

(use-package consult
  :ensure t
  :bind
  (("C-x b"   . consult-buffer)
   ("C-x 4 b" . consult-buffer-other-window)
   ("C-x 5 b" . consult-buffer-other-frame)
   ("C-x p b" . consult-project-buffer)
   ("C-c C-f" . consult-find)
   ("C-c g"   . consult-ripgrep)
   ("C-c C-g" . consult-ripgrep)
   ("C-s"     . consult-line)
   ("C-S-s"   . consult-line-multi)
   :map minibuffer-local-map
   ("M-r"     . consult-history))

  :hook (completion-list-mode . consult-preview-at-point-mode)

  :custom
  ;; Use Consult to select xref locations with preview
  (xref-show-xrefs-function       #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (consult-narrow-key (kbd "C-=")))

(use-package consult-eglot
  :ensure t
  :after (consult eglot)
  :bind
  (:map evil-normal-state-map
	("gs" . 'consult-eglot-symbols)))

(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-preselect-first nil)
  :bind
  (:map corfu-map
	("M-SPC"   . corfu-insert-separator)
	("TAB"     . corfu-next)
	([tab]     . corfu-next)
	("S-TAB"   . corfu-previous)
	([backtab] . corfu-previous))
  :init
  (global-corfu-mode))

(use-package embark
  :ensure t
  :bind
  (("C-."   . embark-act)
   ("M-."   . embark-dwim)
   ("C-,"   . amnn/embark-act-no-quit)
   ("C-h B" . embark-bindings)

   :map embark-file-map
   ("o" . amnn/embark-ace-find-file)

   :map embark-buffer-map
   ("o" . amnn/embark-ace-switch-to-buffer)

   :map embark-bookmark-map
   ("o" . amnn/embark-ace-bookmark-jump))

  :custom
  (embark-indicators '(embark-highlight-indicator
		       embark-isearch-highlight-indicator
		       embark-minimal-indicator))
  (prefix-help-command #'embark-prefix-help-command)
  (enable-recursive-minibuffers t)

  :init
  ;; Hide the mode line of the Embark live/completion buffers
  (add-to-list 'display-buffer-alist
	       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*" nil
		 (window-parameters (mode-line-format . none))))

  (eval-when-compile
    (defmacro amnn/embark-ace-action (fn)
      `(defun ,(intern (concat "amnn/embark-ace-" (symbol-name fn))) ()
	 (interactive)
	 (with-demoted-errors "%s"
           (require 'ace-window)
           (let ((aw-dispatch-always t))
             (aw-switch-to-window (aw-select nil))
             (call-interactively (symbol-function ',fn)))))))

  (amnn/embark-ace-action find-file)
  (amnn/embark-ace-action switch-to-buffer)
  (amnn/embark-ace-action bookmark-jump)

  (defun amnn/embark-act-no-quit ()
    "Run action but don't quit the minibuffer afterwards."
    (interactive)
    (let ((embark-quit-after-action nil))
      (embark-act))))

(use-package embark-consult
  :ensure t
  :demand t
  :after (embark consult)
  :hook  (embark-collect-mode . consult-preview-at-point-mode))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides
   '((file     (styles partial-completion))
     (command  (styles amnn/orderless-with-initialism))
     (variable (styles amnn/orderless-with-initialism))
     (symbol   (styles amnn/orderless-with-initialism))))

  (orderless-component-separator #'orderless-escapable-split-on-space)
  (orderless-style-dispatchers '(amnn/orderless-dispatch))

  :config
  (defvar amnn/orderless-dispatch-alist
    '((?% . char-fold-to-regex)
      (?! . orderless-without-literal)
      (?` . orderless-initialism)
      (?= . orderless-literal)
      (?~ . orderless-flex)))

  (defun amnn/orderless--suffix-regexp ()
    (if (and (boundp 'consult--tofu-char)
	     (boundp 'consult--tofu-range))
	(format "[%c-%c]*$" consult--tofu-char
		(+ consult--tofu-char
		   consult--tofu-range -1))
        "$"))

  (orderless-define-completion-style amnn/orderless-with-initialism
    (orderless-matching-styles
     '(orderless-initialism orderless-literal orderless-regexp)))

  (defun amnn/orderless-dispatch (word -index -total)
    (cond
     ;; Ensure that $ works with Consult commands' disambiguation suffixes
     ((string-suffix-p "$" word)
      `(orderless-regexp  . ,(concat (substring word 0 -1)
				     (amnn/orderless--suffix-regexp))))

     ;; File extensions
     ((and (or minibuffer-completing-file-name
	       (derived-mode-p 'eshell-mode))
	   (string-match-p "\\`\\.." word))
      `(orderless-regexp . ,(concat "\\." (substring word 1)
				    (amnn/orderless--suffix-regexp))))

     ;; Ignore single !
     ((equal "!" word)
      `(orderless-literal . ""))

     ;; Prefixes
     ((when-let (x (assq (aref word 0)
			 amnn/orderless-dispatch-alist))
	(cons (cdr x) (substring word 1))))

     ;; Suffixes
     ((when-let (x (assq (aref word (1- (length word)))
			 amnn/orderless-dispatch-alist))
	(cons (cdr x)(substring word 0 -1)))))))

(use-package savehist
  :ensure t
  :init (savehist-mode))

(use-package vertico
  :ensure t
  :init (vertico-mode))

;;; File/Text Editing ====================================================== ;;;

(use-package dired
  :straight (:type built-in)
  :after evil-collection
  :commands (dired dired-jump)
  :custom
  (dired-dwim-target t)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map "h" 'dired-up-directory)
  (evil-collection-define-key 'normal 'dired-mode-map "l" 'dired-find-file)
  (evil-collection-define-key 'normal 'dired-mode-map "L" 'dired-display-file))

(use-package project
  :config
  (defun amnn/try-project-root (dir)
    (and-let* ((override (locate-dominating-file dir ".project")))
      (cons '.project override)))

  (add-hook 'project-find-functions #'amnn/try-project-root)

  (cl-defmethod project-root ((project (head .project)))
    (cdr project)))

(use-package whitespace
  :hook (prog-mode . whitespace-mode)

  :custom
  (whitespace-line-column 80)
  (whitespace-style '(face lines-tail))

  :custom-face
  (whitespace-line ((t (:inherit error :foreground nil :background nil)))))

;;; Vim Emulation =========================================================  ;;;

(use-package evil
  :ensure t
  :custom
  (evil-search-module #'evil-search)
  (evil-undo-system   #'undo-redo)
  (evil-split-window-below  t)
  (evil-vsplit-window-right t)
  (evil-want-keybinding nil)

  :bind
  ;; Leader bindings
  (:map evil-normal-state-map
   ("SPC b" . 'consult-buffer)
   ("SPC B" . 'ibuffer)
   ("SPC d" . 'dired)
   ("SPC f" . 'find-file)
   ("SPC k" . 'kill-buffer)
   ("SPC g" . 'consult-ripgrep)
   ("SPC p" . 'project-find-file)
   ("SPC P" . 'project-switch-project)
   ("SPC s" . 'consult-line)
   ("SPC v" . 'magit-status)
   ("SPC w" . 'ace-window)
   ("SPC x" . 'execute-extended-command))

  :config
  (evil-mode t)
  ;; Unbind to not conflict with Embark
  (unbind-key "C-." evil-normal-state-map))

(use-package evil-collection
  :after evil
  :ensure t
  :custom
  (evil-want-integration t)
  :config
  (evil-collection-init))

(use-package evil-easymotion
  :after evil
  :ensure t
  :config
  (define-key evil-normal-state-map (kbd "SPC SPC") evilem-map))

(use-package evil-escape
  :after evil
  :ensure t
  :custom
  (evil-escape-key-sequence "jk")
  :config
  (evil-escape-mode))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

;;; Language Server ======================================================== ;;;

(use-package eglot
  :ensure t
  :after evil

  :bind
  (:map evil-normal-state-map
	("[g" . 'flymake-goto-prev-error)
	("]g" . 'flymake-goto-next-error)
	("ga" . 'eglot-code-actions)
	("gx" . 'eglot-code-action-quickfix)
	("gi" . 'eglot-find-implementation)
	("gd" . 'xref-find-definitions)
	("gD" . 'eglot-find-declaration)
	("gr" . 'xref-find-references)
	("gy" . 'eglot-find-typeDefinition))

  :init
  (defun amnn/eglot-prefer-flymake-eldoc ()
    "Show errors/warnings from flymake in eldoc, over eglot's LSP based
     documentations."
    (push 'flymake-eldoc-function eldoc-documentation-functions))

  (add-hook 'eglot-managed-mode-hook 'amnn/eglot-prefer-flymake-eldoc))


;;; Debugging ============================================================== ;;;

(use-package realgud :ensure t :defer t)

(use-package realgud-lldb
  :straight (:host github :repo "realgud/realgud-lldb" :branch "master")
  :commands (lldb))

;;; Language Major Modes =================================================== ;;;

(use-package clojure-mode :ensure t)

(use-package fish-mode :ensure t)

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode))

(use-package move-mode
  :straight (:type git :repo "/Users/amnn/move-mode" :branch "main")
  :hook     (move-mode . eglot-ensure)

  :config
  (add-to-list 'eglot-server-programs '(move-mode "move-analyzer"))

  (defun amnn/move-lsp-project-root (dir)
    (and-let* (((boundp 'eglot-lsp-context))
               (eglot-lsp-context)
               (override (locate-dominating-file dir "Move.toml")))
      (cons 'Move.toml override)))

  (add-hook 'project-find-functions #'amnn/move-lsp-project-root)
  (cl-defmethod project-root ((project (head Move.toml)))
    (cdr project)))

(use-package rustic
  :ensure t
  ;; Hack to re-override rust-mode's habit of stomping the rustic-mode
  ;; in the auto-mode-alist when it gets loaded
  :after rust-mode :mode ("\\.rs\\'" . rustic-mode)
  :bind
  (("C-c C-t C-r" . rustic-cargo-test-rerun))
  :custom
  (rustic-lsp-client 'eglot)
  (rustic-lsp-server 'rust-analyzer)
  :config
  (defun amnn/widen-fill ()
    (setq-local fill-column 100)
    (setq-local whitespace-line-column 100))
  (add-hook 'rust-mode-hook #'amnn/widen-fill))

(use-package typescript-mode
  :ensure t
  :custom
  (typescript-indent-level 2))

;;; Version Control ======================================================== ;;;

(use-package git-gutter-fringe
  :ensure t
  :demand fringe-helper
  :hook (prog-mode . git-gutter-mode)
  :bind
  (:map evil-motion-state-map
   ("]h" . 'git-gutter:next-hunk)
   ("[h" . 'git-gutter:previous-hunk))
  :config
  (set-face-foreground  'git-gutter-fr:added "lime green")
  (define-fringe-bitmap 'git-gutter-fr:added
    [224] nil nil '(center repeated))
  (set-face-foreground  'git-gutter-fr:modified "orange")
  (define-fringe-bitmap 'git-gutter-fr:modified
    [224] nil nil '(center repeated))
  (set-face-foreground  'git-gutter-fr:deleted "firebrick")
  (define-fringe-bitmap 'git-gutter-fr:deleted
    [128 192 224 240] nil nil 'bottom))

(use-package magit
  :ensure t
  :bind
  (("C-x g"   . magit-status)
   ("C-x C-g" . magit-status)))

;;; Utilities ============================================================== ;;;

(use-package ibuffer
  :bind
  (("C-x C-b" . ibuffer)))

(use-package profile-dotemacs :ensure t :defer t)

(use-package wgrep
  :ensure t
  :commands (wgrep-change-to-wgrep-mode))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode +1))

;;; Appearance ============================================================= ;;;

(use-package dashboard
  :ensure t
  :custom
  (dashboard-startup-banner 'logo)
  :config
  (dashboard-setup-startup-hook))

(use-package elec-pair
  :config
  (electric-pair-mode))

(use-package ligature
  :ensure t
  :config
  (ligature-set-ligatures
   't
   '("<--" "<---" "<<-" "<-" "->" "->>" "-->" "--->"
     "<==" "<===" "<<=" "<=" "=>" "=>>" "==>" "===>"

     ">=" ">>="

     "<->" "<-->" "<--->" "<---->"
     "<=>" "<==>" "<===>" "<====>"

     "<!--" "<!---" "<***>"

     "::" ":::"
     "++" "+++"

     "<~~" "</" "</>" "/>" "~~>"

     "!==" "!=" "<>"

     ":-" ":+" "<*" "<*>" "*>" "+:" "-:"
     "<:" ":=" "<|" "<|>" "|>" "=:" ":>"

     ;; Variable length
     ("=" "=+")))

  (global-ligature-mode t))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package smart-mode-line
  :ensure t
  :commands (sml/setup)
  :custom
  (sml/no-confirm-load-theme t)
  (sml/theme 'respectful))

(use-package twilight-anti-bright-theme :ensure t :defer t)
(use-package twilight-bright-theme      :ensure t :defer t)

(use-package theming
  :straight nil :demand t :no-require t
  :after (exec-path-from-shell twilight-anti-bright-theme twilight-bright-theme)

  :custom
  (use-file-dialog nil)
  (use-dialog-box  nil)

  (default-frame-alist
    '((internal-border-width . 24)
      (right-fringe . 0)))

  (window-divider-default-right-width 24)
  (window-divider-default-places 'right-only)

  :preface
  (set-frame-font "Iosevka SS15 16")

  (menu-bar-mode       +1)
  (scroll-bar-mode     -1)
  (tool-bar-mode       -1)
  (window-divider-mode +1)

  ;; Nicer glyphs for continuation and wrap
  (set-display-table-slot standard-display-table
                          'truncation (make-glyph-code ?…))
  (set-display-table-slot standard-display-table
                          'wrap (make-glyph-code ?-))

  (defun amnn/push-kitty-theme (light-or-dark)
    "Send kitty a remote message to change its theme, to match
     Emacs. LIGHT-OR-DARK is a string literal, either `light' or
     `dark'."
    (call-process "/Applications/kitty.app/Contents/MacOS/kitty" nil nil nil
		  "@" "--to" "unix:/tmp/kitty-pipe"
                  "set-colors" "--all" "--configured"
                  (concat "~/.config/kitty/kitty-" light-or-dark ".conf")))

  (defun amnn/load-theme-matching-system ()
    "Pick which theme to run based on whether the system is light or dark."
    (pcase (plist-get (mac-application-state) :appearance)
      ("NSAppearanceNameAqua"
       (load-theme 'twilight-bright t)
       (amnn/push-kitty-theme "light"))
      ("NSAppearanceNameDarkAqua"
       (load-theme 'twilight-anti-bright t)
       (amnn/push-kitty-theme "dark")))

    ;; Apply tweaks to faces in current theme, after it has loaded.
    (set-face-attribute 'fringe nil :background (face-attribute 'default :background))
    (set-face-attribute 'font-lock-doc-face nil :extend t)
    (set-face-attribute 'font-lock-comment-face nil :extend t)

    (custom-set-faces
     `(window-divider ((t (:foreground ,(face-attribute 'default :background)))))
     `(window-divider-first-pixel ((t (:inherit window-divider))))
     `(window-divider-last-pixel  ((t (:inherit window-divider)))))

    (sml/setup))

  (amnn/load-theme-matching-system)
  (add-hook 'mac-effective-appearance-change-hook
            #'amnn/load-theme-matching-system))

;;; Misc. ================================================================== ;;;

(use-package exec-path-from-shell
  :ensure t
  :demand t
  :config
  (exec-path-from-shell-initialize))

(use-package emacs
  :hook
  (before-save-hook . delete-trailing-whitespace)

  :custom
  ;; Store backups centrally, not next to file.
  (backup-directory-alist '(("" . "~/.emacs.d/backup")))
  (indent-tabs-mode nil)
  (mac-option-modifier 'meta)
  (require-final-newline t)
  (warning-suppress-log-types '((comp))))
