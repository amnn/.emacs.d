(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

(require 'setup-straight)
(require 'setup-ui)
(require 'setup-text-modes)
(require 'setup-backups)
(require 'setup-buffers)

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

  :init

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xref-functions       #'consult-xref
	xref-show-definitions-function #'consult-xref

	consult-narrow-key (kbd "C-+")))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

(use-package embark
  :ensure t
  :bind
  (("C-."   . embark-act)
   ("M-."   . embark-dwim)
   ("C-h B" . embark-bindings))

  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; Hide the mode line of the Embark live/completion buffers
  (add-to-list 'display-buffer-alist
	       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*" nil
		 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :demand t
  :after (embark consult)
  :hook  (embark-collect-mode . consult-preview-at-point-mode))

(use-package evil
  :after evil-leader
  :ensure t
  :init
  (setq evil-search-module 'evil-search
	evil-split-window-below  t
	evil-vsplit-window-right t)
  :config
  (evil-mode t)
  (bind-key   "C-o" #'previous-buffer)
  (bind-key   "C-i" #'next-buffer)
  (unbind-key "C-." evil-normal-state-map))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (setq evil-want-integration t)
  (evil-collection-init))

(use-package evil-easymotion
  :after evil
  :ensure t
  :config
  (evilem-default-keybindings "SPC"))

(use-package evil-escape
  :after evil
  :ensure t
  :config
  (setq evil-escape-key-sequence "jk")
  (evil-escape-mode))

(use-package evil-leader
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader ",")
  (evil-leader/set-key
   "b" 'consult-buffer
   "f" 'find-file
   "k" 'kill-buffer
   "g" 'consult-ripgrep
   "p" 'project-find-file
   "s" 'consult-line
   "x" 'execute-extended-command))

(use-package git-gutter-fringe
  :ensure t
  :hook (prog-mode . git-gutter-mode)
  :config
  (set-face-foreground  'git-gutter-fr:added "yellow green")
  (define-fringe-bitmap 'git-gutter-fr:added
    [224] nil nil '(center repeated))
  (set-face-foreground  'git-gutter-fr:modified "gold")
  (define-fringe-bitmap 'git-gutter-fr:modified
    [224] nil nil '(center repeated))
  (set-face-foreground  'git-gutter-fr:deleted "firebrick")
  (define-fringe-bitmap 'git-gutter-fr:deleted
    [128 192 224 240] nil nil 'bottom))

(use-package ligature
  :ensure t
  :config
  (ligature-set-ligatures
   't
   '(
     "<--" "<---" "<<-" "<-" "->" "->>" "-->" "--->"
     "<==" "<===" "<<=" "<=" "=>" "=>>" "==>" "===>"

     ">=" ">>="

     "<->" "<-->" "<--->" "<---->"
     "<=>" "<==>" "<===>" "<====>"

     "<!--" "<!---" "<***>"

     "::" ":::"
     "++" "+++"

     "<~~" "</" "</>" "/>" "~~>"

     "===" "==" "!==" "!=" "<>"

     ":-" ":+" "<*" "<*>" "*>" "+:" "-:"
     "<:" ":=" "<|" "<|>" "|>" "=:" ":>"))

  (global-ligature-mode t))

(use-package magit
  :ensure t
  :bind
  (("C-x g"   . magit-status)
   ("C-x C-g" . magit-status)))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))

(use-package project
  :after counsel
  :config
  (add-hook 'project-find-functions #'amnn--try-project-root))

(use-package rust-mode :ensure t :mode "\\.rs\\'")

(use-package savehist
  :ensure t
  :init (savehist-mode))

(use-package vertico
  :ensure t
  :init (vertico-mode))

(use-package wgrep
  :ensure t
  :commands (wgrep-change-to-wgrep-mode))


;; Ace Window
;; Embark, Karthik's post to choose where a file/buffer is opened to.
;; Fix git-gutter-fringe
;; LSP Mode +consult-lsp
