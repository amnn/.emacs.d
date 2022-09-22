(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

(require 'setup-straight)
(require 'setup-ui)
(require 'setup-text-modes)
(require 'setup-backups)
(require 'setup-buffers)

(use-package ace-window
  :ensure t
  :config
  (bind-key "M-o" #'ace-window)
  (setq aw-dispatch-always t
        aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

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
  (setq xref-show-xrefs-function       #'consult-xref
	xref-show-definitions-function #'consult-xref

	consult-narrow-key (kbd "C-=")))

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

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

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
	("gd" . 'eglot-find-declaration)
	("gr" . 'xref-find-references)
	("gy" . 'eglot-find-typeDefinition)))

(use-package embark
  :ensure t
  :bind
  (("C-."   . embark-act)
   ("M-."   . embark-dwim)
   ("C-h B" . embark-bindings))

  :init
  (setq prefix-help-command #'embark-prefix-help-command
	enable-recursive-minibuffers t)
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
  ;; Unbind to not conflict with Embark
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
   "P" 'project-switch-project
   "s" 'consult-line
   "x" 'execute-extended-command))

(use-package git-gutter-fringe
  :ensure t
  :demand fringe-helper
  :hook ((prog-mode . git-gutter-mode))
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

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

(use-package orderless
  :ensure t
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
	(cons (cdr x)(substring word 0 -1))))))

  (setq completion-styles '(orderless basic)
	completion-category-defaults nil

	completion-category-overrides
	'((file     (styles partial-completion))
	  (command  (styles amnn/orderless-with-initialism))
	  (variable (styles amnn/orderless-with-initialism))
	  (symbol   (styles amnn/orderless-with-initialism)))

	orderless-comment-separator #'orderless-escapable-split-on-space
	orderless-style-dispatchers '(amnn/orderless-dispatch)))

(use-package project
  :after counsel
  :config
  (add-hook 'project-find-functions #'amnn--try-project-root))

(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :config
  (defun widen-line-column () (setq-local whitespace-line-column 100))
  (add-to-list 'eglot-server-programs '((rust-mode) "rust-analyzer"))
  (add-hook 'rust-mode-hook 'eglot-ensure)
  (add-hook 'rust-mode-hook #'widen-line-column))

(use-package savehist
  :ensure t
  :init (savehist-mode))

(use-package vertico
  :ensure t
  :init (vertico-mode))

(use-package wgrep
  :ensure t
  :commands (wgrep-change-to-wgrep-mode))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode +1))

(use-package emacs
  :config
  (setq warning-suppress-log-types '((comp)))
  (setq mac-option-modifier 'meta))
