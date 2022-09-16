(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

(require 'setup-straight)
(require 'setup-ui)
(require 'setup-text-modes)
(require 'setup-backups)
(require 'setup-buffers)

(use-package counsel
  :ensure t
  :commands (ivy-mode)
  :bind
  (("C-s"     . swiper)
   ("C-c C-r" . ivy-resume)
   ("M-x"     . counsel-M-x)
   ("C-x b"   . counsel-switch-buffer)
   ("C-x C-f" . counsel-find-file)
   ("C-h f"   . counsel-describe-function)
   ("C-h v"   . counsel-describe-variable)
   ("C-h o"   . counsel-describe-symbol)
   ("C-h S"   . counsel-info-lookup-symbol)
   :map minibuffer-local-map
   ("C-r"     . counsel-minibuffer-history))
  :config
  (ivy-mode)
  (setq ivy-use-virtual-buffers t
	enable-recursive-minibuffers t))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

(use-package deadgrep
  :ensure t
  :bind
  (("C-c g"   . deadgrep)
   ("C-c C-g" . deadgrep)))

(use-package evil
  :after evil-leader
  :ensure t
  :init
  (setq evil-search-module 'evil-search
	evil-split-window-below  t
	evil-vsplit-window-right t)
  :config
  (evil-mode t)
  (global-set-key (kbd "C-o") 'previous-buffer)
  (global-set-key (kbd "C-i") 'next-buffer))

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
   "b" 'counsel-switch-buffer
   "B" 'ibuffer
   "f" 'counsel-find-file
   "k" 'counsel-kill-buffer
   "g" 'deadgrep
   "p" 'project-find-file
   "s" 'swiper
   "x" 'counsel-M-x))

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

(use-package project
  :after counsel
  :config
  (add-hook 'project-find-functions #'amnn--try-project-root))

(use-package rust-mode :ensure t :mode "\\.rs\\'")

(use-package which-key
  :ensure t
  :demand t
  :bind
  (("C-h m" . which-key-show-major-mode))
  :config
  (which-key-mode))

;; Git Repo for .emacs.d
;; Vertico, Consult, Embark, Marginalia
;; -deadgrep + wgrep
;; -swiper/ivy/counsel
;; Ace Window
;; Embark, Karthik's post to choose where a file/buffer is opened to.
;; LSP Mode
;; Fix git-gutter-fringe
