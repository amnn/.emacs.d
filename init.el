;;; Package Management ===================================================== ;;;

;; Use Straight for package management to keep all configuration
;; contained within init.el.  This needs to go first because all other
;; packages are pulled in with straight.

(setq straight-use-package-by-default t
      use-package-verbose nil
      use-package-expand-minimally t)

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

(use-package breadcrumb
  :straight (:host github :repo "joaotavora/breadcrumb")
  :init
  (breadcrumb-mode))

;;; Minibuffer and Completion Frameworks =================================== ;;;

(use-package consult
  :ensure t

  :autoload
  (consult--jump-preview
   consult--read)

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
  (consult-narrow-key "C-=")

  :config
  (advice-add #'project-find-regexp :override #'consult-ripgrep))

(use-package consult-eglot
  :ensure t
  :after evil

  :bind
  (:map evil-normal-state-map
        ("ge" . 'consult-flymake)
	("gs" . 'consult-eglot-symbols)))

(use-package consult-org
  :straight nil
  :autoload
  (consult-org--headings))

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
  :commands (dired dired-jump)
  :custom
  (dired-dwim-target t)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map "h" 'dired-up-directory)
  (evil-collection-define-key 'normal 'dired-mode-map "l" 'dired-find-file)
  (evil-collection-define-key 'normal 'dired-mode-map "L" 'dired-display-file))

(use-package paredit
  :ensure t
  :hook
  (emacs-lisp-mode . paredit-mode)
  (janet-mode . paredit-mode))

(use-package project
  :config
  ;; The `magit-extra' library does this but it doesn't get loaded
  ;; reliably before `project', so just do this when you load the
  ;; latter, because the function in question is an autoload in
  ;; `magit-extra'.
  (keymap-set project-prefix-map "m" #'magit-project-status)
  (add-to-list 'project-switch-commands '(magit-project-status "Magit"))

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
  (whitespace-line ((t (:inherit error :foreground unspecified :background unspecified)))))

;;; Vim Emulation ========================================================== ;;;

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
   ("SPC b"   . 'consult-buffer)
   ("SPC B"   . 'ibuffer)
   ("SPC d"   . 'dired)
   ("SPC f"   . 'find-file)
   ("SPC k"   . 'kill-buffer)
   ("SPC g"   . 'consult-ripgrep)
   ("SPC m"   . 'switch-to-minibuffer)
   ("SPC N n" . 'narrow-to-region)
   ("SPC N w" . 'widen)
   ("SPC p"   . 'project-find-file)
   ("SPC P"   . 'project-switch-project)
   ("SPC s"   . 'consult-line)
   ("SPC v"   . 'magit-status)
   ("SPC w"   . 'ace-window)
   ("SPC x"   . 'execute-extended-command))

  :config
  (evil-mode t)
  ;; Unbind to not conflict with Embark
  (unbind-key "C-." evil-normal-state-map))

(use-package evil-collection
  :ensure t
  :autoload
  (evil-collection-define-key)
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
  :ensure t
  :custom
  (evil-escape-key-sequence "jk")
  :config
  (evil-escape-mode))

(use-package evil-org
  :ensure t
  :after org
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package evil-paredit
  :ensure t
  :hook (paredit-mode . evil-paredit-mode)

  :preface
  (defmacro amnn/defun-evil-paredit-motion (fn)
    `(defun ,(intern (concat "amnn/evil-" (symbol-name fn))) (&optional n)
       (interactive "p")
       (let ((evil-cursor-move-beyond-eol t))
         (forward-char)
         (,fn n))))

  (amnn/defun-evil-paredit-motion paredit-convolute-sexp)
  (amnn/defun-evil-paredit-motion paredit-forward)
  (amnn/defun-evil-paredit-motion paredit-forward-up)
  (amnn/defun-evil-paredit-motion paredit-forward-down)

  (defun amnn/transpose-sexps-forward ()
    "Move sexp at point forward relative to its siblings, leaving the
     point at the beginning of the sexp."
    (interactive)
    (let ((evil-move-beyond-eol t))
      (forward-sexp)
      (transpose-sexps 1)
      (backward-sexp)))

  (defun amnn/transpose-sexps-backward ()
    "Move sexp at point backward relative to its siblings, leaving the
     point at the beginning of the sexp."
    (interactive)
    (let ((evil-move-beyond-eol t))
      (forward-sexp)
      (transpose-sexps -1)
      (backward-sexp)))

  :bind
  (:map evil-normal-state-map
        ("; ?"   . amnn/evil-paredit-convolute-sexp)

        ("; <"   . paredit-wrap-angled)
        ("; >"   . paredit-close-angled-and-newline)
        ("; {"   . paredit-wrap-curly)
        ("; }"   . paredit-close-curly-and-newline)
        ("; ("   . paredit-wrap-round)
        ("; )"   . paredit-close-round-and-newline)
        ("; ["   . paredit-wrap-square)
        ("; ]"   . paredit-close-square-and-newline)

        ("; f"   . amnn/evil-paredit-forward)
        ("; b"   . paredit-backward)
        ("; u"   . paredit-backward-up)
        ("; U"   . amnn/evil-paredit-forward-up)
        ("; d"   . amnn/evil-paredit-forward-down)
        ("; D"   . paredit-backward-down)

        ("; h b" . paredit-backward-barf-sexp)
        ("; h s" . paredit-backward-slurp-sexp)
        ("; l b" . paredit-forward-barf-sexp)
        ("; l s" . paredit-forward-slurp-sexp)
        ("; J"   . paredit-join-sexps)
        ("; r"   . paredit-raise-sexp)
        ("; s"   . paredit-splice-sexp)
        ("; S"   . paredit-split-sexp)
        ("; t"   . amnn/transpose-sexps-forward)
        ("; T"   . amnn/transpose-sexps-backward)))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

;;; Org Mode =============================================================== ;;;

(use-package org-contrib
  :ensure t
  :after org
  :init
  (add-to-list 'org-modules 'org-checklist))

(use-package org-modern
  :ensure t
  :hook
  (org-mode . auto-fill-mode)
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda)

  :preface
  (defun amnn/open-slack (team _)
    (browse-url (concat "slack://channel?team=" team)))

  (defun amnn/open-sui-pr (pr _)
    (browse-url (concat "https://github.com/MystenLabs/sui/pull/" pr)))

  (defun amnn/org-evil-ret (&optional arg)
    "Try to open the link at point, and if not, behave like RET in evil mode."
    (interactive "P")
    (cond
     ((derived-mode-p 'org-mode)
      (condition-case nil (org-open-at-point arg)
        (user-error (evil-ret))))
     (t (evil-ret))))

  (defun amnn/org-insert-before (arg)
    (interactive "P")
    (if-let (pos (org-in-item-p))
        (progn
          (goto-char pos)
          (let ((indent (current-indentation))
                (check? (org-at-item-checkbox-p)))
            (insert (make-string indent ?\s))
            (insert "- ")
            (when check? (insert "[ ] "))
            (insert "\n")
            (backward-char)))
      (progn
        (org-back-to-heading)
        (if (org-entry-get (point) "TODO")
            (org-insert-todo-heading arg)
          (org-insert-heading arg))))
    (evil-insert 1))

  (defun amnn/org-insert-after (arg)
    (interactive "P")
    (if-let (pos (org-in-item-p))
        (progn
          (goto-char pos)
          (let ((indent (current-indentation))
                (check? (org-at-item-checkbox-p)))
            (org-end-of-item)
            (insert (make-string indent ?\s)) ;; Indentation
            (insert "- ")                     ;; Bullet
            (when check? (insert "[ ] "))     ;; Checkbox
            (insert "\n")
            (backward-char 1)))
      (progn
        (org-back-to-heading)
        (if (org-entry-get (point) "TODO")
            (org-insert-todo-heading-respect-content arg)
          (org-insert-heading-respect-content))))
    (evil-insert 1))

   (defun amnn/consult-org-blocked ()
     (interactive)
     (consult-org-heading "-reviews/WAIT|REVW" (org-agenda-files)))

  (defun amnn/consult-org-done ()
    (interactive)
    (consult-org-heading "-journal/DONE|DROP" (org-agenda-files)))

  (defun amnn/consult-org-next ()
    (interactive)
    (consult-org-heading "/NEXT" (org-agenda-files)))

  (defun amnn/consult-org-working ()
    (interactive)
    (consult-org-heading "/WORK" (org-agenda-files)))

  :config
  (org-link-set-parameters "slack" :follow #'amnn/open-slack)
  (org-link-set-parameters "sui" :follow #'amnn/open-sui-pr)
  (add-to-list 'org-export-backends 'md)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)))

  :bind
  (:map org-mode-map
        ("M-RET" . amnn/org-insert-before)
        ("C-RET" . amnn/org-insert-after))

  (:map evil-normal-state-map
        ("SPC N e" . org-narrow-to-element)
        ("SPC n $" . org-archive-subtree)
        (", !"     . org-priority)
        (", B"     . amnn/consult-org-blocked)
        (", d"     . org-deadline)
        (", D"     . amnn/consult-org-done)
        (", j"     . org-priority-down)
        (", k"     . org-priority-up)
        (", N"     . amnn/consult-org-next)
        (", o"     . amnn/org-insert-after)
        (", O"     . amnn/org-insert-before)
        (", s"     . org-schedule)
        (", t"     . org-todo)
        (", T"     . org-set-tags-command)
        (", W"     . amnn/consult-org-working))

  (:map evil-motion-state-map
        ("]a" . org-next-link)
        ("[a" . org-previous-link)
        ("<return>" . amnn/org-evil-ret))

  :custom
  (org-adapt-indentation t)
  (org-agenda-sticky t)
  (org-agenda-tags-column 0)
  (org-agenda-prefix-format
   '((agenda . "  %-16.16 c%?-12t% s")
     (todo   . "  %-16.16 c ")
     (tags   . "  %-16.16 c ")))
  (org-auto-align-tags nil)
  (org-cycle-separator-lines 1)
  (org-latex-create-formula-image-program 'dvisvgm)
  (org-log-into-drawer t)
  (org-outline-path-complete-in-steps nil)
  (org-refile-targets
   '((org-agenda-files . (:maxlevel . 3))))
  (org-refile-use-outline-path 'file)
  (org-startup-folded t)
  (org-startup-truncated nil)
  (org-tags-column 0)
  (org-tags-sort-function 'org-string-collate-lessp)
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "WORK(k)"
               "WAIT(w)" "REVW(r)" "|"
               "DONE(d)" "DROP(x)")))
  (org-use-fast-todo-selection 'expert)
  (org-modern-todo-faces
   '(("NEXT" . (:foreground "black" :background "lime green"))
     ("WORK" . (:foreground "white" :background "deep sky blue"))
     ("WAIT" . (:foreground "black" :background "gold"))
     ("REVW" . (:foreground "black" :background "gold"))
     ("DROP" . (:foreground "white" :background "black")))))

(use-package org-noter
  :ensure t

  :bind
  (:map evil-normal-state-map
        ("SPC n a" . org-noter))

  :custom
  (org-noter-prefer-root-as-file-level t))

(use-package org-roam
  :ensure t
  :hook (org-mode . amnn/detect-agenda-before-save)

  :autoload
  (amnn/org-roam-agenda-files)

  :bind
  (:map evil-normal-state-map
        ("SPC n l" . org-roam-buffer-toggle)
        ("SPC n f" . org-roam-node-find)
        ("SPC n g" . org-roam-graph)
        ("SPC n i" . org-roam-node-insert)
        ("SPC n c" . org-roam-capture)
        ("SPC n j" . org-roam-dailies-capture-today)
        ("SPC n J" . org-roam-dailies-capture-date)
        ("SPC n n" . org-roam-dailies-goto-tomorrow)
        ("SPC n p" . org-roam-dailies-goto-yesterday)
        ("SPC n r" . amnn/consult-org-refile)
        ("SPC n t" . org-roam-dailies-goto-today)
        ("SPC n T" . org-roam-dailies-goto-date)
        ("SPC n x" . org-roam-extract-subtree))

  :init
  (setq org-roam-v2-ack t)

  :preface
  (advice-add #'org-agenda-files :override #'amnn/org-roam-agenda-files)
  (advice-add #'org-archive-subtree :around #'amnn/org-archive-subtree)

  :config
  (defun amnn/detect-agenda-before-save ()
    "Add agenda detection as a `before-save-hook'"
    (add-hook 'before-save-hook #'amnn/org-roam-detect-for-agenda +1 t)
    (add-hook 'before-save-hook #'amnn/org-roam-detect-stuck      +1 t))

  (defun amnn/org-roam-detect-for-agenda ()
    "Detect whether the current `org-roam' file should be part of
     `org-agenda-files' (i.e. it contains incomplete tasks).  If so, add
     `+agenda' as a tag, and remove it otherwise."
    (save-excursion
      (goto-char (point-min))
      (when-let* ((node (org-roam-node-at-point))

                  (dailies-d
                   (concat org-roam-directory "/"
                           org-roam-dailies-directory))

                  (task-keyword-re
                   (if (file-in-directory-p buffer-file-name dailies-d)
                       org-not-done-regexp org-todo-regexp))

                  (task-re
                   (concat "^" org-outline-regexp
                           " *\\<" task-keyword-re
                           "\\>")))
        (cond
         ;; Skip files with the `-agenda' tag, cleaning up `+agenda'.
         ((member "-agenda" (org-roam-node-tags node))
          (org-roam-tag-remove ("+agenda")))

         ;; Add `+agenda' tag if a relevant task is detected
         ((let ((case-fold-search nil))
            (search-forward-regexp task-re nil t))
          (goto-char (point-min))
          (org-roam-tag-add '("+agenda")))

         ;; Remove it otherwise
         (t (goto-char (point-min))
            (org-roam-tag-remove '("+agenda")))))))

  (defun amnn/org-roam-detect-stuck ()
    "Detect whether the current `org-roam' file should be considered a
     stuck project.  A file is considered a stuck project if it
     is a non-dailies file, part of the agenda (as per
     `amnn/org-roam-detect-for-agenda') but it has no `NEXT',
     `WORK' `WAIT', `REVW', `+SCHEDULED' or `+DEADLINED' tasks."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (when-let* ((node (org-roam-node-at-point))

                  (dailies-d
                   (concat org-roam-directory "/"
                           org-roam-dailies-directory))

                  (pending-re
                   (concat "^" org-outline-regexp
                           " *\\<\\(NEXT\\|WORK\\|WAIT\\|REVW\\)\\>"))

                  (todo-re
                   (concat "^" org-outline-regexp
                           " *\\<TODO\\>")))
        (cond
         ;; Can't be stuck if it's not in the agenda
         ((not (member "+agenda" (org-roam-node-tags node)))
          (org-roam-tag-remove '("+stuck")))

         ;; Daily files are not projects
         ((file-in-directory-p buffer-file-name dailies-d)
          (org-roam-tag-remove '("+stuck")))

         ;; Projects with a pending (NEXT, WORK, WAIT, REVW) task are not stuck
         ((let ((case-fold-search nil))
            (search-forward-regexp pending-re nil t))
          (goto-char (point-min))
          (org-roam-tag-remove '("+stuck")))

         ;; Projects with a SCHEDULED or DEADLINED TODO are not stuck
         ((cl-loop initially (goto-char (point-min))
                   while     (search-forward-regexp todo-re nil t)
                   thereis   (or (org-entry-get nil "SCHEDULED")
                                 (org-entry-get nil "DEADLINE")))
          (org-roam-tag-remove '("+stuck")))

         ;; Anything else must be a stuck project
         (t (goto-char (point-min))
            (org-roam-tag-add '("+stuck")))))))

  (defun amnn/org-roam-agenda-files (&optional unrestricted archives)
    "Override for `org-agenda-files' based on `org-roam' files that are tagged
     with `+agenda'."
    (->> (org-roam-db-query
          [:select [file properties] :from nodes
                   :where      (like properties '"%:+agenda:%")
                   :and   (not (like properties '"%:-agenda:%"))])
         (seq-filter
          (lambda (record)
            (->> (cadr record)
                 (assoc-string "ALLTAGS")
                 cdr org-no-properties
                 (string-match-p (regexp-quote ":+agenda:")))))
         (mapcar #'car)))

  (defun amnn/org-archive-subtree (org/archive-subtree &rest args)
    "Override of `org-archive-subtree' that sets
     `org-archive-location' to the current daily file+olp."
    (let* ((daily-archive-file
            (concat org-roam-directory "/"
                    org-roam-dailies-directory "/"
                    (format-time-string "%Y W%W.org")))
           (org-archive-location
            (concat daily-archive-file "::"
                    (format-time-string "* %a, %d %b"))))
      (unless (file-exists-p daily-archive-file)
        (save-window-excursion
          ;; HACK: This visits the buffer for today after it's created,
          ;; which is not necessary, and we need `save-window-excursion'
          ;; to undo that.
          (org-roam-dailies-goto-today)))
      (apply org/archive-subtree args)))

  (defun amnn/consult-org-refile (&optional arg)
    "Pick a target to refile to using a two-level system: First pick the file
     using `org-roam-node-read' and then pick a heading in there using
     `consult-org-heading'."
    (interactive "P")

    (unless (derived-mode-p 'org-mode)
      (user-error "Must be called from an Org buffer"))

    (let* ((node (org-roam-node-read nil nil nil t "Refile: "))
           (file (org-roam-node-file node))

           (top-level-marker
            (with-current-buffer (org-find-base-buffer-visiting file)
              (point-min-marker)))

           (top-level-candidate "[top-level]")

           (lookup
            (lambda (selected candidates &rest _)
              (list selected (consult--lookup-prop 'org-marker
                                                   selected
                                                   candidates))))

           (return
            (lambda (cand)
              (let* ((pos (marker-position (cadr cand)))
                     (loc (list (car cand) file nil pos)))
                (org-refile arg nil loc))))

           (state* (consult--jump-preview))
           (state
            (lambda (action cand)
              (funcall state* action (cadr cand)))))

      ;; Annotate the special top-level candidate.
      (add-text-properties 0 1
       `(org-marker ,top-level-marker
         consult-org--heading (0 nil))
       top-level-candidate)

      ;; Save excursion to avoid changing the buffer to `node'.
      (save-excursion
        (consult--read
         (consult--slow-operation "Collecting headings..."
           (cons top-level-candidate
                 (consult-org--headings nil nil (list file))))
         :prompt "Refile to heading: "
         :category 'consult-org-heading
         :sort nil
         :require-match t
         :history '(:input consult-org--history)
         :narrow (consult-org--narrow)
         :state (consult--state-with-return state return)
         :lookup lookup))))

  (org-roam-db-autosync-mode)

  :custom
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n#+category: ${title}\n\n")
      :unnarrowed t)))
  (org-roam-dailies-capture-templates
   '(("d" "default" entry "** %?"
      :target (file+head+olp
               "%<%Y W%W>.org"
               "#+title: %<%Y W%W>\n#+category: %<%Y W%W>\n#+filetags: :journal:\n\n"
               ("%<%a, %d %b>"))
      :empty-lines 1
      :unnarrowed t)))
  (org-roam-dailies-directory "Journal/")
  (org-roam-db-location "~/Roam.db")
  (org-roam-directory "~/Roam")
  (org-roam-node-display-template
   (concat "${title:*}" (propertize "${tags:*}" 'face 'org-tag)))
  (org-roam-mode-section-functions
   (list #'org-roam-backlinks-section
         #'org-roam-reflinks-section
         #'org-roam-unlinked-references-section)))

(use-package org-roam-ui
  :straight (:host github
             :repo "org-roam/org-roam-ui"
             :branch "main"
             :files ("*.el" "out"))
  :custom
  (org-roam-sync-ui-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t))

(use-package org-super-agenda
  :ensure t
  :hook (org-agenda-mode . org-super-agenda-mode)

  :bind
  (:map evil-normal-state-map
        ("SPC t" . amnn/agenda))
  ;; Overrides conflicting super-agenda bindings
  (:map org-super-agenda-header-map
        ("j" . org-agenda-next-line)
        ("k" . org-agenda-previous-line))

  :custom
  (org-agenda-show-future-repeats 'next)
  (org-agenda-custom-commands
   '(("x" "Dashboard"
      ((tags-todo "/!"
                  ((org-agenda-overriding-header "")
                   (org-super-agenda-groups
                    '((:name "Oncall"
                             :and (:tag "oncall"
                                   :not (:scheduled future)))
                      (:name "Work"    :todo "WORK")
                      (:name "Inbox"   :file-path "Roam/Journal")
                      (:discard (:anything t))))))

       (agenda ""
               ((org-agenda-entry-types
                 '(:deadline :scheduled :timestamp :sexp))
                (org-agenda-filter "-SCHEDULED>=\"<+7d>\"/!")
                (org-agenda-overriding-header "")
                (org-agenda-skip-scheduled-delay-if-deadline t)
                (org-agenda-skip-scheduled-if-deadline-is-shown t)
                (org-agenda-skip-scheduled-if-done t)
                (org-agenda-span 7)
                (org-agenda-start-on-weekday nil)
                (org-deadline-warning-days 0)

                (org-habit-following-days 7)
                (org-habit-preceding-days 35)
                (org-habit-show-habits t)
                (org-habit-show-all-today t)
                (org-habit-show-habits-only-for-today t)

                (org-super-agenda-groups
                 '((:discard (:and (:scheduled future
                                    :habit t)))
                   (:discard (:todo ("DONE" "DROP")))
                   (:anything t :name nil)))))

       (tags-todo "-SCHEDULED={.+}-DEADLINE<=\"<+7d>\"/NEXT"
                  ((org-agenda-overriding-header "Next")))))))

  (org-agenda-hide-tags-regexp
   "[+-]agenda")

  :config
  (defun amnn/agenda ()
    (interactive)
    (org-agenda nil "x")))

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install))

;;; Large Language Models ================================================== ;;;

(use-package copilot
  :straight (:host github
             :repo "copilot-emacs/copilot.el"
             :files ("dist" "*.el"))
  :hook (prog-mode . copilot-mode)
  :bind
  (:map copilot-completion-map
        ("C-<tab>" . 'copilot-accept-completion)
        ("C-TAB"   . 'copilot-accept-completion)
        ("M-<tab>" . 'copilot-accept-completion-by-line)
        ("M-TAB"   . 'copilot-accept-completion-by-line)))

(use-package gptel
  :ensure t
  :config
  (setq gptel-default-mode #'org-mode)

  :bind
  (:map evil-normal-state-map
        ("SPC c" . gptel-send)
        ("SPC C" . gptel))
  (:map evil-visual-state-map
        ("SPC c" . gptel-send)))

;;; Language Server ======================================================== ;;;

(use-package eglot
  :ensure t
  :after evil

  :autoload
  (eglot-ensure)

  :bind
  (:map evil-normal-state-map
        ("SPC a" . eglot-code-actions)
        ("SPC q" . eglot-code-action-quickfix)
        ("SPC r" . eglot-rename)
        ("gE" . flymake-show-project-diagnostics)
	("gi" . eglot-find-implementation)
	("gd" . xref-find-definitions)
	("gD" . eglot-find-declaration)
	("gr" . xref-find-references)
	("gy" . eglot-find-typeDefinition))

  (:map evil-motion-state-map
	("[g" . flymake-goto-prev-error)
	("]g" . flymake-goto-next-error))

  :init
  (defun amnn/eglot-prefer-flymake-eldoc ()
    "Show errors/warnings from flymake in eldoc, over eglot's LSP based
     documentations."
    (push 'flymake-eldoc-function eldoc-documentation-functions))

  (defun amnn/disable-inlay-hints ()
    "Turn off inlay hints -- they make lines extra long, and the information is
     available contextually."
    (eglot-inlay-hints-mode -1))

  (defun amnn/disable-flymake ()
    "Disable flymake suggestions from eglot (if they are getting very noisy)."
    (remove-hook 'flymake-diagnostic-functions 'eglot-flymake-backend))

  ;; (add-hook 'eglot-managed-mode-hook #'amnn/disable-flymake)
  (add-hook 'eglot-managed-mode-hook #'amnn/eglot-prefer-flymake-eldoc)
  (add-hook 'eglot-managed-mode-hook #'amnn/disable-inlay-hints))

;;; Debugging ============================================================== ;;;

(use-package realgud :ensure t :defer t)

(use-package realgud-lldb
  :straight (:host github :repo "realgud/realgud-lldb" :branch "master")
  :commands (lldb))

;;; Language Major Modes =================================================== ;;;

(use-package clojure-mode :ensure t)

(use-package fish-mode :ensure t)

(use-package graphql-mode
  :ensure t)

(use-package haskell-mode
  :ensure t)

(use-package janet-mode
  :straight (:host github :repo "alschwalm/janet-mode" :branch "master"))

(use-package ijanet
  :straight (:host github :repo "serialdev/ijanet-mode")
  :after janet-mode
  :bind
  (:map janet-mode-map
        ("C-c C-b" . ijanet-eval-buffer)
        ("C-c C-e" . ijanet-eval-sexp-at-point)
        ("C-c C-l" . ijanet-eval-line)
        ("C-c C-p" . ijanet)
        ("C-c C-r" . ijanet-eval-region)))

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode))

(use-package move-mode
  :ensure t
  :hook   (move-mode . amnn/eglot-ensure-move)

  :config
  (add-to-list 'eglot-server-programs '(move-mode "move-analyzer"))

  (defun amnn/eglot-ensure-move ()
    "Only turn on eglot for Move files if we are in a project."
    (when (locate-dominating-file (buffer-file-name) "Move.toml")
      (eglot-ensure)))

  (defun amnn/move-lsp-project-root (dir)
    (and-let* (((boundp 'eglot-lsp-context))
               (eglot-lsp-context)
               (override (locate-dominating-file dir "Move.toml")))
      (cons 'Move.toml override)))

  (add-hook 'project-find-functions #'amnn/move-lsp-project-root)
  (cl-defmethod project-root ((project (head Move.toml)))
    (cdr project)))

(use-package nim-mode
  :ensure t
  :config
  (add-to-list 'eglot-server-programs
               '(nim-mode "~/.nimble/bin/nimlangserver")))

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

(use-package ron-mode
  :ensure t)

(use-package typescript-mode
  :ensure t
  :hook   (typescript-mode . eglot-ensure)
  :custom
  (typescript-indent-level 2)

  :config
  (add-to-list 'eglot-server-programs
               '(typescript-mode "typescript-language-server" "--stdio"))

  (defun amnn/ts-lsp-project-root (dir)
    (and-let* (((boundp 'eglot-lsp-context))
               (eglot-lsp-context)
               (override (locate-dominating-file dir "tsconfig.json")))
      (cons 'tsconfig.json override)))

  (add-hook 'project-find-functions #'amnn/ts-lsp-project-root)
  (cl-defmethod project-root ((project (head tsconfig.json)))
    (cdr project)))

(use-package wgsl-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

;;; Version Control ======================================================== ;;;

(use-package git-gutter-fringe
  :ensure t
  :demand fringe-helper
  :hook (prog-mode . git-gutter-mode)
  :bind
  (:map evil-motion-state-map
        ("]h" . git-gutter:next-hunk)
        ("[h" . git-gutter:previous-hunk))

  :config
  (define-fringe-bitmap 'git-gutter-fr:added
    [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified
    [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted
    [128 192 224 240] nil nil 'bottom))

(use-package magit
  :ensure t
  :bind
  (("C-x g"   . magit-status)
   ("C-x C-g" . magit-status)))

(use-package magit-tbdiff
  :ensure t)

(use-package git-link
  :straight (:host github :repo "sshaw/git-link" :tag "v0.8.6")
  :bind
  (:map evil-normal-state-map
        ("gh" . git-link)
        ("gH" . amnn/git-link-open-in-browser))

  :init
  (defun amnn/git-link-open-in-browser ()
    "Get the git-link for the current point or region and open it in
     the browser."
    (interactive)
    (let ((git-link-open-in-browser t))
      (call-interactively 'git-link))))

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

(use-package modus-themes
  :ensure t
  :config
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs   t
        modus-themes-common-palette-overrides
        '((fringe                    unspecified)
          (bg-mode-line-active       bg-blue-subtle)
          (fg-mode-line-active       fg-main)
          (border-mode-line-active   bg-mode-line-active)
          (border-mode-line-inactive bg-mode-line-inactive)
          (bg-tab-bar                bg-dim)
          (bg-tab-active             bg-main)
          (bg-tab-other              bg-dim))

        modus-themes-headings
        '((0 . (1.62))
          (1 . (1.20)))))

(use-package theming
  :straight nil :demand t :no-require t
  :after (exec-path-from-shell modus-themes)

  :custom
  (use-file-dialog nil)
  (use-dialog-box  nil)

  (default-frame-alist
    '((internal-border-width . 0)
      (right-fringe . 0)))

  (bookmark-set-fringe-mark nil)

  (window-divider-default-right-width 24)
  (window-divider-default-places 'right-only)

  :preface
  ;; Strangely, Extralight is heavier than Light
  (set-frame-font "Iosevka SS15-16:weight=light")

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
                  (concat "~/.config/kitty/ayu-" (symbol-name light-or-dark) ".conf")))

  (defun amnn/load-theme-matching-system (light-or-dark)
    "Pick which theme to run based on whether the system is light or dark."
    (pcase light-or-dark
      ('light (load-theme 'modus-operandi t))
      ('dark  (load-theme 'modus-vivendi  t)))

    (amnn/push-kitty-theme light-or-dark)

    (let ((background (face-attribute 'default :background)))
      (custom-set-faces
       `(bold                       ((t :weight      regular)))
       `(git-gutter-fr:added        ((t :background ,background
                                        :foreground "lime green")))
       `(git-gutter-fr:modified     ((t :background ,background
                                        :foreground "orange")))
       `(git-gutter-fr:deleted      ((t :background ,background
                                        :foreground "firebrick")))
       `(header-line                ((t :background ,background
                                        :slant       italic)))
       `(window-divider             ((t :foreground ,background)))
       `(window-divider-first-pixel ((t :foreground ,background)))
       `(window-divider-last-pixel  ((t :foreground ,background)))))

    (sml/setup))

  (add-hook 'ns-system-appearance-change-functions
            #'amnn/load-theme-matching-system))

;;; Misc. ================================================================== ;;;

(use-package exec-path-from-shell
  :ensure t
  :demand t
  :config
  (exec-path-from-shell-initialize))

(use-package emacs
  :hook
  (before-save . delete-trailing-whitespace)

  :custom
  ;; Auto-save org mode files in-place
  (auto-save-visited-interval 2)
  (auto-save-visited-predicate
   (lambda () (derived-mode-p 'org-mode)))

  ;; Store backups centrally, not next to file.
  (backup-directory-alist '(("" . "~/.emacs.d/backup")))
  (indent-tabs-mode nil)
  (mac-option-modifier 'meta)
  (require-final-newline t)
  (scroll-margin 5)
  ;; You will probably live to regret this, but copilot is very noisy
  (warning-minimum-level :emergency)
  (warning-suppress-log-types '((comp)))

  ;; Text editing
  (sentence-end-double-space nil)

  :config
  (auto-save-visited-mode))
