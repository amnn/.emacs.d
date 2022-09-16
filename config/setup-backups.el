;; Don't store backups in the same directory as the file, store them centrally.
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))

(provide 'setup-backups)
