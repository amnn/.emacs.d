(electric-pair-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq require-final-newline t)

(provide 'setup-text-modes)

