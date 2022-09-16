(setq find-program "c:\\cygwin64\\bin\\find.exe")

(defun amnn--try-project-root (dir)
  (and-let* ((override (locate-dominating-file dir ".project")))
    ;; Not really a VC-based project root, but do this to re-use existing
    ;; project.el methods
    (cons '.project override)))

(cl-defmethod project-root ((project (head .project)))
  (cdr project))

(global-set-key (kbd "C-x C-b") 'ibuffer)

(provide 'setup-buffers)
